import Qry._

import scala.sys.process._
import scala.collection.JavaConversions._
import java.io.File
import java.util.Properties

/**
*   An expanded representation of a task specification.
*/
case class ExpandedTask(baseArgs:List[String],
                        crossProductArgs:List[List[String]],
                        independentOrArgs:List[List[String]]) {
  /** Updates the cross product with a single new argument */
  private def updateCross(key:Option[String], value:ConcreteArgumentValue
      ):List[List[String]] = {
    key match {
      case Some(key) => crossProductArgs.map{ key :: value.get ::: _ }
      case None => crossProductArgs.map{ value.get ::: _ }
    }
  }
  
  /** Updates the piecewise product with a single new argument */
  private def updateIndependent(key:Option[String], value:ConcreteArgumentValue
      ):List[List[String]] = {
    key match {
      case Some(key) => independentOrArgs.map{ key :: value.get ::: _ }
      case None => independentOrArgs.map{ value.get ::: _ }
    }
  }

  /** Updates the 'base' (default) run with a given argument or argument piece
  *   @param str The argument to be appended
  *   @return A new task, updated with this argument
  */
  def appendToBase(str:ConcreteArgumentValue):ExpandedTask = {
    val newBase = str.get ::: baseArgs;
    ExpandedTask(newBase, updateCross(None, str), updateIndependent(None, str))
  }

  /** Updates the cross product of arguments with a given key/value pair.
  *   @param key The (optional) key of this key/value pair
  *   @param value The value of this key/value pair
  *   @return A new task, updated with this pair
  */
  def appendToCrossProduct(key:Option[String], value:ArgumentValue
      ):ExpandedTask = {
    val headString:String = value.head.get.head
    val headTail:List[String] = value.head.get.tail
    val newBase:List[String] = key match {
      case Some(key) => key :: headString :: headTail ::: baseArgs;
      case None => headString :: headTail ::: baseArgs;
    }
    var newCross:List[List[String]] = 
      if(crossProductArgs.size == 0) List(baseArgs)
      else crossProductArgs
    newCross = {key match {
      case Some(key) => newCross.map{ (args:List[String]) =>
                            value.all.map{ key :: _.get ::: args }}
      case None => newCross.map{ (args:List[String]) =>
                       value.all.map{ _.get ::: args }}
    }}.flatten
    ExpandedTask(newBase, newCross, updateIndependent(key,
                                      ConcreteStringValue(headString)))
  }
  
  /** Updates the piecewise (independent) 'or' arguments with a given key/value pair.
  *   @param key The (optional) key of this key/value pair
  *   @param value The value of this key/value pair
  *   @return A new task, updated with this pair
  */
  def appendToIndependentOr(key:Option[String], value:ArgumentValue
      ):ExpandedTask = {
    val headString:String = value.head.get.head
    val headTail:List[String] = value.head.get.tail
    val newBase:List[String] = key match {
      case Some(key) => key :: headString :: headTail ::: baseArgs;
      case None => headString :: headTail ::: baseArgs;
    }
    val newIndependent:List[List[String]] = key match {
      case Some(key) => value.tail.map{ key :: _.get ::: baseArgs } :::
                            updateIndependent(Some(key), value.head)
      case None => value.tail.map{ _.get ::: baseArgs } :::
                       updateIndependent(key, value.head)
    }
    ExpandedTask(newBase, updateCross(key, value.head), newIndependent)
  }

  /** The concrete instances that should be run given this expanded task
  *   @return A list of argument lists
  */
  def instances:List[List[String]] = {
    {if (crossProductArgs.size == 0) List(baseArgs) else Nil} :::
      crossProductArgs ::: independentOrArgs
  }
}

/**
*   An task specification, in general yielding many runs.
*/
case class Task(program:List[String], argsRev:List[Argument],
                postProcessRev:List[ProcessBuilder=>List[ProcessBuilder]],
                stdoutFile:Option[(File,Boolean)]) {

  override def toString:String = {
    val joined = processes(0).toString.replaceAll(", ", " ")
    joined.substring(1, joined.length - 1)
  }

  /** Cast this program as a single list of terms */
  def toList:List[String] = {
    val terms = argsRev.foldLeft(ExpandedTask(Nil, Nil, Nil)){
        case (task:ExpandedTask, arg:Argument) => arg(task);
      }.instances.map{ (dynamicArgs:List[String]) =>
          program.toList ::: (Task.propertiesToArgs(Qry.staticProperties) ::: dynamicArgs)
      }
    if (terms.length > 1) err("Program cannot be uniquely converted to list: " + toString)
    terms(0)
  }

  /** Cast this program as a single array of terms */
  def toArray:Array[String] = toList.toArray

  /**
   *  Expand the processes to be created by this task.
   */
  /* Here be dragons -- grok at your own risk */
  private def processes:List[(ProcessBuilder,Boolean=>String,Option[String])] = {
    // Create a shell script from a list of arguments
    def argsAsShell(args:List[String], runDir:Option[String]):Boolean=>String = {
      if (postProcessRev.length > 0) {
        (rerun:Boolean) => """echo "Qry cannot recreate piped processes""""
      } else {
        Task.argsAsShell(args, runDir)
      }
    }
    // Get the jobs created by exploding this task's arguments
    val procs:List[(ProcessBuilder, Boolean=>String, Option[String])]
      = argsRev.foldLeft(ExpandedTask(Nil, Nil, Nil)){
        case (task:ExpandedTask, arg:Argument) => arg(task);
      }.instances.map{ (dynamicArgs:List[String]) =>
            val args = program ::: (Task.propertiesToArgs(Qry.staticProperties) ::: dynamicArgs)
            val execDir = Task.ensureRunDir // get an execution directory
            (Process(execDir
              .map( dir =>
                args.map( _.replaceAll("ℵexecdir_thunkℵ", dir.getPath) ) )
              .getOrElse(args) ),
             argsAsShell(args, execDir.map( _.getPath )),
             execDir.map( _.getPath ))
          }
    // Get the procs created by postprocessing this task
    // For example, this includes the result of pipes.
    postProcessRev.foldRight(procs) {
          case (fn:(ProcessBuilder=>List[ProcessBuilder]),
                procs:List[(ProcessBuilder,String,Option[String])]) =>
      procs.map{ x => fn(x._1).map( (_, x._2, x._3) ) }.flatten
    }
  }

  /** 
  *   Create a specification of jobs to be run by the system.
  *   The jobs are not run yet as of this return.
  *   @return the jobs to be run
  */
  def jobs:List[Job] = {
    // Pipe procs to to a file, if applicable.
    // Then, convert them to Job objects
    {stdoutFile match {
      case Some((file, append)) =>
        if (append) processes.map( x => (x._1 #>> file, x._2, x._3) )
        else processes.map( x => (x._1 #> file, x._2, x._3) )
      case None => processes
    }}.map{ case (proc, bash, dir) => Job(proc, false, None, bash, dir) }
  }

  /** 
  *   Add an argument to task.
  *   @param arg The argument ot append
  *   @return This task, with the argument appended
  */
  private def addArgument(arg:Argument):Task
    = new Task(program, arg :: argsRev, postProcessRev, stdoutFile)

  /** @see addArgument */
  def -(arg:Argument):Task = addArgument(arg)
  
  /** 
  *   Register a textual argument (i.e., without the leading '-')
  *   @param arg The argument's value
  *   @return This task, with the argument appended
  */
  def arg(argValue:ArgumentValue):Task = addArgument(Argument(argValue))
  
  /**
  *  Adds a product (list/tuple/etc) of arguments to the task
  *  @param arg The argument collection
  *  @return Returns the task with all arguments added
  */
  def arg(argValues:Product):Task = {
    (0 until argValues.productArity).foldLeft(this){ case (task:Task, i:Int) =>
      task.addArgument(
        Argument(guessArgumentValue(argValues.productElement(i))))
    }
  }
  
  /** @see addArgument */
  def ->(argValue:ArgumentValue):Task = arg(argValue)
  
  /** @see addArgument */
  def ->(argValues:Product):Task = arg(argValues)

  /** 
  *   Register a flag without a value
  *   @param arg The flag
  *   @return This task, with the flag appended
  */
  def flag(arg:ArgumentKey):Task = addArgument(Argument(arg))
  
  /** @see flag */
  def -(key:ArgumentKey, value:ArgumentValue):Task = flag(key, value)

  /** 
  *   Register a flag, with a value
  *   @param arg The flag
  *   @return This task, with the flag appended
  */
  def flag(key:ArgumentKey, value:ArgumentValue):Task
    = addArgument(Argument(key, value))

  /** @see flag */
  def -(arg:ArgumentKey):Task = flag(arg)

  /** Pipe the output of this task to a file. This is executed after
  *   all standard pipes ('|') have been executed.
  *   @param file The file to output to
  *   @return A task which will eventually output stdout to the file
  */
  def >(file:File):Task = {
    Task(program, argsRev, postProcessRev, Some((file, false)))
  }

  /** Append the output of this task to a file. This is executed after
  *   all standard pipes ('|') have been executed.
  *   @param file The file to append to
  *   @return A task which will eventually output stdout to the file
  */
  def >>(file:File):Task = {
    Task(program, argsRev, postProcessRev, Some((file, true)))
  }

  /** Pipe the output of this task into another task.
  *   If each task defines multiple jobs, this will take the cross-product
  *   of the jobs. That is, it will start every instance of the second job
  *   given every output of the first job.
  *   @param file The task the output of this task is being appended to
  *   @return The piped task
  */
  def |(task:Task):Task = {
    if (stdoutFile.isDefined) {
      err("Cannot pipe ('|') after piping to a file ('>')")
    }
    // TODO(gabor): I think exec directories get messed up here...
    Task(program, argsRev,
         {(p:ProcessBuilder) => task.processes.map{ p #| _._1 }} :: postProcessRev,
         task.stdoutFile)
  }

  /** Repeat this task n times, with values evaluated lazily
  *   @param repeatCount The number of times to repeat the task
  *   @return An iterator of tasks to run
  */
  def *(repeatCount:Int):Iterator[Task] = {
    var index = 0;
    def self:Task = Task( program, argsRev, postProcessRev, stdoutFile)
    new Iterator[Task] {
      override def hasNext:Boolean = index < repeatCount
      override def next:Task = { index += 1; self }
    }
  }

  def repeat(repeatCount:Int) = this * repeatCount
}

object Task {
  private val runDirLock = new scala.concurrent.Lock
  private val RUN_FILE = """^.*/([0-9]+)$""".r
  /**
   *  Ensure that the run directory is created, and return its path.
   *  @return The path to the run directory, where stats and results are logged.
  */
  def ensureRunDir:Option[File] = {
    try {
      execRoot.map { (root:String) =>
        runDirLock.acquire
        val runs = new File(root).listFiles
          .map { _.getAbsolutePath }
          .map { _ match { case RUN_FILE(run) => Some(run.toInt)
                           case _ => None } }
          .filter( _.isDefined ).map( _.get )
          .sortWith( _ > _ )
        val lastRun = if (runs.isEmpty) -1 else runs.head.toInt
        runDirLock.release
        val runDir = new File(root + "/" + (lastRun + 1))
        runDir.mkdir
        runDir
      }
    } catch {
      case (e:Exception) => err(e.getMessage); None
    }
  }

  def argsAsShell(args:Seq[String], runDir:Option[String]):Boolean=>String = {
    (rerun:Boolean) => {
      val sh = new StringBuilder
      // (program name)
      sh.append("'").append(args.head).append("'\\\n\t")
      // (arguments)
      var lastDash = false
      args.tail.foreach{ (uncleanArg:String) =>
        val arg = uncleanArg.replaceAll("'", "\\'")
                            .replaceAll("ℵexecdir_thunkℵ",
                                        runDir.getOrElse("/tmp/qry") +
                                          {if (rerun) "/_rerun" else ""})
        if (arg.startsWith(Qry.dash_value)) {
          if (lastDash) sh.append("\\\n\t")
          sh.append("'").append(arg).append("' ")
          lastDash = true
        } else {
          sh.append("'").append(arg).append("'\\\n\t")
          lastDash = false
        }
      }
      // (return)
      sh.substring(0, sh.lastIndexOf('\\'))
    }
  }

  def propertiesToArgs(props:Properties):List[String] = {
    {for (key:Any <- props.keySet) yield {
      val value = props.get(key)
      assert (value != null)
      List[String]( Qry.dash_value + key.toString, value.toString )
    }}.toList.flatten
  }
}
