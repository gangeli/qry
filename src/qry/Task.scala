import Qry._

import scala.sys.process._
import java.io.File

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
    ExpandedTask(newBase, newCross, updateIndependent(key, headString))
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
case class Task(program:String, argsRev:List[Argument],
                postProcessRev:List[ProcessBuilder=>List[ProcessBuilder]],
                stdoutFile:Option[(File,Boolean)]) {

  /** Expand the processes to be created by this task */
  private def processes:List[ProcessBuilder] = {
    // Get the jobs created by exploding this task's arguments
    val procs:List[ProcessBuilder]
      = argsRev.foldLeft(ExpandedTask(Nil, Nil, Nil)){
        case (task:ExpandedTask, arg:Argument) => arg(task);
      }.appendToBase(program).instances.map{ x => Process(x) }
    // Get the procs created by postprocessing this task
    // For example, this includes the result of pipes.
    postProcessRev.foldRight(procs) {
          case (fn:(ProcessBuilder=>List[ProcessBuilder]), procs:List[ProcessBuilder]) =>
      procs.map{ fn(_) }.flatten
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
        if (append) processes.map( _ #>> file )
        else processes.map( _ #> file )
      case None => processes
    }}.map( Job(_, false, None) )
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
    Task(program, argsRev,
         {(p:ProcessBuilder) => task.processes.map{ p #| _ }} :: postProcessRev,
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
