import scala.collection.JavaConversions._
import scala.sys.process._
import java.util.concurrent._
import java.util.Properties
import java.io.File

/**
*   The main object for interacting with Qry.
*/
object Qry {
  //
  // Configuraiton
  //
  /** The character to use for flags. */
  var dash_value = "-"
  /** Set a new dash character. */
  def dash(newDash:String):Unit = { dash_value = newDash }
  /** Get the dash character. */
  def dash():String = dash_value
  
  /** The thread pool to use for starting tasks. */
  var executor = Executors.newFixedThreadPool(1)
  /** The size of the thread pool to use for starting tasks. */
  var threadPoolSize = 1
  /** Set the size of the thread (process) pool. */
  def parallel(threadCount:Int
      ):{def submit(t:Task); def submit(t:Iterator[Task])} = {
    executor = Executors.newFixedThreadPool(if (usingPBS) 1 else  threadCount)
    threadPoolSize = threadCount
    new {
      def submit(t:Task):Unit = Qry.submit(t)
      def submit(t:Iterator[Task]):Unit = Qry.submit(t)
    }
  }
  /** @see parallel */
  def parallel:{def submit(t:Task); def submit(t:Iterator[Task])}
    = parallel(Runtime.getRuntime.availableProcessors)
  /** Get the size of the thread (process) pool. */
  def procs:Int = threadPoolSize

  //
  // "Private" Configuration
  //
  /** The root execution directory, where runs would be logged */
  var execRoot:Option[String] = None

  def touch(relativeFilename:String):String = "ℵexecdir_thunkℵ/" + relativeFilename
  
  /** Properties to prepend to every run */
  val staticProperties:Properties = new Properties

  var usingPBS = false

  //
  // Using keyword
  //
  def input(spec:String, force:Boolean = true):Boolean = {
    val specFile = new File(spec)
    if (force ||
        (!spec.endsWith("/") && specFile.canRead() && !specFile.isDirectory)) {
      // Check to make sure we have plugin
      if (!Plugins.haveTypesafeConfig) {
        err("Cannot load input " + spec + " without Typesafe Config in classpath")
      }
      // Load Input
      TypesafeConfigPlugin.appendProperties(staticProperties, specFile)
      true
    } else {
      false
    }
  }

  def execdir(spec:String, force:Boolean = true):Boolean = {
    val root = new File(spec)
    if (!(force || spec.endsWith("/")) &&
        (!root.canRead() || !root.isDirectory)) {
      return false
    }
    // Create and error check root execdir directory
    if (root.exists && !root.isDirectory) {
      err("Argument to execdir is not a directory: '" + spec + "'")
    }
    if (!root.exists) {
      if (!root.mkdirs) {
        err("Could not create directory: '" + spec + "'")
      }
    }
    if (!root.canRead) {
      err("Cannot read directory: '" + spec + "'")
    }
    // Set variable
    execRoot = Some(root.getAbsolutePath);
    return true
  }
  
  def redis(spec:String, force:Boolean = true):Boolean = {
    return false
  }

  def remote(spec:String, force:Boolean = true):Boolean = {
    return false
  }

  def pbs(spec:String, force:Boolean = true):Boolean = {
    if (spec.equalsIgnoreCase("pbs") || spec.equalsIgnoreCase("qsub") ||
        spec.equalsIgnoreCase("nlpsub")) {
      usingPBS = true
      true
    } else {
      false
    }
  }

  def using(spec:String):Unit = {
    val success = redis(spec, false) ||
                  remote(spec, false) ||
                  input(spec, false) ||
                  pbs(spec, false) ||
                  execdir(spec, false)
    if (!success) {
      var user = System.getenv("USER")
      if (user == null) user = "[user]"
      err("Could not parse argument to using: '" + spec + "'. Did you mean:" +
          "\n\t? '" + spec + "'       (properties file: doesn't exist)" +
          "\n\t? '" + spec + "/'      (execution directory)" +
          "\n\t? '" + user + "@" + spec + "'  (remote machine)" +
          "\n\t? 'redis:" + spec + "' (qry database)" +
          "")
    }
  }
  
  //
  // Utilities
  //
  def $(key:String):String = System.getenv(key)
  def $(key:Symbol):String = $(key.name)
  
  //
  // Top Level Entry Points
  //
  /** Submit a task for running. This is the usual entry point.
  *   @param task The task to run
  */
  def submit(task:Task,
             whenDone:()=>Unit = { () => },
             shutdownAtEnd:Boolean = true):Unit = {
    beginLock.acquire
    val jobs:List[Job] = task.jobs
    jobs.foreach{ _.queue(false, whenDone) }
    println("-- " + jobs.length + " job" + {if(jobs.length > 1) "s" else ""} +
      " submitted")
    beginLock.release
    if (shutdownAtEnd) { shutdown }
  }
  
  /** Submit a task a number of times for running.
  *   @param task The task to run
  */
  def submit(tasks:Iterator[Task]):Unit = {
    // -- Setup
    import java.util.concurrent.atomic.AtomicInteger
    var activeTasks = new AtomicInteger(0)
    tasks.zipWithIndex.foreach{ case (task:Task, i:Int) =>
      while (activeTasks.get > threadPoolSize) {
        Thread.sleep(1000);
      }
      new Thread {
        override def run:Unit = {
          // -- Submit particular job
          activeTasks.addAndGet(1)
          println("-- Repeat iteration " + i)
          submit(task, () => activeTasks.addAndGet(-1), false)
        }
      }.run
    }
    // -- Await termination
    shutdown
  }
  
  /** Start a set of task, but abandon it to the wind as an asynchronous task */
  def async(tasks:Iterator[Task]):Unit = {
    new Thread() {
      override def run:Unit = {
        submit(tasks)
      }
    }.start
  }
  
  /** Start a task, but abandon it to the wind as an asynchronous task */
  def async(task:Task,
            whenDone:()=>Unit = { () => },
            shutdown:Boolean = false):Unit = {
    new Thread() {
      override def run:Unit = {
        submit(task, whenDone, shutdown)
      }
    }.start
  }

  def shutdown:Unit = {
    executor.shutdown
    executor.awaitTermination(42000, java.util.concurrent.TimeUnit.DAYS)
    executor = Executors.newFixedThreadPool(threadPoolSize)
  }
  
  
  /** Explain what Qry would do for a given task, printing out the results.
  *   This is useful for, e.g., debugging how many runs will be started
  *   without actually starting the runs.
  *   @param task The task to analyze
  */
  def explain(task:Task) {
    val jobs:List[Job] = task.jobs
    println("-- Begin jobs (" + jobs.length + "):")
    jobs.foreach{ (job:Job) => println(job) }
    println("-- " + jobs.length + " job" + {if(jobs.length > 1) "s" else ""})
    if (procs != 1)
      println("--   " + procs + " processes at a time")
    if (dash != "-")
      println("--   '" + dash + "' as as the argument prefix")
  }
  
  //
  // Internal Helpers
  //
  /** Register an error and exit.
  *   @param msg The error message
  */
  def err(msg:Any):Unit = {
    println(msg)
    System.exit(1)
  }
  /** A lock to help with pretty printing */
  val beginLock = new scala.concurrent.Lock

  def guessArgumentValue(arg:Any):ArgumentValue = arg match {
    case (str:String) => ArgumentValue(str)
    case (sym:Symbol) => ArgumentValue(sym.name)
    case (fn:(()=>Any)) => ArgumentValue(fn)
    case _ => ArgumentValue(arg.toString)
  }
  
  //
  // Implicits
  // Here be dragons and magic.
  //
  /** Create a job directly from a string */
  implicit def string2job(cmd:String):Job
    = new Job(Process(cmd), true, None, (rerun:Boolean) => cmd, Task.ensureRunDir.map( _.getPath ))
  implicit def symbol2job(cmd:Symbol):Job
    = new Job(Process(cmd.name), true, None, (rerun:Boolean) => cmd.name, Task.ensureRunDir.map( _.getPath ))
  /** Create a job directly from a list of program name + arguments */
  implicit def list2job(cmd:List[String]):Job
    = new Job(Process(cmd), true, None,
              Task.argsAsShell(cmd, Task.ensureRunDir.map( _.getPath )),
              Task.ensureRunDir.map( _.getPath ))
  /** Create a task directly */
  implicit def string2task(programName:String):Task = new Task(List(programName), Nil, Nil, None)
  implicit def symbol2task(programName:Symbol):Task = new Task(List(programName.name), Nil, Nil, None)
  implicit def list2task(program:Iterable[String]):Task = new Task(program.toList, Nil, Nil, None)
  
  /** Create an argument from a pair of (ArgumentKey, ArgumentValue) */
  implicit def pair2argument(arg:(ArgumentKey,ArgumentValue)):Argument = Argument(arg._1, arg._2)
  /** Create an argument key from a string */
  implicit def string2argument_key(arg:String):ArgumentKey = ArgumentKey(arg)
  /** Create an argument value from a string */
  implicit def string2argument_value(arg:String):ArgumentValue = ArgumentValue(arg)
  /** Create an argument key from a symbol */
  implicit def symbol2argument_key(arg:Symbol):ArgumentKey = ArgumentKey(arg.name)
  /** Create an argument value from a symbol */
  implicit def symbol2argument_value(arg:Symbol):ArgumentValue = ArgumentValue(arg.name)
  /** Create an argument value from a Function */
  implicit def fn2argument_value(arg:(()=>Any)):ArgumentValue = ArgumentValue(arg)
  /** Create an argument key from a Number / Boolean*/
  implicit def anyval2argument_key(arg:AnyVal):ArgumentKey = ArgumentKey(arg.toString)
  /** Create an argument value from a Number / Boolean */
  implicit def anyval2argument_value(arg:AnyVal):ArgumentValue = ArgumentValue(arg.toString)
  
  /** Create a File from a String filename */
  implicit def string2file(filename:String):File = new File(filename)
  
  //
  // Queryable Command Line
  //
  def main(args:Array[String]):Unit = {
    //(imports)
    import scala.tools.nsc.interpreter.{IMain,JLineReader,JLineCompletion}
    import scala.tools.nsc.Settings
    // (args)
    if (args.length > 0) {
      args.foreach{ using(_) }
    }
    //(create interpreter)
    val settings = new Settings
    settings.usejavacp.value = true
    val interpreter:IMain = new IMain(settings)
    val reader:JLineReader = new JLineReader(new JLineCompletion(interpreter))
    //(initial commands)
    interpreter.interpret("import Qry._")
    interpreter.interpret("import QrySQL._")
    //(REPL)
    var cond = true
    while(cond){
      try {
        val str = reader.readLine("qry> ")
        interpreter.interpret(str)
      } catch {
        case (e:Exception) => e.printStackTrace
      }
    }
  }
}

object Plugins {
  def haveTypesafeConfig:Boolean = {
    try {
      Class.forName("com.typesafe.config.ConfigFactory")
      true
    } catch {
      case (e:ClassNotFoundException) => false
    }
  }
}

//
// Utilities
//
def powerset(elems:String*):ArgumentValue = {
  // recursive function
  @annotation.tailrec
  def pwr(s: List[String], acc: List[List[String]]): List[List[String]] = s match {
    case Nil     => acc
    case a :: as => pwr(as, acc ::: (acc map (a :: _)))
  }
  // call function
  val ps:List[List[String]] = pwr(elems.toList, Nil :: Nil).filter( _.size > 0 )
  // sanity check
  if (ps.length > 1000) {
    println("WARNING: the power set of " + elems.mkString(",") + " is very large!")
    println("(" + ps.length + " options)")
    print("Are you sure you want to continue (true/false)? ")
    if (!scala.io.StdIn.readBoolean) {
      System.exit(0)
    }
  }
  // convert to options
  ps.tail.foldLeft(ArgumentValue(ps.head.mkString(","))){
    case (soFar:ArgumentValue, elem:List[String]) =>
      soFar | elem.mkString(",")
  };
}

