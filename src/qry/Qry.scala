import scala.collection.JavaConversions._
import scala.sys.process._
import java.util.concurrent._
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
    executor = Executors.newFixedThreadPool(threadCount)
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
  
  //
  // Using keyword
  //
  def input(spec:String, force:Boolean = true):Boolean = {
    return false
  }

  def execdir(spec:String, force:Boolean = true):Boolean = {
    if (!force && !spec.endsWith("/")) return false
    // Create and error check root execdir directory
    val root = new File(spec)
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

  def using(spec:String):Unit = {
    val success = redis(spec, false) ||
                  remote(spec, false) ||
                  input(spec, false) ||
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
             shutdown:Boolean = true):Unit = {
    beginLock.acquire
    val jobs:List[Job] = task.jobs
    jobs.foreach{ _.queue(false, whenDone) }
    println("-- " + jobs.length + " job" + {if(jobs.length > 1) "s" else ""} +
      " submitted")
    beginLock.release
    if (shutdown) {
      executor.shutdown
      executor.awaitTermination(42000, java.util.concurrent.TimeUnit.DAYS)
      executor = Executors.newFixedThreadPool(threadPoolSize)
    }
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
    executor.shutdown
    executor.awaitTermination(42000, java.util.concurrent.TimeUnit.DAYS)
    executor = Executors.newFixedThreadPool(threadPoolSize)
  }
  
  /** Analyze what Qry would do for a given task, printing out the results.
  *   This is useful for, e.g., debugging how many runs will be started
  *   without actually starting the runs.
  *   @param task The task to analyze
  */
  def analyze(task:Task) {
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
    = new Job(Process(cmd), true, None, cmd, Task.ensureRunDir.map( _.getPath ))
  /** Create a job directly from a list of program name + arguments */
  implicit def list2job(cmd:List[String]):Job
    = new Job(Process(cmd), true, None,
              Task.argsAsShell(cmd, Task.ensureRunDir.map( _.getPath )),
              Task.ensureRunDir.map( _.getPath ))
  /** Create a task directly */
  implicit def string2task(programName:String):Task = new Task(programName, Nil, Nil, None)
  
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
		interpreter.interpret("import QryLang._")
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
    reader.reset
  }
}
