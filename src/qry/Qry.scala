//(scala)
import scala.collection.JavaConversions._
import scala.sys.process._
//(java)
import java.util.concurrent._

/**
*   The main object for interacting with Qry.
*/
object Qry {
  //
  // Helpers
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
  var executor = new ScheduledThreadPoolExecutor(1);
  /** The size of the thread pool to use for starting tasks. */
  var threadPoolSize = 1
  /** Set the size of the thread (process) pool. */
  def procs(threadCount:Int):Unit = {
    executor = new ScheduledThreadPoolExecutor(threadCount);
    threadPoolSize = threadCount
  }
  /** Get the size of the thread (process) pool. */
  def procs:Int = threadPoolSize
  
  //
  // Implicits
  // Here be dragons and magic.
  //
  /** Create a job directly from a string */
  implicit def string2job(cmd:String):Job = new Job(Process(cmd), true, None)
  /** Create a job directly from a list of program name + arguments */
  implicit def list2job(cmd:List[String]):Job
    = new Job(Process(cmd), true, None)
  /** Create a task directly */
  implicit def string2task(programName:String):Task = new Task(programName, Nil)
  
  /** Create an argument from a pair of (ArgumentKey, ArgumentValue) */
  implicit def pair2argument(arg:(ArgumentKey,ArgumentValue)):Argument
      = Argument(arg._1, arg._2)
  /** Create an argument key from a string */
  implicit def string2argument_key(arg:String):ArgumentKey = ArgumentKey(arg)
  /** Create an argument value from a string */
  implicit def string2argument_value(arg:String):ArgumentValue
    = new ArgumentValue(arg, Nil, 'none)
  
  
  //
  // Top Level Entry Points
  //
  /** Submit a task for running. This is the usual entry point.
  *   @param task The task to run
  */
  def submit(task:Task):Unit = {
    beginLock.acquire
    val jobs:List[Job] = task.jobs
    jobs.foreach{ _.queue }
    executor.shutdown
    println("-- " + jobs.length + " job" + {if(jobs.length > 1) "s" else ""} +
      " submitted")
    beginLock.release
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
    executor.shutdown
  }
}
