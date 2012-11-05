import Qry._

import scala.sys.process._
import java.io._
import java.util.Date

/**
*   A concrete job to be run.
*/
object Job {
  val RUN_FILE = """^.*/([0-9]+)$""".r

  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }

  def write(fileName:String, data:String) = 
    using (new FileWriter(fileName)) {
      fileWriter => fileWriter.write(data)
    }
  
  def append(fileName:String, textData:String) =
    using (new FileWriter(fileName, true)){ 
      fileWriter => using (new PrintWriter(fileWriter)) {
        printWriter => printWriter.println(textData)
      }
    }

  def formatTimeDifference(diff:Long, b:StringBuilder):StringBuilder = {
    //--Get Values
    val mili = (diff % 1000).toInt
    var rest = diff / 1000
    val sec = (rest % 60).toInt
    rest = rest / 60
    val min = (rest % 60).toInt
    rest = rest / 60
    val hr = (rest % 24).toInt
    rest = rest / 24
    val day = rest.toInt
    //--Make String
    if(day > 0) b.append(day).append(if (day > 1) " days, " else " day, ")
    if(hr > 0) b.append(hr).append(if (hr > 1) " hours, " else " hour, ")
    if(min > 0) {
      if(min < 10){ b.append("0") }
      b.append(min).append(":")
    }
    if(min > 0 && sec < 10){ b.append("0") }
    b.append(sec).append(".").append(mili)
    if(min > 0) b.append(" minutes")
    else b.append(" seconds")
  }
}

case class Job(proc:ProcessBuilder, var isQueued:Boolean, var status:Option[Int]) {
  import Job._

  private def ensureRunDir:Option[File] = {
    try {
      execRoot.map { (root:String) =>
        val runs = new File(root).listFiles
          .map { _.getAbsolutePath }
          .map { _ match { case RUN_FILE(run) => Some(run.toInt)
                           case _ => None } }
          .filter( _.isDefined ).map( _.get )
          .sortWith( _ > _ )
        val lastRun = if (runs.isEmpty) -1 else runs.head.toInt
        val runDir = new File(root + "/" + (lastRun + 1))
        runDir.mkdir
        runDir
      }
    } catch {
      case (e:Exception) => err(e.getMessage); None
    }
  }
  
  /**
  *   Queue a task to be run.
  *   If a thread pool is set, this will submit the
  *   task to the thread pool. If the job is set up to run on the cluster,
  *   it will submit an appropriate job. Else, it will run the job outright.
  *   @param force Force running this task, even if the isQueued bit is set.
  */
  def queue(force:Boolean, whenDone:(()=>Unit)):Unit = {
    if (force || !isQueued) {
      executor.submit( new Runnable {
        override def run:Unit = {
          // Collect helpful info
          val start:Long = System.currentTimeMillis
          // Run the program
          status = Some(proc.!)  // <-- RUN
          // Write helpful info
          ensureRunDir match {
            case Some(runDir) =>
              val b:StringBuilder = new StringBuilder
              b.append("Qry Run Statistics\n")
              b.append("------------------\n")
              b.append("\n")
              b.append("begin time:   ")
              b.append(new Date(start).toString).append("\n")
              b.append("end time:     ")
              b.append(new Date(System.currentTimeMillis).toString).append("\n")
              b.append("elapsed time: ")
              formatTimeDifference(System.currentTimeMillis - start, b).append("\n")
              write(runDir + "/_qry.stats", b.toString)
            case None => // do nothing
          }
          whenDone()
        }
      })
    }
  }
  
  /**
  *   Queue a task to be run; @see Queue(force)
  */
  def queue:Unit = queue(false, ()=>{})

  override def toString:String = proc.toString

  // Constructor
  if (isQueued) queue(true, ()=>{})  // Optionally start running right away
}
