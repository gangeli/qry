import Qry._

import scala.sys.process._
import scala.collection.mutable.HashMap
import java.io._
import java.util.Date
import java.util.concurrent.ThreadPoolExecutor

/**
*   An object storing utilities for the Job class
*/
object Job {
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

  def json(o:Any):String = "\"" + o.toString.replaceAll("\"", "\\\"") + "\""
}

/**
*   A concrete job to be run.
*/
case class Job(proc:ProcessBuilder, var isQueued:Boolean,
               var status:Option[Int], bashCmd:Boolean=>String, execDir:Option[String]) {
  import Job._
  
  /**
  *   Queue a task to be run.
  *   If a thread pool is set, this will submit the
  *   task to the thread pool. If the job is set up to run on the cluster,
  *   it will submit an appropriate job. Else, it will run the job outright.
  *   @param force Force running this task, even if the isQueued bit is set.
  */
  def queue(force:Boolean, whenDone:(()=>Unit)):Unit = {
    if (force || !isQueued) {
      // -- Get Code
      val code = new Runnable {
        override def run:Unit = {
          // Collect helpful info
          val start:Long = System.currentTimeMillis
          var results = HashMap[String, String]()
          // Save configuration
          // (git revision)
          var gitRevision:Option[String] = None
          try {
            if ("""git rev-parse --verify HEAD""" ! ProcessLogger(
                    out => gitRevision = Some(out),
                    err => ()
                  ) == 0 && !gitRevision.isDefined) {
              Qry.err("Git command did not output revision SHA")
            }
          } catch {
            case (e:Throwable) => { e.printStackTrace; Qry.err("Exception in getting Git revision") }
          }
          // (bash file)
          execDir match {
            case Some(runDir) =>
              write(runDir + "/_rerun.sh",
                "#!/bin/sh\n" +
                "#\n" +
                "# Automatically generated with Qry (https://github.com/gangeli/qry)\n\n" +
                "# Restore working directory\n" +
                """cd """ + System.getProperty("user.dir") + "\n" + 
                { gitRevision match {
                  case Some(sha) =>
                    "# Manage Git\n" +
                    """if [ `git rev-parse --verify HEAD` != """ +
                       "\"" + sha + "\"" + """ ]; then """ +
                       """echo "WARNING: Git revision has changed from """ +
                       sha + "\"" + """; fi""" + "\n"
                  case None => ""
                }} +
                "mkdir -p " + runDir + "/_rerun" +
                "\n" +
                "# Run Program\n" +
                bashCmd(true))
            case None =>
          }
          // Run the program
          status = if (Qry.usingPBS) {    // Run on PBS
            PBS.run(bashCmd(false), execDir)
          } else if (Qry.usingSlurm) {    // Run on Slurm
            Slurm.run(bashCmd(false), execDir)
          } else {
            import ResultRegex._
            Some(proc !< ProcessLogger(
              {(out:String) => 
                out match {
                  case ExplicitResult(key, value) => results(key.trim) = value.trim
                  case ImplicitResultNumeric(key, value) => results(key.trim) = value.trim
                  case _ => 
                }
                println(out)
              },
              { (err:String) =>
                err match {
                  case ExplicitResult(key, value) => results(key.trim) = value.trim
                  case ImplicitResultNumeric(key, value) => results(key.trim) = value.trim
                  case _ => 
                }
                  System.err.println(err)
              }
            ))
          }
          // Write info
          execDir match {
            case Some(runDir) =>
              val b:StringBuilder = new StringBuilder
              b.append("{\n")
              // (time)
              b.append("  \"begin_time\":   ")
              b.append(json(new Date(start).toString)).append(",\n")
              b.append("  \"end_time\":     ")
              b.append(json(new Date(System.currentTimeMillis).toString)).append(",\n")
              b.append("  \"elapsed_time\": \"")
              formatTimeDifference(System.currentTimeMillis - start, b).append("\",\n")
              b.append("  \"exit_status\": " + status.getOrElse(0) + ",\n")
              b.append("\n")
              // (run info)
              b.append("  \"host\":         ")
              b.append(json(java.net.InetAddress.getLocalHost().getHostName())).append(",\n")
              b.append("  \"working_dir\":  ")
              b.append(json(System.getProperty("user.dir"))).append(",\n")
              b.append("  \"run_dir\":      ")
              b.append(json(runDir)).append(",\n")
              // (git revision info)
              gitRevision match {
                case Some(revisionSHA) =>
                  b.append("  \"git_rev\":      ")
                  b.append(json(revisionSHA)).append(",\n")
                case None => 
              }
              b.append("\n")
              // (results)
              results.map{ case (key:String, value:String) =>
                "  " + json(key) + ": " + json(value) 
              }.toArray.mkString(",\n")
              b.append("\n}")
              write(runDir + "/_qry.json", b.toString)
            case None => // do nothing
          }
          // Write results
          execDir.foreach{ (dir:String) =>
            println("-- stats saved to " + dir)
          }
          // Signal completion
          whenDone()
        }
      }
      // -- Submit
      executor.submit(code)
    }
  }
  
  /**
  *   Queue a task to be run; @see Queue(force)
  */
  def queue:Unit = queue(false, ()=>{})

  /** {@inheritDoc} */
  override def toString:String = bashCmd(false).toString

  // Constructor
  if (isQueued) queue(true, ()=>{})  // Optionally start running right away
}

object ResultRegex {
  /** A result of the form "result key = value" */
  val ExplicitResult
   = """^.*result[\s\]]*[:\-\s]\s*(.+)\s*[=:\-]>?\s*([^>].+)\s*(?iu)$""".r

  /** A result of the form "key = number" */
  val ImplicitResultNumeric
   = """^\s*(.+)\s*[=:\-]>?\s*([0-9\.\-e\%\(\)\$]+)\s*(?iu)$""".r
}
