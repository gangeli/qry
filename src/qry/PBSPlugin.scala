import scala.collection.JavaConversions._
import scala.sys.process._

import java.io._

object PBS {
  /** The name of the PBS job */
  var name:String = ""
  /** The queue to put the PBS job on (default Queue.VERYLONG) */
  var queue:String = Queue.NLP
  /** The priority to rub PBS job on (default Priority.NORMAL) */
  var priority:String = Priority.NORMAL
  /** The memory to allocate for the job, if it's not autodetected (default 2gb)*/
  var memory:String = "2gb"
  /** The cores to allocate for the job (default 1) */
  var cores:Int = 1
  /** The cores to allocate for the job (default 1) */
  var hosts:List[String] = List[String]()
  
  /** Scrape just the memory from the bash command*/
  private def detectMemory(bashCmd:String):String = {
    val MEMORY_REGEX = """.*\s--?X?mx([0-9]+..)(\s|\\|\n).*""".r
    bashCmd match {
      case MEMORY_REGEX(x, separator) => x
      case _ => memory
    }
  }

  /** Create the resources string for the qsub command */
  private def resources(bashCmd:String):List[String] = {
    List[String](
      "-l", "mem=" + detectMemory(bashCmd),
      "-l", "nodes=1:" + "ppn=" + cores)
  }

  /** Copy over only specific environment variables */
  private def mkEnv(bashCmd:String):String = {
    def clean(s:String):String = s.replace(""""""", """\"""" )
    ( sys.env.filter{ case (key, value) => 
         key.toLowerCase != "krb5ccname"  &&
         key.toLowerCase != "ls_colors"  &&
         key.toLowerCase != "lscolors"  &&
         !key.toLowerCase.startsWith("ssh_") &&
         !value.contains(" ")  // TODO(gabor) prohibiting vars with spaces is a bit awkward
        } +
      ("PBS_MEMORY" -> detectMemory(bashCmd)) )
          .map{ case (key, value) => clean(key) + "=\"" + clean(value) + "\"" }
          .mkString(",")
  }

  def run(bashCmd:String, execDir:Option[String]):Option[Int] = {
    // Wait for free machine
    waitOnFreeMachine
    for (dir <- execDir) { while (!new File(dir).exists) { Thread.sleep(1000); } }
    // Create script file
    val pbsScript:File = execDir.map{ (dir:String) => new File(dir + "/_pbs.bash") }
        .getOrElse( File.createTempFile("pbs", ".bash") )
    if (!pbsScript.exists) {
      pbsScript.createNewFile
    }
    val logDir:String = execDir.getOrElse( {
      val tmpDir = File.createTempFile("pbs_log", ".dir")
      tmpDir.delete; tmpDir.mkdir; tmpDir.getPath
    } )
    // Create qsub command
    def job(passVariables:Boolean) = List[String]( 
        // the program (qsub)
        "qsub",
        // working directory
        "-d", System.getProperty("user.dir")) :::
        // resources
        resources(bashCmd) ::: List[String](
        // job name
        "-N", name.replaceAll(" ","_") + execDir.map( (path:String) => "@" + path.substring(path.lastIndexOf("/") + 1) ).getOrElse(""),
        // job queue
        "-q", queue
        // pass along environment variables
        ) ::: { if (passVariables) List[String]("-v", mkEnv(bashCmd)) else Nil } ::: List[String](
        // Not rerunnable
        "-r", "n"
        // Hosts
        ) ::: { if (hosts.isEmpty()) Nil else List[String]("-W", "x=\"HOSTLIST=" + hosts.mkString(",").replace(" ","") + "\"") } ::: List[String](
        // stderr and stdout
        "-o", logDir + "/_stdout_pbs.log",
        "-e", logDir + "/_stderr_pbs.log",
        // command
        pbsScript.getPath
      )
    // Write script file
    val writer = new PrintWriter(pbsScript)
    try {
      writer.write("#!/bin/bash\n")
      writer.write("#\n")
      writer.write("# qsub command (without passing environment variables):\n")
      writer.write("# " + job(false).map( x => x.toString).mkString(" ") + "\n")
      writer.write("#\n")
      writer.write("WD=" + System.getProperty("user.dir") + "\n")
      writer.write("LOG_DIR=" + logDir + "\n")
      writer.write(header)
      writer.write(bashCmd)
      writer.write(footer)
    } catch {
      case (e:IOException) => throw new RuntimeException(e)
    } finally {
      writer.close
    }
    // Run and return
    Some(job(true).!)
  }

  /**
   *  We would like to rate limit the number of jobs we are submittimg, so
   *  we only allow up to numThreads jobs on the cluster. This function waits
   *  for a machine to free up so that the job can start
   */
  private def waitOnFreeMachine:Unit = {
    while (Qry.threadPoolSize > 1) {  
      val jobs:Seq[String] = List("qstat", "-a").!!.split("\n")
      val myJobCount = jobs.filter( _.contains(System.getProperty("user.name")) ).length
      if (myJobCount < Qry.threadPoolSize) { return }
      Thread.sleep(10000)
    }
  }

  /**
   *  PBS is more stringent than java in recognizing memory usages.
   *  Thus, normalize the memory usage parameter.
   */
  private def normalizeMemory(raw:String):String = {
    val MEMORY_PARTS = """([0-9]+)\.?([0-9]*)(k|K|m|M|g|G)(b|B)?""".r
    raw match {
      case MEMORY_PARTS(whole, frac, prefix, theLetterBOrNull) =>
        var num:Int = whole.toInt;
        var fraction:String = frac
        var order:String = prefix.toLowerCase
        while (fraction != "") {
          var toAdd:String = fraction.substring(0, math.min(fraction.length, 3))
          toAdd = toAdd + ("0" * (3 - toAdd.length))
          num = (num * 1000) + (toAdd.toDouble * 1024 / 1000).toInt
          fraction = fraction.substring(math.min(3, fraction.length))
          order = order match {
            case "" => Qry.err("memory specificiation is finer than a byte: " + raw); ""
            case "k" => ""
            case "m" => "k"
            case "g" => "m"
          }
        }
        num + order.toLowerCase
      case _ => raw
    }
  }

  /** The header of the script to run the PBS job with. The command goes between the header and footer. */
  private val header = """
# set -x

export USERNAME=`whoami`
export HOSTNAME=`hostname | sed -r -e 's/\.[^\.]+\.(edu|com)//i'`
export JOBID=`echo $PBS_JOBID | sed -r -e 's/\..*\.(edu|com)//'`

# due to NFS sync issues, our log directory might not exist yet.
# we'll sleep a little bit in hopes that it will appear soon
sleep_counter=0
mkdir -p $LOG_DIR
while [ $sleep_counter -lt 20 ] && [ ! -d "$LOG_DIR" ]; do
  echo "waiting on log directory..."
  let sleep_counter=sleep_counter+1
  sleep $sleep_counter
done

cd "$WD"
echo `date +"%a %b %d %k:%M:%S %Y"`: Started on ${HOSTNAME} in ${PBS_O_QUEUE} queue. >> "$LOG_DIR/_pbs.log"
echo "" >> "$LOG_DIR/_pbs.log"
echo "Working directory: $WD" >> "$LOG_DIR/_pbs.log"
echo "Current directory: `pwd`" >> "$LOG_DIR/_pbs.log"
echo "JAVA_HOME:         $JAVA_HOME" >> "$LOG_DIR/_pbs.log"
echo "JAVANLP_HOME:      $JAVANLP_HOME" >> "$LOG_DIR/_pbs.log"
echo "LD_LIBRARY_PATH:   $LD_LIBRARY_PATH" >> "$LOG_DIR/_pbs.log"
echo "" >> "$LOG_DIR/_pbs.log"
echo "Job id:            $PBS_JOBID" >> "$LOG_DIR/_pbs.log"
echo "Job name:          $PBS_JOBNAME" >> "$LOG_DIR/_pbs.log"
echo "Job num cores:     $PBS_NUM_PPN" >> "$LOG_DIR/_pbs.log"
echo "Job memory:        $PBS_MEMORY" >> "$LOG_DIR/_pbs.log"
echo "" >> "$LOG_DIR/_pbs.log"
echo "$ env | grep PBS" >> "$LOG_DIR/_pbs.log"
echo "`env | grep PBS`" >> "$LOG_DIR/_pbs.log"
echo "" >> "$LOG_DIR/_pbs.log"

# actually run the command
( """

  /** The footer of the script to run the PBS job with. The command goes between the header and footer. */
  private val footer = """ > "$LOG_DIR/_stdout.log" 2> "$LOG_DIR/_stderr.log" )

echo `date +"%a %b %d %k:%M:%S %Y"`: Completed on ${HOSTNAME} >> "$LOG_DIR/_pbs.log"
echo "" >> "$LOG_DIR/_pbs.log"
"""
}

object Priority {
  val HIGH = "high"
  val NORMAL = "normal"
  val BACKGROUND = "background"
  val PREEMPTABLE = "preemptable"
}

object Queue {
  val SCAIL = "scail"
  val NLP = "nlp"
  val JAG = "jag"
  val JOHN = "john"
}
