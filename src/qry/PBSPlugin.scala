import scala.collection.JavaConversions._
import scala.sys.process._

import java.io._

object PBS {
  /** The name of the PBS job */
  var name:String = "(no name)"
  /** The queue to put the PBS job on (default Queue.VERYLONG) */
  var queue:String = Queue.VERYLONG
  /** The priority to rub PBS job on (default Priority.NORMAL) */
  var priority:String = Priority.NORMAL
  /** The memory to allocate for the job, if it's not autodetected (default 2gb)*/
  var memory:String = "2gb"
  /** The cores to allocate for the job (default 1) */
  var cores:Int = 1

  /** Create the resources string for the qsub command */
  private def resources(bashCmd:String):String = {
    val MEMORY_REGEX = """.*\s--?X?mx([0-9]+..)\s.*""".r
    val mem:String = bashCmd match {
      case MEMORY_REGEX(x) => x
      case _ => memory
    }
    "mem=" + mem + ",ncpus=" + cores + ",nodes=1"
  }

  def run(bashCmd:String, execDir:Option[String]):Option[Int] = {
    // Wait for free machine
    waitOnFreeMachine
    // Create script file
    val pbsScript:File = execDir.map{ (dir:String) => new File(dir + "/_pbs.bash") }
        .getOrElse( File.createTempFile("pbs", ".bash") )
    if (!pbsScript.exists) {
      pbsScript.createNewFile
    }
    val logDir:String = execDir.getOrElse( File.createTempFile("pbs_log", ".dir").getPath )
    // Write script file
    val writer = new PrintWriter(pbsScript)
    try {
      writer.write("#!/bin/bash\n")
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
    // Create qsub command
    val job = List[String]( "qsub",
        // working directory
        "-d", System.getProperty("user.dir"),
        // resources
        "-l", resources(bashCmd),
        // job name
        "-N", name + "@" + execDir.getOrElse("???"),
        // job queue
        "-q", queue,
        // set the QoS
        "-W",
        // pass along environment variables
        "-V",
        // Not rerunnable
        "-r", "n",
        // stderr and stdout
        "-o", logDir + "/stdout_pbs.log",
        "-e", logDir + "/stderr_pbs.log",
        // command
        pbsScript.getPath
      )
    // Run and return
    Some(job.!)
  }

  private def waitOnFreeMachine:Unit = {
    while (true) {  
      val lines = (List("qstat", "-a") #|
                   List("grep", System.getProperty("user.name"))).!!.split("\n")
      if (lines.length < Qry.threadPoolSize) { return }
      Thread.sleep(10000)
    }
  }

  private val header = """
set -x

USERNAME=`whoami`
HOSTNAME=`hostname | sed -r -e 's/\.[^\.]+\.(edu|com)//i'`
JOBID=`echo $PBS_JOBID | sed -r -e 's/\..*\.(edu|com)//'`

# setup of a bunch of environment variables that PBS jobs can use
export TMP=/tmp/${USERNAME}/${JOBID}-pbs/
export TEMP=$TMP
export TEMPDIR=$TMP
export TMPDIR=$TMP
mkdir -p $TMP

# don't source it if it (or similar things) have already been sourced
if [ -z "$JAVA_HOME" ]; then
  if [ -e /u/nlp/bin/setup.bash ]; then
    source /u/nlp/bin/setup.bash
  fi
fi

if [ -e $HOME/.profile ]; then
  source $HOME/.profile
fi
if [ -e $HOME/.bashrc ]; then
  source $HOME/.bashrc
fi

# due to NFS sync issues, our log directory might not exist yet.
# we'll sleep a little bit in hopes that it will appear soon
sleep_counter=0
mkdir -p $LOG_DIR
while [ $sleep_counter -lt 20 ] && [ ! -d "$LOG_DIR" ]; do
  echo "waiting on log directory..."
  let sleep_counter=sleep_counter+1
  sleep 1
done

cd "$WD"
echo `date +"%a %b %d %k:%M:%S %Y"`: Started on ${HOSTNAME} in ${PBS_O_QUEUE} queue. >> "$LOG_DIR/qsub.log"

# actually run the command
( """

  private val footer = """ > "$LOG_DIR/stdout.log" 2> "$LOG_DIR/stderr.log" )

echo `date +"%a %b %d %k:%M:%S %Y"`: Completed on ${HOSTNAME} >> '%(qsub_log)s'

# cleanup the temp files we made -- this should really be done in the epilog
# if we ever figure out how to get PBS to run them
rm -rf $TMP
"""
}

object Priority {
  val HIGH = "high"
  val NORMAL = "normal"
  val BACKGROUND = "background"
  val PREEMPTABLE = "preemptable"
}

object Queue {
  val SHORT = "short"
  val LONG = "long"
  val VERYLONG = "verylong"
}
