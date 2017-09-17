import scala.collection.JavaConversions._
import scala.sys.process._

import java.io._

object Slurm {
  /** 
   *  The hostname of the Slur controller. 
   *  If this is set, we will SSH into the controller to run all of the jobs.
   */
  var controller:String = {
    if (java.net.InetAddress.getLocalHost().getHostName() == "cicero") {
      "localhost"
    } else {
      "cicero.hybridcrowd.io"
    }
  }


  /** The name of the Slurm job */
  var name:String = System.getProperty("user.name") + "-" + new scala.util.Random().alphanumeric.take(5).mkString
  /** The partition to put the Slurm job on. Defaults to Partition.ANY. */
  var partition:String = Partition.ANY
  /** The memory to allocate for the job, if it's not autodetected (default 2gb)*/
  var memory:String = "2gb"
  /** The cores to allocate for the job (default 1) */
  var cores:Int = 1
  /** The GPU instances to allocate for the job (default 0) */
  var gpus:Int = 0
  /** The maximum amount of time the job is allowed to run for (default 168:00:00 for 168 hours). */
  var time:String = "168:00:00"
  /** The address to mail Slurm updates to. */
  var mailto:String = "slurm-notifications@eloquent.ai"
 

  /** Scrape just the memory from the bash command*/
  private def detectMemory(bashCmd:String):String = {
    val MEMORY_REGEX = """.*\s--?X?mx([0-9]+..)(\s|\\|\n).*""".r
    bashCmd match {
      case MEMORY_REGEX(x, separator) => x
      case _ => memory
    }
  }


  def run(bashCmd:String, execDir:Option[String]):Option[Int] = {
    // Wait for free machine
    waitOnFreeMachine
    for (dir <- execDir) { while (!new File(dir).exists) { Thread.sleep(1000); } }
    // Create script file
    val slurmScriptFinal:File
      = execDir.map{ (dir:String) => new File(dir + "/_slurm.sbatch") }
        .getOrElse( File.createTempFile("slurm", ".sbatch") )
    val slurmScript:File = {
      if (controller == "localhost") {
        slurmScriptFinal
      } else {
        slurmScriptFinal.delete();
        File.createTempFile("slurm", ".sbatch") 
      }
    }
    if (!slurmScript.exists) {
      slurmScript.createNewFile
    }
    val logDir:String = execDir.getOrElse( {
      val tmpDir = File.createTempFile("slurm_log", ".dir")
      tmpDir.delete; tmpDir.mkdir; tmpDir.getPath
    } )
    // Create qsub command
    def job(passVariables:Boolean) = List[String]( 
        // the program (sbatch)
        "sbatch",
        // pass along environment variables
        "--export", "ALL",
        // stderr and stdout
//        "-output", logDir + "/_stdout_slurm.log",  // run into permissions issues -- this is run as slurm user
//        "--error", logDir + "/_stderr_slurm.log",
        // command
        slurmScript.getPath
      )
    // Write script file
    val writer = new PrintWriter(slurmScript)
    try {
      writer.write("#!/bin/bash\n")
      writer.write("#\n")
      writer.write("# (begin sbatch directives)\n")
      if (partition != "") {
        writer.write("#SBATCH --partition=" + partition + "\n")
      }
      writer.write("#SBATCH --nodes=1\n")
      writer.write("#SBATCH --ntasks=1\n")
      writer.write("#SBATCH --cpus-per-task=" + cores + "\n")
      writer.write("#SBATCH --mem=" + memory + "\n")
      writer.write("#SBATCH --time=" + time + "\n")
      writer.write("#SBATCH --job-name=" + name + "\n")
      writer.write("#SBATCH --gres=gpu:" + gpus + "\n")
      writer.write("#SBATCH --mail-type=FAIL,TIME_LIMIT\n")
      writer.write("#SBATCH --mail-user=" + mailto + "\n")
      writer.write("# (end sbatch directives)\n")
      writer.write("#\n")
      writer.write("# Run by: " + System.getProperty("user.name"))
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
    if (controller != "localhost") {
      List[String]("scp", slurmScript.getPath(), controller + ":" + slurmScriptFinal.getPath()).!!
      Some(List[String]("ssh", controller, "\"" + job(true).mkString("\" \"") + "\"").!)
    } else {
      Some(job(true).!)
    }
  }

  /**
   *  We would like to rate limit the number of jobs we are submittimg, so
   *  we only allow up to numThreads jobs on the cluster. This function waits
   *  for a machine to free up so that the job can start
   */
  private def waitOnFreeMachine:Unit = {
    while (Qry.threadPoolSize > 1) {  
      val jobs:Seq[String]
        = if (controller != "localhost") {
          List[String]("ssh", controller, "squeue --name=" + name + " --noheader").!!.split("\n")
        } else {
          List("squeue", "--name=" + name, "--noheader").!!.split("\n")
        }
      if (jobs.length < Qry.threadPoolSize) { return }
      Thread.sleep(10000)
    }
  }

  /**
   *  Slurm is more stringent than java in recognizing memory usages.
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
          }
        }
        num + order.toLowerCase
      case _ => raw
    }
  }

  /** The header of the script to run the Slurm job with. The command goes between the header and footer. */
  private val header = """#!/bin/bash
#
# @generated by Qry
#
set -x

export USERNAME=`whoami`
export HOSTNAME=`hostname | sed -r -e 's/\.[^\.]+\.(edu|com|ai|io)//i'`
export JOBID=`echo $SLURM_JOBID | sed -r -e 's/\..*\.(edu|com|ai|io)//'`
echo "$USERNAME  $HOSTNAME  $JOBID" > /home/gabor/basic_vars

# Kinit
if [[ -e "/etc/krb_keys/${USERNAME}.keytab" ]]; then
  kinit -k -t "/etc/krb_keys/${USERNAME}.keytab" "${USERNAME}"
fi

# due to NFS sync issues, our log directory might not exist yet.
# we'll sleep a little bit in hopes that it will appear soon
sleep_counter=0
mkdir -p $LOG_DIR  # we always have a log dir -- though it may be a temp directory
while [[ $sleep_counter -lt 20 ]] && [[ ! -d "$LOG_DIR" ]]; do
  echo "waiting on log directory..."
  let sleep_counter=sleep_counter+1
  sleep $sleep_counter
done
echo "got log directory: $LOG_DIR"

cd "$WD"
echo "`date +"%a %b %d %k:%M:%S %Y"`: Started on ${HOSTNAME} in ${SLURM_PARTITION} queue." >> "$LOG_DIR/_slurm.log"
echo "" >> "$LOG_DIR/_slurm.log"
echo "Working directory: $WD" >> "$LOG_DIR/_slurm.log"
echo "Current directory: `pwd`" >> "$LOG_DIR/_slurm.log"
echo "JAVA_HOME:         $JAVA_HOME" >> "$LOG_DIR/_slurm.log"
echo "JAVANLP_HOME:      $JAVANLP_HOME" >> "$LOG_DIR/_slurm.log"
echo "LD_LIBRARY_PATH:   $LD_LIBRARY_PATH" >> "$LOG_DIR/_slurm.log"
echo "" >> "$LOG_DIR/_slurm.log"
echo "Job id:            $SLURM_JOBID" >> "$LOG_DIR/_slurm.log"
echo "Job name:          $SLURM_JOB_NAME" >> "$LOG_DIR/_slurm.log"
echo "Job num cores:     $SLURM_CPUS_ON_NODE" >> "$LOG_DIR/_slurm.log"
echo "Job memory:        $SLURM_MEM" >> "$LOG_DIR/_slurm.log"
echo "Submitted from:    $SLURM_SUBMIT_HOST" >> "$LOG_DIR/_slurm.log"
echo "" >> "$LOG_DIR/_slurm.log"
echo "$ env | grep SLURM" >> "$LOG_DIR/_slurm.log"
echo "`env | grep SLURM`" >> "$LOG_DIR/_slurm.log"
echo "" >> "$LOG_DIR/_slurm.log"

# actually run the command
( """


  /** The footer of the script to run the Slurm job with. The command goes between the header and footer. */
  private val footer = """ > "$LOG_DIR/_stdout.log" 2> "$LOG_DIR/_stderr.log" )

EXIT_CODE=$?
echo "`date +"%a %b %d %k:%M:%S %Y"`: Completed on ${HOSTNAME} w/exit code $EXIT_CODE" >> "$LOG_DIR/_slurm.log"
echo "" >> "$LOG_DIR/_slurm.log"
exit $EXIT_CODE
"""
}


object Partition {
  val GOOGLE = "google"
  val AWS    = "aws"
  val AZURE  = "azure"
  val ANY    = ""
}
