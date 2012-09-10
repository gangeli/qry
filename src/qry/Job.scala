import scala.sys.process._

import Qry._

/**
*   A concrete job to be run.
*/
case class Job(proc:ProcessBuilder, var isQueued:Boolean,
               var status:Option[Int]) {

  /**
  *   Queue a task to be run.
  *   If a thread pool is set, this will submit the
  *   task to the thread pool. If the job is set up to run on the cluster,
  *   it will submit an appropriate job. Else, it will run the job outright.
  *   @param force Force running this task, even if the isQueued bit is set.
  */
  def queue(force:Boolean):Unit = {
    if (force || !isQueued) {
      executor.execute( new Runnable {
        override def run:Unit = {
          beginLock.acquire
          beginLock.release
          status = Some(proc.!)
        }
      })
    }
  }
  
  /**
  *   Queue a task to be run; @see Queue(force)
  */
  def queue:Unit = queue(false)

  override def toString:String = proc.toString

  // Constructor
  if (isQueued) queue(true)  // Optionally start running right away
}
