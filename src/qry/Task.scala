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
  private def updateCross(key:Option[String], value:String
      ):List[List[String]] = {
    key match {
      case Some(key) => crossProductArgs.map{ key :: value :: _ }
      case None => crossProductArgs.map{ value :: _ }
    }
  }
  
  /** Updates the piecewise product with a single new argument */
  private def updateIndependent(key:Option[String], value:String
      ):List[List[String]] = {
    key match {
      case Some(key) => independentOrArgs.map{ key :: value :: _ }
      case None => independentOrArgs.map{ value :: _ }
    }
  }

  /** Updates the 'base' (default) run with a given argument or argument piece
  *   @param str The argument to be appended
  *   @return A new task, updated with this argument
  */
  def appendToBase(str:String):ExpandedTask = {
    val newBase = str :: baseArgs;
    ExpandedTask(newBase, updateCross(None, str), updateIndependent(None, str))
  }

  /** Updates the cross product of arguments with a given key/value pair.
  *   @param key The (optional) key of this key/value pair
  *   @param value The value of this key/value pair
  *   @return A new task, updated with this pair
  */
  def appendToCrossProduct(key:Option[String], value:ArgumentValue
      ):ExpandedTask = {
    val newBase = key match {
      case Some(key) => key :: value.head :: baseArgs;
      case None => value.head :: baseArgs;
    }
    var newCross = 
      if(crossProductArgs.size == 0) List(baseArgs)
      else crossProductArgs
    newCross = {key match {
      case Some(key) => newCross.map{ (args:List[String]) =>
                            value.all.map{ key :: _ :: args }}
      case None => newCross.map{ (args:List[String]) =>
                       value.all.map{ _ :: args }}
    }}.flatten
    ExpandedTask(newBase, newCross, updateIndependent(key, value.head))
  }
  
  /** Updates the piecewise (independent) 'or' arguments with a given key/value pair.
  *   @param key The (optional) key of this key/value pair
  *   @param value The value of this key/value pair
  *   @return A new task, updated with this pair
  */
  def appendToIndependentOr(key:Option[String], value:ArgumentValue
      ):ExpandedTask = {
    val newBase = key match {
      case Some(key) => key :: value.head :: baseArgs;
      case None => value.head :: baseArgs;
    }
    val newIndependent = key match {
      case Some(key) => value.tail.map{ key :: _ :: baseArgs } :::
                            updateIndependent(Some(key), value.head)
      case None => value.tail.map{ _ :: baseArgs } :::
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
  def arg(arg:ArgumentValue):Task = addArgument(Argument(arg))
  
  /** @see arg */
  def ->(arg:ArgumentValue):Task = addArgument(Argument(arg))

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
}
