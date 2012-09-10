import Qry._

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
class Task(program:String, args_rev:List[Argument]) {
  /** 
  *   Submit a task specification to the system for running.
  *   The jobs are not run yet as of this return.
  *   @return the jobs to be run
  */
  def jobs:List[Job] = {
    val jobs:List[List[String]]
      = args_rev.foldLeft(ExpandedTask(Nil, Nil, Nil)){
        case (task:ExpandedTask, arg:Argument) => arg(task);
      }.appendToBase(program).instances
    jobs.map( Job(_, false, None) )
  }

  /** 
  *   Add an argument to task.
  *   @param arg The argument ot append
  *   @return This task, with the argument appended
  */
  private def addArgument(arg:Argument):Task
    = new Task(program, arg :: args_rev)

  /** @see addArgument */
  def -(arg:Argument):Task = addArgument(arg)
  
  /** 
  *   Register a textual argument (i.e., without the leading '-')
  *   @param arg The argument's value
  *   @return This task, with the argument appended
  */
  def arg(arg:ArgumentValue):Task = addArgument(Argument(arg))
  
  /** @see arg */
  def $(arg:ArgumentValue):Task = addArgument(Argument(arg))

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
}
