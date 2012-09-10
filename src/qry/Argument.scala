import Qry._

/**
*   A fully specified argument to the system, potentially spawning many
*   runs corresponding to different values the argument could take.
*/
class Argument(val key:Option[ArgumentKey], val value:Option[ArgumentValue]) {
  /** Updates an ExpandedTask with the specification defined by this argument.
  *   @param task The task to update, in its current state
  *   @return A new task, updated with this argument
  */
  def apply(task:ExpandedTask):ExpandedTask = {
    def handleValue(task:ExpandedTask, key:Option[String], value:ArgumentValue
        ):ExpandedTask = {
      value.mode match {
        case 'none =>
          task.appendToBase(value.head)
        case 'cross_product =>
          task.appendToCrossProduct(key, value)
        case 'independent_or =>
          task.appendToIndependentOr(key, value)
        case _ => throw new IllegalArgumentException("Internal error")
      }
    }
    (key, value) match {
      case (None, None) => throw new IllegalArgumentException("Internal error")
      case (Some(key), None) => task.appendToBase(dash() + key)
      case (None, Some(value)) => handleValue(task, None, value)
      case (Some(key), Some(value)) => handleValue(task, Some(dash() + key),
                                                   value)
    }
  }

  override def toString:String = {key match {
       case Some(k) => dash() + k.key
       case None => ""
     }} + " " + {value match {
       case Some(v) => v.head
       case None => ""
     }}

  override def hashCode:Int = key.hashCode ^ value.hashCode

  override def equals(o:Any):Boolean = o match {
    case (other:Argument) => other.key == key && other.value == value
    case _ => false
  }
}

/**
*   A helper object for Argument.
*/
object Argument {
  /** Create a new argument given a key and value
  *   @param key The key
  *   @param value The arguments value
  *   @return A fully specified argument
  */
  def apply(key:ArgumentKey, value:ArgumentValue):Argument
    = new Argument(Some(key), Some(value));

  /** Create a new argument given a value only
  *   @param value The value
  *   @return A fully specified argument, with only a value
  */
  def apply(value:ArgumentValue):Argument = new Argument(None, Some(value));

  /** Create a new argument given a key only
  *   @param key The key value
  *   @return A fully specified argument, with only a key
  */
  def apply(key:ArgumentKey):Argument = new Argument(Some(key), None);
}

/**
*   An argument key.
*/
case class ArgumentKey(key:String) {
  /** Set an argument key defined by this argument to a value.
  *   @param value The value to set
  *   @return A fully specified argument
  */
  def :=(value:ArgumentValue):Argument = Argument(this, value)

  override def toString:String = key
}

/**
*   An abstract representation of an argument value,
*   potentially taking on many concrete values.
*/
case class ArgumentValue(head:String, tailRev:List[String], mode:Symbol) {
  
  /** All but the default argument for this argument value
  *   @return All but the default argument, as an in-order list
  */
  def tail:List[String] = tailRev.reverse
  
  /** All the possible instances of this argument value
  *   @return All possble arguments
  */
  def all:List[String] = head :: tailRev.reverse

  /** Piecewise (independent) branchout of the arguments.
  *   This branches out in a dimension of the input space, but with all other
  *   dimensions fixed.
  *   This is in contrast to taking the cross product of the arguments.
  *   @param alternative The argument to append
  *   @return The ArgumentValue with this value appended
  */
  def |(alternative:String):ArgumentValue = {
    if (mode != 'independent_or && mode != 'none) {
      err("Mixing '|' and '&' in argument list")
    }
    ArgumentValue(head, alternative :: tailRev, 'independent_or);
  }
  
  /** Cross product branchout of the arguments.
  *   This branches out in a dimension of the input space, taking all
  *   combinations with all other arguments
  *   @param alternative The argument to append
  *   @return The ArgumentValue with this value appended
  */
  def &(alternative:String):ArgumentValue = {
    if (mode != 'cross_product && mode != 'none) {
      err("Mixing '|' and '&' in argument list")
    }
    ArgumentValue(head, alternative :: tailRev, 'cross_product);
  }

  /** Choses a particular value from the set of possible arguments
  *   defined in this ArgumentValue.
  *   @param index of the argument to choose
  *   @return The ArgumentValue with only that index selected
  */
  def ?:(headIndex:Int):ArgumentValue = {
    if (headIndex < 0 || headIndex > tailRev.size) {
      err("Index out of bounds: " + headIndex + " for options: " + this)
    }
    return ArgumentValue(all(headIndex), Nil, 'none)
  }

  /** Choses a new default value from the set of possible arguments
  *   defined in this ArgumentValue.
  *   @param index of the argument to choose as the new default
  *   @return The ArgumentValue with all alternatives intact, but a new default
  */
  def ??:(headIndex:Int):ArgumentValue = {
    if (headIndex < 0 || headIndex > tailRev.size) {
      err("Index out of bounds: " + headIndex + " for options: " + this)
    }
    val newHead = all(headIndex)
    return ArgumentValue(newHead,
                         tailRev diff List(newHead) ::: List(head),
                         mode)
  }

  override def toString:String = all.mkString(" ")
}
