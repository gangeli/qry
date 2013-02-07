import scala.collection.JavaConversions._
import scala.util.matching.Regex
import java.io.File

/**
 * A DSL for querying results. This only works if a run directory is
 * specified
 */
object QrySQL {
  //
  // Classes
  //
  /** A key-value store of results or inputs */
  trait KeyValueStore {
    // (to override)
    def apply(key:String):String
    def singletons:Set[String]
    // utilities
    def double(key:String):Double = apply(key).replaceAll("#","")
                                              .replaceAll("%","")
                                              .replaceAll("$","")
                                              .replaceAll(" +","")
                                              .trim.toDouble
    def tryDouble(key:String):String
      = try   { double(key).toString }
        catch { case (x:NumberFormatException) => apply(key) }
  }

  /** A key-value store of inputs */
  case class Input(name:String, binary:Map[String,String], unary:Set[String]
      ) extends KeyValueStore {
    override def apply(key:String):String
      = binary.get(key).getOrElse( if (unary(key)) "true" else "false" )
    override def singletons:Set[String] = unary
  }
  
  /** A key-value store of results */
  case class Result(binary:Map[String,String]) extends KeyValueStore {
    override def apply(key:String):String = binary.get(key).getOrElse("false")
    override def singletons:Set[String] = Set[String]()
  }
  
  //
  // Selection
  //
  /** A selection query (get only outputs matching a criteria) */
  trait SelectionLike {
    def key:String
    def accept(in:KeyValueStore, out:KeyValueStore):Boolean
    def computeGlobal(seq:Seq[(KeyValueStore,KeyValueStore)]):Unit = {}
  }
 
  /** A simple selection, based on only this element */
  case class LocalSelection(override val key:String,
                            criterium:(KeyValueStore,KeyValueStore)=>Boolean
      ) extends SelectionLike {
    override def accept(in:KeyValueStore, out:KeyValueStore):Boolean
      = criterium(in, out)
  }
 
  /** A global selection of a boundary criteria (e.g., max, min, etc) */
  case class BoundarySelection(override val key:String,
                               boundaryElements:Seq[String]=>Set[String]
                               ) extends SelectionLike {
    var boundary:Option[Set[String]] = None

    override def computeGlobal(seq:Seq[(KeyValueStore,KeyValueStore)]):Unit = {
      val values = seq.map{ case (in:KeyValueStore, out:KeyValueStore) =>
        if (in(key) == "false") out.tryDouble(key) else in.tryDouble(key)
      }
      val boundary = boundaryElements(values)
      this.boundary = Some(boundary.toSet)
    }

    override def accept(in:KeyValueStore, out:KeyValueStore):Boolean = {
      boundary.map{ (boundary:Set[String]) =>
        boundary(in.tryDouble(key)) || boundary(out.tryDouble(key))
      }.getOrElse(false)
    }
  }

  case class SelectionKey(key:String) extends AnyVal {
    def ===(value:Any):SelectionLike
      = LocalSelection(key, (a,b) => a(key) == value.toString ||
                                b(key) == value.toString    )
   
    def <(value:Int):SelectionLike
      = LocalSelection(key, (a,b) => try { a.double(key) < value ||
                                      b.double(key) < value    }
                                catch { case _ => false })
    def <(value:Number):SelectionLike
      = LocalSelection(key, (a,b) => try { a.double(key) < value.doubleValue ||
                                      b.double(key) < value.doubleValue    }
                                catch { case _ => false })
    def <(value:String):SelectionLike
      = LocalSelection(key, (a,b) => augmentString(a(key)) < value.toString ||
                                augmentString(b(key)) < value.toString    )
    
    def <=(value:Int):SelectionLike
      = LocalSelection(key, (a,b) => try { a.double(key) <= value ||
                                      b.double(key) <= value    }
                                catch { case _ => false })
    def <=(value:Number):SelectionLike
      = LocalSelection(key, (a,b) => try { a.double(key) <= value.doubleValue ||
                                      b.double(key) <= value.doubleValue    }
                                catch { case _ => false })
    def <=(value:String):SelectionLike
      = LocalSelection(key, (a,b) => augmentString(a(key)) <= value.toString ||
                                augmentString(b(key)) <= value.toString    )
    
    def >(value:Int):SelectionLike
      = LocalSelection(key, (a,b) => try { a.double(key) > value ||
                                      b.double(key) > value    }
                                catch { case _ => false })
    def >(value:Number):SelectionLike
      = LocalSelection(key, (a,b) => try { a.double(key) > value.doubleValue ||
                                      b.double(key) > value.doubleValue    }
                                catch { case _ => false })
    def >(value:String):SelectionLike
      = LocalSelection(key, (a,b) => augmentString(a(key)) > value.toString ||
                                augmentString(b(key)) > value.toString    )
    
    def >=(value:Int):SelectionLike
      = LocalSelection(key, (a,b) => try { a.double(key) >= value ||
                                      b.double(key) >= value    }
                                catch { case _ => false })
    def >=(value:Number):SelectionLike
      = LocalSelection(key, (a,b) => try { a.double(key) >= value.doubleValue ||
                                      b.double(key) >= value.doubleValue    }
                                catch { case _ => false })
    def >=(value:String):SelectionLike
      = LocalSelection(key, (a,b) => augmentString(a(key)) >= value.toString ||
                                augmentString(b(key)) >= value.toString    )
    
    def is(value:Any) = this === value

    def min:SelectionLike = BoundarySelection(key, {(s:Seq[String]) =>
      try { Set[String](s.map{ _.toDouble }.minBy( x => x).toString) }
      catch { case _ => Set[String](s.minBy( x => x )) }
    })
    def minimum:SelectionLike = min
    def minimize:SelectionLike = min
    
    def max:SelectionLike = BoundarySelection(key, {(s:Seq[String]) =>
      try { Set[String](s.map{ _.toDouble }.maxBy( x => x).toString) }
      catch { case _ => Set[String](s.maxBy( x => x )) }
    })
    def maximum:SelectionLike = max
    def maximize:SelectionLike = max
  }

  //
  // Projection
  //
  /** An entity that can act as a projection */
  trait ProjectionLike {
    // (required override)
    def matches(key:String):Boolean
    def ::(other:ProjectionLike):ProjectionLike
    // (other methods)
    def :::(other:ProjectionLike):ProjectionLike = this :: other
    val (*):ProjectionLike = QrySQL.*
    def where(selection:SelectionLike):Unit = {
      selection.computeGlobal(database)
      // Run query
      val matches:Map[String, Iterable[String]] = database
        .filter{ case (in, out) => selection.accept(in, out) }
        .map{ case (in, out) =>
          (in.name,
            (in.binary.filter{ case (key, value) => this.matches(key) }
                      .filter{ case (key, value) => relevantKeys(key) }
                      .map{ case (key, value) => 
                                "[input] " + key + ": " + value }) ++
            (in.unary.filter{ key => this.matches(key) }
                     .filter{ key => relevantKeys(key) }
                     .map{ x => "[input] " + x }) ++
            (out.binary.filter{ case (key, value) => this.matches(key) }
                       .filter{ case (key, value) => relevantKeys(key) }
                       .map{ case (key, value) => 
                                "[output] " + key + ": " + value })
          )
        }.toMap
      // Print function
      def display(lst:Iterable[(String,Iterable[String])]):Unit = {
        lst.foreach{ case (name, results) =>
          println(name)
          results.toList.sortBy( x => x ).foreach{ (result:String) =>
            println("  " + result)
          }
        }
      }
      // Print first results
      if (matches.size == 0) {
        println("<no results>")
      }
      display(matches.take(1))
      val rest = matches.drop(1)
      // Print other results
      if (rest.size > 0) {
        val showRest:String =
          Console.readLine("" + rest.size + " more results; show (y/N)?")
        if(showRest.toLowerCase == "y" || showRest.toLowerCase == "yes") {
          display(rest)
        }
      }
    }
  }

  /** A projection query (get certain fields of output) */
  case class Projection(columns:List[Regex]) extends ProjectionLike {
    def matches(key:String) = columns.exists(r => r.findFirstIn(key).isDefined)
    def ::(other:ProjectionLike):ProjectionLike = other match {
      case Projection(columns) => Projection(columns ::: this.columns)
      case (x:ProjectionEverything) => x
      case _ => throw new IllegalStateException("Unknown projection: " + other)
    }
  }

  /** Implementation of the '*' projection symbol */
  case class ProjectionEverything() extends ProjectionLike {
    override def matches(key:String) = true
    override def ::(other:ProjectionLike):ProjectionLike = this
  }
  /** Instance of the '*' projection symbol */
  val * = ProjectionEverything()
  
  //
  // Database
  //

  /** Read the data from a run directory */
  // TODO(gabor): This should really be far less heuristic
  private lazy val database:List[(Input,Result)] = {
    val Binary = """^\s+'-(.*)'\s+'(.*)'\\?\s*$""".r
    val Unary = """^\s+'-?(.*)'\\?\s*$""".r
    val JSON = """^\s+"(.*)":\s+"(.*)"\s*,?\s*$""".r
    Qry.execRoot.map{ (execDir:String) =>
      print("Reading runs...")
      // (get directories)
      print("[getting folders]...")
      val runDirs = new File(execDir).listFiles.map { _.getAbsolutePath }
      // (read files)
      print("[getting data]...")
      val db:List[(Input,Result)] = runDirs.map{ (runDir:String) =>
        // (get input parameters)
        val input = if (new File(runDir + "/_rerun.sh").exists) {
            val (binary, unary) = io.Source.fromFile(runDir + "/_rerun.sh").getLines
              .foldLeft( (Map[String,String](), List[String]()) ){
                case ((binary:Map[String,String], unary:List[String]), line:String) =>
              line match {
                case Binary(key, value) => (binary + ((key, value)), unary)
                case Unary(key) => (binary, key :: unary)
                case _ => (binary, unary)
              }
            }
            Input(new File(runDir).getName, binary, unary.toSet)
          } else {
            Input("[unknown]", Map[String,String](), Set[String]())
          }
        // (get results)
        val result = if (new File(runDir + "/_qry.json").exists) {
            Result( io.Source.fromFile(runDir + "/_qry.json").getLines
              .foldLeft( Map[String,String]() ){
                case (binary:Map[String,String], line:String) =>
              line match {
                case JSON(key, value) => binary + ((key, value))
                case _ => binary
              }
            })
          } else {
            Result(Map[String,String]())
          }
        // (construct tuple)
        (input, result)
      }.toList
      println("done.")
      db
    }.getOrElse({
      println("Warning: No run directory has been specified.")
      println("         Either specify one with using(path/to/dir/), or start the")
      println("         program as java -jar qry.jar path/to/dir/")
      List[(Input, Result)]()
    })
  }

  private lazy val relevantKeys:Set[String] = {
    var unchangedKeys = new scala.collection.mutable.HashMap[String,String]()
    var changedKeys   = new scala.collection.mutable.HashSet[String]()
    database.flatMap{ case (in:Input, out:Result) =>
      // get all keys
      val all:Map[String,String]
        = in.binary ++ in.unary.map( (_, "true") ).toMap ++ out.binary
      all.filter{ case (x, y) => !changedKeys(x) }
         .foreach{ case (key:String, value:String) =>
        // update changed keys
        unchangedKeys.get(key) match {
          case Some(lastValue) => if (value != lastValue) {
              changedKeys += key
              unchangedKeys.remove(key)
            }
          case None => unchangedKeys(key) = value
        }
      }
      all.keys
    }.toSet &~ unchangedKeys.keys.toSet
  }

  //
  // Implicits
  //
  implicit def str2projection(str:String):Projection = Projection(List[Regex](str.r))
  implicit def re2projection(re:Regex):Projection = Projection(List[Regex](re))
  implicit def lst2projection(lst:List[String]):Projection = Projection(lst.map( _.r ))
  implicit def sym2projection(sym:Symbol):Projection = Projection(List[Regex](sym.name.r))
  
  implicit def str2selectionkey(str:String):SelectionKey = SelectionKey(str)
  implicit def sym2selectionkey(sym:Symbol):SelectionKey = SelectionKey(sym.name)
  
  //
  // Entry Functions
  //

  def select(proj:ProjectionLike*):ProjectionLike = {
    proj.tail.foldLeft(proj.head){
        case (soFar:ProjectionLike, term:ProjectionLike) =>
      soFar ::: term
    }
  }
  def select:ProjectionLike = QrySQL.*

  def get(sel:SelectionLike):Unit = select(sel.key) where sel

  /** List all available keys to query on */
  def keys:Unit = {
    relevantKeys.toList.sortBy{ (x:String) => x }.foreach{ (key:String) =>
      println(key)
    }
  }

  /** List all available keys matching a prefix */
  def key(segmentsRaw:String*):String = {
    val segments = segmentsRaw.flatMap( _.toLowerCase.split(" ") )
    database.flatMap{ case (in:Input, out:Result) =>
      in.binary.keys ++ in.unary ++ out.binary.keys
    }.toSet.filter{ (key:String) =>
      val lowerKey = key.toLowerCase
      segmentsRaw.forall( lowerKey.contains(_) )
    }.toList.sortBy{ x => x }.head
  }
    
  def min(key:SelectionKey):SelectionLike = key.min
  def minimum(key:SelectionKey):SelectionLike = key.min
  def minimize(key:SelectionKey):SelectionLike = key.min
  def minimized(key:SelectionKey):SelectionLike = key.min
  def max(key:SelectionKey):SelectionLike = key.max
  def maximum(key:SelectionKey):SelectionLike = key.max
  def maximize(key:SelectionKey):SelectionLike = key.max
  def maximized(key:SelectionKey):SelectionLike = key.max

}
