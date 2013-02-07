import scala.collection.JavaConversions._
import java.io.File

/**
 * A DSL for querying results. This only works if a run directory is
 * specified
 */
object QryLang {
  //
  // Classes
  //
  /** A key-value store of results or inputs */
  trait KeyValueStore {
    def apply(key:String):Option[String]
    def singletons:Seq[String]
  }

  /** A key-value store of inputs */
  case class Input(name:String, binary:Map[String,String], unary:List[String]
      ) extends KeyValueStore {
    override def apply(key:String):Option[String] = binary.get(key)
    override def singletons:Seq[String] = unary
  }
  
  /** A key-value store of results */
  case class Result(binary:Map[String,String]) extends KeyValueStore {
    override def apply(key:String):Option[String] = binary.get(key)
    override def singletons:Seq[String] = Nil
  }
  
  //
  // Selection
  //
  /** A selection query (get only outputs matching a criteria) */
  case class Selection(criterium:(KeyValueStore,KeyValueStore)=>Boolean) extends AnyVal

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
    def *():ProjectionLike = QryLang.*
    def where(selection:Selection):Unit = {
      // Run query
      val matches:Map[String, Iterable[String]] = database
        .filter{ case (in, out) => selection.criterium(in, out) }
        .map{ case (in, out) =>
          (in.name,
            (in.binary.filter{ case (key, value) => this.matches(key) }
                         .map{ case (key, value) => 
                                "[input] " + key + ": " + value }) ++
            (in.unary.filter{ key => this.matches(key) }
                         .map{ x => "[input] " + x }) ++
            (out.binary.filter{ case (key, value) => this.matches(key) }
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
      display(matches.take(5))
      val rest = matches.drop(5)
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
  case class Projection(columns:List[String]) extends ProjectionLike {
    def matches(key:String) = columns.contains(key)
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
    val JSON = """^\s+"(.*)":\s+"(.*)"\s*$""".r
    Qry.execRoot.map{ (execDir:String) =>
      print("Reading runs...")
      // (get directories)
      val runDirs = new File(execDir).listFiles.map { _.getAbsolutePath }
      print("[got folders]...")
      // (read files)
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
            Input(new File(runDir).getName, binary, unary)
          } else {
            Input("[unknown]", Map[String,String](), List[String]())
          }
        // (get results)
        val result = if (new File(runDir + "/_qry.json").exists) {
            Result( io.Source.fromFile(runDir + "/_rerun.sh").getLines
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

  //
  // Implicits
  //
  implicit def str2projection(str:String):Projection = Projection(List[String](str))
  implicit def lst2projection(lst:List[String]):Projection = Projection(lst)
  implicit def sym2projection(sym:Symbol):Projection = Projection(List[String](sym.name))
  
  //
  // Entry Functions
  //

  def select(proj:ProjectionLike*):ProjectionLike = {
    proj.tail.foldLeft(proj.head){
        case (soFar:ProjectionLike, term:ProjectionLike) =>
      soFar ::: term
    }
  }

}
