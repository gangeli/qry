
object Stanford {
  //
  // Classpath
  //
  def JAVANLP = System.getenv("JAVANLP_HOME") + "/projects/core/classes:" +
                System.getenv("JAVANLP_HOME") + "/projects/more/classes:" +
                System.getenv("JAVANLP_HOME") + "/projects/research/classes:" +
                "/u/nlp/data/StanfordCoreNLPModels/stanford-corenlp-models-current.jar:" +
                "/u/nlp/data/StanfordCoreNLPModels/stanford-srparser-models-current.jar:" +
                "/u/nlp/data/StanfordCoreNLPModels/stanford-corenlp-caseless-models-current.jar"
  
  def getLibraries(path:String):String = {
    try {
      new java.io.File(path)
        .listFiles()
        .filter( f => f.getName().endsWith(".jar") )
        .map( f => f.getAbsolutePath() )
        .mkString(":")
    } catch {
      case (e:java.io.IOException) => "Could not find path"
    }
  }
  
  /// Load all the JAVANLP libraries
  def JAVANLP_LIBS = 
      getLibraries( System.getenv("JAVANLP_HOME") + "/projects/core/lib/" ) + ":" +
      getLibraries( System.getenv("JAVANLP_HOME") + "/projects/more/lib/" ) + ":" +
      getLibraries( System.getenv("JAVANLP_HOME") + "/projects/research/lib/" ) 
  //
  // Java Options
  //
  def guessClassOrElse(args:Array[String], default:String):String = {
    def tryLoad(clazz:String):Option[String] = {
      try {
        Class.forName(clazz); Some(clazz)
      } catch {
        case (e:ClassNotFoundException) => None
      }
    }
    (args.foldLeft( None.asInstanceOf[Option[String]] ){ case (soFar:Option[String], arg:String) =>
      soFar.orElse(tryLoad(arg))
        .orElse(tryLoad("edu." + arg))
        .orElse(tryLoad("edu.stanford." + arg))
        .orElse(tryLoad("edu.stanford.nlp." + arg))
        .orElse(tryLoad("edu.stanford.nlp.kbp." + arg))
        .orElse(tryLoad("edu.stanford.nlp.kbp.slotfilling." + arg))
        .orElse(tryLoad("edu.stanford.nlp.kbp.slotfilling.scripts." + arg))
    }).getOrElse(default)
  }
  
  def getConfigOr(args:Array[String], default:String):String = {
    def tryLoad(path:String):Option[String] = {
      try {
        val candidate = new java.io.File(path)
        if (candidate.exists) Some(path) else None
      } catch {
        case (e:java.io.IOException) => None
      }
    }
    (args.foldLeft( None.asInstanceOf[Option[String]] ){ case (soFar:Option[String], arg:String) =>
      soFar.orElse(tryLoad(arg))
        .orElse(tryLoad("config/" + arg))
        .orElse(tryLoad("scripts/config/" + arg))
    }).getOrElse(default)
  }
}
