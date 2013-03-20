import scala.collection.JavaConversions._
import java.util.Properties
import java.io.File
import com.typesafe.config._

object TypesafeConfigPlugin {
  def appendProperties(staticProperties:Properties, specFile:File):Unit = {
    for (entry <- ConfigFactory.parseFile(specFile).entrySet) {
      staticProperties.put(entry.getKey, entry.getValue.unwrapped)
    }
  }
}
