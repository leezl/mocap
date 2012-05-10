package main.scala
import scala.collection.mutable._

/**
 * Created by IntelliJ IDEA.
 * User: lieslw
 * Date: 4/28/12
 * Time: 10:49 AM
 * To change this template use File | Settings | File Templates.
 *
 * This should be the main class for running, but SBT can't find it...
 */

object GeneratorMain {

  def main(args: Array[String]) {
    //make MotionGenerator (pass files)
    var generator = new MotionGenerator()
    var skeletonFile = "data/02"
    var motionFiles = List[String]()
    for (i<- 1 until  2){
      motionFiles = "data/02_0" + i.toString + ".amc" ::motionFiles
    }
    println("Done Loading.")
    generator.init(skeletonFile, motionFiles)
    
    println("Made it to end...?")
  }

}
