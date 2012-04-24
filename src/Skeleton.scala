import scala.io.Source._
import scala.actors._
import scala.actors.Actor._

/**
 * Created by IntelliJ IDEA.
 * User: lieslw
 * Date: 4/22/12
 * Time: 9:30 AM
 * To change this template use File | Settings | File Templates.
 *
 * Clearly some trouble committing from Windows. This file is for
 * storing the skeleton structure. I believe we only need one type. Form is:
 * pelv<aX> pelv<aY> pelv<aZ> pelv<tX> pelv<tY> pelv<tZ> lfem<aX> ...
 * The rest of the numbers in the line specify the remaining joints.
 * These joints are specified in the following order:
   pelvis 1->2 6 10
    lfemur 2->3
      ltibia 3->4
        lfoot 4->5
          ltoes 5->
    rfemur 6->7
      rtibia 7->8
        rfoot 8->9
          rtoes 9->
    thorax 10-> 11 15
      lclavicle 11->12
        lhumerus 12->13
          lradius 13->14
            lhand 14->
      rclavicle 15->16
        rhumerus 16->17
          rradius 17->18
            rhand 18->
 */

class Skeleton {
  //skeleton & motions(List)
  var name : String
  var defaultLength : Double
  var defaultMass : Double
  var angleUnit : String //in case we need to swap radians and degrees
  var originalFileType : String
  var skel : List[SkeletonSegment]
  var motions : List[String]
  var newrep: List[Double]
  var grdvel: List[Double]
  var velocity: Double
  
  class SkeletonSegment extends Actor {
    var order : List[String] = Nil
    var axisCode : String = ""
    var position : List[Int] = Nil
    var orientation : List[Int] = Nil
    var mass : Double= 0.0
    var length : Double =0.0
    var id : Int = -1
    var name :String = ""
    var direction : List[Double] = Nil
    var axis : List[Double] = Nil
    var dof : List[String] = Nil
    var limits : List[(Double,Double)] = Nil
    var parent : String = ""
    var children : List[String] = Nil //store name of children for now

    def act(){
      //apply translation

      //pass translation to children

    }

  }

  //def load function: check file type: bvh, asf & acm, or txt?
  def loadSkeleton(filename: String) {

  }

  def loadMotion(filename: String) {

  }

  def loadAcclaimFileASF(filename : String){     //Skeleton
    originalFileType = "acclaim"
    name = filename
    //open file
    val line = fromFile(filename).getLines()
    var where = ""
    var lin = Nil
    var temp = new SkeletonSegment
    //read a line
    for(text<-line){
      //get rid of whitespace
      lin = text.split("""[\s]+""")
      if (lin(0)==""){
        lin = lin.tail
      } else if(where == ":hierarchy"){
        //add connections between segments
        if (lin(0)=="begin" || lin(0)=="end"){
          //assume these don't matter after hierarchy(only one skel per file)
        } else {
          val currentParent = skel.find(item => item.name==lin(0))
          currentParent match{
            case Some(segment)=>
              for (j<-1 until  lin.length){
                segment.children = lin(i) :: segment.children
                val currentChild = skel.find(item2 => item2.name==lin(i))
                currentChild match{
                  case Some(segmentC)=> segmentC.parent = lin(0)
                  case None => println("No Child Bone...")
                }
              }
            case None => println("Missing Bones...")
          }
        }
      } //else: match line values and assign them
      lin(0) match{
        case "begin" =>
          //store current segment
          skel = temp :: skel
          temp = new SkeletonSegment
        case "mass" =>
          if(where==""){
            defaultMass = lin(2).toDouble()
          }
        case "length" =>
          if(where==""){
            defaultLength = lin(2).toDouble()
          } else{
            temp.length = lin(2)
          }
        case "angle" => angleUnit = lin(2)
        case "order" => temp.order = lin.tail
        case "id" => temp.id =lin(2)
        case "name" => temp.name = lin(2)
        case "direction" =>
          temp.direction = lin(4).toInt :: temp.direction
          temp.direction = lin(3).toInt :: temp.direction
          temp.direction = lin(2).toInt :: temp.direction
        case "dof" =>
          for(i<-2 until lin.length){
            temp.dof = lin(i) :: temp.dof
          }
        case "limits" =>
          where = "limits"
          temp.limits = ((lin(3).replace("(","")).toDouble , (lin(4).replace(")","")).toDouble)
        case "(_*" =>
          temp.limits = ((lin(2).replace("(","")).toDouble , (lin(3).replace(")","")).toDouble)
        case "axis" =>
          if(where==":root"){
            temp.axisCode = lin(2)
          } else{
            temp.axis = lin(4).toDouble :: temp.axis
            temp.axis = lin(3).toDouble :: temp.axis
            temp.axis = lin(2).toDouble :: temp.axis
          }
        case "position" =>
          temp.position = lin(4).toInt :: temp.position
          temp.position = lin(3).toInt :: temp.position
          temp.position = lin(2).toInt :: temp.position
        case "orientation" =>
          temp.orientation = lin(4).toInt :: temp.orientation
          temp.orientation = lin(3).toInt :: temp.orientation
          temp.orientation = lin(2).toInt :: temp.orientation
        case ":root" =>
          where = ":root"
          temp.name = "root"
        case ":bonedata" => where =":bonedata"
        case ":hierarchy" =>
          where = ":hierarchy"
          skel = skel.reverse  //to try and put in order from root out
        case _ => {}
        }
      }
    }

  def loadAcclaimFileACM(filename : String){     //Motion
    //open file
    val line = fromFile(filename).getLines()
    var where = ""
    var lin = Nil
    //var temp =
  }

  def downsampling(rate : Int) {
    motions.filter()
  }

  def preprocessing1(skel: Skeleton) {

  }

  def preprocessing2(skel: Skeleton) {

  }

  }
