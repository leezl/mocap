package main.scala

import scala.io.Source._
import scalala.tensor.dense._
//import scalala.operators.Implicits._
import scalala.tensor.dense.DenseVector._
import scalala.tensor.mutable.Matrix
//import scalala.tensor.mutable.Vector
import scalala.library.LinearAlgebra._
//import scala.actors._
import scala.actors.Actor
//import scala.actors.Actor._
import scala.collection.mutable.Map
//import scala.util.matching.Regex.Match
import scalala.library.Library._
import scala.math._
import scala.collection.mutable._
import scalala.tensor.::
import scala.util._
import java.io._

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
 //Possibly change rotmap2quat etc into Option returning functions so error returns None.
 */

class Skeleton {
  //skeleton & motions(List)
  var filetype = ""
  var name: String = ""
  var defaultLength: Double = 1.0
  var defaultMass: Double = 1.0
  var angleUnit: String = "deg" //in case we need to swap radians and degrees
  var originalFileType: String = "acclaim"
  var skel = List[SkeletonSegment]()
  var segmentList = List[String]()
  //list of all actions for this skeleton
  var Motions = List[Frames]()
  var resultFrames = new Frames
  var useFixedAction = 0
  //var MotionMatrix = DenseMatrix[Double](1,1) //what sizes? won't know till done loading...
  //list of frames; contains all bones mapped to actions
  var velocity: Double = 0.0

  class SkeletonSegment extends Actor {
    var name: String = ""
    var id: Int = 0
    var axisOrder: String = ""
    var mass: Double = 0.0
    var length: Double = 0.0
    var order = Array[String]()
    var position = DenseVector.zeros[Double](3)
    var orientation = DenseVector.zeros[Double](3)
    var direction = DenseVector.zeros[Double](3)
    var axis = DenseVector.zeros[Double](3)
    var dof = List[String]()
    var limits = List[(Double, Double)]()
    var parent : String = ""
    var children =  List[String]() //store names of children
    var rotInd = DenseVector.ones[Int](3)*-1
    var posInd = DenseVector.ones[Int](3)*-1
    var offsetInMatrix = 0

    def printSegmentDebug() {
      println("Name: " + name)
      println("ID: " + id)
      println("Parent: " + parent)
      println("\n")
    }

    def act() {
      //apply translation

      //pass translation to children

    }

  }

  class Frames{
    var frames = List[Action]()
  }
  
  class Action{
    var frameID : Int = 0
    //action is all moves for all bones, for this frame
    var action = Map[String, List[Double]]()//all channels
    var fixedAction = Map[String, DenseVector[Double]]()
    //Check that actionVector is consistent order
    var actionVector = MutableList[Double]()
    //body-centered representation
    var newRep = DenseVector.zeros[Double](3)/////////////////
    //relative to body representation
    var grdVel = DenseVector.zeros[Double](3)/////////////////
  }
  
  //def load function: check file type: bvh, asf & acm, or txt?
  def loadSkeleton(filename : String, fileTypeg : String) {
    //check file type, then call correct load function
    if (fileTypeg == "asf"){
      filetype=fileTypeg
      loadAcclaimFileASF(filename + "." + fileTypeg)
    }
    else {
      println("not acclaim type...")
      sys.exit()
    }
  }

  //load motions (check fieltype)
  def loadMotion(filename: String) {
    //check file type, then load with correct function
    if(filetype == "asf"){
      loadAcclaimFileAMC(filename)
    } else{
      println("Wrong motion file type...")
      sys.exit()
    }
  }

  //load Acclaim Skeleton
  def loadAcclaimFileASF(filename: String) {
    //Skeleton
    println("Loading Skeleton")
    originalFileType = "acclaim"
    name = filename
    println(filename)
    //open file
    val line = fromFile(filename).getLines()
    var where = ""
    var lin = Array[String]()
    var temp = new SkeletonSegment
    //read a line
    for (text <- line) {
      //get rid of whitespace
      lin = text.split("""[\s]+""")
      //println(lin(0))
      if (lin(0) == "") {
        lin = lin.tail
      }
      if (where == ":hierarchy") {
        //add connections between segments
        if (lin(0) == "begin" || lin(0) == "end") {
          //assume these don't matter after hierarchy(only one skel per file)
        } else {
          val currentParent = lookUpBone(lin(0))
          for (j <- 1 until lin.length) {
            currentParent.children = lin(j) :: currentParent.children
            val currentChild = lookUpBone(lin(j))
            currentChild.parent = lin(0)
          }
        }
      } //else: match line values and assign them
      //println(lin(0))
      lin(0) match {
        case "begin" =>
          //store current segment
          skel = temp :: skel
          temp = new SkeletonSegment
        case "mass" =>
          if (where == "") {
            defaultMass = lin(1).toDouble
          } else {
            temp.mass = lin(1).toDouble
          }
        case "length" =>
          if (where == "") {
            defaultLength = lin(1).toDouble
          } else {
            temp.length = lin(1).toDouble
          }
        case "angle" => angleUnit = lin(1)
        case "order" =>
          temp.order = lin.tail
          temp.dof = lin.tail
        case "id" => temp.id = lin(1).toInt
        case "name" => 
          temp.name = lin(1)
          segmentList = lin(1) :: segmentList
        case "direction" =>
          temp.direction(2) = lin(3).toDouble
          temp.direction(1) = lin(2).toDouble
          temp.direction(0) = lin(1).toDouble
        case "dof" =>
          for (i <- 1 until lin.length) {
            temp.dof = lin(i) :: temp.dof
          }
        case "limits" =>
          where = "limits"
          temp.limits = ((lin(1).replace("(", "")).toDouble, (lin(2).replace(")", "")).toDouble) :: temp.limits
        case "(_*" =>
          temp.limits = ((lin(0).replace("(", "")).toDouble, (lin(1).replace(")", "")).toDouble) :: temp.limits
        case "axis" =>
          if (where == ":root") {
            temp.axisOrder = lin(1)
          } else {
            temp.axis(2) = lin(3).toDouble
            temp.axis(1) = lin(2).toDouble
            temp.axis(0) = lin(1).toDouble
          }
        case "position" =>
          temp.position(2) = lin(3).toInt
          temp.position(1) = lin(2).toInt
          temp.position(0) = lin(1).toInt
        case "orientation" =>
          temp.orientation(2) = lin(3).toInt
          temp.orientation(1) = lin(2).toInt
          temp.orientation(0) = lin(1).toInt
        case ":root" =>
          where = ":root"
          temp.name = "root"
          segmentList = "root" :: segmentList
        case ":bonedata" => where = ":bonedata"
        case ":hierarchy" =>
          where = ":hierarchy"
        case _ => {}
      }
    }
    skel = skel.reverse //to try and put in order from root out
    skel.foreach(arg=> setPositionAndIndex(arg))
    //calculate C and Cinv here?
    println("Skeleton Loaded")
  }

  //load Acclaim motion
  def loadAcclaimFileAMC(filename: String) {///////tempChannel
    println("Loading Channels")
    //Motion
    //open file
    val line = fromFile(filename).getLines()
    var where = ""
    var lin = Array[String]()
    var currentValue = MutableList[Double]()
    val currentAction = new Action
    val numCheck = """[1-9]+"""
    var tempFrames =  new Frames
    var stop = 0
    //read each line
    for (text <- line) {
      lin = text.split("""[\s]+""")
      if (stop==0){
        //check FULLY-SPECIFIED?
        //check DEGREES?
        //check number, if number append current motion to list
        if(lin(0).matches(numCheck)){
          where = lin(0)//set where to current action frame
          if(lin(0)!= 1){//append motion
            tempFrames.frames = currentAction :: tempFrames.frames ///
          }
          if (lin(0).toInt >= 100){ ///////////////////////////LIMIT file size
            stop=1
          }
          currentAction.frameID = lin(0).toInt
          currentAction.action.drop(currentAction.action.size)
        }
        //check string/in skel names, append current segment to motion
        else if(segmentList.contains(lin(0))){
          //iter through dof and append to values list
          for (i<- 1 until lin.length){
            currentValue += lin(i).toDouble//need to be in correct order
          }
          //append value to segment motion
          currentAction.action += (lin(0) -> currentValue)
          currentValue.clear() //empty
        } else{

        }
      }
    }
    //sort action to be in frame order
    //channels.sort((i, j) => (i.frame< j.frame))
    tempFrames.frames.sortWith(_.frameID< _.frameID)
    tempFrames = downsampling(4,tempFrames)//downsample early to avoid memory problems
    Motions = tempFrames :: Motions
  }

  def storeResultMotions(){
    //store resultFrames in amc format
    //.asf file should be same...? yeah...
    var resultFile = new File("/data/result02.amc")
    resultFile.createNewFile()
    val writer = new FileWriter(resultFile.getPath)
    //:FULLY-SPECIFIED
    writer.write(":FULLY-SPECIFIED")
    //:DEGREES
    writer.write(":DEGREES")
    //sort frames: frame 0 to ...
    resultFrames.frames.sortWith(_.frameID < _.frameID)
    //write each frame
    for (i <- 0 until resultFrames.frames.length){
      //frame number (1-...)
      writer.write((resultFrames.frames(i).frameID+1).toString + "\n")
      //write each bone: name val val val ...
      storeResultBone(List("root"), writer, i)
    }
    //close FIle
    writer.close()
  }

  def storeResultBone(children : List, writer : FileWriter, frame : Int){
    //for each child
    for (i<- 0 until children.length){
      //check current child is in resultMotion frame
      if (resultFrames.frames(frame).fixedAction.contains(children(i))){
        //if it is, print name
        writer.write(children(i) + " ")
        //print values
        var values = resultFrames.frames(frame).fixedAction(children(i))
        for (j <- 0 until values.length){
          //print each value in order (should be correct order already
          writer.write(values(j).toString + " ")
        }
        writer.write("\n") //endline after each bone
      } else {
        println("Result Frames did not contain " + children(i))
      }
      //recursively print children of children
      var bone = lookUpBone(children(i))
      storeResultBone(bone.children, writer, frame)
    }
  }

  //downsample (drop extra frames / interpolate)
  def downsampling(rate: Int, frame : Frames) : Frames = {
    val shortTemp = new Frames
    var i=0
    //store every fourth frame
    while (i != frame.frames.length){
      if (i%rate == 0){
        shortTemp.frames = frame.frames(i) :: shortTemp.frames
      }
      i = i + 1
    }
    //replace (and fix order)
    println("Originally: " + frame.frames.length + "  Downsampled: " + shortTemp.frames.length)
    shortTemp.frames.reverse   //return
    shortTemp
  }

  //
  def preprocessing1() {
    //delta for newrep, about z
    //val velocity : Double = 0.0
    val forward = DenseVector.zeros[Double](3)
    var newForward = DenseVector.zeros[Double](3)
    var newRight = DenseVector.zeros[Double](3)
    forward(2) = 1.0
    val right = DenseVector.zeros[Double](3)
    right(0) = 1.0
    val up = DenseVector.zeros[Double](3)
    var rotMatrix = DenseMatrix.zeros[Double](3,3)
    up(1) = 1.0
    euler2expmap()//runs on skel and all motions (has full access from here)  //DEfines expMapInd? Problem: they never do this, why? In FixedAction and actionVector
    //var root = lookUpBone("root")
    var totalPiChange = 0.0 /////////unwrapping
    var previousFrame = 0.0
    var ct1 = 0.0
    var ct2 = 0.0
    //Body Centered Coordinates
    for (i<- 0 until Motions.length){
      previousFrame = 0.0 ////////////unwrapping
      for (j<- 0 until Motions(i).frames.length){
        //body rotation base
        rotMatrix = expmap2rotmap(lookUpAction(i, j, "root"))
        //normal vectors:
        newForward = ((forward).t *rotMatrix).t
        newRight = ((right).t*rotMatrix).t
        //new angles with -z
        ct1 = (((newForward).t * (-up)) / newForward.norm(2))  //norm(newForward, 2))
        Motions(i).frames(j).newRep(0) = acos(ct1)
        ct2 = (((newRight).t * (-up)) / newRight.norm(2))   //norm(newRight, 2))
        Motions(i).frames(j).newRep(1) = acos(ct2)
        //About Vertical:
        Motions(i).frames(j).newRep(2) = atan2(newRight(2), newRight(0))
        /////TRICKY unwrap stuff//////
        if (j>0){//unwrapping here...
          if ((Motions(i).frames(j).newRep(2) - previousFrame) > Pi){
            while((Motions(i).frames(j).newRep(2) - previousFrame) > Pi){
              totalPiChange = totalPiChange - (2*Pi)
              Motions(i).frames(j).newRep(2) = Motions(i).frames(j).newRep(2) - (2*Pi)
            }
          }
          if ((Motions(i).frames(j).newRep(2) - previousFrame) < -Pi){
            while((Motions(i).frames(j).newRep(2) - previousFrame) < -Pi){
              totalPiChange = totalPiChange + (2*Pi)
              Motions(i).frames(j).newRep(2) = Motions(i).frames(j).newRep(2) + (2*Pi)
            }
          }
        }
        previousFrame = Motions(i).frames(j).newRep(2)
      }
      /////////////////////////////
      //Second Half: GroundPlaneVectors
      var mydiff = DenseVector.zeros[Double](3)
      var tempRootPos = DenseVector.zeros[Double](3)
      var tempRoot1Pos = DenseVector.zeros[Double](3)
      var mh = 0.0
      var apos = 0.0
      for(j<- 0 until Motions(i).frames.length){
        if(j < Motions(i).frames.length-1){ //not last frame
          mydiff *=0.0
          mydiff(0) = Motions(i).frames(j+1).newRep(2) - Motions(i).frames(j).newRep(2)
          tempRootPos = lookUpAction(i, j+1, "root").asCol
          tempRoot1Pos = lookUpAction(i, j, "root").asCol
          mydiff(1) = tempRootPos(0) - tempRoot1Pos(0)
          mydiff(2) = tempRootPos(2) - tempRoot1Pos(2)
          mh = DenseVector(Array(mydiff(1), mydiff(2))).norm(2)//norm(DenseVector(Array(mydiff(1), mydiff(2))), 2) ///horizontal magnitude
          apos = atan2(mydiff(2), mydiff(1))
          Motions(i).frames(j).grdVel(0) = mh * cos(Motions(i).frames(j).newRep(2) - apos)
          Motions(i).frames(j).grdVel(1) = mh * sin(Motions(i).frames(j).newRep(2) - apos)
          Motions(i).frames(j).grdVel(2) = mydiff(0)
        } else {
          //last frame default
          Motions(i).frames(j).grdVel = Motions(i).frames(j-1).grdVel
        }
      }
      for (j<- 0 until Motions(i).frames.length){//WOW THIS COULD BE WRONG: Assign NewRep to ROOT of each frame:
        Motions(i).frames(j).fixedAction("root") = DenseVector(List(Motions(i).frames(j).newRep(0), Motions(i).frames(j).newRep(1), Motions(i).frames(j).grdVel(2), Motions(i).frames(j).grdVel(0), Motions(i).frames(j).grdVel(1), Motions(i).frames(j).fixedAction("root")(2)).toArray)
        Motions(i).frames(j).actionVector(0) = Motions(i).frames(j).newRep(0)
        Motions(i).frames(j).actionVector(1) = Motions(i).frames(j).newRep(1)
        Motions(i).frames(j).actionVector(2) = Motions(i).frames(j).grdVel(2)
        Motions(i).frames(j).actionVector(3) = Motions(i).frames(j).grdVel(0)
        Motions(i).frames(j).actionVector(4) = Motions(i).frames(j).grdVel(1)
        Motions(i).frames(j).actionVector(5) = Motions(i).frames(j).fixedAction("root")(2)
      }
    }
  }

  def euler2expmap(){ ///check that List values are set correctly now...
    //for each motion
    for (i<- 0 until Motions.length){
      //for each frame
      for (j<- 0 until Motions(i).frames.length){
        //leave root position same
        //get orientation vector (root)
        val bone = lookUpBone("root")
        val rotVal = bone.orientation
        var list = DenseVector.zeros[Double](3)
        for (k<- 0 until bone.rotInd.length) { //till 3, really /bone.rotInd.length
          list = (lookUpAction(i, j, "root")).asCol
          if (bone.rotInd!=-1){
            rotVal(k) = rotVal(k) + list(bone.rotInd(k))
          } else {
            rotVal(k) = 0
          }
        }
        //make rotation matrix
        val rotMap = makeRotationMatrix(toRadians(rotVal(0)), toRadians(rotVal(1)), toRadians(rotVal(2)), bone.axisOrder)
        //make expmap from rotmap
        val rot = rotmap2expmap(rotMap)
        //set channels (new values)
        if(rot.length > 3){
          println("ROT TOO LARGE?")
        }
        //Motions(i).frames(j).fixedAction("root") = rot
        Motions(i).frames(j).fixedAction("root") = rot
        for (k<-0 until  rot.length){
          Motions(i).frames(j).actionVector += rot(k)
        }
        //expMapInd? not needed...
        //convert children...Yay...
        convertChildren(bone.children, i, j)
      }
    }
    useFixedAction = 1

  }

  def convertChildren(currentBones : List[String], motion:Int, frame:Int) {
     if (currentBones == Nil){
       //return
     } else {
       for (i<- 0 until currentBones.length){
         val bone = lookUpBone(currentBones(i))
         val rotVal = DenseVector.zeros[Double](3)
         var list = DenseVector.zeros[Double](3)
         for(j<- 0 until bone.rotInd.length){ //till 3 really / bone.rotInd.length
           list = (lookUpAction(motion, frame, currentBones(i))).asCol
           if (bone.rotInd!=-1){
             rotVal(j) = list(bone.rotInd(j))
           } else{
             rotVal(j) = 0
           }
         }
         //make rotation matrix
         val rotMap = makeRotationMatrix(toRadians(rotVal(0)), toRadians(rotVal(1)), toRadians(rotVal(2)), bone.axisOrder)
         //make expmap from rotmap
         val rot = rotmap2expmap(rotMap)
         //set channels (new values)
         if(rot.length > 3){
           println("ROT TOO LARGE?")
         }
         Motions(motion).frames(frame).fixedAction(currentBones(i)) = rot
         if (Motions(0).frames(0).actionVector.length!=bone.offsetInMatrix){
           if (bone.offsetInMatrix!=0){
             println("Bone Offset in matrix has changed...Pad with zeros?")
           } else{

           }
         }
         bone.offsetInMatrix = Motions(motion).frames(frame).actionVector.length///get bone's position in matrix
         var bone2 = lookUpBone(currentBones(i))
         if (bone2.offsetInMatrix!=bone.offsetInMatrix){
           println("OFFSET NOT SAVED")
         }

         for (k<-0 until  rot.length){
           Motions(motion).frames(frame).actionVector += rot(k)
         }
         //expMapInd? not needed...
         //convert children...Recurse, god I hope this finishes...
         convertChildren(bone.children, motion, frame)
       }
     }
     //end
  }

  def rotmat2euler(rotmat: DenseMatrix[Double]) : DenseVector[Double] = {//NEED TO PROPAGATE
  var E1, E2, E3 = 0.0
    if (rotmat(0,2)==1 || rotmat(0,2) == -1){
      E3 = 0
      dlta = atan2(rotmat(0,1),rotmat(0,2))
      if (rotmat(0,2)==-1){
        E2 = Pi/2
        E1 = E3 + dlta
      } else {
        E2 = -Pi/2
        E1 = -E3 + dlta
      }
    } else{
      E2 = -asin(rotmat(0,2))
      E1 = atan2(rotmat(1,2)/cos(E2), rotmat(2,2)/cos(E2))
      E3 = atan2(rotmat(0,1)/cos(E2), rotmat(0,0)/cos(E2))
    }
    DenseVector(E1.toDegrees, E2.toDegrees, E3.toDegrees)  //return
  }

  def makeRotationMatrix(xAngle:Double, yAngle:Double,  zAngle:Double, order:String="zxy") : DenseMatrix[Double] = {
    val c1 = cos(xAngle)
    val c2 = cos(yAngle)
    val c3 = cos(zAngle)
    val s1 = sin(xAngle)
    val s2 = sin(yAngle)
    val s3 = sin(zAngle)
    var rotMatrix = DenseMatrix.eye[Double](3)
    order match{
      case "zxy" =>
        rotMatrix = DenseMatrix((c2*c3-s1*s2*s3, c2*s3+s1*s2*c3, -s2*c1), (-c1*s3, c1*c3, s1), (s2*c3+c2*s1*s3, s2*s3-c2*s1*c3, c2*c1))
      case _=>
        for (i<-0 until order.length){
          order(i).toLower match{
            case 'x' =>
              rotMatrix = DenseMatrix((1.0, 0.0, 0.0), (0.0, c1, s1), (0.0, -s1, c1)) * rotMatrix
            case 'y' =>
              rotMatrix = DenseMatrix((c2, 0.0, -s2), (0.0, 1.0, 0.0), (s2, 0.0, c2)) * rotMatrix
            case 'z' =>
              rotMatrix = DenseMatrix((c3, s3, 0.0), (-s3, c3, 0.0), (0.0, 0.0, 1.0)) * rotMatrix
            case wtf =>
              println(wtf)
              sys.exit()
          }
        }
    }
    rotMatrix
  }

  def rotmap2expmap(rot : DenseMatrix[Double]) : DenseVector[Double] = {
    quat2expmap(rotmap2quat(rot)) //return
  }
  
  def expmap2rotmap(rot : DenseVector[Double]) : DenseMatrix[Double] = {
    val theta = rot.norm(2)//norm(rot, 2)
    var rO = rot / (rot.norm(2)+ scalala.library.Library.pow (2,-52))
    //norm(rot,2)+ scalala.library.Library.pow(2, -52))
    /*if (theta>Pi){
      println("expmap(2rotmap) not canonical form")
      sys.exit()
    }*/
    var rOx = Matrix((0.0, -rO(3), rO(2)), (0.0, 0.0, -rO(1)), (0.0, 0.0, 0.0))
    rOx = rOx-rOx.t
    (DenseMatrix.eye[Double](3) += rOx*=(sin(theta)) += (rOx*=(1-cos(theta))):*=rOx) //return
  }
  
  def quat2expmap(q : DenseVector[Double]) : DenseVector[Double] = {//check return type
    if (scalala.library.Library.abs(norm(q, 2)-1)>1*scalala.library.Library.pow(10, -3)){
      println("Quaternion not norm 1")
      sys.exit()
    }
    val tempSlice = DenseVector(q(1), q(2), q(3))
    val sinhalftheta=norm(tempSlice, 2)
    val coshalftheta=q(0)
    var rO = DenseVector.zeros[Double](3)
    rO = tempSlice/(norm (tempSlice, 2) +  scalala.library.Library.pow(2, -52)) //check this type worked
    var theta = 2*atan2(sinhalftheta, coshalftheta)
    theta = (theta+2*Pi)%(2*Pi)
    if (theta>Pi){
      theta = 2*Pi-theta
      rO = -rO
    }
    DenseVector(rO(0)*theta, rO(1)*theta, rO(2)*theta) //return
  }
  
  def rotmap2quat(rotMat : DenseMatrix[Double]) : DenseVector[Double] = { //check return type
    val check = (rotMat*rotMat.t-DenseMatrix.eye[Double](3))
    if (norm(check.data, 2)> 1*scalala.library.Library.pow(10, -10) || det(rotMat)<0){
      println("Rotation matrix failed Quaternion test")
      sys.exit()
    }
    var dat = DenseMatrix.zeros[Double](3,3)
    dat = rotMat-rotMat.t            //Matrix 3x3
    val rot = DenseVector(-dat(1,2), dat(0,2), -dat(0,1))  //Vector 1x3
    val sintheta = norm(rot, 2)/2                       //Scalar 1
    var rO = DenseVector.zeros[Double](3)
    rO = rot / (norm(rot, 2) +  scalala.library.Library.pow(2, -52)) //Vector 1x3
    val costheta = (rotMat.trace-1)/2                   //Scalar 1
    val theta = atan2(sintheta, costheta)          //Scalar 1
    ///check this came out right form
    DenseVector(cos(theta/2), rO(0)*sin(theta/2), rO(1)*sin(theta/2), rO(2)*sin(theta/2))  //return
  }

  def setPositionAndIndex(bone : SkeletonSegment){
    if (bone.name == "root") {
      for (i<- 0 until bone.order.length){
        if (bone.order(i) == "TX"){
          bone.posInd(0) = i
        } else if (bone.order(i) == "TY"){
          bone.posInd(1) = i
        } else if (bone.order(i) == "TZ"){
          bone.posInd(2) = i
        } else if (bone.order(i) == "RX"){
          bone.rotInd(0) = i
        } else if (bone.order(i) == "RY"){
          bone.rotInd(1) = i
        } else if (bone.order(i) == "RZ"){
          bone.rotInd(2) = i
        } else {
          println("No order match..." + bone.order(i))
          sys.exit()
        }
      }
    } else {
      for (i<- 0 until bone.dof.length){
        if (bone.dof(i) == "tx"){
          bone.posInd(0) = i
        } else if (bone.dof(i) == "ty"){
          bone.posInd(1) = i
        } else if (bone.dof(i) == "tz"){
          bone.posInd(2) = i
        } else if (bone.dof(i) == "rx"){
          bone.rotInd(0) = i
        } else if (bone.dof(i) == "ry"){
          bone.rotInd(1) = i
        } else if (bone.dof(i) == "rz"){
          bone.rotInd(2) = i
        } else {
          println("No order match..." + bone.dof(i))
          sys.exit()
        }
      }
    }
  }

  //check load with this
  def printSkeletonDebug() {
    println("Name: " + name)
    println("Default Length: " + defaultLength)
    println("Default Mass: " + defaultMass)
    println("Angle Units: " + angleUnit)
    println("File Format: " + originalFileType)
    println("Skel: ")
    skel.foreach(arg => arg.printSegmentDebug())
  }

  def lookUpBone(bone : String) : SkeletonSegment = {
    skel.find(arg => arg.name==bone) match{
      case Some(x) =>
        x
      case None => 
        println("Failed match...")
        new SkeletonSegment
        sys.exit()
    }
  }
  
  def lookUpAction(motion : Int,  frame : Int, bone : String) : DenseVector[Double] = {
    if (useFixedAction==0){
      Motions(motion).frames(frame).action.get(bone) match{
        case Some(x) =>
          DenseVector(x.toArray).t
        case None =>
          //println("No match in Get..." + bone)
          //Motions(motion).frames(frame).action.foreach{case (key, value) => println(key)}
          DenseVector.zeros[Double](3)
          //sys.exit()
      }
    } else{//get from array actionVector/ fixedAction
      Motions(motion).frames(frame).fixedAction.get(bone) match{
        case Some(x) =>
          DenseVector(x.toArray).t
        case None =>
          //println("No match in Get..." + bone)
          //Motions(motion).frames(frame).action.foreach{case (key, value) => println(key)}
          DenseVector.zeros[Double](3)
        //sys.exit()
      }
    }
  }

  def cleanMemory(){
    for(i <- 0 until Motions.length){
      for(j <- 0 until Motions(i).frames.length){
        Motions(i).frames(j).action.clear()
        Motions(i).frames(j).fixedAction.clear()
      }
    }
  }

}
