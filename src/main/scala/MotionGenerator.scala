package main.scala

import util.Random
import util.Random._
import java.util.Collections
import scalala.tensor.dense._
import scalala.tensor.dense.DenseVector._
import scalala.tensor.dense.DenseMatrix._
import scala.collection.mutable._
import scalala.scalar._
import scalala.tensor.::
import scalala.tensor.mutable._
import scalala.tensor.sparse._
import scalala.library.Library._
import scalala.library.LinearAlgebra._
import scalala.library.Statistics._
import scalala.library.Plotting._
import scalala.operators.Implicits._
import swing.Action

/**
 * Created by IntelliJ IDEA.
 * User: lieslw
 * Date: 4/22/12
 * Time: 9:41 AM
 * To change this template use File | Settings | File Templates.
 *
 * Main Object.
 */

class MotionGenerator() {
  //(skeletonFile : String, motionFiles : List[String])
  //set initial variables
  //nX = Model order: time looked forward and back in each layer
  val n1 = 3
  val n2 = 3
  var batchsize = 100
  var numvalidatebatches = 0
  var minibatchsize = 0 //?
  var numdims = 0
  var numcases = 0
  var batchIndex = MutableList[Int]()
  //contains List of DenseVectors (batches of indexes into batchData)
  var minibatchIndex = MutableList[DenseVector[Int]]()
  //var batchData = DenseMatrix.zeros[Double](3,3) //created in init, passed to all
  //Default network properties:
  var numhid1 = 150
  var numhid2 = 150
  var numepochs = 10 //2000
  //Learning Rates:
  var epsilonw = 1*pow(10,-3)   //undirected
  var epsilonbi = 1*pow(10,-3)  //visibles
  var epsilonbj = 1*pow(10,-3)  //hidden units
  var epsilonA = 1*pow(10,-3)   //autoregressive
  var epsilonB = 1*pow(10,-3)   //prev visibles to hidden
  var wdecay = 0.0002  //same decay for all..
  var mom = 0.9        //momentum
  var gsd = 1   //gaussian std dev
  var w1 = DenseMatrix.zeros[Double](3,3) //is this legal
  var bi1 = DenseVector.zeros[Double](3)
  var bj1 = DenseVector.zeros[Double](3)
  var A1 = MutableList[DenseMatrix[Double]]()
  var B1 = MutableList[DenseMatrix[Double]]()
  var bjstar = DenseMatrix.zeros[Double](3,3)
  var batchData = DenseMatrix.zeros[Double](3,3)
  //second Layer
  var w2 = DenseMatrix.zeros[Double](3,3) //is this legal
  var bi2 = DenseVector.zeros[Double](3)
  var bj2 = DenseVector.zeros[Double](3)
  var A2 = MutableList[DenseMatrix[Double]]()
  var B2 = MutableList[DenseMatrix[Double]]()
  var startTime = 0.0
  var endTime = 0.0
  var midStartTime =0.0
  var midEndTime = 0.0
  //var nt = n1, n2, etc //crbm order how far forward and back (3 default)
  //var numhid = numhid1, numhid2, etc
  var restart = 1
  var filteringdist =0.0
  var initData = DenseMatrix.zeros[Double](3,3)
  var dataMean = DenseVector.zeros[Double](dim)
  var dataStd = DenseVector.zeros[Double](dim)
  //Randoms: need uniform and gaussian random weights for matrices in crbms;
  //  may need 2 separate? can't see where they needed uniform dist.
  //  rand.nextDouble, rand.nextGaussian
  //val rand = new Random(System.currentTimeMillis())

  //load data (file/list of files)
  //soooo...only use data from one skeleton? limits data and system.
  // Add skeleton to data going into network?: larger rewrite. Change later.
  //default asf for now? Load one .asf skel, then load motions?(.acm)
  val skel = new Skeleton

  def init(skeletonFile : String,  motionFiles : List[String]){
    skel.loadSkeleton(skeletonFile,"asf")//check these
    motionFiles.foreach(file => skel.loadMotion(file))//check these
    //Downsample/ preprocess (Put these together? why didn't they?)
    //skel.downsampling(4)
    //get exponential map representation for all
    skel.preprocessing1()
    skel.cleanMemory()
    //split up into batches: batchData is Dense Matrix of totalFrames x BoneVals
    batchData = preprocessing2()
    //split into minibatches (collect indices into batchData for this)
    preprocessing3()
    //Bone dimensions
    numdims = batchData.numCols
    //GET some intializer frames
    if (batchData.numRows>100){
      initData = batchData(1 to  100, ::) //get first hundred (check longer then 100)
    }
    //try running
    println("Training Layer 1 CRBM, order " + n1 + " :" + numdims + " - " + numhid1)
    gaussiancrbm(n1, numhid1)


    //plot weights figure YAY SCALALA HAS PLOTTING
    weightreport(n1)

    restart = 1
    //set minibatchIndex, filteringdist, bjstar
    getfilteringdist()
    println("Training layer 2")
    binarycrbm(n2, numhid2)
    println("Done training layer 2")

    //save weights?

    //generate sequence
    println("Begining Generation")
    var visibles = gen()
    println("Done Generating")

    //postprocess/save as acm file with channels (mark as skeleton num)
    var newData = postprocess1()

    postprocess2()

    //plot weights

    //plot activations?

    //play data/ save data again

  }

  //change back to body centered coordinates; undo preprocess2
  def postprocess1() = DenseMatrix[Double]{//CHECK ROW VS COL HERE
    //fix normalization (undo)
    var newdata : DenseMatrix[Double]  = repmat(visible.numRows,dataStd,0) :* visible + repmat(visible.numRows,dataMean,0)

    ////POSTPROCESS2
    //extract angles...//newdata is in...expmap form, need to return to euler angles...
    var phi = newdata(::, 0) //vector of root rotations
    var theta = newdata(::,1)  //vector root y rot (or is it z in this axis setup?)
    var vertrotdelta = newdata(::,2) //root z rot
    var groundxdelta = newdata(::,3) //root x pos
    var groundydelta = newdata(::,4) //root y pos
    var pos_x = skel.lookUpBone("root").posInd(0)
    var pos_y = skel.lookUpBone("root").posInd(1)
    var pos_z = skel.lookUpBone("root").posInd(2)
    var rot_x = skel.lookUpBone("root").rotInd(0)//expmapInd
    var rot_y = skel.lookUpBone("root").rotInd(1)
    var rot_z = skel.lookUpBone("root").rotInd(2)

    //temps
    var m, dab, be, psi = 0.0
    var ux, uy, uz  = 0.0
    var uz_over_ux = 0.0
    var a, b, c = 0.0
    var vz, vx = 0.0
    var rxx, rxy, rxz, ryx, ryy, ryz, rzx, rzy, rzz = 0.0
    var u = DenseVector.zeros[Double](3)
    var v = DenseVector.zeros[Double](3)

    //MOVE vertical position back
    newdata(::,pos_y) = newdata(::,6)

    //FIRST FRAME
    newdata(0,rot_y) = 0
    newdata(0,pos_x) = 0
    newdata(0,pos_z) = 0
    //END FIRST FRAME

    //ALL OTHER FRAMES
    for(i <- 1 until newdata.numRows){
      //Convert to standard reference frame
      newdata(i,rot_y) = newdata(i-1,rot_y) + vertrotdelta(i-1)
      //fix position with magnitude and offset
      m = norm(DenseVector(groundxdelta(i-1), groundydelta(i-1)), 2)
      dab = atan2(groundydelta(i-1),groundxdelta(i-1))
      be = newdata(i-1,rot_y)
      ///original orientation
      al = be - dab
      newdata(i,pos_x) = newdata(i-1,pos_x) + m*cos(al)
      newdata(i,pos_z) = newdata(i-1,pos_z) + m*sin(al)

    }

    //CONVERT TO EXPMAPS (from body-centered)
    for(i <- 0 until newdata.numRows){
      //rewrap
      psi = newdata(i,rot_y)%(2*Pi)
      uy = -cos(theta(i))

      //find x and z
      ux = 0.0
      uz = 0.0
      uz_over_ux = 0.0
      if (psi < Pi/2){
        uz_over_ux = tan(psi)
        ux = sqrt((1 - uy^2)/(1 + uz_over_ux^2))
        uz = ux*uz_over_ux
      } else if (psi < Pi ){
        uz_over_ux = tan(Pi -psi)
        ux = -sqrt((1 - uy^2)/(1 + uz_over_ux^2))
        uz = ux*uz_over_ux
      } else if(psi < (3*Pi/2)){
        uz_over_ux = tan(psi-Pi)
        ux = -sqrt((1 - uy^2)/(1 + uz_over_ux^2))
        uz = ux*uz_over_ux
      } else{
        uz_over_ux = tan(2*Pi - psi)
        ux = sqrt((1 - uy^2)/(1 + uz_over_ux)^2)
        uz = -ux*uz_over_ux
      }

      //Solve for vz
      a = (ux^2 + uz^2)
      b = 2*uy*uz*vy
      c = (ux^2 * vy^2 - ux^2 + uy^2 * vy^2)

      //GEt solution for correct quadrant (right hip, not left)
      if((psi > (3*Pi/2)) || (psi < Pi/2)){
        vz = (-b + sqrt(b^2 - 4*a*c))/(2*a)
      } else{
        vz = (-b - sqrt(b^2 - 2*a*c))/(2*a)
      }

      //Get Rotation Matrix
      vx = (uy*cos(phi(i)) - uz*vz) / ux
      u = DenseVector(ux, uy, uz)
      v = DenseVector(vx, vy, vz)
      if(abs(norm(v,2)-1)> 1E5){
        println("Not a unit vector..opps")
        v = v/norm(v,2)//Force Unit Vector...this shouldn't have happened...
      }

      wv = -cross(u,v)

      if(abs(u)-1 > 1E-5 || abs(norm(v)-1)>1E-5 || abs(norm(wv)-1)>1E-5){
        println("Somethings is still not a unit vector...")
      }

      rxx = u dot DenseVector(1, 0, 0)
      rxy = u dot DenseVector(0, 1, 0)
      rxz = u dot DenseVector(0, 0, 1)
      ryx = wv dot DenseVector(1, 0, 0)
      ryy = wv dot DenseVector(0, 1, 0)
      ryz = wv dot DenseVector(0, 0, 1)
      rzx = v dot DenseVector(1, 0, 0)
      rzy = v dot DenseVector(0, 1, 0)
      rzz = v dot DenseVector(0, 0, 1)

      var R = ((rxx,ryx,rzx),(rxy,ryy,rzy),(rxz,ryz,rzz)).t //This is not in zyx form...?
      /*var r = skel.rotmap2expmap(R)//skel.rotmat2euler(R) //wow, rotmat = rotmap   ////MAY NEED TO TRANSFER TO CHILDREN......CHECK EULER2EXPMAP */
      //need to change into Euler Angles instead, need to propogate?
      var r = skel.rotmat2euler(R)
      if (rot_x!=-1){
        newdata(i,rot_x) = r(0)
      }
      if (rot_y!=-1){
        newdata(i,rot_y) = r(1)
      }
      if (rot_z!=-1){
        newdata(i,rot_z) = r(2)
      }
      if (r.length>3){
        println("r greater then 3...?")
      }
      //write to skeleton
      var tempAction = new skel.Action
      tempAction.frameID = i //set frame number to i
      //add root
      var tempVector = DenseVector.zeros[Double](6)
      tempVector(::) = newdata(i,0 to skel.lookUpBone("root").dof.length)//how many degrees of freedom = how many values per bone
      tempAction.fixedAction("root") = tempVector

      //repeat above for children:
      postprocessChildren(skel.lookUpBone("root").children, i, newdata, tempAction)

      //tempAction now full of 1 frame of data for all bones (we hope)
      skel.resultFrames.frames += tempAction

    }//end all frames processed

    //skel.resultFrames now contains all frames, all bones, etc...try to save to file...

  }

  def postprocessChildren(children : List[String], frame : Int, newdata : DenseMatrix[Double], tempAction : skel.Action){//newdata, tempAction (add to tempAction.fixedAction
    for(i <- 0 until children.length){
      //get bone
      var bone = skel.lookUpBone(children(i))
      //Find rot vector in newdata
      var rot = DenseVector.zeros[Double](3)
      for (i<-0 until bone.rotInd.length){
        if (rotInd(i)!=-1){
          rot(i) = newdata(frame, bone.offsetInMatrix+bone.rotInd(i))
        } else{
          rot(i) = 0
        }
      }
      //expmap2rotmap
      //rotmat2euler
      var rotFixed = skel.rotmat2euler(skel.expmap2rotmap(rot))///Hope this is correct
      //move pos and rot to tempAction
      //check order
      //not all degrees of freedom, check exist, leave out if 0 by default
      var valueVector = DenseVector.zeros[Double](bone.dof.length)
      for (j <- 0 until bone.dof.length){
        //add what exists in dof, check that others are zero, if they aren't...well that's bad
        if (bone.dof(j) == "tx"){
          valueVector(bone.posInd(0)) = newdata(frame,bone.offsetInMatrix+bone.posInd(0))
        } else if (bone.dof(i) == "ty"){
          valueVector(bone.posInd(1)) = newdata(frame,bone.offsetInMatrix+bone.posInd(1))
        } else if (bone.dof(i) == "tz"){
          valueVector(bone.posInd(2)) = newdata(frame,bone.offsetInMatrix+bone.posInd(2))
        } else if (bone.dof(i) == "rx"){
          valueVector(bone.rotInd(0)) = rotFixed(0)
        } else if (bone.dof(i) == "ry"){
          valueVector(bone.rotInd(1)) = rotFixed(1)
        } else if (bone.dof(i) == "rz"){
          valueVector(bone.rotInd(2)) = rotFixed(2)
        } else {
          println("No order match..." + bone.dof(i))
          sys.exit()
        }
      }
      //ValueVector should contain whatever needs to be printed next to the given bone, now add it to tempAction
      tempAction.fixedAction(children(i)) = valueVector

      //recurse to children
      postprocessChildren(bone.children,frame,newdata,tempAction)

    }//end children update
  }

  def gen() : DenseMatrix[Double] = {
    var A2flat = DenseMatrix[Double](numhid1, numhid1*n2)
    var B2flat = DenseMatrix[Double](numhid2, numhid1*n2)
    for (i<- 0 until numhid1){
    //make cols = numhid1, change rows to be all n2...Append Matrix in each place in list,
      var tempVector = DenseVector.zeros[Double](n2*numhid1)
      var tempList = MutableList[Double]()
      for (j<- 0 until n2){
        for (k<- 0 until A2(j).numCols)
        tempList += A2(j)(i,k)  //store in order
      }
      tempVector := DenseVector(tempList.toArray)
      A2flat(i,::) = tempVector(::)
    }
    for (i<- 0 until numhid2){
      //make cols = numhid1, change rows to be all n2...Append Matrix in each place in list,
      var tempVector = DenseVector.zeros[Double](n2*numhid1)
      var tempList = MutableList[Double]()
      for (j<- 0 until n2){
        for (k<- 0 until B2(j).numCols)
          tempList += B2(j)(i,k)  //store in order
      }
      tempVector := DenseVector(tempList.toArray)
      B2flat(i,::) = tempVector(::)
    }

    var numGibbs = 30
    var max_clamped = n1+n2
    numcases = n2
    numdims = initData.numCols
    var fr = 4
    var numframes = 400

    //visible data
    var visible = DenseMatrix.zeros[Double](numframes, numdims)
    for (i <- 0 until max_clamped){
      visible(i,::) = initData(i+fr, ::)
    }

    //dATA
    var data = MutableList[DenseMatrix[Double]]()
    for(i <- 0 until n1+1){         numhid =
      data += DenseMatrix.zeros[Double](numcases, numdims)
    }
    var dataindex = DenseVector.range(n1+1, max_clamped)

    for (i<- 0 until dataIndex.length){
      data(0)(i,::) = visible(i, ::)
    }
    //store delayed data
    for(hh <- 0 until n1){
      for (i <- 0 until dataIndex.length){
        data(hh+1)(i,::) = visible(i-hh,::)
      }
    }

    //visible to hidden contributions
    bjstar = DenseMatrix.zeros[Double](numhid1, numcases)
    for (hh <- 0 until n1){
      bjstar += B1(hh)*data(hh+1)
    }

    var eta = w1*(data(0)/gsd).t + repmat(numcases,bj1,0) + bjstar
    var hposteriors = 1/(1 + exp(-eta))

    //initialize hidden layer 1 (first n1 frames padded)
    var hidden1 = DenseMatrix.ones(numframes,numhid1)
    var j =0
    for (i <- n1+1 until n1+n2){
      hidden1(i, ::) = hposteriors.t(j,::)//tedious, but necessary
      j++
    }

    //init second layer
    var hidden2 = DenseMatrix.ones[Double](numframes,numhid2)

    //keep track of last couple frames: //reverse rows hidden1 from max_clamped+1-n2 to max_clamped, then transpose
    var tempMatrix = DenseMatrix.zeros[Double](max_clamped-max_clamped+1-n2, hidden1.numCols)
    j = 0
    for (i<- 0 until max_clamped-max_clamped+1-n2){//max_clamped+1-n2 until max_clamped){
      tempMatrix(j,::) = hidden1(max_clamped-i,::)
      j++
    }
    tempMatrix = tempMatrix.t
    var tempList = MutableList[Double]()
    for (i <- 0 until tempMatrix.numRows){
      for (j <- 0 until tempMatrix.numCols){
        tempList += tempMatrix(i,j)
      }
    }
    var past = DenseVector(tempList.toArray)

    ////Generate Hidden Sequence
    println("Generating Hidden Sequence")
    for (tt <- max_clamped+1 until numframes){
      //initialize using last frame
      hidden1(tt,::) = hidden1(tt-1,::)

      //dynamic Biases
      var bistar = A2flat*past
      bjstar = B2flat*past

      //Gibbs Sampling
      for (gg <- 0 until numGibbs){
        var bottomup = w2*hidden1(tt,::).t
        var eta = bottomup + bj2 + bjstar

        var hposteriors = 1/(1 + exp(-eta))

        //activate hidden
        var tempVector = DenseVector.rand(numhid2)
        for (i <- 0 until nimhid2){
          hidden2(tt, i) = max(hposteriors(i), tempVector(i))
        }

        //downward pass
        var topdown = hidden2(tt,::)*w2

        eta = topdown + bi2.t + bistar.t
        hidden1(tt,::) = 1/(1+exp(-eta))

      }

      //Done Gibbs Sampling
      var topdown = hposteriors.t*w2
      var eta = topdown + bi2.t + bistar.t
      hidden1(tt,::) = 1/(1+exp(-eta))

      //update history
      tempList.clear()
      for (i <- 0 until numhid1){
        tempList += hidden1(tt, i)
      }
      tempList
      for (i<- 1 until past.length-numhid1){
        tempList += past(i)
      }
      past = DenseVector(tempList.toArray)

      if (tt%10 == 0){
        println("finished frame: " + tt)
      }

    }

    //Generate Visible Data
    println("Generating Visible")
    for(tt <- max_clamped+1 until numframes){
      //add autoregressive contributions
      bistar = DenseVector.zeros[Double](numdims)
      for (hh <- 0 until n1){
        bistar += A1(hh)*visible(tt-hh,::).t
      }

      //Mean-field Approximation
      var topdown = gsd*(hidden1(tt,::)*w1)
      visible(tt,::) = topdown + bi1.t + bistar.t

    }
    visible //return

  }

  def binarycrbm(nt : Int, numhid : Int){

    //batchData = filteringdist
    numbatches = minibatchIndex.length
    numcases = batchData.numCols //CHeck this
    //nt =n2, numhid = numhid2
    //w = w2, bj = bj2, bi = bi2, A = A2, B = B2
    var wgrad = DenseMatrix.zeros[Double](numhid,numdims) //make a default for now
    var negwgrad = DenseMatrix.zeros[Double](numhid,numdims)
    var negbigrad = DenseVector.zeros[Double](numdims)
    var negbjgrad = DenseVector.zeros[Double](numhid)
    w2 = (DenseMatrix.randn(numhid, numdims)*=0.01)                  //w(numhid,numdims)
    bi2 = (DenseVector.randn(numdims)*=0.01)
    bj2 = ((DenseVector.randn(numhid)*=0.01)+=(-1))  //favor OFF units
    var bigrad = DenseVector.zeros[Double](numdims)
    var bjgrad = DenseVector.zeros[Double](numhid)
    //autoregressive weights: A(:,:,j) weight from t-j to visible
    var Aupdate = MutableList[DenseMatrix[Double]]()
    var Agrad = MutableList[DenseMatrix[Double]]()
    var negAgrad = MutableList[DenseMatrix[Double]]()
    //weights from previous timesteps to hiddens: B(:,:,j) is weight from t-j to hidden layer
    var Bupdate = MutableList[DenseMatrix[Double]]()
    var Bgrad = MutableList[DenseMatrix[Double]]()
    var negBgrad = MutableList[DenseMatrix[Double]]()
    var data = MutableList[DenseMatrix[Double]]()
    ///
    var wupdate = DenseMatrix.zeros[Double](numhid, numdims)//.zero //
    var biupdate = DenseVector.zeros[Double](numdims)//.zero //
    var bjupdate = DenseVector.zeros[Double](numhid)//.zero //
    var tempMatrix = DenseMatrix.zeros[Double](3,3)
    var bistar = DenseMatrix.zeros[Double](3,3)
    var eta = DenseMatrix.zeros[Double](3,3)
    var hposteriors = DenseMatrix.zeros[Double](3,3)
    var hidstates = DenseMatrix.zeros[Double](3,3)

    var epoch = 1
    var momentum = 0.0
    var errsum = 0.0
    var mb = DenseVector.zeros[Int](3)

    println("Making List storage...")
    //get matrix Lists set up
    midStartTime = System.currentTimeMillis()
    for (i<- 0 until nt){
      A2 += (DenseMatrix.randn(numdims,numdims)*=0.01)
      Aupdate += DenseMatrix.zeros[Double](numdims,numdims)
      Agrad += DenseMatrix.zeros[Double](numdims,numdims)
      negAgrad += DenseMatrix.zeros[Double](numdims,numdims)
      B2 += (DenseMatrix.randn(numhid,numdims)*=0.01)
      Bupdate += DenseMatrix.zeros[Double](numhid,numdims)
      Bgrad += DenseMatrix.zeros[Double](numhid,numdims)
      negBgrad += DenseMatrix.zeros[Double](numhid,numdims)
    }
    midEndTime = System.currentTimeMillis()
    println("Initializing A and B took " + (midEndTime-midStartTime))

    println("Done initializing")

    if (restart == 1){
      midStartTime = System.currentTimeMillis()
      restart = 0
      epoch = 1

      //Initialize Weights
      //empty all:
      wgrad*=0.0
      bigrad*=0.0
      bjgrad*=0.0
      negwgrad*=0.0
      negbigrad*=0.0
      negbjgrad*=0.0
      //Keep previous updates for momentum?
      wupdate*=0.0 //
      biupdate*=0.0 //
      bjupdate*=0.0 //
      for (i<- 0 until nt){
        A2(i) := DenseMatrix.randn(numdims,numdims)*=0.01
        Aupdate(i)*=0.0
        Agrad(i)*=0.0
        negAgrad(i)*=0.0
        B2(i) := DenseMatrix.randn(numhid,numdims)*=0.01
        Bupdate(i)*=0.0
        Bgrad(i)*=0.0
        negBgrad(i)*=0.0
      }
      midEndTime = System.currentTimeMillis()
      println("Restart took: " + (midEndTime-midStartTime))
    }//be sure updates leave here with 0 values

    //MAIN//
    //Loop across Epochs
    startTime = System.currentTimeMillis()
    for (epoch<-epoch until numepochs){
      println("Begining new epoch " + epoch + " of " + numepochs)
      errsum = 0.0
      //Loop Across Batches
      for (batch <- 0 until numbatches){
        println("Begining batch " + batch + " of " + numbatches)
        ///POSITIVE PHASE///
        numcases = minibatchIndex(batch).length //length of Vector at current batch location (frames)
        mb = minibatchIndex(batch).asCol          //contains indices of current batch
        //data is current and delay data (First Batch current frame + previous nt frames)
        data.clear()
        bistar = DenseMatrix.zeros[Double](numdims, numcases)
        bjstar = DenseMatrix.zeros[Double](numhid,numcases)
        println("Setting data...")
        tempMatrix = DenseMatrix.zeros[Double](numcases,numdims)
        //get matrix Lists set up
        //midStartTime = System.currentTimeMillis()
        for (i<- 0 until nt+1 ){
          data += DenseMatrix.zeros[Double](numcases,numdims)   //data(numcases,numdims)
        }
        for (i<- 0 until numcases){
          tempMatrix(i,::) := batchData(mb(i).toInt,::)
        }
        //set first data vals
        data(0) := tempMatrix
        for (hh<- 1 until nt){
          for (i<- 0 until numcases){
            tempMatrix(i,::) := batchData(mb(i).toInt-hh, ::)
          }
          data(hh) := tempMatrix
        }
        //midEndTime = System.currentTimeMillis()

        //Calculate autoregressive connection contributions
        //midStartTime = System.currentTimeMillis()
        bistar *= 0.0 //DenseMatrix.zeros[Double](numdims, numcases)              //bistar(numdims,numcases)
        //Calculate visible to hidden contributions
        bjstar *= 0.0 // DenseMatrix.zeros[Double](numhid, numcases)               //bjstar(numhid,numcases)
        //midEndTime = System.currentTimeMillis()
        //println("Zeroing bistar and bjstar: " + (midEndTime-midStartTime))
        for (hh <- 0 until nt){
          bistar += (A2(hh) * (data(hh+1).t))
          bjstar += (B2(hh) * (data(hh+1).t))
        }
        //midEndTime = System.currentTimeMillis()
        //println("setting bi and bj: " + (midEndTime-midStartTime))

        //Calculate posterior probability (chance of hidden being on)
        tempMatrix = repmat(numcases, bj2, 1)
        //midStartTime = System.currentTimeMillis()
        eta = (w2 *(data(0).t)) + tempMatrix + bjstar                   //eta(numhid,numcases)
        //midEndTime = System.currentTimeMillis()
        //println("Eta: " + (midEndTime-midStartTime))
        //can make better? exp() probably not overloaded for matrices...
        hposteriors = ((exp(-eta)+1):^(-1))//.foreachValue{case f=>1/f}  //Does this really work?
        //println("hposteriors: " + hposteriors.numRows + " , " + hposteriors.numCols)

        //Activate Hidden Units
        hidstates = DenseMatrix.zeros[Double](numcases,numhid)
        tempMatrix = DenseMatrix.rand(numcases,numhid)
        for (i<- 0 until  numcases){
          for (j<- 0  until numhid){
            if((hposteriors.t)(i,j) > tempMatrix(i,j)){
              hidstates(i,j) = (hposteriors.t)(i,j)
            } else{
              tempMatrix(i,j)
            }
          }
        }
        //println("hidstates: " + hidstates.numRows + " , " + hidstates.numCols)

        //Calculate positive Gradients
        //NEED TO STORE THESE UP TOP...KEEP VAL, but don't know size until here
        wgrad := hidstates.t * data(0)                                      //wgrad(numhid,numdims)
        tempMatrix = repmat(numcases, bi, 1)
        bigrad := sum(data(0).t-tempMatrix-bistar, Axis.Vertical).toDense    //bigrad(numdims)
        bjgrad := sum(hidstates, Axis.Horizontal).t                               //bjgrad(numhid)
        //println("wgrad: " + wgrad.numRows + " , " + wgrad.numCols)
        //println("bigrad: " + bigrad.length)
        //println("bjgrad: " + bjgrad.length)

        for (hh<- 0 until nt){
          Agrad(hh) := (((data(0).t-repmat(numcases,bi2,1)-bistar)) *data(hh+1)).toDense//Agrad(0)(numdims,numdims)
          Bgrad(hh) := (hidstates.t * data(hh+1)).toDense                                   //Bgrad(0)(numhid,numdims)
        }
        //END POSISTIVE PHASE
        println("End PositivePhase")

        //Activate the Visibles
        //Find mean of Gaussian
        // //Add in gsd.*randn(numcases, numdims) for real sampling
        tempMatrix = (hidstates*w2) + repmat(numcases, bi2.t,0) + bistar.t  //negdata(numcases,numdims)

        tempMatrix = (1+exp(-tempMatrix)):^(-1) //new from binarycrbm...negdata

        //Conditional on negdata calculate posterior for hidden
        eta = (w2 *tempMatrix.t) + repmat(numcases, bj2, 1) + bjstar                    //eta(numhid,numcases)
        hposteriors = (exp(-eta)+1):^(-1)//.foreachValue{case f=>1/f}                         //hposteriors(numhid,numcases)

        println("Finding Negative Gradient")

        //Calculate negative gradients
        negwgrad := hposteriors*tempMatrix                                    //negwgrad(numhid,numdims)
        negbigrad := sum(tempMatrix.t-repmat(numcases,bi2,1)-bistar,Axis.Vertical).toDense//negbigrad(numdims)
        negbjgrad := sum(hposteriors, Axis.Vertical)  //negbjgrad(numhid)
        //println("negwgrad: " + negwgrad.numRows + " , " + negwgrad.numCols)
        //println("negbigrad: " + negbigrad.length)
        //println("negbjgrad: " + negbjgrad.length)

        for (hh<- 0  until nt){
          negAgrad(hh) := ((tempMatrix.t-repmat(numcases,bi2,1)-bistar)) *data(hh+1)//negAgrad(numdims,numdims)
          negBgrad(hh) := (hposteriors*data(hh+1))                                  //negBgrad(numhid,numdims)
        }
        //println("negAgrad: " + negAgrad(0).numRows + " , " + negAgrad(0).numCols)
        //println("negBgrad: " + negBgrad(0).numRows + " , " + negBgrad(0).numCols)

        //END NEGATIVE PHASE
        errsum += (((data(0)-tempMatrix):^2).sum)

        //check momentum
        if (epoch > 5){
          momentum = mom
        } else{
          momentum=0
        }

        //UPDATE WEIGHTS   (and biases)
        wupdate = (wupdate*momentum) + (((wgrad-negwgrad)/numcases - (w2*wdecay))*epsilonw)//wupdate(numhid,numdims)
        biupdate = (biupdate*momentum) + ((bigrad-negbigrad)*(epsilonbi/numcases)) //biupdate(numdims)
        bjupdate = (bjupdate*momentum) + ((bjgrad-negbjgrad)*(epsilonbj/numcases)) //bjupdate(numhid)
        //println("wupdate: " + wupdate.numRows + " , " + wupdate.numCols)

        for (hh<- 0 until nt){
          Aupdate(hh) := (Aupdate(hh)*momentum) + (((Agrad(hh)-negAgrad(hh))/numcases - (A2(hh)*wdecay))*epsilonA)//Aupdate(numdims,numdims)
          Bupdate(hh) := (Bupdate(hh)*momentum) + (((Bgrad(hh)-negBgrad(hh))/numcases - (B2(hh)*wdecay))*epsilonB)//Bupdate(numhid,numdims)
        }
        w2 :+= wupdate
        bi2 :+= biupdate
        bj2 :+= bjupdate

        println("MADE IT TO UPDATES")

        for (hh<- 0 until nt){
          A2(hh) :+= Aupdate(hh)
          B2(hh) :+= Bupdate(hh)
        }
        //END OF UPDATES
        println("End batch: " + batch)
      }//end batches

      //print every 10 epochs
      if (epoch%10 == 0){
        println("%10 epoch: " + epoch + "  error: " + errsum)
        //plot? nah
      }
    }//end current epoch training

    endTime = System.currentTimeMillis()

    println("Training " + numepochs + " epochs and " + numbatches + " batches took " + (endTime-startTime) + " milliseconds, or " + ((endTime-startTime)/1000/60) + " minutes.")


  }

  def getfilteringdist(){
    var tempMatrix = DenseMatrix.zeros[Double](batchIndex.length, batchData.numCols)

    //Calculate visible to hidden contributions
    bjstar*=0.0
    for (hh <- 0 until n1){
      for (i <- 0 until batchIndex.length){
        tempMatrix(i, ::) :=  batchData(batchIndex(i)-hh,::)
      }
      for (i <- 0 until batchIndex.length){
        bjstar += (B(hh) * tempMatrix)
      }
    }

    //Calculate Posterior probability: chance of hidden state being on
    for (i <- 0 until batchIndex.length){
      tempMatrix(i, ::) :=  batchData(batchIndex(i),::)
    }
    var bottomup = w * (batchData(batchIndex,::)/gsd).t

    var eta  = bottomup + repmat(numcases,bj,1) + bjstar

    filteringdist = 1/(1 + exp(-eta.t))

    //Index Valid cases
    for (j <- 0 until skel.Motions.length){
      if (j==0){
        //get range, skipping first 3 and last 3 (basically)
        batchIndex = DenseVector.range(n2+0,skel.Motions(j).frames.length-n1)
      } else {
        batchIndex = DenseVector((batchIndex.toList :: DenseVector.range(batchIndex(batchIndex.length-1)+1+n2, batchIndex(batchIndex.length-1)+skel.Motions(j).frames.length-n1).toList).toArray)
      }
    }

    //shuffle order (random order)
    var permindex = DenseVector(shuffle(batchIndex.toList).toArray)

    //get minibatches
    minibatchIndex.clear() //= DenseMatrix.zeros[Double](scala.math.floor(permindex.length/batchsize).toInt, batchsize)
    var j = 0
    while(j < batchsize*scala.math.floor(permindex.length/batchsize)){
      minibatchIndex += permindex(j to _j+batchsize-1)
      j += batchsize
    }
    minibatchIndex += permindex(j to permindex.size-1)

  }

  def weightreport(nt : Int){
    plot.hold = true
    subplot(3, nt, 1)
    hist(DenseVector[Double](w.data),50)
    title("w max: " + w.max + " min: " + w.min)

    subplot(3, nt, 2)
    hist(bi,50)
    title("bi max: " + bi.max + " min: " + bi.min)

    subplot(3, nt, 3)
    hist(bj,50)
    title("bj max: " + bj.max + " min: " + bj.min)

    for (i<-0 until nt){
      subplot(3, nt, nt+i)
      hist(DenseVector[Double](A(i).data),50)
      title("A(" + i + ") max: " + A(i).max + " min: " + A(i).min)
    }

    for (i<-0 until nt){
      subplot(3, nt, nt+i)
      hist(DenseVector[Double](B(i).data),50)
      title("B(" + i + ") max: " + B(i).max + " min: " + B(i).min)
    }
    
    saveas("n1Plot.png")

  }
  
  def gaussiancrbm(nt : Int, numhid : Int) {
    var numbatches = minibatchIndex.length
    println("NUM BATCHES: " + numbatches)
    //need numdims, have in main numdims = batchData.numCols (==numBoneVals)
    var wgrad = DenseMatrix.zeros[Double](numhid,numdims) //make a default for now
    var negwgrad = DenseMatrix.zeros[Double](numhid,numdims)
    var negbigrad = DenseVector.zeros[Double](numdims)
    var negbjgrad = DenseVector.zeros[Double](numhid)
    w1 = (DenseMatrix.randn(numhid, numdims)*=0.01)                  //w(numhid,numdims)
    bi1 = (DenseVector.randn(numdims)*=0.01)
    bj1 = ((DenseVector.randn(numhid)*=0.01)+=(-1))  //favor OFF units
    var bigrad = DenseVector.zeros[Double](numdims)
    var bjgrad = DenseVector.zeros[Double](numhid)
    //autoregressive weights: A(:,:,j) weight from t-j to visible
    var Aupdate = MutableList[DenseMatrix[Double]]()
    var Agrad = MutableList[DenseMatrix[Double]]()
    var negAgrad = MutableList[DenseMatrix[Double]]()
    //weights from previous timesteps to hiddens: B(:,:,j) is weight from t-j to hidden layer
    var Bupdate = MutableList[DenseMatrix[Double]]()
    var Bgrad = MutableList[DenseMatrix[Double]]()
    var negBgrad = MutableList[DenseMatrix[Double]]()
    var data = MutableList[DenseMatrix[Double]]()
    ///
    var wupdate = DenseMatrix.zeros[Double](numhid, numdims)//.zero //
    var biupdate = DenseVector.zeros[Double](numdims)//.zero //
    var bjupdate = DenseVector.zeros[Double](numhid)//.zero //
    var tempMatrix = DenseMatrix.zeros[Double](3,3)
    var bistar = DenseMatrix.zeros[Double](3,3)
    var eta = DenseMatrix.zeros[Double](3,3)
    var hposteriors = DenseMatrix.zeros[Double](3,3)
    var hidstates = DenseMatrix.zeros[Double](3,3)

    var epoch = 1
    var momentum = 0.0
    var errsum = 0.0
    var mb = DenseVector.zeros[Int](3)

    println("Making List storage...")
    //get matrix Lists set up
    midStartTime = System.currentTimeMillis()
    for (i<- 0 until nt){
      A1 += (DenseMatrix.randn(numdims,numdims)*=0.01)
      Aupdate += DenseMatrix.zeros[Double](numdims,numdims)
      Agrad += DenseMatrix.zeros[Double](numdims,numdims)
      negAgrad += DenseMatrix.zeros[Double](numdims,numdims)
      B1 += (DenseMatrix.randn(numhid,numdims)*=0.01)
      Bupdate += DenseMatrix.zeros[Double](numhid,numdims)
      Bgrad += DenseMatrix.zeros[Double](numhid,numdims)
      negBgrad += DenseMatrix.zeros[Double](numhid,numdims)
    }
    midEndTime = System.currentTimeMillis()
    println("Initializing A and B took " + (midEndTime-midStartTime))

    println("Done initializing")
    
    if (restart == 1){
      midStartTime = System.currentTimeMillis()
      restart = 0
      epoch = 1

      //Initialize Weights
      //empty all:
      wgrad*=0.0
      bigrad*=0.0
      bjgrad*=0.0
      negwgrad*=0.0
      negbigrad*=0.0
      negbjgrad*=0.0
      //Keep previous updates for momentum?
      wupdate*=0.0 //
      biupdate*=0.0 //
      bjupdate*=0.0 //
      for (i<- 0 until nt){
        A1(i) := DenseMatrix.randn(numdims,numdims)*=0.01
        Aupdate(i)*=0.0
        Agrad(i)*=0.0
        negAgrad(i)*=0.0
        B1(i) := DenseMatrix.randn(numhid,numdims)*=0.01
        Bupdate(i)*=0.0
        Bgrad(i)*=0.0
        negBgrad(i)*=0.0
      }
    midEndTime = System.currentTimeMillis()
      println("Restart took: " + (midEndTime-midStartTime))
    }//be sure updates leave here with 0 values

    //MAIN//
    //Loop across Epochs
    startTime = System.currentTimeMillis()
    for (epoch<-epoch until numepochs){
      println("Begining new epoch " + epoch + " of " + numepochs)
      errsum = 0.0
      //Loop Across Batches
      for (batch <- 0 until numbatches){
        println("Begining batch " + batch + " of " + numbatches)
        ///POSITIVE PHASE///
        numcases = minibatchIndex(batch).length //length of Vector at current batch location (frames)
        mb = minibatchIndex(batch).asCol          //contains indices of current batch
        //data is current and delay data (First Batch current frame + previous nt frames)
        data.clear()
        bistar = DenseMatrix.zeros[Double](numdims, numcases)
        bjstar = DenseMatrix.zeros[Double](numhid,numcases)
        println("Setting data...")
        tempMatrix = DenseMatrix.zeros[Double](numcases,numdims)
        //get matrix Lists set up
        //midStartTime = System.currentTimeMillis()
        for (i<- 0 until nt+1 ){
          data += DenseMatrix.zeros[Double](numcases,numdims)   //data(numcases,numdims)
        }
        for (i<- 0 until numcases){
           tempMatrix(i,::) := batchData(mb(i).toInt,::)
        }
        //set first data vals
        data(0) := tempMatrix
        for (hh<- 1 until nt){
          for (i<- 0 until numcases){
            tempMatrix(i,::) := batchData(mb(i).toInt-hh, ::)
          }
          data(hh) := tempMatrix
        }
        //midEndTime = System.currentTimeMillis()

        //Calculate autoregressive connection contributions
        //midStartTime = System.currentTimeMillis()
        bistar *= 0.0 //DenseMatrix.zeros[Double](numdims, numcases)              //bistar(numdims,numcases)
        //Calculate visible to hidden contributions
        bjstar *= 0.0 // DenseMatrix.zeros[Double](numhid, numcases)               //bjstar(numhid,numcases)
        //midEndTime = System.currentTimeMillis()
        //println("Zeroing bistar and bjstar: " + (midEndTime-midStartTime))
        for (hh <- 0 until nt){
          bistar += (A1(hh) * (data(hh+1).t))
          bjstar += (B1(hh) * (data(hh+1).t))
        }
        //midEndTime = System.currentTimeMillis()
        //println("setting bi and bj: " + (midEndTime-midStartTime))

        //Calculate posterior probability (chance of hidden being on)
        tempMatrix = repmat(numcases, bj1, 1)
        //midStartTime = System.currentTimeMillis()
        eta = (w1 * ((data(0) / gsd).t)) + tempMatrix + bjstar                   //eta(numhid,numcases)
        //midEndTime = System.currentTimeMillis()
        //println("Eta: " + (midEndTime-midStartTime))
        //can make better? exp() probably not overloaded for matrices...
        hposteriors = ((exp(-eta)+1):^(-1))//.foreachValue{case f=>1/f}  //Does this really work?
        //println("hposteriors: " + hposteriors.numRows + " , " + hposteriors.numCols)

        //Activate Hidden Units
        hidstates = DenseMatrix.zeros[Double](numcases,numhid)
        tempMatrix = DenseMatrix.rand(numcases,numhid)
        for (i<- 0 until  numcases){
          for (j<- 0  until numhid){
            if((hposteriors.t)(i,j) > tempMatrix(i,j)){
              hidstates(i,j) = (hposteriors.t)(i,j)
            } else{
              tempMatrix(i,j)
            }
          }
        }
        //println("hidstates: " + hidstates.numRows + " , " + hidstates.numCols)

        //Calculate positive Gradients
        //NEED TO STORE THESE UP TOP...KEEP VAL, but don't know size until here
        wgrad := hidstates.t * (data(0)/gsd)                                      //wgrad(numhid,numdims)
        tempMatrix = repmat(numcases, bi1, 1)
        bigrad := sum(data(0).t-tempMatrix-bistar, Axis.Vertical).toDense/pow(gsd,2)//bigrad(numdims)
        bjgrad := sum(hidstates, Axis.Horizontal).t                               //bjgrad(numhid)
        //println("wgrad: " + wgrad.numRows + " , " + wgrad.numCols)
        //println("bigrad: " + bigrad.length)
        //println("bjgrad: " + bjgrad.length)
        
        for (hh<- 0 until nt){
          Agrad(hh) := (((data(0).t-repmat(numcases,bi1,1)-bistar)/pow(gsd,2)) *data(hh+1)).toDense//Agrad(0)(numdims,numdims)
          Bgrad(hh) := (hidstates.t * data(hh+1)).toDense                                   //Bgrad(0)(numhid,numdims)
        }
        //END POSISTIVE PHASE
        println("End PositivePhase")

        //Activate the Visibles
        //Find mean of Gaussian
        // //Add in gsd.*randn(numcases, numdims) for real sampling
        tempMatrix = ((hidstates*w1)*gsd) + repmat(numcases, bi1.t,0) + bistar.t  //negdata(numcases,numdims)
        
        //Conditional on negdata calculate posterior for hidden
        eta = (w1 *(tempMatrix/gsd).t) + repmat(numcases, bj1, 1) + bjstar                    //eta(numhid,numcases)
        hposteriors = (exp(-eta)+1):^(-1)//.foreachValue{case f=>1/f}                         //hposteriors(numhid,numcases)

        println("Finding Negative Gradient")

        //Calculate negative gradients
        negwgrad := hposteriors*(tempMatrix/gsd)                                    //negwgrad(numhid,numdims)
        negbigrad := sum(tempMatrix.t-repmat(numcases,bi1,1)-bistar,Axis.Vertical).toDense/pow(gsd,2)//negbigrad(numdims)
        negbjgrad := sum(hposteriors, Axis.Vertical)  //negbjgrad(numhid)
        //println("negwgrad: " + negwgrad.numRows + " , " + negwgrad.numCols)
        //println("negbigrad: " + negbigrad.length)
        //println("negbjgrad: " + negbjgrad.length)
        
        for (hh<- 0  until nt){
          negAgrad(hh) := ((tempMatrix.t-repmat(numcases,bi1,1)-bistar)/pow(gsd,2)) *data(hh+1)//negAgrad(numdims,numdims)
          negBgrad(hh) := (hposteriors*data(hh+1))                                  //negBgrad(numhid,numdims)
        }
        //println("negAgrad: " + negAgrad(0).numRows + " , " + negAgrad(0).numCols)
        //println("negBgrad: " + negBgrad(0).numRows + " , " + negBgrad(0).numCols)
        
        //END NEGATIVE PHASE
        errsum += (((data(0)-tempMatrix):^2).sum)
        
        //check momentum
        if (epoch > 5){
          momentum = mom
        } else{
          momentum=0
        }
        
        //UPDATE WEIGHTS   (and biases)
        wupdate = (wupdate*momentum) + (((wgrad-negwgrad)/numcases - (w1*wdecay))*epsilonw)//wupdate(numhid,numdims)
        biupdate = (biupdate*momentum) + ((bigrad-negbigrad)*(epsilonbi/numcases)) //biupdate(numdims)
        bjupdate = (bjupdate*momentum) + ((bjgrad-negbjgrad)*(epsilonbj/numcases)) //bjupdate(numhid)
        //println("wupdate: " + wupdate.numRows + " , " + wupdate.numCols)
        
        for (hh<- 0 until nt){
          Aupdate(hh) := (Aupdate(hh)*momentum) + (((Agrad(hh)-negAgrad(hh))/numcases - (A1(hh)*wdecay))*epsilonA)//Aupdate(numdims,numdims)
          Bupdate(hh) := (Bupdate(hh)*momentum) + (((Bgrad(hh)-negBgrad(hh))/numcases - (B1(hh)*wdecay))*epsilonB)//Bupdate(numhid,numdims)
        }
        w1 :+= wupdate
        bi1 :+= biupdate
        bj1 :+= bjupdate

        println("MADE IT TO UPDATES")
        
        for (hh<- 0 until nt){
          A1(hh) :+= Aupdate(hh)
          B1(hh) :+= Bupdate(hh)
        }
        //END OF UPDATES
        println("End batch: " + batch)
      }//end batches
      
      //print every 10 epochs
      if (epoch%10 == 0){
        println("%10 epoch: " + epoch + "  error: " + errsum)
        //plot? nah
      }
    }//end current epoch training

    endTime = System.currentTimeMillis()

    println("Training " + numepochs + " epochs and " + numbatches + " batches took " + (endTime-startTime) + " milliseconds, or " + ((endTime-startTime)/1000/60) + " minutes.")

  }
  
  //Creates matrix out of repeating vector, dim is row(0) vs col(other) repeat 
  def repmat(matSize : Int, vector : DenseVector[Double], dim : Int) : DenseMatrix[Double] = {
    var matrix = DenseMatrix.zeros[Double](3,3)
    if (dim==0){
      matrix = DenseMatrix.zeros[Double](matSize, vector.length)
      for (i<-0 until matSize){
        matrix(i, ::) := vector //assume bj is column vector
      }
    } else {
      matrix = DenseMatrix.zeros[Double](vector.length, matSize)
      for (i<-0 until matSize){
        matrix(::, i) := vector
      }
    }
    matrix//return matrix
  }

  //
  def preprocessing2() : DenseMatrix[Double] = {
    var dim = skel.Motions(0).frames(0).actionVector.length
    var totalFrames = 0
    //Extract dimensions into batches (combine all different frames into one giant list
    //batchdata is frames x bones (all frames across all motions)
    for (i <- 0 until skel.Motions.length){
      for (j <- 0 until skel.Motions(i).frames.length){
        totalFrames += 1
        if (dim != skel.Motions(i).frames(j).actionVector.length){
          println("Dimensions not the same across vectors..." + skel.Motions(i).frames(j).actionVector.length + " vs. " + dim)
          sys.exit()
        } else{

        }
      }
    }//batchList should now have all bones, all frames, in long list
    var batchData = DenseMatrix.zeros[Double](totalFrames, dim)
    for (i <- 0 until skel.Motions.length){
      for (j <- 0 until skel.Motions(i).frames.length){
        for (k <- 0 until skel.Motions(i).frames(j).actionVector.length){
          batchData(i+j, k) = skel.Motions(i).frames(j).actionVector(k)
        }
      }
    }
    //numcases is num frames (batchData.rows)

    for (i<- 0 until dim){
      //dataMean is vector of means of each bone across all frames (mean for each column)
      //println("i: " + i + " rows,cols: " + batchData.numRows + " , " + batchData.numCols + "mean? " + dataMean.length + " dim " + dim)
      dataMean(i) = avg_short(batchData(::, i))
      //dataStd is standard deviation of bone values (std dev fro each col)
      dataStd(i) = stdDev(batchData(::, i))
    }
    //scale data to be 0 mean and unit variance
    var tempMatM = DenseMatrix.zeros[Double](totalFrames, dataMean.length)
    var tempMatS = DenseMatrix.zeros[Double](totalFrames, dataStd.length)
    for (i <- 0 until totalFrames){
      tempMatM(i,::) := dataMean
      tempMatS(i,::) := dataStd
    }
    batchData = (batchData - tempMatM)
    batchData :/= tempMatS

    //batchData //return
  }

  def stdDev(numbers : DenseVector[Double]) : Double = {
    val sum : Double =
      if(numbers.length>=2){
        val mean = avg_short(numbers)
        val factor:Double = 1.0/(numbers.length.toDouble-1);
        factor * numbers.toList.foldLeft(0.0)((acc,x) =>acc+math.pow(x-mean,2))//shouldn't need toList
      } else {
        0.0
      }
    math.sqrt(sum);
  }

  def avg_short(numbers : DenseVector[Double]) : Double = numbers.toList.reduceLeft(_ + _) / numbers.length.toDouble

  def squaredDifference(value1: Double, value2: Double) : Double = {math.pow(value1-value2, 2.0)}

  def stdDev(list: DenseVector[Double], average: Double) = {
    list.toList.isEmpty match {
      case false =>
        val squared = list.toList.foldLeft(0.0)(_ + squaredDifference(_, avg_short(list)))
        math.sqrt(squared / list.length.toDouble)
      case true => 0.0
    }
  }

  def preprocessing3(){
    //collect indices for minibatches (split data up)
    var seqlength = 0//THIS MAY BE FIXABLE: DENSEVECTOR.RANGE
    for(i<- 0 until skel.Motions.length){
      for (j <- n1 until skel.Motions(i).frames.length){ //skip n1 frames
        batchIndex += (n1 + seqlength)
      }
      seqlength += skel.Motions(i).frames.length //add up total number frames so far
    }
    var permindex = DenseVector(shuffle(batchIndex.toList).toArray)
    var numfullbatches= scala.math.floor(permindex.length/batchsize).toInt
    var j = 0
    //grab numfullbatches batches and split into batches (vectors) in minibatchindex
    while (j < batchsize*(numfullbatches-numvalidatebatches)){
      minibatchIndex += permindex(j to j+batchsize-1)
      j += batchsize
    }
    //start where ket off: grab validation batches?
    j = batchsize*(numfullbatches-numvalidatebatches)+1
    while (j < batchsize*numfullbatches){
      minibatchIndex += permindex(j to j+batchsize-1)
      j += batchsize
    }
    //Add leftover Frames to end of minibatchIndex (do we need this?)
    minibatchIndex += permindex(j to permindex.size-1)
  }

}
