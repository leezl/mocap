package main.scala

import util.Random
import scalala.tensor.dense._
import scalala.tensor.dense.CommonDenseVectorConstructors._
import collection.generic.GenericCompanion._
import scala.MathCommon._
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
  var batchIndex = MutableList[Double]()
  //contains List of DenseVectors (batches of indexes into batchData)
  var minibatchIndex = MutableList[DenseVector]()
  //var batchData = DenseMatrix.zeros[Double](3,3) //created in init, passed to all
  //Default network properties:
  var numhid1 = 150
  var numhid2 = 150
  var numepochs = 2000
  var gsd = 1   //gaussian std dev
  //var nt = n1, n2, etc //crbm order how far forward and back (3 default)
  //var numhid = numhid1, numhid2, etc
  var restart = 1
  //Randoms: need uniform and gaussian random weights for matrices in crbms;
  //  may need 2 separate? can't see where they needed uniform dist.
  //  rand.nextDouble, rand.nextGaussian
  val rand = new Random(System.currentTimeMillis())

  //load data (file/list of files)
  //soooo...only use data from one skeleton? limits data and system.
  // Add skeleton to data going into network?: larger rewrite. Change later.
  //default asf for now? Load one .asf skel, then load motions?(.acm)
  val skel = new Skeleton

  def init(skeletonFile : String,  motionFiles : List[String]){
    skel.loadSkeleton(skeletonFile)//check these
    motionFiles.foreach(file => skel.loadMotion)//check these
    //Downsample/ preprocess (Put these together? why didn't they?)
    skel.downsampling(4)
    //get exponential map representation for all
    skel.preprocessing1()
    //split up into batches: batchData is Dense Matrix of totalFrames x BoneVals
    var batchData = preprocessing2()
    //split into minibatches (collect indices into batchData for this)
    preprocessing3()
    //Bone dimensions
    numdims = batchData.numCols
    //GET some intializer frames
    if (batchData.numRows>100){
      var initialScene = batchData(1 to  100, ::) //get first hundred (check longer then 100)
    }
    //try running
    println("Training Layer 1 CRBM, order " + n1 + " :" + numdims + " - " + numhid1)
    gaussiancrbm(batchData, n1, numhid1)
  }
  
  def gaussiancrbm(batchData : DenseMatrix[Double], nt : Int, humhid : Int) {
    var numbatches = minibatchIndex.length
    //need numdims, have in main numdims = batchData.numCols (==numBoneVals)
    //Learning Rates:
    var epsilonw = 1*10^-3   //undirected
    var epsilonbi = 1*10^-3  //visibles
    var epsilonbj = 1*10^-3  //hidden units
    var epsilonA = 1*10^-3   //autoregressive
    var epsilonB = 1*10^-3   //prev visibles to hidden
    var wgrad = DenseMatrix.zeros[Double](numhid,numdims) //make a default for now
    var negwgrad = DenseMatrix.zeros[Double](numhid,numdims)
    var negbigrad = DenseVector.zeros[Double](numdims)
    var negbjgrad = DenseVector.zeros[Double](numhid)
    var w : DenseMatrix[Double] = 0.01*randn(numhid, numdims)                  //w(numhid,numdims)
    var bi = 0.01*DenseVector.randn(numdims)
    var bj = -1+0.01*DenseVector.randn(numhid)  //favor OFF units
    var bigrad = DenseVector.zeros[Double](numdims)
    var bjgrad = DenseVector.zeros[Double](numhid)
    //autoregressive weights: A(:,:,j) weight from t-j to visible
    var A = MutableList[DenseMatrix[Double]]()
    var Aupdate = MutableList[DenseMatrix[Double]]()
    var Agrad = MutableList[DenseMatrix[Double]]()
    var negAgrad = MutableList[DenseMatrix[Double]]()
    //weights from previous timesteps to hiddens: B(:,:,j) is weight from t-j to hidden layer
    var B = MutableList[DenseMatrix[Double]]()
    var Bupdate = MutableList[DenseMatrix[Double]]()
    var Bgrad = MutableList[DenseMatrix[Double]]()
    var negBgrad = MutableList[DenseMatrix[Double]]()
    ///
    var wupdate = DenseMatrix.zeros[Double](numhid, numdims)//.zero //
    var biupdate = DenseVector.zeros[Double](numdims)//.zero //
    var bjupdate = DenseVector.zeros[Double](numhid)//.zero //

    var wdecay = 0.0002  //same decay for all..
    var mom = 0.9        //momentum

    //get matrix Lists set up
    for (i<- 0 until nt){
      A += 0.01*DenseMatrix.randn(numdims,numdims)
      Aupdate += DenseMatrix.zeros[Double](numdims,numdims)
      Agrad += DenseMatrix.zeros[Double](numdims,numdims)
      negAgrad += DenseMatrix.zeros[Double](numdims,numdims)
      B += 0.01*DenseMatrix.randn(numhid,numdims)
      Bupdate += DenseMatrix.zeros[Double](numhid,numdims)
      Bgrad += DenseMatrix.zeros[Double](numhid,numdims)
      negBgrad += DenseMatrix.zeros[Double](numhid,numdims)
    }
    
    if (restart == 1){
      restart = 0
      epoch = 1

      //Initialize Weights
      //empty all:
      wgrad.zero
      bigrad.zero
      bjgrad.zero
      negwgrad.zero
      negbigrad.zero
      negbjgrad.zero
      //Keep previous updates for momentum?
      wupdate.zero //
      biupdate.zero //
      bjupdate.zero //
      for (i<- 0 until nt){
        A(i)= 0.01*DenseMatrix.randn(numdims,numdims)
        Aupdate(i).zero
        Agrad(i).zero
        negAgrad(i).zero
        B(i) = 0.01*DenseMatrix.randn(numhid,numdims)
        Bupdate(i).zero
        Bgrad(i).zero
        negBgrad(i).zero
      }
      
    }//be sure updates leave here with 0 values
    
    //MAIN//
    //Loop across Epochs
    for (epoch<-epoch until numepochs){
      var errsum=0
      //Loop Across Batches
      for (batch <- 1 until numbatches){
        ///POSITIVE PHASE///
        numcases = minibatchIndex(batch).length //length of Vector at current batch location (frames)
        var mb = miniBatchIndex(batch)          //contains indices of current batch
        //data is current and delay data (First Batch current frame + previous nt frames)
        var data = MutableList[DenseMatrix[Double]]()
        var tempMatrix = DenseMatrix.zeros[Double](numcases,numdims)
        //get matrix Lists set up
        for (i<- 0 until nt){
          data += DenseMatrix.zeros[Double](numcases,numdims)   //data(numcases,numdims)
        }
        for (i<- 0 until numcases){
           tempMatrix(i, ::) = batchData(minibatchIndex(i), ::)
        }
        //set first data vals
        data(0) = tempMatrix
        for (hh<- 1 until nt){
          for (i<- 0 until numcases){
            tempMatrix(i, ::) = batchData(minibatchIndex(i-hh), ::)
          }
          data(hh) = tempMatrix
        }

        //Calculate autoregressive connection contributions
        var bistar = DenseMatrix.zeros[Double](numdims, numcases)              //bistar(numdims,numcases)
        //Calculate visible to hidden contributions
        var bjstar = DenseMatrix.zeros[Double](numhid, numcases)               //bjstar(numhid,numcases)
        for (hh<- 0 until nt){
          bistar = bistar + A(hh) * data(hh+1).t
          bjstar = bjstar + B(hh) * data(hh+1).t
        }

        //Calculate posterior probability (chance of hidden being on)
        tempMatrix = repmat(numcases, bj, 1)
        var eta = w*((data(0) / gsd).t) + tempMatrix + bjstar                   //eta(numhid,numcases)
        /*var hposteriors = DenseMatrix.zeros[Double](numhid,numcases)
        for (i<-0 until numhid){
          for (j<-0 until numcases){
            hposteriors(i,j) = 1/(1+exp(-eta(i,j)))
          }
        }*/
        //can make better? exp() probably not overloaded for matrices...
        var hposteriors = 1/(1+exp(-eta))  //Does this really work?

        //Activate Hidden Units
        var hidstates = (hposteriors.t > DenseMatrix.rand(numcases,numhid))      //hidstates(numcases,numhid)

        //Calculate positive Gradients
        //NEED TO STORE THESE UP TOP...KEEP VAL, but don't know size until here
        wgrad = hidstates.t * (data(0)/gsd)                                      //wgrad(numhid,numdims)
        tempMatrix = repmat(numcases, bi, 1)
        bigrad = sum(data(0).t - tempMatrix - bistar, Axis.Vertical) / pow(gsd,2)//bigrad(numdims)
        bjgrad = sum(hidstates, Axis.Horizontal).t                               //bjgrad(numhid)
        
        for (hh<- 0 until nt){
          Agrad(hh) = (data(0).t-repmat(numcases,bi,1)-bistar) /pow(gsd,2) *data(hh+1)//Agrad(0)(numdims,numdims)
          Bgrad(hh) = hidstates.t * data(hh+1)                                   //Bgrad(0)(numhid,numdims)
        }
        //END POSISTIVE PHASE

        //Activate the Visibles
        //Find mean of Gaussian                                                 //topdown(numcases,numdims)
        var topdown = gsd * (hidstates*w)
        // //Add in gsd.*randn(numcases, numdims) for real sampling
        negdata = topdown + repmat(numcases, bi.t,0) + bistar.t                 //negdata(numcases,numdims)
        
        //Conditional on negdata calculate posterior for hidden
        eta = w  * (negdata/gsd).t + repmat(numcases, bj, 1)                    //eta(numhid,numcases)
        hposteriors = 1/(1 + exp(-eta))                                         //hposteriors(numhid,numcases)
        
        //Calculate negative gradients
        negwgrad = hposteriors*(negdata/gsd)                                    //negwgrad(numhid,numdims)
        negbigrad = sum(negdata.t-repmat(numcases,bi,1)-bistar,Axis.Vertical)/pow(gsd,2)//negbigrad(numdims)
        negbjgrad = sum(hposteriors, Axis.Vertical)                             //negbjgrad(numhid)
        
        for (hh<- 0  until nt){
          negAgrad += (negdata.t-repmat(numcases,bi,1)-bistar)/pow(gsd,2) *data(hh+1)//negAgrad(numdims,numdims)
          negBgrad += (hposteriors*data(hh+1))                                  //negBgrad(numhid,numdims)
        }
        
        //END NEGATIVE PHASE
        
        err = (pow(data(0)-negdata, 2)).sum //single sum of all matrix elements
        errsum += err
        
        //check momentum
        if (epoch > 5){
          momentum =mom
        } else{
          momentum=0
        }
        
        //UPDATE WEIGHTS   (and biases)
        wupdate = momentum*wupdate + epsilonw*((wgrad-negwgrad)/numcases - wdecay*w)//wupdate(numhid,numdims)
        biupdate = momentum*biupdate + (epsilonbi/numcases)*(bigrad-negbigrad) //biupdate(numdims)
        bjupdate = momentum*bjupdate + (epsilonbj/numcases)*(bjgrad-negbjgrad) //bjupdate(numhid)
        
        for (hh<- 0 until nt){
          Aupdate(hh) = momentum*Aupdate(hh) + epsilonA*((Agrad(hh)-negAgrad(hh))/numcases - wdecay*A(hh))//Aupdate(numdims,numdims)
          Bupdate(hh) = momentum*Bupdate(hh) + epsilonB*((Bgrad(hh)-negBgrad(hh))/numcases - wdecay*B(hh))//Bupdate(numhid,numdims)
        }
        w = w + wupdate
        bi = bi + biupdate
        bj = bj + bjupdate
        
        for (hh<- 0 until nt){
          A(hh) = A(hh) + Aupdate(hh)
          B(hh) = B(hh) + Bupdate(hh)
        }
        //END OF UPDATES
        
      }//end batches
      
      //print every 10 epochs
      if (epoch%10 == 0){
        println("epoch: " + epoch + "  error: " + errsum)
        //plot? nah
      }
    }//end current epoch training
    
  }
  
  //Creates matrix out of repeating vector, dim is row(0) vs col(other) repeat 
  def repmat(matSize : Int, vector : DenseVector[Double], dim : Boolean) : DenseMatrix[Double] = {
    if (dim==0){
      var matrix = DenseMatrix.zeros[Double](matSize, vector.length)
      for (i<-0 until matSize){
        matrix(i, ::) = vector.t //assume bj is column vector
      }
    } else {
      var matrix = DenseMatrix.zeros[Double](vector.length, matSize)
      for (i<-0 until matSize){
        matrix(::, i) = vector
      }
    }
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
    var dataMean = DenseVector[Double](dim)
    var dataStd = DenseVector[Double](dim)
    for (i<- 0 until dim){
      //dataMean is vector of means of each bone across all frames (mean for each column)
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

  def preprocessing3(){
    //collect indices for minibatches (split data up)
    var seqlength = 0
    for(i<- 0 until skel.Motions.length){
      for (j <- n1 until skel.Motions(i).frames.length){ //skip n1 frames
        batchIndex += (n1 + seqlength)
      }
      seqlength += skel.Motions(i).frames.length //add up total number frames so far
    }
    var permindex = shuffle(batchIndex)
    var numfullbatches = floor(permindex.length/batchsize)
    var j = 0
    while (j < batchsize*(numfullbatches-numvalidatebatches)){
      minibatchIndex += permindex(j to j+batchsize-1)
      j += batchsize
    }
    j = batchsize*(numfullbatches-numvalidatebatches)+1
    while (j < batchsize*numfullbatches){
      minibatchIndex += permindex(j to j+batchsize-1)
      j += batchsize
    }
    //Add leftover Frames to end of minibatchIndex (do we need this?)
    miniBatchIndex += permindex(j to permindex.size-1)
  }

}
