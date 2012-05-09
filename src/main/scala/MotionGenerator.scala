package main.scala

import util.Random
import scalala.tensor.dense._
import scalala.tensor.dense.CommonDenseVectorConstructors._
import collection.generic.GenericCompanion._
import scala.MathCommon._
import scala.collection.mutable._
import scalala.library._

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
  var batchIndex = MutableList[Double]()
  //contains List of DenseVectors (batches of indexes into batchData)
  var minibatchIndex = MutableList[DenseVector]()
  //Default network properties:
  var numhid1 = 150
  var numhid2 = 150
  var numepochs = 2000
  var gsd = 1   //gaussian std dev
  var nt = n1 //crbm order how far forward and back (3 default)
  var numhid = numhid1
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
    var numdims = batchData.numCols
    //GET some intializer frames
    if (batchData.numRows>100){
      var initialScene = batchData(1 to  100, ::) //get first hundred (check longer then 100)
    }
    //try running
    println("Training Layer 1 CRBM, order " + nt + " :" + numdims + " - " + numhid)
    gaussiancrbm(batchData)
  }
  
  def gaussiancrbm(batchData : DenseMatrix[Double]) {
    var numbatches = minibatchIndex.length
    //need numdims, have in main numdims = batchData.numCols (==numBoneVals)
    //Learning Rates:
    var epsilonw = 1*10^-3   //undirected
    var epsilonbi = 1*10^-3  //visibles
    var epsilonbj = 1*10^-3  //hidden units
    var epsilonA = 1*10^-3   //autoregressive
    var epsilonB = 1*10^-3   //prev visibles to hidden

    var wdecay = 0.0002  //same decay for all..
    var mom = 0.9        //momentum
    
    if (restart == 1){
      restart = 0
      epoch = 1

      //Initialize Weights
      var w : DenseMatrix[Double] = 0.01*randn(numhid, numdims)
      var bi = 0.01*DenseMatrix.randn(numdims,1)
      var bj = -1+0.01*DenseMatrix.randn(numhid,1)  //favor OFF units

      //autoregressive weights: A(:,:,j) weight from t-j to visible
      var A = MutableList[DenseMatrix[Double]]()
      for (i<- 0 until nt){
        A += 0.01*DenseMatrix.randn(numdims,numdims)
      }
      //weights from previous timesteps to hiddens: B(:,:,j) is weight from t-j to hidden layer
      var B = MutableList[DenseMatrix[Double]]()
      for (i<- 0 until nt){
        B += 0.01*DenseMatrix.randn(numhid,numdims)
      }
      //empty all: wgrad, bigrad,bjgrad,Agrad,Bgrad,newgrad,negbiggrad,negbjgrad,negAgrad,nedBgrad
      
      //Keep previous updates for momentum
      wupdate = DenseMatrix.zeros[Double](numhid, numdims)//.zero //
      biupdate = DenseVector.zeros[Double](numdims)//.zero //
      bjupdate = DenseVector.zeros[Double](numhid)//.zero //
      /*
      for (i<- 0 until Aupdate.length){
        Aupdate(i).zero
      }
      */
      Aupdate = MutableList[DenseMatrix[Double]]()
      for (i<- 0 until nt){
        Aupdate += DenseMatrix.zeros[Double](numdims,numdims)
      }
      /*
      for (i<- 0 until Bupdate.length){
        Bupdate(i).zero
      }
      */

     Bupdate = MutableList[DenseMatrix[Double]]()
     for (i<- 0 until nt){
       Bupdate += DenseMatrix.zeros[Double](numhid,numdims)
     }
      
    }//be sure updates leave here with values
    
    //MAIN//
    for (epoch<-epoch until numepochs){
      var errsum=0
      for (batch = 1 until numbatches){
        ///POSITIVE PHASE///
        numcases = minibatchIndex(batch).length //length of Vector at current batch location (frames)
        var mb = miniBatchIndex(batch)          //contains indices of current batch
        
        //data is current and delay data (First Batch current frame + previous nt frames)
        var data = MutableList[DenseMatrix[Double]]()
        for (i<- 0 until nt){
          data += DenseMatrix.zeros[Double](numcases,numdims)
        }
        var tempMatrix = DenseMatrix.zeros[Double](numcases,numdims)
        for (i<- 0 until numcases){
           tempMatrix(i, ::) = batchData(minibatchIndex(i), ::)
        }
        data(0) = tempMatrix
        for (hh<- 1 until nt){
          for (i<- 0 until numcases){
            tempMatrix(i, ::) = batchData(minibatchIndex(i-hh), ::)
          }
          data(hh) = tempMatrix
        }

        //Calculate autoregressive connection contributions
        var bistar = DenseMatrix.zeros[Double](numdims, numcases)
        for (hh<- 0 until nt){
            bistar = bistar + A(hh) * data(hh+1).t
        }

        //Calculate visible to hidden contributions
        var bjstar = DenseMatrix.zeros[Double](numhid, numcases)
        for (hh<- 0 until nt){
          bjstar = bjstar + B(hh) * data(hh+1).t
        }

        //Calculate posterior probability (chance of hidden being on) REPMAT() Functions?
        tempMatrix = DenseMatrix.zeros[Double](numhid,numcases)
        for (i<-0 until numcases){
          tempMatrix(::, i) = bj
        }
        var eta = w*((data(0) / gsd).t) + tempMatrix + bjstar
        /*var hposteriors = DenseMatrix.zeros[Double](numhid,numcases)
        for (i<-0 until numhid){
          for (j<-0 until numcases){
            hposteriors(i,j) = 1/(1+exp(-eta(i,j)))
          }
        }*/
        //can make better? exp() probably not overloaded for matrices...
        var hposteriors = 1/(1+exp(-eta))  //Does this really work?

        //Activate Hidden Units
        var hidstates = (hposteriors.t > DenseMatrix.rand(numcases,numhid))//ummm

        //Calculate positive Gradients
        //NEED TO STORE THESE UP TOP...KEEP VAL
        var wgrad = hidstates.t * (data(0)/gsd)

        //END POSISTIVE PHASE
        
      }//end batch
    }//end current epoch training
    
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
