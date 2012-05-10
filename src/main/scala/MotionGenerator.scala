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
  var numepochs = 2000
  var gsd = 1   //gaussian std dev
  var w = DenseMatrix.zeros[Double](3,3) //is this legal
  var bi = DenseVector.zeros[Double](3)
  var bj = DenseVector.zeros[Double](3)
  var A = MutableList[DenseMatrix[Double]]()
  var B = MutableList[DenseMatrix[Double]]()
  //second Layer
  var w2 = DenseMatrix.zeros[Double](3,3) //is this legal
  var bi2 = DenseVector.zeros[Double](3)
  var bj2 = DenseVector.zeros[Double](3)
  var A2 = MutableList[DenseMatrix[Double]]()
  var B2 = MutableList[DenseMatrix[Double]]()
  //var nt = n1, n2, etc //crbm order how far forward and back (3 default)
  //var numhid = numhid1, numhid2, etc
  var restart = 1
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

    //plot weights figure YAY SCALALA HAS PLOTTING
    weightreport(n1)

    //binarycrbm(batchData, n2, numhid2)

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
  
  def gaussiancrbm(batchData : DenseMatrix[Double], nt : Int, numhid : Int) {
    var numbatches = minibatchIndex.length
    println("NUM BATCHES: " + numbatches)
    //need numdims, have in main numdims = batchData.numCols (==numBoneVals)
    //Learning Rates:
    var epsilonw = 1*pow(10,-3)   //undirected
    var epsilonbi = 1*pow(10,-3)  //visibles
    var epsilonbj = 1*pow(10,-3)  //hidden units
    var epsilonA = 1*pow(10,-3)   //autoregressive
    var epsilonB = 1*pow(10,-3)   //prev visibles to hidden
    var wgrad = DenseMatrix.zeros[Double](numhid,numdims) //make a default for now
    var negwgrad = DenseMatrix.zeros[Double](numhid,numdims)
    var negbigrad = DenseVector.zeros[Double](numdims)
    var negbjgrad = DenseVector.zeros[Double](numhid)
    w = (DenseMatrix.randn(numhid, numdims)*=0.01)                  //w(numhid,numdims)
    bi = (DenseVector.randn(numdims)*=0.01)
    bj = ((DenseVector.randn(numhid)*=0.01)+=(-1))  //favor OFF units
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
    var bjstar = DenseMatrix.zeros[Double](3,3)
    var eta = DenseMatrix.zeros[Double](3,3)
    var hposteriors = DenseMatrix.zeros[Double](3,3)
    var hidstates = DenseMatrix.zeros[Double](3,3)

    var wdecay = 0.0002  //same decay for all..
    var mom = 0.9        //momentum
    var epoch = 1
    var momentum = 0.0
    var errsum = 0.0
    var mb = DenseVector.zeros[Int](3)

    println("Making List storage...")
    //get matrix Lists set up
    for (i<- 0 until nt){
      A += (DenseMatrix.randn(numdims,numdims)*=0.01)
      Aupdate += DenseMatrix.zeros[Double](numdims,numdims)
      Agrad += DenseMatrix.zeros[Double](numdims,numdims)
      negAgrad += DenseMatrix.zeros[Double](numdims,numdims)
      B += (DenseMatrix.randn(numhid,numdims)*=0.01)
      Bupdate += DenseMatrix.zeros[Double](numhid,numdims)
      Bgrad += DenseMatrix.zeros[Double](numhid,numdims)
      negBgrad += DenseMatrix.zeros[Double](numhid,numdims)
    }

    println("Done initializing")
    
    if (restart == 1){
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
        A(i) := DenseMatrix.randn(numdims,numdims)*=0.01
        Aupdate(i)*=0.0
        Agrad(i)*=0.0
        negAgrad(i)*=0.0
        B(i) := DenseMatrix.randn(numhid,numdims)*=0.01
        Bupdate(i)*=0.0
        Bgrad(i)*=0.0
        negBgrad(i)*=0.0
      }
      
    }//be sure updates leave here with 0 values

    //MAIN//
    //Loop across Epochs
    for (epoch<-epoch until numepochs){
      errsum = 0.0
      //Loop Across Batches
      for (batch <- 0 until numbatches){
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
        println("rows and cols: " + data(0).numRows + " , " + data(0).numCols)

        //Calculate autoregressive connection contributions
        bistar *= 0.0 //DenseMatrix.zeros[Double](numdims, numcases)              //bistar(numdims,numcases)
        //Calculate visible to hidden contributions
        bjstar *= 0.0 // DenseMatrix.zeros[Double](numhid, numcases)               //bjstar(numhid,numcases)
        //println("WHATCHATTA: " + A(0).numRows + "," + A(0).numCols + "   " + data(0).t.numRows + "," + data(0).t.numCols)
        //println("heheheh: " + bistar.numRows + " , " + bistar.numCols)
        for (hh<- 0 until nt){
          bistar := bistar += (A(hh) * (data(hh+1).t))
          bjstar := bjstar += (B(hh) * (data(hh+1).t))
        }
        //println("bistar: " + bistar.numRows + " , " + bistar.numCols)
        //println("bjstar: " + bjstar.numRows + " , " + bjstar.numCols)

        //Calculate posterior probability (chance of hidden being on)
        tempMatrix = repmat(numcases, bj, 1)
        eta = w*((data(0) /= gsd).t) += tempMatrix += bjstar                   //eta(numhid,numcases)
        //println("eta: " + eta.numRows + " , " + eta.numCols)
        /*var hposteriors = DenseMatrix.zeros[Double](numhid,numcases)
        for (i<-0 until numhid){
          for (j<-0 until numcases){
            hposteriors(i,j) = 1/(1+exp(-eta(i,j)))
          }
        }*/
        //can make better? exp() probably not overloaded for matrices...
        hposteriors = (exp(-eta)+=1):^=(-1)//.foreachValue{case f=>1/f}  //Does this really work?
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
        wgrad := hidstates.t * (data(0)/=gsd)                                      //wgrad(numhid,numdims)
        tempMatrix = repmat(numcases, bi, 1)
        bigrad := sum(data(0).t-=tempMatrix-=bistar, Axis.Vertical).toDense/=pow(gsd,2)//bigrad(numdims)
        bjgrad := sum(hidstates, Axis.Horizontal).t                               //bjgrad(numhid)
        //println("wgrad: " + wgrad.numRows + " , " + wgrad.numCols)
        //println("bigrad: " + bigrad.length)
        //println("bjgrad: " + bjgrad.length)
        
        for (hh<- 0 until nt){
          Agrad(hh) := (((data(0).t-=repmat(numcases,bi,1)-=bistar)/=pow(gsd,2)) *data(hh+1)).toDense//Agrad(0)(numdims,numdims)
          Bgrad(hh) := (hidstates.t * data(hh+1)).toDense                                   //Bgrad(0)(numhid,numdims)
        }
        //END POSISTIVE PHASE
        println("End PositivePhase")

        //Activate the Visibles
        //Find mean of Gaussian
        // //Add in gsd.*randn(numcases, numdims) for real sampling
        tempMatrix = ((hidstates*w)*=gsd) += repmat(numcases, bi.t,0) += bistar.t  //negdata(numcases,numdims)
        
        //Conditional on negdata calculate posterior for hidden
        eta = w  * (tempMatrix/=gsd).t += repmat(numcases, bj, 1)                    //eta(numhid,numcases)
        hposteriors = (exp(-eta)+=1):^=(-1)//.foreachValue{case f=>1/f}                         //hposteriors(numhid,numcases)

        println("Finding Negative Gradient")

        //Calculate negative gradients
        negwgrad := hposteriors*(tempMatrix/=gsd)                                    //negwgrad(numhid,numdims)
        negbigrad := sum(tempMatrix.t-=repmat(numcases,bi,1)-=bistar,Axis.Vertical).toDense/=pow(gsd,2)//negbigrad(numdims)
        negbjgrad := sum(hposteriors, Axis.Vertical)  //negbjgrad(numhid)
        //println("negwgrad: " + negwgrad.numRows + " , " + negwgrad.numCols)
        //println("negbigrad: " + negbigrad.length)
        //println("negbjgrad: " + negbjgrad.length)
        
        for (hh<- 0  until nt){
          negAgrad(hh) := ((tempMatrix.t-=repmat(numcases,bi,1)-=bistar)/=pow(gsd,2)) *data(hh+1)//negAgrad(numdims,numdims)
          negBgrad(hh) := (hposteriors*data(hh+1))                                  //negBgrad(numhid,numdims)
        }
        //println("negAgrad: " + negAgrad(0).numRows + " , " + negAgrad(0).numCols)
        //println("negBgrad: " + negBgrad(0).numRows + " , " + negBgrad(0).numCols)
        
        //END NEGATIVE PHASE
        errsum += (((data(0)-=tempMatrix):^=2).sum)
        
        //check momentum
        if (epoch > 5){
          momentum = mom
        } else{
          momentum=0
        }
        
        //UPDATE WEIGHTS   (and biases)
        wupdate := (wupdate*=momentum) :+= (((wgrad-=negwgrad)/=numcases :-= w*=wdecay)*=epsilonw)//wupdate(numhid,numdims)
        biupdate := (biupdate*=momentum) :+= ((bigrad-=negbigrad)*=(epsilonbi/numcases)) //biupdate(numdims)
        bjupdate := (bjupdate*=momentum) :+= ((bjgrad-=negbjgrad)*=(epsilonbj/numcases)) //bjupdate(numhid)
        //println("wupdate: " + wupdate.numRows + " , " + wupdate.numCols)
        
        for (hh<- 0 until nt){
          Aupdate(hh) := (Aupdate(hh)*=momentum) += (((Agrad(hh)-=negAgrad(hh))/=numcases :-= (A(hh)*=wdecay))*=epsilonA)//Aupdate(numdims,numdims)
          Bupdate(hh) := (Bupdate(hh)*=momentum) += (((Bgrad(hh)-=negBgrad(hh))/=numcases :-= (B(hh)*=wdecay))*=epsilonB)//Bupdate(numhid,numdims)
        }
        w := w + wupdate
        bi := bi += biupdate
        bj := bj += bjupdate

        println("MADE IT TO UPDATES")
        
        for (hh<- 0 until nt){
          A(hh) := A(hh) += Aupdate(hh)
          B(hh) := B(hh) += Bupdate(hh)
        }
        //END OF UPDATES
        println("End batch: " + batch)
      }//end batches
      
      //print every 10 epochs
      if (epoch%10 == 0){
        println("epoch: " + epoch + "  error: " + errsum)
        //plot? nah
      }
    }//end current epoch training
    
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
    var dataMean = DenseVector.zeros[Double](dim)
    var dataStd = DenseVector.zeros[Double](dim)
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
    var seqlength = 0
    for(i<- 0 until skel.Motions.length){
      for (j <- n1 until skel.Motions(i).frames.length){ //skip n1 frames
        batchIndex += (n1 + seqlength)
      }
      seqlength += skel.Motions(i).frames.length //add up total number frames so far
    }
    var permindex = DenseVector(shuffle(batchIndex.toList).toArray)
    var numfullbatches= scala.math.floor(permindex.length/batchsize).toInt
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
    minibatchIndex += permindex(j to permindex.size-1)
  }

}
