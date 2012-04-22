import util.Random

/**
 * Created by IntelliJ IDEA.
 * User: lieslw
 * Date: 4/22/12
 * Time: 9:41 AM
 * To change this template use File | Settings | File Templates.
 *
 * Had this class already, didn't make it from Windows to github.
 */

class MotionGenerator {
  //set initial variables
  //nX = Model order: time looked forward and back in each layer
  val n1 = 3
  val n2 = 3
  //Randoms: need uniform and gaussian random weights for matrices in crbms;
  //  may need 2 separate? can't see where they needed uniform dist.
  //  rand.nextDouble, rand.nextGaussian
  val rand = new Random(System.currentTimeMillis())

  //load data (file/list of files)

  //Downsample/ preprocess (Put these together? why didn't they?)

  //
}
