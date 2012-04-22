package skeletonProcessing

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
  //set storage form: tree, or whatever

  //def load function: check file type: bvh, asf & acm, or txt?
  def loadSkeleton(filename : String) {

  }

}
