package main

/**
 * Created by IntelliJ IDEA.
 * User: lieslw
 * Date: 4/28/12
 * Time: 10:49 AM
 * To change this template use File | Settings | File Templates.
 *
 * This should be the main class for running, but SBT can't find it...
 */

class GeneratorMain {
  def main(args: Array[String]) {
    val skel = new Skeleton

    skel.loadAcclaimFileASF(args(0))
    skel.printSkeletonDebug()
  }

}
