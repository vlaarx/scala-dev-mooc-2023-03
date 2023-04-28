package homework

import scala.annotation.tailrec
import scala.util.Random

class BallsExperiment {

  val originBasket: List[Int] = List(1, 1, 1, 0, 0, 0)
  @tailrec
  final def shuffleBasket(basket: List[Int]): List[Int] = {
    Random.nextInt(10) match {
      case 0 => basket
      case _ =>
        val (elt, lst) = takeBall(basket)
        shuffleBasket(elt :: lst)
    }
  }
  def takeBall(basket: List[Int]): (Int, List[Int]) = {
    val ballIndex = Random.nextInt(basket.length)
    (basket(ballIndex), basket.slice(0, ballIndex) ::: basket.slice(ballIndex + 1, basket.length))
  }
  def isFirstBlackSecondWhite(): Boolean = {
    takeBall(shuffleBasket(originBasket)) match {
      case (0, bkt) => takeBall(bkt) match {
        case (1, _) => true
        case _ => false
      }
      case _ => false
    }
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = (1 to count).map(_ => new BallsExperiment).toList
    val countOfExperiments = listOfExperiments.map(_.isFirstBlackSecondWhite())
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}
