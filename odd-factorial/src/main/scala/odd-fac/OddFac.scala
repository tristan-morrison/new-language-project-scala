package oddFac

object Hello extends App {
  def main(args: Array[String]) {
      println("Hello, world!") // prints Hello World

      // computeOddFac(args(0))

   }

   def computeOddFac(n: Int) : Int = {
     assert(args.length > 0)

     nthOdd = (2 * n) - (if (n == 0) 0 else 1)

     computeOddFacHelper(nthOdd, 1)

   }

   def computeOddFacHelper(n: Int, product: Int) : Int = {

   }
}
