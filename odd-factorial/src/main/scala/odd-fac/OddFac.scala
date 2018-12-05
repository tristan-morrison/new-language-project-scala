package oddFac

object Hello extends App {
  def main(args: Array[String]) {
      println("Hello, world!") // prints Hello World

      // computeOddFac(args(0))

   }

   def computeOddFac(n: Int) : Int = {
     //assert(args.length > 0)

     //nthOdd = (2 * n) - (if (n == 0) 0 else 1)

     //computeOddFacHelper(nthOdd, 1)
     if (n == 1) {
       return 1
     } else if (n % 2 != 0) {
       return computeOddFac(n)*computeOddFac(n-1)
     } else if (n % 2 == 0){
       return 1
     }
   }

   //def computeOddFacHelper(n: Int, product: Int) : Int = {

   //}

   def computeTrib(n: Int) {
     if (n == 1) {
       return 0
     } else if (n == 2){
       return 1
     } else if (n == )
   }
}
