import scala.annotation.tailrec

def abs(x:Double) = if (x < 0) -x else x

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)

def isGoodEnough(guess: Double, x: Double) = abs(guess * guess - x) / x < 0.0001

def improve(guess: Double, x: Double) =(guess + x / guess) / 2

def sqrt(x: Double) = sqrtIter(1.0, x)

def square(x: Double) = x * x

sqrt(12)
sqrt(13)
sqrt(0.001)
sqrt(0.1e-50)
def factorial(x: Int): Int = {
  @tailrec
  def iter(x: Int, result: Int): Int =
    if(x == 0) result else iter(x-1, result * x)
  iter(x, 1)
}
factorial(10)
factorial(0)
factorial(1)
factorial(2)
factorial(3)
factorial(4)




