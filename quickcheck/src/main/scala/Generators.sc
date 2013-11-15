trait Generators[+A] {
  self =>
  def generate: A

  def map[S](f: A => S): Generators[S] = new Generators[S] {
    def generate: S = f(self.generate)
  }

  def flatMap[S](f: A => Generators[S]): Generators[S] = new Generators[S] {
    def generate: S = f(self.generate).generate
  }

}

val integers = new Generators[Int] {
  val rand = new java.util.Random

  def generate = rand.nextInt()
}
println(integers.generate)
val booleans = new Generators[Boolean] {
  val rand = new java.util.Random

  def generate = rand.nextInt() > 0
}
val pairs = new Generators[(Int, Int)] {
  val rand = new java.util.Random

  def generate = (rand.nextInt(), rand.nextInt())
}
val boolean2 = integers.map(_ > 0)

def pairs2[A, B](x: Generators[A], y: Generators[B]) = for {
  a <- x
  b <- y
}
yield (a, b)

def single[T](x: T): Generators[T] = new Generators[T] {
  def generate: T = x
}

def choose(lo: Int, hi: Int): Generators[Int] = for {
  x <- integers
}
yield (lo + x % (hi - lo))
def oneOf[T](x: T*): Generators[T] = for {
  a <- choose(1, x.size)
}
yield (x(a))

trait Tree

case class Inner(left: Tree, right: Tree) extends Tree

case class Leaf(x: Int) extends Tree

def leafs: Generators[Leaf] = for {
  x <- integers
} yield Leaf(x)

def inners: Generators[Inner] = for {
  l <- trees
  r <- trees
} yield Inner(l, r)


def trees: Generators[Tree] = for {
  isLeaf <- booleans
  tree <- if (isLeaf) leafs else inners
}
yield tree


