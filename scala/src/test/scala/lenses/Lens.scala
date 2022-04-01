package lenses

import scala.language.implicitConversions

trait Lens[A,B] private[lenses] () {
  def get(b:B) : A
  def put(a:A, b:B) : B

  /** Pair of two lenses (implemented using [[PairedLens]]) */
  def |[C](other: Lens[C,B])(implicit compatible: Compatible[this.type,other.type]) = new PairedLens[A,C,B,this.type,other.type](this,other)
  /** Chain of two lenses (implemented using [[ChainedLens]]) */
  def ->[C](other: Lens[C,A]): ChainedLens[C, A, B, this.type, other.type] = new ChainedLens(this, other)
}

class PairedLens[A, B, C, F <: Singleton, G <: Singleton] (val f:Lens[A,C] & F, val g:Lens[B,C] & G)
                                                          (implicit compatible: Compatible[f.type,g.type]) extends Lens[(A,B),C] {
  def isPair: IsPair[F, G, this.type] = new IsPair[F,G,this.type](())
  def get(mem: C) : (A,B) = (f.get(mem),g.get(mem))
  def put(ab: (A,B), mem: C) : C = g.put(ab._2, f.put(ab._1, mem))
}

/** A proof that any `f:F, g:G, fg:FG` satisfy `fg = f|g`. */
final class IsPair[F<:Singleton,G<:Singleton,FG<:Singleton] private[lenses] (nothing : Unit) extends AnyVal

class ChainedLens[A, B, C, F <: Singleton, G <: Singleton] (val f:Lens[B,C] & F, val g:Lens[A,B] & G) extends Lens[A,C] {
  def isChain: IsChain[F, G, this.type] = new IsChain[F,G,this.type](())
  def get(mem: C) : A = g.get(f.get(mem))
  def put(a: A, mem: C) : C = f.put(g.put(a, f.get(mem)), mem)
}
/** A proof that any `f:F, g:G, fg:FG` satisfy `fg = f->g`. */
final class IsChain[F<:Singleton,G<:Singleton,FH<:Singleton] private[lenses] (nothing : Unit) extends AnyVal

/** Lens focusing on the first part of a pair */
class Fst[A,B] extends Lens[A,(A,B)] {
  override def get(b:(A,B)) : A = b._1
  override def put(a:A, b:(A,B)) : (A,B) = (a,b._2)
}
/** Lens focusing on the second part of a pair */
class Snd[A,B] extends Lens[B,(A,B)] {
  override def get(b:(A,B)) : B = b._2
  override def put(a:B, b:(A,B)) : (A,B) = (b._1,a)
}

/** A proof that any `f:F, g:G` are the same object.
 *
 * Not used at this point, but could be used to encode facts like `(f|g)->Fst = f`
 **/
final class IsSame[F<:Singleton,G<:Singleton,FG<:Singleton] private[lenses] (nothing : Unit) extends AnyVal

/** A [[Lens]] that fails when accessed.
 * Only used for examples if we only care about checking the compatibility proofs. */
class FakeLens[A,B] extends Lens[A,B] {
  override def get(b:B) : A = throw new UnsupportedOperationException()
  override def put(a:A, b:B) : B = throw new UnsupportedOperationException()
}

/** A proof that for any `f:F, g:G`, f and g are compatible. */
final class Compatible[F<:Singleton,G<:Singleton] private (nothing : Unit) extends AnyVal
object Compatible {
  def claim[A,B,C](a:Lens[A,C], b:Lens[B,C]) : Compatible[a.type,b.type] = new Compatible(())
  def claimType[F<:Singleton,G<:Singleton] : Compatible[F,G] = new Compatible(())

  /* More rules should be added. E.g. if IsPair(f,g), then f,g compatible, compatibility preserved under IsSame, compatibility rules for ->, missing rules for |. */
  implicit def pairRight[F<:Singleton,G<:Singleton,FG<:Singleton,H<:Singleton](pair: IsPair[F,G,FG], compatFH:Compatible[F,H], compatibleGH:Compatible[G,H]) : Compatible[FG,H] = Compatible.claimType
  implicit def pairRightX[A,B,C,F<:Singleton,G<:Singleton,FG<:Singleton,H<:Singleton](pair: PairedLens[A,B,C,F,G], compatFH:Compatible[F,H], compatibleGH:Compatible[G,H]) : Compatible[FG,H] = Compatible.claimType
}

/** Some unspecified type for examples. */
class Memory

/** Example */
object Test {
  // We define three lenses
  val f: Lens[Int, Memory] = FakeLens[Int, Memory]
  val g: Lens[(Int, Int), Memory] = FakeLens[(Int,Int), Memory]
  val h: Lens[String, Memory] = FakeLens[String, Memory]

  // We claim (axiomatically) that they are mutually compatible
  given compatFG: Compatible[f.type, g.type] = Compatible.claim(f,g)
  given compatFH: Compatible[f.type, h.type] = Compatible.claim(f,h)
  given compatGH: Compatible[g.type, h.type] = Compatible.claim(g,h)

  val fg: PairedLens[Int, (Int, Int), Memory, f.type, g.type] = f|g
  // If we want to avoid growing type signatures, we can also extract the witness for the relationship between fg,f,g
  // via `fg.isPair` and then cast fg to `Lens[(Int,(Int,Int)), Memory]`.

  // The search for fg|h below needs a hint, unfortunately. So we give it here.
  implicit val i1 : Compatible[fg.type, h.type] = implicitly
  // For reasons unknown to me, the previous line does not work if we move the code into main (implicit search fails)

  // The following more compact syntax doesn't work, unfortunately. It defines the given in terms of itself
  //  given Compatible[fg.type, h.type] = implicitly

  val fgh: PairedLens[(Int, (Int, Int)), String, Memory, fg.type, h.type] = fg | h

  def main(args: Array[String]): Unit = {
    println("hello")
  }
}
