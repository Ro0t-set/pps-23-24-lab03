package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u02.AnonymousFunctions.h
import u03.EncapsulatedSequences.Sequence.nil
import u02.CaseMatch.f

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:
    
    extension (l: Sequence[Int])

      def sum(): Int = l match
        case Cons(h, t) => h + t.sum()
        case _          => 0

      @annotation.tailrec
      def min(): Optional[Int] = l match
        case Cons(h1, t1) => t1 match
          case Cons(h2, t2) if h1 > h2 => t1.min()
          case Cons(h2, t2) =>  Cons(h1, t2).min()
          case Nil() =>  Optional.Just(h1)
        case _          => Optional.Empty()

      
    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = l.flatMap(v=>Cons(mapper(v), Nil()))

      def filter(pred: A => Boolean): Sequence[A] =
        l.flatMap( v => pred(v) match
          case true => Cons(v, Nil())
          case false => Nil()
      )


      // Lab 03
      def zip[B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
        (first, second) match
          case (Cons(head1, tail1), Cons(head2, tail2)) =>
            Cons((head1, head2), zip(tail1, tail2))
          case _ => Nil()

      def take(n: Int): Sequence[A] = l match
        case Cons(head, tail) if n > 0 => Cons(head, tail.take(n - 1))
        case _                         => Nil()

      def concat(l2: Sequence[A]): Sequence[A] = l match
        case Cons(head, tail) => Cons(head, tail.concat(l2))
        case Nil()            => l2

      def flatMap[ B](mapper: A => Sequence[B]): Sequence[B] =
        l match
          case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
          case Nil()      => Nil()



      @annotation.tailrec
      def foldLeft[B](a:B)(f:(B, A)=>B): B = l match
        case Cons(h, t) => t.foldLeft(f(a,h))(f)
        case Nil() => a
      



    

@main def trySequences =
  import Sequences.*
  val l =
    Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(l.sum()) // 30



