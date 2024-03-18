package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u02.AnonymousFunctions.h
import u03.EncapsulatedSequences.Sequence.nil

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = flatMap(l)(v=>Cons(mapper(v), Nil()))

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] =
       flatMap(l1)( v => pred(v) match
        case true => Cons(v, Nil())
        case false => Nil()
    )


    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
      (first, second) match
        case (Cons(head1, tail1), Cons(head2, tail2)) =>
          Cons((head1, head2), zip(tail1, tail2))
        case _ => Nil()

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(head, tail) if n > 0 => Cons(head, take(tail)(n - 1))
      case _                         => Nil()

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = l1 match
      case Cons(head, tail) => Cons(head, concat(tail, l2))
      case Nil()            => l2

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      l match
        case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
        case Nil()      => Nil()

    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h1, t1) => t1 match
        case Cons(h2, t2) if h1 > h2 => min(t1)
        case Cons(h2, t2) =>  min(Cons(h1, t2))  
        case Nil() =>  Optional.Just(h1)
      case _          => Optional.Empty()

    

@main def trySequences =
  import Sequences.*
  val l =
    Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
