package u02

import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil, flatMap}

object Modules extends App:

  // An ADT: type + module
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

  println(Person.name(Person.Student("mario", 2015)))

  import Person.*

  println(name(Student("mario", 2015)))

  // a method outside the Person module
  def isStudent(p: Person): Boolean = p match
    case Student(_, _) => true
    case _             => false

  println(isStudent(Student("mario", 2015)))

// 3. Consider Person and Sequence as implemented in class slides.
// Create a function that takes a sequence of Persons and returns a
// sequence containing only the courses of Student in that list
// ▶ Hint 1: you essentially need to combine filter and map
// ▶ Hint 2: there is a very concise solution that reuses flatMap

  def getPersonReturnTeacher(p: Sequence[Person]): Sequence[String] =
    flatMap(p)(v => v match
      case Student(name, year) => Nil()
      case Teacher(name, course) => Cons(course, Nil())
    )
