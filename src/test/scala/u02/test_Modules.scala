import u02.Modules._
import u03.Sequences._
import org.junit.*
import org.junit.Assert.*

import u02.Modules.Person.{Student, Teacher}
import u03.Sequences.Sequence.{Cons, Nil}

class task2Test {

  @Test
  def testGetPersonReturnTeacher(): Unit = {
    val students = Cons(Student("Alice", 2020), Cons(Student("Bob", 2021), Nil()))
    val teachers = Cons(Teacher("Carol", "Math"), Cons(Teacher("Dave", "Physics"), Nil()))
    val mixed = Cons(Student("Eve", 2019), Cons(Teacher("Frank", "Chemistry"), Nil()))

    assertEquals(Cons("Math", Cons("Physics", Nil())), getPersonReturnTeacher(teachers))

  }
}