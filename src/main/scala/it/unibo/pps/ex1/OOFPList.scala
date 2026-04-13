package it.unibo.pps.ex1

import scala.annotation.tailrec

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)
  
  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] = this.map(e => (e, value))/*
    @tailrec
    def zip(value: B, remainingList: List[A], result: List[(A, B)]): List[(A, B)] = remainingList match
      case h :: t => zip(value, t, result.append(List((h, value))))
      case _ => result

    zip(value, this, Nil())*/

  def length(): Int = foldLeft(0)((counter, _) => counter + 1)
  def indices(): List[Int] = foldLeft(Nil())((list, _) => list.append(List(list.length())))
  def zipWithIndex: List[(A, Int)] = foldLeft(Nil())((list, h) => list.append(List((h, list.length())))) // Done with foldLeft instead of foldRight?
  def partition(predicate: A => Boolean): (List[A], List[A]) =
    @tailrec
    def part(predicate: A => Boolean, remainingList: List[A], list1: List[A], list2: List[A]): (List[A], List[A]) = remainingList match
      case h :: t if predicate(h) => part(predicate, t, list1.append(List(h)), list2)
      case h :: t => part(predicate, t, list1, list2.append(List(h)))
      case _ => (list1, list2)

    part(predicate, this, Nil(), Nil())

  def span(predicate: A => Boolean): (List[A], List[A]) =
    @tailrec
    def spanHelper(predicate: A => Boolean, remainingList: List[A], passingPredicateList: List[A]): (List[A], List[A]) = remainingList match
      case h :: t if predicate(h) => spanHelper(predicate, t, passingPredicateList.append(List(h)))
      case _ => (passingPredicateList, remainingList)

    spanHelper(predicate, this, Nil())

  def takeRight(n: Int): List[A] =
    @tailrec
    def right(n: Int, list: List[A]): List[A] = list match
      case _ :: t if list.length() > n => right(n, t)
      case _ => list

    right(n, this)

  def collect(predicate: PartialFunction[A, A]): List[A] =
    @tailrec
    def coll(predicate: PartialFunction[A, A], remainingList: List[A], result: List[A]): List[A] = remainingList match
      case h :: t if predicate.isDefinedAt(h) => coll(predicate, t, result.append(List(predicate(h))))
      case _ :: t => coll(predicate, t, result)
      case _ => result

    coll(predicate, this, Nil())

// Factories
object List:

  def unzip[A, B](list: List[(A, B)]): (List[A], List[B]) = list match
    case (left, right) :: rest =>
      val (leftList, rightList) = unzip(rest)
      (left :: leftList, right :: rightList)
    case Nil() => (Nil(), Nil())

  def unzipWithFold[A, B](list: List[(A, B)]): (List[A], List[B]) =
    list.foldRight((Nil(), Nil())) {
      case ((left, right), (leftList, rightList)) => (left :: leftList, right :: rightList)
    }
  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:
  import List.*
  val reference = List(1, 2, 3, 4)
  println(unzip(List((1, 2), (4, 3), (10, 20)))) //
  println(unzipWithFold(List((1, 2), (4, 3), (10, 20)))) //
  println(reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  /*println(reference.length()) // 4
  println(reference.indices()) // List(0, 1, 2, 3)
  println(reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.takeRight(3)) // List(2, 3, 4)
  println(reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)*/