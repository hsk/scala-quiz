package com.chatwork.quiz.collection

import com.chatwork.quiz.{ MySome, MyNone, MyOption }

import scala.annotation.tailrec

sealed trait MyList[+A] {

  // Easy
  def length: Int = {
    @tailrec
    def go(n: Int, l: MyList[A]): Int = l match {
      case MyNil        => n
      case MyCons(h, t) => go(n + 1, t)
    }
    go(0, this)
  }

  // Normal
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(l: MyList[A], z: B, f: (B, A) => B): B = l match {
      case MyNil        => z
      case MyCons(h, t) => go(t, f(z, h), f)
    }
    go(this, z, f)
  }

  // 難易度選択制
  // Normal: 条件 - 特にありません、気の向くままに実装してください。
  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B =
    foldLeft(identity[B] _)((bb, a) => b => bb(f(a, b)))(z)

  // Normal
  // scalastyle:off
  def ::[B >: A](b: B): MyList[B] = MyCons(b, this)
  // scalastyle:on

  // Normal
  def reverse: MyList[A] = {
    @tailrec
    def go(l1: MyList[A], l2: MyList[A]): MyList[A] = l1 match {
      case MyNil        => l2
      case MyCons(h, t) => go(t, h :: l2)
    }
    go(this, MyNil)
  }

  // Normal
  // scalastyle:off
  def ++[B >: A](b: MyList[B]): MyList[B] =
    this.foldRight(b)((h, t) => h :: t)
  // scalastyle:on

  // Normal
  def map[B](f: A => B): MyList[B] =
    flatMap(a => MyCons(f(a), MyNil))

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] =
    foldRight(MyNil: MyList[B])((a, bs) => f(a) ++ bs)

  // Normal
  def filter(f: A => Boolean): MyList[A] =
    foldRight(MyNil: MyList[A])((a, as) => if (f(a)) a :: as else as)

  // Normal: 条件 - filterと同様の実装でも構いません。
  // Hard:   条件 - 中間リストを生成しないように実装してください。
  def withFilter(f: A => Boolean): MyList[A] = filter(f)

  // Normal
  def find(f: A => Boolean): MyOption[A] = filter(f) match {
    case MyNil        => MyNone
    case MyCons(h, _) => MySome(h)
  }

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = (this, prefix) match {
    case (MyNil, _) => false
    case (_, MyNil) => true
    case (MyCons(h1, t1), MyCons(h2, t2)) =>
      if (h1 == h2) t1.startsWith(t2)
      else false
  }

}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] =
    if (as.size == 0) MyNil
    else MyCons(as.head, apply(as.tail: _*))

}
