package com.chatwork.quiz.collection

import com.chatwork.quiz.MyOption
import com.chatwork.quiz.MyNone

sealed trait MyList[+A] {

  // Easy
  def length: Int = {
    this match {
      case MyNil         => 0
      case MyCons(x, xs) => 1 + xs.length
    }
  }

  // Normal
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    this match {
      case MyNil         => z
      case MyCons(x, xs) => xs.foldLeft(f(z, x))(f)
    }
  }

  // 難易度選択制
  // Normal: 条件 - 特にありません、気の向くままに実装してください。
  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B = {
    this match {
      case MyNil         => z
      case MyCons(x, xs) => f(x, xs.foldRight(z)(f))
    }
  }

  // Normal
  // scalastyle:off
  def ::[B >: A](b: B): MyList[B] = {
    MyCons(b, this)
  }
  // scalastyle:on

  // Normal
  def reverse: MyList[A] = {
    this match {
      case MyNil         => this
      case MyCons(x, xs) => xs.reverse ++ MyCons(x, MyNil)
    }
  }

  // Normal
  // scalastyle:off
  def ++[B >: A](b: MyList[B]): MyList[B] = {
    this match {
      case MyNil         => b
      case MyCons(x, xs) => MyCons(x, xs ++ b)
    }
  }
  // scalastyle:on

  // Normal
  def map[B](f: A => B): MyList[B] = {
    this match {
      case MyNil         => MyNil
      case MyCons(x, xs) => f(x) :: xs.map(f)
    }
  }

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] = {
    this match {
      case MyNil         => MyNil
      case MyCons(x, xs) => f(x) ++ xs.flatMap(f)
    }
  }

  // Normal
  def filter(f: A => Boolean): MyList[A] = {
    this match {
      case MyNil                   => this
      case MyCons(x, xs) if (f(x)) => MyCons(x, xs.filter(f))
      case MyCons(x, xs)           => xs.filter(f)
    }
  }

  // Normal: 条件 - filterと同様の実装でも構いません。
  // Hard:   条件 - 中間リストを生成しないように実装してください。
  def withFilter(f: A => Boolean): MyList[A] = {
    this match {
      case MyNil                   => this
      case MyCons(x, xs) if (f(x)) => MyCons(x, xs.filter(f))
      case MyCons(x, xs)           => xs.filter(f)
    }
  }

  // Normal
  def find(f: A => Boolean): MyOption[A] = {
    this match {
      case MyNil                   => MyNone
      case MyCons(x, xs) if (f(x)) => MyOption(x)
      case MyCons(x, xs)           => xs.find(f)
    }
  }

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = {
    this match {
      case MyCons(x, _) if (prefix == x) => true
      case _                             => false
    }
  }

}

case object MyNil extends MyList[Nothing]

case class MyCons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] = {
    def toMyList(as: List[A]): MyList[A] = {
      as match {
        case List()  => MyNil
        case x :: xs => MyCons(x, toMyList(xs))
      }
    }
    toMyList(as.toList)
  }

}
