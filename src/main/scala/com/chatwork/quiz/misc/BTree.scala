package com.chatwork.quiz.misc

/**
 * [[BTree]]に格納される要素。
 */
sealed trait Node {

  /**
   * ノードが持つ値。
   */
  val value: Int

  /**
   * ノード数。
   */
  val size: Int

  /**
   * ノードが保持するすべての値の合計値。
   */
  val sum: Int

  /**
   * ノードが保持するすべての値の平均値。
   */
  val avg: Double

  /**
   * ノードが保持する最大値。
   */
  val max: Int

  /**
   * ノードが保持する最小値。
   */
  val min: Int

  /**
   * 指定した値を保持するノードを検索する。
   *
   * @param value 値
   * @return ノード
   */
  def find(value: Int): Option[Node]

}

/**
 * 枝を表す[[Node]]。
 *
 * @param left　左の[[Node]]
 * @param value 値
 * @param right 右の[[Node]]
 */
case class Branch(left: Node, value: Int, right: Node) extends Node {

  val size: Int = {
    1 +
      (if (left == null) 0 else left.size) +
      (if (right == null) 0 else right.size)
  }

  val sum: Int = {
    value +
      (if (left == null) 0 else left.sum) +
      (if (right == null) 0 else right.sum)
  }

  val avg: Double = {
    sum * 1.0 / size
  }
  val max: Int = {
    scala.math.max(
      if (left == null) value else scala.math.max(left.max, value),
      if (right == null) value else right.max)
  }

  val min: Int = {
    scala.math.min(
      if (left == null) value else scala.math.min(left.min, value),
      if (right == null) value else right.min)
  }

  def find(value: Int): Option[Node] = {
    val v = (this.value - value)
    if (v == 0) Some(this)
    else if (v > 0) left.find(value)
    else right.find(value)
  }

}

/**
 * 葉を表す[[Node]]。
 *
 * @param value 値
 */
case class Leaf(value: Int) extends Node {

  val size: Int = 1

  val sum: Int = value

  val avg: Double = value

  val max: Int = value

  val min: Int = value

  def find(value: Int): Option[Node] = {
    if (this.value == value) Some(this) else None
  }

}

/**
 * 二分木データ構造。
 *
 * @param node 頂点のノード
 */
case class BTree(node: Node) {

  lazy val size: Int = node.size

  lazy val sum: Int = node.sum

  lazy val avg: Double = node.avg

  lazy val max: Int = node.max

  lazy val min: Int = node.min

  def find(value: Int): Option[Node] = node.find(value)

}

/**
 * [[BTree]]のコンパニオンオブジェクト。
 */
object BTree {

  /**
   * ファクトリメソッド。
   *
   * @param values ノードに格納する値の集合
   * @return [[BTree]]
   */
  def apply(values: List[Int]): BTree = {
    def insertNode(node: Node, value: Int): Node = {
      node match {
        case null => Leaf(value)
        case Leaf(n) =>
          if (value < n) Branch(Leaf(value), n, null)
          else Branch(Leaf(n), value, null)
        case Branch(l, n, null) if (value > n) => Branch(l, n, Leaf(value))
        case Branch(l, n, r) =>
          if (value < n) Branch(insertNode(l, value), n, r)
          else Branch(l, n, insertNode(r, value))
      }
    }

    BTree(
      values.foldLeft(null: Node) {
        case (node, v) => insertNode(node, v)
      })
  }

}

