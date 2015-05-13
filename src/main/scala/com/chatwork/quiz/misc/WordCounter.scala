package com.chatwork.quiz.misc

/**
 * ワードをカウントするオブジェクト。
 */
object WordCounter {

  /**
   * 文字列から単語数をカウントする。
   *
   * @param words 文字列
   * @return 単語がキー、単語数がヴァリューのマップ
   */
  def countWords(words: List[String]): Map[String, Int] = {
    words.foldLeft(Map[String, Int]()) {
      case (map, word) =>
        word.split(" ").foldLeft(map) {
          case (map, value) =>
            if (map.contains(value)) map + (value -> (map(value) + 1))
            else map + (value -> 1)
        }
    }
  }

}
