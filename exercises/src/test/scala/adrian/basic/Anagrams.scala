package adrian.basic

object Anagrams extends App {
  anagrams("mara").foreach(println)

        /* maro
        m - aro
          ma - ro
            mar - o
              maro
          mr - ao
            mro -a
              mroa
         a - mro
         */

  def anagrams(word: String): Set[String] = {
    word.length match {
      case 0 => Set.empty
      case 1 => Set(word)
      case 2 => Set(word, word.reverse)
        //abc: bac acb cba -  bca cab
      case _ =>
        def bt(currentWord: String, remainingLetters: List[Char], acc: Set[String]): Set[String] = {
          if (remainingLetters.isEmpty)
            acc + currentWord
          else  {
            val r = remainingLetters.zipWithIndex
            val x = for {
              c <- r
            } yield bt(
              currentWord + c._1,
              r.filter(e => e._2 != c._2).map(_._1),
              acc
            )
            x.reduce(_ ++ _)
          }
        }

        bt("", word.toList, Set.empty)




    }


  }

}
