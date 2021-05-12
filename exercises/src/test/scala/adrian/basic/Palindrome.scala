package adrian.basic

import scala.annotation.tailrec

object Palindrome extends App {

  assert(isPalindrome("0".toCharArray))
  assert(isPalindrome("".toCharArray))
  assert(isPalindrome("11".toCharArray))
  assert(isPalindrome("1221".toCharArray))
  assert(isPalindrome("12321".toCharArray))

  assert(!isPalindrome("12".toCharArray))
  assert(!isPalindrome("123".toCharArray))

  assert(isPalindromeRec("0".toCharArray))
  assert(isPalindromeRec("".toCharArray))
  assert(isPalindromeRec("11".toCharArray))
  assert(isPalindromeRec("1221".toCharArray))
  assert(isPalindromeRec("12321".toCharArray))

  assert(!isPalindromeRec("12".toCharArray))
  assert(!isPalindromeRec("123".toCharArray))

  def isPalindrome(a: Array[Char]): Boolean = {
    if (a.isEmpty)
      true
    else {
      var i = 0
      var j = a.length - 1
      var isPalindrome = true
      while (i <= j && isPalindrome) {
        isPalindrome = a(i) == a(j)
        i = i + 1
        j = j - 1
      }
      isPalindrome
    }
  }

  def isPalindromeRec(a: Array[Char]): Boolean = {

    @tailrec
    def isPalindromeTR(a: Array[Char], i: Int, j: Int, isPal: Boolean): Boolean = {
      if (i > j || !isPal)
        isPal
      else
        isPalindromeTR(a, i + 1, j - 1, isPal && a(i) == a(j))
    }
    if (a.length <= 1)
      true
    else
      isPalindromeTR(a, 0, a.length - 1, isPal = true)
  }
}
