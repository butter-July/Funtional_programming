package io.github.zz



object Main {
  def score(word: String): Int = {
    word.replaceAll("a", "").length
  }

  def rankedWords(wordScore: String => Int, words: List[String]): List[String] = { //函数作为参数传入
    words.sortBy(wordScore).reverse //sortBy
  }

  def bonus(word: String): Int =
    if (word.contains("c")) 5 else 0

  def penalty(word: String): Int =
    if (word.contains("s")) 7 else 0

  def cumulativeScore(wordScore: String => Int, words: List[String]): Int =
    words.foldLeft(0)((total,word)=>total+wordScore(word))
  def highScoringWords(wordScore: String => Int): Int => List[String] => List[String] = //单参数函数"柯里化"
    higherThan => words => words.filter(word => wordScore(word) > higherThan) //这个函数返回一个函数,被返回的函数接受Int=>List[String],接受LIstanbul[string],返回List[String]
  /*def  highScoringWord(wordScore:String=>Int)(higherThan:Int)(words:List[String]):List[String]=
    words.filter((word=>wordScore(word)>higherThan))
*/
  //作用同上,只是另一种写法(多参数列表)莫名其妙
  * /* def highScoringWords(wordScore: String => Int, words: List[String]): Int => List[String] =//双参数函数
     higherThan => words.filter(word => wordScore(word) > higherThan)//返回函数的函数.函数返回--接受一个Int并且返回一个List[String]的函数,返回值是一个以higherThan作为参数并返回一个List[String],并在函数内部使用higherThan->这里-- words.filter(word => wordScore(word) > higherThan)
 */

  def main(args: Array[String]): Unit = {
    var words = List("java", "ads", "asdasddasd")
    //println(rankedWords(w => score(w) + bonus(w) - penalty(w), words))
    /* val wordsWithScorehigherThan: Int => List[String] = highScoringWords(w => score(w) + bonus(w) - penalty(w), words) //wordsWithScorehigherThan是函数Int=>List[String]的名字(自己定义的),它可以用来计算分值并且最终返回一个List[String],他的函数特征标记为(higherThan:Int)
     println(wordsWithScorehigherThan(1))
   */
    val wordWithSocreHigerThan: Int => List[String] => List[String] = highScoringWords(w => score(w) + bonus(w) - penalty(w))
    println(wordWithSocreHigerThan(1)(words)) //使用的时候,因为是有两步,所以传参数传两个,一个Int,一个List[String],所以()()
  }
}



/*object Main {
  def number(numbers:Int):Int=
    -numbers
  def main(args: Array[String]): Unit = {
    println(List(2,3,13,42,2).map(number))
  }
}*/
/*object Main {
  def wordss(word: String): Int = word.length

  def nunberofS(word: String): Int = word.length - word.replaceAll("s", "").length

  def odd(number: Int): Boolean = number % 2 == 1

  def main(args: Array[String]): Unit = {
    println(List("java", "rust", "scala", "zzzzz").filter(word => wordss(word) < 5))
    println(List("java", "aaf", "www", "zzssss", "sffe").filter(word => nunberofS(word) > 2))
    println(List(12, 33, 1, 3, 4, 2).filter(odd))
    }
}*/
/*object Main {
  def divided(n: Int): Int => Boolean =
    i => i %n==0
  def lens(number:Int):String=>Boolean=
    word=>word.length<number
  def ContainsS(n:Int):String=>Boolean=
    s=>(s.length-s.replaceAll("s","").length)>n
  def main(args: Array[String]): Unit = {
    println(List(2, 3, 4, 144, 55).filter(divided(5)))
    println(List("zzzzzzzzzzzzz","xxxxxxxx","ccc","vvv","aa").filter(lens(7)))
    println(List("zdsd","ssss","sdsf","zzz").filter(ContainsS(0)))
  }
}*/
/*object Main {
  def larger(n:Int)(number:Int):Boolean=
    number>n
  def divided(n:Int)(number:Int):Boolean=
    number%n==0
  def lenss(n:Int)(word:String):Boolean=
    word.length>n
  def containsS(n:Int)(word:String):Boolean=
    word.length-word.replaceAll("s","").length>n
  def main(args: Array[String]): Unit = {
    println(List(5,1,2,4,0).filter(larger(4)))
    println(List(5,1,2,4,15).filter(divided(5)))
    println(List("zzzz","asdd","ewre","dasdad").filter(lenss(4)))
    println(List("zzzz","asdd","ewre","dasdadss").filter(containsS(2)))
  }
}*/
/*
object Main {
  def length(word:String):Int=
    word.length
  def ContainS(word:String):Int=
    word.length-word.replaceAll("s","").length
  def main(args: Array[String]): Unit = {
    println(List(1,32,42).foldLeft(0)((total,number)=>total+number))
    println(List("zz","zzz","zzz").foldLeft(0)((total,word)=>total+length(word)))
    println(List("zzz","sss","a").foldLeft(0)((total,word)=>total+ContainS(word)))
    println(List(2,1,3,222,11,44).foldLeft(Int.MinValue)((max,i)=>if(i>max)i else max))//=>后面还能进行别的计算,不止++--
  }
}*
//求积类型,他是不变的!!
