import org.w3c.dom.events.Event

object Main {
  case class Movie(title: String)

  case class Book(title: String, authors: List[String])

  def bookAdaptations(author: String): List[Movie] =
    if (author == "Tolkien")
      List(Movie("An Unexpected Journey"),
        Movie("The Desolation of Saug"))
    else List.empty

  def recommendedBooks(friend: String): List[Book] = {
    val scala = List(
      Book("Fp in scala", List("Chiusano", "Bjarnason")),
      Book("Get ", List("sa"))
    )
    val fiction = List(
      Book("Harry Potter", List("Rowling")),
      Book("The Lord of the Rings", List("Sfregola"))
    )
    if (friend == "Alice") scala
    else if (friend == "Bob") fiction
    else List.empty
  }

  def recommendatoinFeed(books: List[Book]) = {
  //  books.flatMap(book => book.authors.flatMap(author => bookAdaptations(author).map(movie => s"You may like${movie.title}," + s"becouse you like $author's${book.title}'")))
  for{
    book<-books
    author<-book.authors
    movie<-bookAdaptations(author)
  }yield s"You may like${book.title},"+
    s"because you liked $author's${book.title}'"

  //  books.flatMap(book => book.authors.flatMap(author => bookAdaptations(author).map(movie => s"You may like${movie.title}," + s"becouse you like $author's${book.title}'")))
  //他的结果是是List(You may likeAn Unexpected Journey,becouse you like Tolkien'sThe hobbit', You may likeThe Desolation of Saug,becouse you like Tolkien'sThe hobbit')
  //因为Tolkien
  }

  def main(args: Array[String]): Unit = {
    val books = List(
      Book("Fp in Scala", List("Chiusano", "Bjarnason")),
      Book("The hobbit", List("Tolkien")),
      Book("Modern Java inAction", List("Urma", "Fusco", "Mycroft"))
    )

    /* val authors=books.flatMap(_.authors)  flatMap-多列表变成一个
    println(authors.flatMap(bookAdaptations))
   */

    val friens = List("Alice", "Bob", "Peter")
    friens
      .flatMap(recommendedBooks)
      .flatMap(_.authors)
    println(friens
      .flatMap(recommendedBooks)
      .flatMap(_.authors)
    )
    println(books
      .flatMap(_.authors) //返回的是的是书的所有作者名字组成的列表
      .flatMap(bookAdaptations)//传进去的是书的所有作者名字组成的列表
    )
    println(recommendatoinFeed(books))
  }
}
/*
object Main {
  case class Point(i: Int, i1: Int)

  def isInside(point: Point, radius: Int): Boolean = {
    radius * radius >= point.i * point.i + point.i1 * point.i1
  }

  def validateRadius(radius: Int): List[Int] =
    if (radius > 0) List(radius) else List.empty

  def main(args: Array[String]): Unit = {
    val points = List(Point(5, 2), Point(1, 1))
    val riskyRadiuses = List(-10, 0, 2)
    for {
      r <- riskyRadiuses.filter(r => r > 0)
      point <- points.filter(p => isInside(p, r))
    } yield s"$point is within a radius of$r"
    for {
      r <- riskyRadiuses
      if r > 0
      point <- points
      if isInside(point, r)
    } yield s"$point is within a radius of$r"
    for{
      r<-riskyRadiuses
      value<-validateRadius(r)
      point<-points.filter(p=>isInside(p,value))
    }yield s"$point is within a radius of$r"

  }
}
*/
/*
object Main {
  case class Event(name:String,start:Int,end:Int)
  def validateLength(start: Int, end: Int, minLength: Int): Option[Int] =
    if (end - start > minLength) Some(end - start)
    else None

  def validateName(name: String): Option[String] =
    if (name.size > 0) Some(name)
    else None

  def validateEnd(end: Int): Option[Int] =
    if (end < 3000) Some(end)
    else None

  def validateStart(start: Int, end: Int): Option[Int] =
    if (start <= end) Some(start)
    else None
  def pardeLongEvent(name:String,start:Int,end:Int):Option[Event]=
    for{
      name<-validateName(name)
      start<-validateStart(start,end)
      end<-validateEnd(end)
      length<-validateLength(start,end,minLength = 10)
    }yield Event(name,start,end)
  def main(args: Array[String]): Unit = {
  println( pardeLongEvent("as",1111,2222))
  }
}
*/
