
object Main {
  case class TvShow(title: String, start: Int, end: Int)

  def exactYear(rawShow: String): Either[String, Int] = { //start
    val bracketOpen = rawShow.indexOf('(')
    val dash = rawShow.indexOf('-')
    for {
      yearstr <- if (bracketOpen != -1 && dash > bracketOpen + 1) {
        Right(rawShow.substring(bracketOpen + 1, dash))
      }
      else Left(s"Can't extract year from $rawShow")
      year <- yearstr.toIntOption.toRight(s"Can't extract year from $rawShow")
  }yield year

   /* for {
      yearStart <- if (bracketOpen != -1 && dash > bracketOpen - 1)
        Some(rawShow.substring(bracketOpen + 1, dash))
      else None
      year <- yearStart.toIntOption
    } yield year
*/  //Option 版本
    /*
      exactName((rawShow).flatMap(name=>{
        exactYear(rawShow).flatMap(year=>{
          exactEnd(rawShow).map(end=>{
            TvShow(name,year,end)
        })
      })
    })
  }*/
  }

  def exactEnd(rawShow: String): Either[String,Int] = {
    val bracketClose = rawShow.indexOf(")")
    val dash = rawShow.indexOf("-")
    for {
      yearEnd <- if (dash != -1 && bracketClose > dash + 1)
        Right(rawShow.substring(dash + 1, bracketClose))
      else Left(s"Can't get exact yearEnd form $rawShow")
      year <- yearEnd.toIntOption.toRight(s"Can't get exact yearEnd form $rawShow")
    } yield year
  }

  def exactName(rawShow: String): Either[String, String] = {
    val bractOpen = rawShow.indexOf('(')
    if (bractOpen > 0)
      Right(rawShow.substring(0, bractOpen).trim)
    else Left(s"Can't exact name from $rawShow ")
  }

  def extractSingleYear(rawShow: String): Either[String,Int]= {
    val dash = rawShow.indexOf('-')
    val bracketOpen = rawShow.indexOf('(')
    val bracketClose = rawShow.indexOf(')')
    for {
      yearStr <- if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1)
        Right(rawShow.substring(bracketOpen + 1, bracketClose))
      else Left("Can't")
      year <- yearStr.toIntOption.toRight("Can't")
    } yield year
  }

  def parseShow(rawShow: String): Either[String, TvShow] =
    for {
      name <- exactName(rawShow)
      yearStart <- exactYear(rawShow).orElse(extractSingleYear(rawShow))
      yearEnd <- exactEnd(rawShow).orElse(extractSingleYear(rawShow))
    } yield TvShow(name, yearStart, yearEnd)

  def parseShows(rawShow: List[String]): Either[String,List[TvShow]] = {
    val initialResult:Either[String,List[TvShow]]=Right(List.empty)
    rawShow
      .map(parseShow)
      .foldLeft(initialResult)(addOrResign)
  /* val initialResult: Option[List[TvShow]]=Some(List.empty)
    rawShow.map(parseShow)
      .foldLeft(initialResult)(addOrResign)  //返回一个Option[List[TvShow]]类型  //List[Option[TvShow]]->Option[List[TvShow]]版本
  */
    //rawShow.map(parseShow).flatten

  }
    def addOrResign(parsedShow:Either[String,List[TvShow]],newParsedShow:Either[String,TvShow]):Either[String,List[TvShow]]=
      for{
        shows<-parsedShow
        parseShow<-newParsedShow
      }yield shows.appended(parseShow)
    def main(args: Array[String]): Unit = {
      val rawShows: List[String] = List(
        "Breaking bad(2008-2013)",
        "The Wire(2007-2008)",
        "Mad Men(2007-2015)",
        "Name 00"
      )

      println(parseShows(rawShows))
    }
  }

