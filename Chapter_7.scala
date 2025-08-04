


import Main.model.Location

import javax.print.attribute.Size2DSyntax.INCH
import javax.print.attribute.standard.MediaSize.NA
import scala.None


object Main {

  /*
  opaque type Genre = String

  object Genre {
    def apply(value: String): Genre = value

    extension (a: Genre) def name: String = a
  }

  opaque type YearsActiveStart = Int

  object YearsActiveStart {
    def apply(value: Int): YearsActiveStart = value

    extension (a: YearsActiveStart) def value: Int = a
  }

  opaque type YearsActiveEnd = Int

  object YearsActiveEnd {
    def apply(value: Int): YearsActiveEnd = value

    extension (a: Int) def value: Int = a
  }*/
  //练习 newtype

  object model {
    opaque type Location = String

    object Location {
      def apply(value: String): Location = value

      extension (a: Location) def name: String = a
    }
  }

  //一个只能使用有限集的类型--求和类型--它还可以用来case,case(比如矛盾的两类)  ,
  enum MusicGenre {
    case HeavyMetal
    case Pop
    case HardRock
  }

  case class PeriodInYears(start: Int, end: Int)

  enum YearsActive {
    case stillActive(since: Int, previosPeriods: List[PeriodInYears])
    case ActiveInPast(period: List[PeriodInYears])
  }

  enum SearchCondition {
    case SearchByGenre(genre: List[MusicGenre])
    case SearchByOrigin(location: List[Location])
    case SearchByActiveYears(period:PeriodInYears)
    case SearchByLength(length: Int, until: Int)
  }

  import SearchCondition._
  import model._
  import MusicGenre._
  import YearsActive._

  //没有YearsActive之前使用的
  case class Artist(
                     name: String,
                     genre: List[MusicGenre],
                     origin: Location,
                     yearsActive: YearsActive
                   ) //求积类型+求和类型==代数数据类型

  //  def wasArtistActive(artist: Artist, yearStart: Int, yearend: Int): Boolean =
  def periodOverlapsWithPeriods(checkedPeriod: PeriodInYears, period: List[PeriodInYears]): Boolean =
    period.exists(p => p.start <= checkedPeriod.end && p.end >= checkedPeriod.start)

  def wasArtistActive(artist: Artist, searchPeriod: PeriodInYears): Boolean = {
    artist.yearsActive match {
      case stillActive(since, previosPeriods) => since <= searchPeriod.end || periodOverlapsWithPeriods(searchPeriod, previosPeriods)
      case ActiveInPast(period) => periodOverlapsWithPeriods(searchPeriod, period)
    }

  }

  def activeLength(artist: Artist, currentYear: Int): Int =
    val period = artist.yearsActive match {
      case stillActive(since, previosPeriods) => previosPeriods.appended(PeriodInYears(since, currentYear))
      case ActiveInPast(period) => period
    }
    period.map(p => p.end - p.start).foldLeft(0)((x, y) => x + y)

  def searchArtists(artists: List[Artist], requiredConditions: List[SearchCondition]): List[Artist] = {
    artists.filter(artists => requiredConditions.forall {
      case SearchByGenre(genre) => genre.contains(artists.genre)
      case SearchByOrigin(location) => location.contains(artists.origin)
      case SearchByLength(howlong, until) => activeLength(artists, until) >= howlong
      case SearchByActiveYears(period) => wasArtistActive(artists, period)
    }
    )
  }

  def main(args: Array[String]): Unit = {
  }
}

/*object Main {
  case class User(name: String, city: Option[String], favoriteArtists: List[String])

  def f1(user: List[User]): List[User] =
    // user.filter { user => (user.city.isEmpty || user.city.contains("Melbourne")) }
    user.filter(_.city.forall(_ == "Melbourne"))

  def f2(user: List[User]): List[User] = {
    user.filter(_.city.contains("Lagos"))
  }

  def f3(user: List[User]): List[User] =
    user.filter(_.favoriteArtists.contains("Bee Gees"))

  def f4(user: List[User]): List[User] =
    user.filter(_.city.exists(_.startsWith("T")))

  def f5(user: List[User]): List[User] =
    user.filter(_.favoriteArtists.forall(_.length > 8))

  def f6(user: List[User]): List[User] =
    user.filter(_.favoriteArtists.exists(_.startsWith("M")))

  def main(args: Array[String]): Unit = {
    val users = List(
      User("Alice", Some("Me"), List("Bee Gees")),
      User("Bob", Some("La"), List("Bee Gees")),
      User("Eve", Some("Tokyo"), List.empty),
      User("Maa", None, List("Metallica", "Bee Gees")),
      User("Trent", Some("Buneos Aires"), List("Led Zeppelin"))
    )
    println(f1(users).map(_.name))
    println(f2(users).map(_.name))
    println(f3(users).map(_.name))
    println(f4(users).map(_.name))
    println(f5(users).map(_.name))
    println(f6(users).map(_.name))

  }
}*/

/*
def activeYear(artist: Artist, CurrentYear: Int): Int =
  artist.yearsActive match {
    case stillActive(since) =>
      CurrentYear - since
    case ActiveBetween(start, end) =>
      end - start
  }
*/
/*
object model {
  opaque type User = String
  object User {
    def apply(name: String): User = name
  }

  opaque type Artist = String
  object Artist {
    def apply(name: String): Artist = name
  }

  case class Song(artist: Artist, title: String)

  enum MusicGenre {
    case House
    case Funk
    case HipHop
  }

  enum PlaylistKind {
    case CuratedByUser(user: User)
    case BasedOnArtist(artist: Artist)
    case BasedOnGenres(genres: Set[MusicGenre])
  }

  case class Playlist(name: String, kind: PlaylistKind, songs: List[Song])
}
import model._, model.MusicGenre._, model.PlaylistKind._

  val fooFighters = Artist("Foo Fighters")
  val playlist1   = Playlist(
    "This is Foo Fighters",
    BasedOnArtist(fooFighters),
    List(Song(fooFighters, "Breakout"), Song(fooFighters, "Learn To Fly"))
  )

  val playlist2 = Playlist(
    "Deep Focus",
    BasedOnGenres(Set(House, Funk)),
    List(Song(Artist("Daft Punk"), "One More Time"), Song(Artist("The Chemical Brothers"), "Hey Boy Hey Girl"))
  )

  val playlist3 = Playlist(
    "My Playlist",
    CuratedByUser(User("Michał Płachta")),
    List(Song(fooFighters, "My Hero"), Song(Artist("Iron Maiden"), "The Trooper"))
  )

  def gatherSongs(playlists: List[Playlist], artist: Artist, genre: MusicGenre): List[Song] =
    playlists.foldLeft(List.empty[Song])((songs, playlist) =>
      val matchingSongs = playlist.kind match {
        case CuratedByUser(user)           => playlist.songs.filter(_.artist == artist)
        case BasedOnArtist(playlistArtist) => if (playlistArtist == artist) playlist.songs else List.empty
        case BasedOnGenres(genres)         => if (genres.contains(genre)) playlist.songs else List.empty
      }
      songs.appendedAll(matchingSongs)
    )

  assert(
    gatherSongs(List(playlist1, playlist2, playlist3), fooFighters, Funk)
      == playlist1.songs.appendedAll(playlist2.songs).appended(Song(fooFighters, "My Hero"))
)
}*/
