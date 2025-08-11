import cats.effect._
import org.apache.jena.rdfconnection.{RDFConnection, RDFConnectionRemote}
import org.apache.jena.query.{QueryExecution, QueryFactory, QuerySolution}
import scala.io.Codec.fallbackSystemCodec.name

import cats.effect.unsafe.implicits.global
//import ch11_WikidataDataAccess.getSparqlDataAccess
import scala.concurrent.duration._
import scala.jdk.javaapi.CollectionConverters.asScala

object Main {

  import model._, model.PopCultureSubject
  import AttractionOrdering._

  object model {

    opaque type LocationID = String

    object LocationID {
      def apply(value: String): LocationID = value

      extension(a: LocationID);

      def value: String = a
    }

    case class Location(id: LocationID, name: String, population: Int)

    case class Attraction(name: String, description: Option[String], location: Location)

    enum PopCultureSubject {
      case Artist(name: String, followers: Int) =>
      case Movie(name: String, boxOffice: Int) =>
    }

    case class TravelGuide(attraction: Attraction, subject: List[PopCultureSubject])


  }

  enum AttractionOrdering {
    case ByName =>
    case ByLocationPopulation =>
  }

  trait DataAccess {
    def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]]

    def findArtistsFrom(location: LocationID, limit: Int): IO[List[Artist]]

    def findMovieAboutLocation(location: LocationID, limit: Int): IO[List[Movie]]
  }

  def travelGuide(data: DataAccess, attractionname: String): IO[Option[TravelGuide]] =
    for {
      attraction <- data.findAttractions(attractionname, ByLocationPopulation, 1)
      guide <- attraction.headOption match {
        case None => IO.pure(None)
        case Some(attraction) => for {
          artist <- data.findArtistsFrom(attraction.location.id, 2)
          movie <- data.findMovieAboutLocation(attraction.location.id, 2)
        } yield Some(TravelGuide(attraction, artist.appendedAll(movie)))
      }
    } yield guide

  val getConnection: IO[RDFConnection] = IO.delay(
    RDFConnectionRemote.create // we will make it better, see at the end
      .destination("https://query.wikidata.org/")
      .queryEndpoint("sparql")
      .build
  )

  def execQuery(getConnection: IO[RDFConnection], query: String): IO[List[QuerySolution]] = {
    getConnection.flatMap(c =>
      IO.delay(
        asScala(c.query(QueryFactory.create(query)).execSelect()).toList
      )
    )
  }

  def parseAttraction(s: QuerySolution): IO[Attraction] = {
    IO.delay(
      Attraction(
        name = s.getLiteral("attractionLabel").getString,
        description = if (s.contains("description")) Some(s.getLiteral("description").getString) else None,
        location = Location(
          id = LocationID(s.getResource("location").getLocalName),
          name = s.getLiteral("locationLabel").getString,
          population = s.getLiteral("population").getInt
        )
      )
    )
  }

  def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = {
    val orderBy = ordering match {
      case ByName => "?attractionLabel"
      case ByLocationPopulation => "DESC(?population)"
    }

    val query =
      s"""
                      PREFIX wd: <http://www.wikidata.org/entity/>
                      PREFIX wdt: <http://www.wikidata.org/prop/direct/>
                      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                      PREFIX schema: <http://schema.org/>
                      SELECT DISTINCT ?attraction ?attractionLabel ?description ?location ?locationLabel ?population WHERE {
                        ?attraction wdt:P31 wd:Q570116;
                                    rdfs:label ?attractionLabel;
                                    wdt:P131 ?location.
                        FILTER(LANG(?attractionLabel) = "en").

                        OPTIONAL {
                          ?attraction schema:description ?description.
                          FILTER(LANG(?description) = "en").
                        }

                        ?location wdt:P1082 ?population;
                                  rdfs:label ?locationLabel;
                        FILTER(LANG(?locationLabel) = "en").

                        FILTER(CONTAINS(?attractionLabel, "$name")).
                      } ORDER BY $orderBy LIMIT $limit
                      """

    for {
      solutions <- execQuery(getConnection, query)
      attractions <- solutions.traverse(parseAttraction)
    } yield attractions
  }

  def execQuery(connection: RDFConnection)(query: String): IO[List[QuerySolution]] = IO.blocking(
    asScala(connection.query(QueryFactory.create(query)).execSelect()).toList
  )

  def getSparqlDataAccess(execQuery: String => IO[List[QuerySolution]]): DataAccess = new DataAccess {
    val prefixes =
      """
        |PREFIX wd: <http://www.wikidata.org/entity/>
        |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
        |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        |PREFIX schema: <http://schema.org/>
        |""".stripMargin

    def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = {
      val orderBy = ordering match {
        case ByName => "?attractionLabel"
        case ByLocationPopulation => "DESC(?population)"
      }

      val query =
        s"""
           |$prefixes
           |SELECT DISTINCT ?attraction ?attractionLabel ?description ?location ?locationLabel ?population WHERE {
           |  ?attraction wdt:P31 wd:Q570116;
           |              rdfs:label ?attractionLabel;
           |              wdt:P131 ?location.
           |  FILTER(LANG(?attractionLabel) = "en").
           |
           |  OPTIONAL {
           |    ?attraction schema:description ?description.
           |    FILTER(LANG(?description) = "en").
           |  }
           |
           |  ?location wdt:P1082 ?population;
           |            rdfs:label ?locationLabel;
           |  FILTER(LANG(?locationLabel) = "en").
           |
           |  FILTER(CONTAINS(?attractionLabel, "$name")).
           |} ORDER BY $orderBy LIMIT $limit
           |""".stripMargin

      for {
        solutions <- execQuery(query)
        attractions <- IO.delay(
          solutions.map(s =>
            Attraction( // introduce named parameters
              name = s.getLiteral("attractionLabel").getString,
              description =
                if (s.contains("description")) Some(s.getLiteral("description").getString) else None,
              location = Location(
                LocationID(s.getResource("location").getLocalName),
                s.getLiteral("locationLabel").getString,
                s.getLiteral("population").getInt
              )
            )
          )
        )
      } yield attractions
    }

    def findArtistsFromLocation(locationId: LocationID, limit: Int): IO[List[Artist]] = {
      val query =
        s"""
           |$prefixes
           |SELECT DISTINCT ?artist ?artistLabel ?followers WHERE {
           |  ?artist wdt:P136 ?genre;
           |          wdt:P8687 ?followers;
           |          rdfs:label ?artistLabel.
           |  FILTER(LANG(?artistLabel) = "en").
           |
           |  ?artist wdt:P740 wd:${locationId.value}
           |
           |} ORDER BY DESC(?followers) LIMIT $limit
           |""".stripMargin

      for {
        solutions <- execQuery(query)
        artists <-
          IO.delay(
            solutions.map[Artist](s =>
              Artist(name = s.getLiteral("artistLabel").getString, followers = s.getLiteral("followers").getInt)
            )
          )
      } yield artists
    }

    def findMoviesAboutLocation(locationId: LocationID, limit: Int): IO[List[Movie]] = {
      val query =
        s"""
           |$prefixes
           |SELECT DISTINCT ?subject ?subjectLabel ?boxOffice WHERE {
           |  ?subject wdt:P31 wd:Q11424;
           |           wdt:P2142 ?boxOffice;
           |           rdfs:label ?subjectLabel.
           |
           |  ?subject wdt:P840 wd:${locationId.value}
           |
           |  FILTER(LANG(?subjectLabel) = "en").
           |
           |} ORDER BY DESC(?boxOffice) LIMIT $limit
           |""".stripMargin

      for {
        solutions <- execQuery(query)
        movies <- IO.delay(
          solutions.map[Movie](s =>
            Movie(name = s.getLiteral("subjectLabel").getString, boxOffice = s.getLiteral("boxOffice").getInt)
          )
        )
      } yield movies
    }
  }

  def guideScore(guide: TravelGuide): Int = {
    val descriptionScore = guide.attraction.description.map(_ => 30).getOrElse(0)
    val quantityScore = Math.min(40, guide.subject.size * 10)
    val totalFollwers = guide.subject
      .map(_match {
        case Artist(_, followers) => followers
        case _ => 0
      })
      .sum
    val totalBoxOffice = guide.subject
      .map(_match {
        case Movie(_, boxOffice) => boxOffice
        case _ =>
      })
      .sum
    val follwersScore = Math.min(15, totalFollwers / 100000)
    val boxOfficeScore = Math.min(15, totalBoxOffice / 10000000)
    descriptionScore + quantityScore + follwersScore + boxOfficeScore
  }

  def 

  def main(args: Array[String]): Unit = {

  }
}
