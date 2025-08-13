import Main.{AttractionOrdering, DataAccess, travelGuide}
import Main.AttractionOrdering.{ByLocationPopulation, ByName}
import Main.model.{Attraction, Location, LocationID, TravelGuide}
import Main.model.PopCultureSubject.{Artist, Movie}
import cats.Traverse.nonInheritedOps.toTraverseOps
import cats.effect.*
import org.apache.jena.rdfconnection.{RDFConnection, RDFConnectionRemote}
import org.apache.jena.query.{QueryExecution, QueryFactory, QuerySolution}

import scala.jdk.javaapi.CollectionConverters.asScala
import cats.effect.unsafe.implicits.global
import cats.implicits.catsSyntaxParallelSequence1

object Main {

  import model._, model.PopCultureSubject
  import AttractionOrdering._

  object model {
    opaque type LocationID = String

    object LocationID {
      def apply(value: String): LocationID = value

      extension (a: LocationID) def value: String = a
    }

    case class Location(id: LocationID, name: String, population: Int)

    case class Attraction(name: String, description: Option[String], location: Location)

    enum PopCultureSubject {
      case Artist(name: String, followers: Int)
      case Movie(name: String, boxOffice: Int)
    }

    case class TravelGuide(attraction: Attraction, subject: List[PopCultureSubject])
  }

  enum AttractionOrdering {
    case ByName
    case ByLocationPopulation
  }

  trait DataAccess {
    def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]]

    def findArtistsFrom(location: LocationID, limit: Int): IO[List[Artist]]

    def findMovieAboutLocation(location: LocationID, limit: Int): IO[List[Movie]]
  } //接口,里面是函数

  def travelGuide(data: DataAccess, attractionName: String): IO[Option[TravelGuide]] = {
    for {
      attractions <- data.findAttractions(attractionName, ByLocationPopulation, 3)
      guides <- attractions
        .map(attraction =>
          List(
            data.findArtistsFrom(attraction.location.id, 2),
            data.findMovieAboutLocation(attraction.location.id, 2)
          ).parSequence.map(_.flatten).map(popCultureSubjects =>
            TravelGuide(attraction, popCultureSubjects)
          )
        )
        .parSequence
    } yield guides.sortBy(guideScore).reverse.headOption
  }
}


val getConnection: IO[RDFConnection] = IO.delay(
  RDFConnectionRemote.create
    .destination("https://query.wikidata.org/")
    .queryEndpoint("sparql")
    .build
)

private def createExecution(connection: RDFConnection, query: String): IO[QueryExecution] =
  IO.blocking(connection.query(QueryFactory.create(query))) //创造连接

private def closeExecution(execution: QueryExecution): IO[Unit] =
  IO.blocking(execution.close()) //关闭连接


private def execQuery(connection: RDFConnection)(query: String): IO[List[QuerySolution]] =
  val executionResource: Resource[IO, QueryExecution] = //Resource值,可以避免忘记释放资源的情况,无论发生什么都会释放资源---Resource类型
    Resource.make(createExecution(connection, query))(closeExecution) //make()()
  executionResource.use(execution => IO.blocking(asScala(execution.execSelect()).toList)) //use

private def parseAttraction(s: QuerySolution): IO[Attraction] =
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
  )//连接

def findAttractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = {
  val orderBy = ordering match {
    case ByName => "?attractionLabel"
    case ByLocationPopulation => "DESC(?population)"
  }

  val query =
    s"""
         PREFIX wd: <https://www.wikidata.org/entity/>
         PREFIX wdt: <https://www.wikidata.org/prop/direct/>
         PREFIX rdfs: <https://www.w3.org/2000/01/rdf-schema#>
         PREFIX schema: <https://schema.org/>
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
    connection <- getConnection
    solutions <- execQuery(connection)(query)
    attractions <- solutions.traverse(parseAttraction)
  } yield attractions
}

def getSparqlDataAccess(execQueryFn: String => IO[List[QuerySolution]]): DataAccess = new DataAccess {
  val prefixes: String =
    """
      |PREFIX wd: <https://www.wikidata.org/entity/>
      |PREFIX wdt: <https://www.wikidata.org/prop/direct/>
      |PREFIX rdfs: <https://www.w3.org/2000/01/rdf-schema#>
      |PREFIX schema: <https://schema.org/>
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
      solutions <- execQueryFn(query)
      attractions <- IO.delay(
        solutions.map(s =>
          Attraction(
            name = s.getLiteral("attractionLabel").getString,
            description = if (s.contains("description")) Some(s.getLiteral("description").getString) else None,
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


  def findArtistsFrom(locationId: LocationID, limit: Int): IO[List[Artist]] = {
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
      solutions <- execQueryFn(query)
      artists <- IO.delay(
        solutions.map[Artist](s =>
          Artist(name = s.getLiteral("artistLabel").getString, followers = s.getLiteral("followers").getInt)
        )
      )
    } yield artists
  }

  def findMovieAboutLocation(locationId: LocationID, limit: Int): IO[List[Movie]] = {
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
      solutions <- execQueryFn(query)
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
  val totalFollowers = guide.subject.collect { case Artist(_, f) => f }.sum
  val totalBoxOffice = guide.subject.collect { case Movie(_, b) => b }.sum

  val followersScore = Math.min(15, totalFollowers / 100000)
  val boxOfficeScore = Math.min(15, totalBoxOffice / 10000000)
  descriptionScore + quantityScore + followersScore + boxOfficeScore
}

def main(args: Array[String]): Unit = {
  val connectionResource: Resource[IO, RDFConnection] =
    Resource.make(IO.blocking(
      RDFConnectionRemote.create
        .destination("https://query.wikidata.org/")
        .queryEndpoint("sparql")
        .build
    ))(connection => IO.blocking(connection.close()))

  val program: IO[Option[TravelGuide]] =
    connectionResource.use { connection =>
      val wikidata = getSparqlDataAccess(execQuery(connection))
      travelGuide(wikidata, "Yellowstone")
    }

  println(program.unsafeRunAsync(_ => ()))
}
