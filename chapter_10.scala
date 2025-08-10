import cats.effect.{Concurrent, IO, Ref}
import cats.implicits.*
import cats.effect.unsafe.implicits.global
import fs2.*

import scala.concurrent.duration
import java.util.concurrent.*
import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Main {
  object model {
    opaque type City = String

    object City {
      def apply(name: String): City = name

      extension (city: City) def name: String = city
    }

    case class CityStats(city: City, checkIns: Int)
  }

  import model._

  def topCities(cityCheckIns: Map[City, Int]): List[CityStats] =
    cityCheckIns.toList
      .map {
        case (city, checkIns) => CityStats(city, checkIns)
      }
      .sortBy(_.checkIns)
      .reverse
      .take(3)

  def processCheckIns(checkIns: Stream[IO, City]): IO[Unit] =
    for {
      storeCheckIns <- Ref.of[IO, Map[City, Int]](Map.empty)
      storeRanking <- Ref.of[IO, List[CityStats]](List.empty)
      rankingProgram = updateRanking(storeCheckIns, storeRanking)
      checkInsProgram = checkIns.evalMap(storeChecKIn(storeCheckIns)).compile.drain
      outputProgram=IO.sleep(1.second)
        .flatMap(_=>storeRanking.get)
        .flatMap(IO.println)
        .foreverM
      _ <- List(rankingProgram, checkInsProgram,outputProgram).parSequence
      
    } yield ()

  def updateRanking(storeCheckIns: Ref[IO, Map[City, Int]], storedRanking: Ref[IO, List[CityStats]]): IO[Nothing] =
    for{
      newRanking<-storeCheckIns.get.map(topCities)
      _ <-storedRanking.set(newRanking)
      result <-updateRanking(storeCheckIns,storedRanking)
    }yield result

  def storeChecKIn(storeCheckIns: Ref[IO, Map[City, Int]])(city: City): IO[Unit] =
    storeCheckIns.update(_.updatedWith(city)) {
      case None => Some(1)
      case Some(checkIns) => Some(checkIns + 1)
    }
  
  /*putPro

  def processCheckIns(checkINS: Stream[IO, City]): IO[Unit] =
    checkINS
      .scan(Map.empty[City, Int])((cityCheckIns, city) =>
        cityCheckIns.updatedWith(city)(_.map(_ + 1).orElse(Some(1)))
      ) //更新统计表
      .chunkN(100_000) //流组合器,取数字n,并且将n个元素转换为一个类似与集合的元素,然后输出他,每100000次处理一次
      .map(_.last)
      .unNone
      .map(topCities)
      .foreach(IO.println)
      .compile.drain

*/
  def main(args: Array[String]): Unit = {
    val checkIns: Stream[IO, City] =
      Stream(City("Sydney"), City("Dublin"), City("Cape Town"), City("Lima"), City("Singapore"))
        .repeatN(100_000)
        .append(Stream.range(0, 100_000).map(i => City(s"City $i")))
        .append(Stream(City("Sydney"), City("Sydney"), City("Lima")))
        .covary[IO]
    processCheckIns(checkIns).unsafeRunSync()
  }

}
