import cats.effect.{Concurrent, IO}
import cats.implicits.*
import cats.effect.unsafe.implicits.global
import fs2.*

import scala.concurrent.duration
import java.util.concurrent.*
import scala.concurrent.duration.FiniteDuration

object Main {
  object model {
    opaque type Currency = String

    object Currency {
      def apply(name: String): Currency = name

      extension (currency: Currency) def name: String = currency
    }
  }

  import model._

  def rates(from: Currency, to: Currency): Stream[IO, BigDecimal] =
    Stream
      .eval(exchangeTable(from)) //创建一个单元素流,该流使用IO,产生单个BigDecimal,然后repeat
      .repeat
      .map(extractSingleCurrencyRate(to)) //提取单个汇率的函数映射流的单个值
      .unNone //过滤None
      .orElse(rates(from, to))

  def trending(rates: List[BigDecimal]): Boolean =
    rates.size > 1 && rates.zip(rates.drop(1)).forall {
      case (previousRate, rate) => rate > previousRate
    }

  def extractSingleCurrencyRate(currentToExtract: Currency)(table: Map[Currency, BigDecimal]): Option[BigDecimal] =
    table.get(currentToExtract)

  def exchangeTable(from: Currency): IO[Map[Currency, BigDecimal]] =
    IO.delay(exchangeRateTableApiCall(from.name)) //没有翻到exchangeRateTableApiCall在哪里,他的作用应该是获取兑换率
      .map(table => table.map {
        case (currencyName, rate) => (Currency(currencyName), rate)
      })

  def retry[A](action: IO[A], maxRetries: Int): IO[A] =
    List.range(0, maxRetries)
      .map(_ => action)
      .foldLeft(action)((program, retryAction) => {
        program.orElse(retryAction)
      })

  def currencyRate(from: Currency, to: Currency): IO[BigDecimal] =
    for {
      table1 <- retry(exchangeTable(from), 10)
      rate <- extractSingleCurrencyRate(to)(table1) match {
        case Some(value) => IO.pure(value)
        case None => currencyRate(from, to) //如果没有成功获取,那么再对自身递归调用一次
      }
    } yield rate

  def lastRate(from: Currency, to: Currency, n: Int): IO[List[BigDecimal]] = //获取被兑换货币和目的货币,,返IO[]
    List.range(0, n).map(_ => currencyRate(from, to)).sequence

  val delay: FiniteDuration = FiniteDuration(1, TimeUnit.SECONDS)
  val ticks: Stream[IO, Unit] = Stream.fixedRate[IO](delay)  //一个Stream值,执行后每一s输出一个Unit值,在等待时,不会阻塞运行线程

  def exchangeIfTrending(amount: BigDecimal, from: Currency, to: Currency): IO[BigDecimal] =
    rates(from, to)
      .zipLeft(ticks) //等待IO调用
      .sliding(3)
      .map(_.toList)
      .filter(trending)
      .map(_.last)
      .take(1)
      .compile
      .lastOrError
      .map(_ * amount)
  /*  for {
      rates <- lastRate(from, to, 3)
      result <- if (trending(rates)) IO.pure(amount * rates.last)
      else exchangeIfTrending(amount, from, to)
    } yield result //有忽略潜在数据的问题
  */
  /* 2,返回值与一相同为IO[Option[BigDecimal]].for {
    rates <- lastRate(from, to)
    result <- if (trending(rates)) IO.pure(Some(amount * rates.last))
    else exchangeIfTrending(amount, from, to)
  } yield result*/
  //递归了,但是可能会带来--无限性,但是安全(?),不会返回None因为在递归,如果没有Some那么就一直在递归中,不会返回值
  // 1:  lastRate(from, to).map(rates => if (trending(rates)) Some(amount * rates.last) else None)
  //如果一直增长那么交换lastRate,trending,exchangeTable.extractSingleCurrencyRate,自下而上

  def main(args: Array[String]): Unit = {
    exchangeIfTrending(BigDecimal(100), Currency("USD"), Currency("EUR"))
  }
}
