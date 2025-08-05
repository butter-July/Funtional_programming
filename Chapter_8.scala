/*import cats.effect.IO
import cats.implicits.**/
import java.io.IO
import scala.util.Try

/*
def calendarEntries(name:String):IO(List[MeetingTime])={
  IO.delay(calendarEntriesApiCall(name))
}*/
object Main {
  case class MeetingTime(startHour: Int, endHour: Int)

  def calendarEntriesApiCall(name: String): IO[List[MeetingTime]] = {
    IO.delay(calendarEntriesApiCall(name))
  }

  def createMeetings(names: List[String], meeting: MeetingTime): IO[Unit] = {
    IO.delay(createMeetings(names, meeting))
  }

  def schedualdMeetings(person1: String, person2: String): IO[List[MeetingTime]] =
    for {
      person1Entries <- calendarEntriesApiCall(person1)
      person2Entries <- calendarEntriesApiCall(person2)
    } yield person1Entries.appendedAll(person2Entries)

  def meetingsOverlap(meeting1: MeetingTime, meeting2: MeetingTime): Boolean =
    meeting1.endHour > meeting2.startHour && meeting2.endHour > meeting1.startHour

  def possibleMeetings(existingMeetins: List[MeetingTime], statrHour: Int, endHour: Int, lengthHours: Int): List[MeetingTime] = {
    val slots = List.range(statrHour, endHour - lengthHours + 1).map(statrHour => MeetingTime(startHour, statrHour + lengthHours))
    slots.fliter(slots => existingMeetins.forall(meeting =>! meetingsOverlap(meeting, slots)))
  }

  def schudule(person1: String, person2: String, lengthHours: Int): IO[Option[MeetingTime]] =
    for {
      existingMeetings <- schedualdMeetings(person1, person2)
      meetings = possibleMeetings(existingMeetings, 8, 16, lengthHours)
    } yield meetings.headOption
  def main(args: Array[String]): Unit = {
    val peogram=schudule("Alce","Bob",1)
    println(peogram)
    println("明天再补全")
  }
}
