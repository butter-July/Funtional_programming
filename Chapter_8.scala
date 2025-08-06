import cats.effect.IO
import cats.implicits.*
import cats.effect.unsafe.implicits.global

import scala.compiletime.ops.string.Length
import scala.util.Try

//import ch08_SchedulingMeetings.calendarEntriesApiCall
object Main {
  case class MeetingTime(startHour: Int, endHour: Int)

  def calendarEntriesApiCall(name: String): List[MeetingTime] = {
    import scala.jdk.CollectionConverters._
    Main.calendarEntriesApiCall(name).toList.map(mt =>
      MeetingTime(mt.startHour, mt.endHour) // convert from Java to Scala
    )
  }


  def calendarEntries(name: String): IO[List[MeetingTime]] =
    IO.delay(calendarEntriesApiCall(name))

  def createMeeting(names: List[String], meeting: MeetingTime): IO[Unit] = {
    IO.delay(createMeeting(names, meeting))
  }

  def scheduledMeetings(attendees:List[String]): IO[List[MeetingTime]] = {
    attendees
      .map(attendee=>retry(calendarEntries(attendee),10))
      .sequence
      .map(_.flatten)
  }a

  def meetingOverlaps(meeting1: MeetingTime, meeting2: MeetingTime): Boolean =
    meeting1.endHour > meeting2.startHour && meeting2.endHour > meeting1.startHour

  def possibleMeetings(
                        existingMeetings: List[MeetingTime],
                        startHour: Int,
                        endHour: Int,
                        lengthHours: Int
                      ): List[MeetingTime] = {
    val slots =
      List.range(startHour, endHour - lengthHours + 1).map(startHour => MeetingTime(startHour, startHour + lengthHours))
    slots.filter(slot => existingMeetings.forall(meeting => !meetingOverlaps(meeting, slot)))
  }

  def shedule(attendees:List[String], lengthHours: Int): IO[Option[MeetingTime]] =
    for {
      existingMeetinfgs <- scheduledMeetings(attendees)
      meetins = possibleMeetings(existingMeetinfgs, 8, 16, lengthHours)
      possibleMeeting = meetins.headOption
      _ <- possibleMeeting match {
        case Some(meeting) => createMeeting(attendees, meeting)
        case None => IO.unit
      } 
    } yield possibleMeeting
  def retry[A](action:IO[A],maxRetries:Int):IO[A]=
    List.range(0,maxRetries)
      .map(_=>action)
      .foldLeft(action)((program,retryAction)=>{
        program.orElse(retryAction)
      })
      
  def main(args: Array[String]): Unit = {
  }
}
