package models.submission

import anorm._
import anorm.SqlParser._
import play.api.db.DB

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 1:58 PM
 */

//!
object SubmissionManager {

  import play.api.Play.current

  //@ It would be good to have a better way of doing this...
  def getUserWork(period: String, run: String, user: String) : Seq[UserWork] = {
    DB.withConnection { implicit connection =>
      SQL (
        """
          SELECT * FROM user_work
          WHERE period_id = {period} AND run_id = {run} AND user_id = {user};
        """
      ) on (
        "period"    -> period,
        "run"       -> run,
        "user"      -> user
      ) as {
        long("id") ~ long("timestamp") ~ str("period_id") ~ str("run_id") ~ str("user_id") ~
          str("data") ~ str("metadata") ~ str("description") map {
          case id ~ timestamp ~ session ~ run ~ user ~ data ~ metadata ~ description =>
            UserWork(Option(id), timestamp, session, run, user, data, metadata, description,
                     getWorkSupplementsByRefID(id), getWorkCommentsByRefID(id))
          case _ => throw new Exception("Bad format, newb!")
        } *
      }
    }
  }

  def getWorkCommentsByRefID(workRefID: Long) : Seq[UserWorkComment] = {
    DB.withConnection { implicit connection =>
      SQL (
        """
          SELECT * FROM user_work_comments
          WHERE ref_id = {refID};
        """
      ) on (
        "refID" -> workRefID
      ) as {
        long("id") ~ long("ref_id") ~ long("timestamp") ~ str("user_id") ~ str("comment") map {
          case id ~ refID ~ timestamp ~ user ~ comment => UserWorkComment(Option(id), Option(refID), timestamp, user, comment)
          case _ => throw new Exception("Bad format, newb!")
        } *
      }
    }
  }

  def getWorkSupplementsByRefID(workRefID: Long) : Seq[UserWorkSupplement] = {
    DB.withConnection { implicit connection =>
      SQL (
        """
          SELECT * FROM user_work_supplements
          WHERE ref_id = {refID};
        """
      ) on (
        "refID" -> workRefID
      ) as {
        long("id") ~ long("ref_id") ~ str("type") ~ str("data") ~ str("metadata") map {
          case id ~ refID ~ typ ~ data ~ metadata => UserWorkSupplement(Option(id), Option(refID), typ, data, metadata)
          case _ => throw new Exception("Bad format, newb!")
        } *
      }
    }
  }

  def submit[T <% Submittable](submission: T) : BigInt = submission.submit

}

sealed trait Submittable {
  def submit : BigInt
}

private object Submittable {

  implicit def userWork2Submittable(userWork: UserWork) = new Submittable {
    override def submit : BigInt = 0 //@
  }

  implicit def workComment2Submittable(workAssoc: UserWorkComment) = new Submittable {
    override def submit : BigInt = 0 //@
  }

  implicit def workSupplement2Submittable(workAssoc: UserWorkSupplement) = new Submittable {
    override def submit : BigInt = 0 //@
  }

}
