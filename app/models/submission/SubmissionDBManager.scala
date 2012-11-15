package models.submission

import anorm._
import anorm.SqlParser._
import play.api.db.DB

import scalaz.{ Scalaz, ValidationNEL }, Scalaz.ToValidationV

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 1:58 PM
 */

import play.api.Play.current

//@ Constantify field names

object SubmissionDBManager {

  import AnormExtras._

  def getStudentsByRunAndPeriod(runID: String, periodID: String) : Seq[String] = {
    DB.withConnection { implicit connect =>
      SQL (
        """
          |SELECT DISTINCT user_id FROM user_work
          |WHERE run_id = {run_id} AND period_id = {period_id};
        """.stripMargin
      ) on (
        "run_id"    -> runID,
        "period_id" -> periodID
        ) as {
        str("user_id") map { identity } *
      }
    }
  }

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
        long("id") ~ timestamp("timestamp") ~ str("period_id") ~ str("run_id") ~ str("user_id") ~
          str("type") ~ str("data") ~ str("metadata") ~ str("description") map {
          case id ~ timestamp ~ session ~ run ~ user ~ typ ~ data ~ metadata ~ description =>
            UserWork(Option(id), timestamp, session, run, user, typ, data, metadata, description,
                     getWorkSupplementsByRefID(id), getWorkCommentsByRefID(id))
          case _ => raiseDBAccessException
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
        long("id") ~ long("ref_id") ~ timestamp("timestamp") ~ str("user_id") ~ str("comment") map {
          case id ~ refID ~ timestamp ~ user ~ comment => UserWorkComment(Option(id), Option(refID), timestamp, user, comment)
          case _ => raiseDBAccessException
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
          case _ => raiseDBAccessException
        } *
      }
    }
  }

  def getTypeBundleByName(name: String) : ValidationNEL[String, TypeBundle] = {
    DB.withConnection { implicit connection =>
      val opt = SQL (
        """
          SELECT * FROM type_bundles
          WHERE name = {name};
        """
      ) on (
        "name" -> name
      ) as {
        str("name") ~ str("action_js") ~ str("presentation_js") ~ str("file_extension") map {
          case name ~ action ~ presentation ~ ext => TypeBundle(name, action, presentation, ext)
          case _                                  => raiseDBAccessException
        } *
      } headOption;
      opt map (_.successNel[String]) getOrElse ("No type bundle found with name %s".format(name).failNel)
    }
  }

  def submit[T <% Submittable](submission: T) : ValidationNEL[String, Long] = submission.submit
  def update[T <% Updatable]  (update: T)                                   { update.update() }

}

sealed trait Submittable {
  def submit : ValidationNEL[String, Long]
}

private object Submittable {

  implicit def userWork2Submittable(userWork: UserWork) = new Submittable {
    override def submit : ValidationNEL[String, Long] = DB.withConnection { implicit connection =>

        val sql = SQL (
          """
            INSERT INTO user_work
            (timestamp, period_id, run_id, user_id, type, data, metadata, description) VALUES
            ({timestamp}, {periodID}, {runID}, {userID}, {type}, {data}, {metadata}, {description});
          """
        ) on (
          "timestamp"   -> userWork.timestamp,
          "periodID"    -> userWork.periodID,
          "runID"       -> userWork.runID,
          "userID"      -> userWork.userID,
          "type"        -> userWork.typ,
          "data"        -> userWork.data,
          "metadata"    -> userWork.metadata,
          "description" -> userWork.description
          )

        sql.executeInsert().get.successNel[String]

    }
  }

  implicit def workComment2Submittable(workComment: UserWorkComment) = new Submittable {
    override def submit : ValidationNEL[String, Long] = DB.withConnection { implicit connection =>

        val sql = SQL (
          """
            INSERT INTO user_work_comments
            (ref_id, timestamp, user_id, comment) VALUES
            ({refID}, {timestamp}, {userID}, {comment});
          """
        ) on (
          "refID"     -> workComment.refID,
          "timestamp" -> workComment.timestamp,
          "userID"    -> workComment.userID,
          "comment"   -> workComment.comment
        )

        sql.executeInsert().get.successNel[String]

    }
  }

  implicit def workSupplement2Submittable(workSupplement: UserWorkSupplement) = new Submittable {
    override def submit : ValidationNEL[String, Long] = DB.withConnection { implicit connection =>

        val sql = SQL (
          """
            INSERT INTO user_work_supplements
            (ref_id, type, data, metadata) VALUES
            ({refID}, {type}, {data}, {metadata});
          """
        ) on (
          "refID"    -> workSupplement.refID,
          "type"     -> workSupplement.typ,
          "data"     -> workSupplement.data,
          "metadata" -> workSupplement.metadata
        )

        sql.executeInsert().get.successNel[String]

    }
  }

  implicit def typeBundle2Submittable(bundle: TypeBundle) = new Submittable {
    override def submit : ValidationNEL[String, Long] = DB.withConnection { implicit connection =>

      val sql = SQL (
        """
          INSERT INTO type_bundles
          (name, action_js, presentation_js, file_extension) VALUES
          ({name}, {action_js}, {presentation_js}, {file_extension});
        """
      ) on (
        "name"            -> bundle.name,
        "action_js"       -> bundle.actionJS,
        "presentation_js" -> bundle.presentationJS,
        "file_extension"  -> bundle.fileExtension
      )

      sql.executeInsert(); 0L.successNel[String]
      // It makes no sense to get an ID back here, since the unique key for these is their already-known names

    }
  }

}

sealed trait Updatable {
  def update()
}

private object Updatable {

  implicit def userWork2Updatable(userWork: UserWork) = new Updatable {
    override def update() { DB.withConnection { implicit connection =>

      val sql = SQL (
        """
          UPDATE user_work
          SET timestamp={timestamp}, period_id={period_id}, run_id={run_id}, user_id={user_id},
              type={type}, data={data}, metadata={metadata}, description={description}
          WHERE id={id};
        """
      ) on (
        "id"          -> userWork.id,
        "timestamp"   -> userWork.timestamp,
        "period_id"   -> userWork.periodID,
        "run_id"      -> userWork.runID,
        "user_id"     -> userWork.userID,
        "type"        -> userWork.typ,
        "data"        -> userWork.data,
        "metadata"    -> userWork.metadata,
        "description" -> userWork.description
      )

      sql.executeUpdate()

    }}
  }

  implicit def workSupplement2Updatable(workSupplement: UserWorkSupplement) = new Updatable {
    override def update() { DB.withConnection { implicit connection =>

      val sql = SQL (
        """
          UPDATE user_work_supplements
          SET ref_id={ref_id}, type={type}, data={data}, metadata={metadata}
          WHERE id={id};
        """
      ) on (
        "id"       -> workSupplement.id,
        "ref_id"   -> workSupplement.refID,
        "type"     -> workSupplement.typ,
        "data"     -> workSupplement.data,
        "metadata" -> workSupplement.metadata
      )

      sql.executeUpdate()

    }}
  }

  implicit def typeBundle2Updatable(bundle: TypeBundle) = new Updatable {
    override def update() { DB.withConnection { implicit connection =>

      val sql = SQL (
        """
          UPDATE type_bundles
          SET action_js={action_js}, presentation_js={presentation_js}, file_extension={file_extension}
          WHERE name={name};
        """
      ) on (
        "name"            -> bundle.name,
        "action_js"       -> bundle.actionJS,
        "presentation_js" -> bundle.presentationJS,
        "file_extension"  -> bundle.fileExtension
      )

      sql.executeUpdate()

    }}
  }

}

object AnormExtras {
  import java.math.{ BigInteger => JBigInt }
  def timestamp(columnName: String) : RowParser[Long] = get[JBigInt](columnName)(implicitly[Column[JBigInt]]) map (new BigInt(_).toLong)
  def raiseDBAccessException = throw new java.sql.SQLException("Retrieved data from database in unexpected format.")
}
