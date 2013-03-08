package models.submission

import anorm._
import anorm.SqlParser._
import play.api.db.DB

import com.mysql.jdbc.exceptions.MySQLIntegrityConstraintViolationException

import scalaz.{ Scalaz, ValidationNel }, Scalaz.ToValidationV

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 1:58 PM
 */

import play.api.Play.current

object SubmissionDBManager {

  import AnormExtras._

  def getRuns : Seq[String] = {
    DB.withConnection { implicit connect =>
      import DBConstants.UserWork._
      SQL (
       s"""
          |SELECT DISTINCT $RunIDKey FROM $TableName;
        """.stripMargin
      ) as {
        str(RunIDKey) map { identity } *
      }
    }
  }

  def getPeriodsByRun(runID: String) : Seq[String] = {
    DB.withConnection { implicit connect =>
      import DBConstants.UserWork._
      SQL (
       s"""
          |SELECT DISTINCT $PeriodIDKey FROM $TableName
          |WHERE $RunIDKey = {run_id};
        """.stripMargin
      ) on (
        "run_id" -> runID
      ) as {
        str(PeriodIDKey) map { identity } *
      }
    }
  }

  def getStudentsByRunAndPeriod(runID: String, periodID: String) : Seq[String] = {
    DB.withConnection { implicit connect =>
      import DBConstants.UserWork._
      SQL (
       s"""
          |SELECT DISTINCT $UserIDKey FROM $TableName
          |WHERE $RunIDKey = {run_id} AND $PeriodIDKey = {period_id};
        """.stripMargin
      ) on (
        "run_id"    -> runID,
        "period_id" -> periodID
      ) as {
        str(UserIDKey) map { identity } *
      }
    }
  }

  def getUserWork(run: String) : Seq[UserWork] = {
    DB.withConnection { implicit connection =>
      import DBConstants.UserWork._
      parseUserWork(SQL (
       s"""
          |SELECT * FROM $TableName
          |WHERE $RunIDKey = {run};
        """.stripMargin
      ) on (
        "run" -> run
      ))
    }
  }

  def getUserWork(run: String, period: String) : Seq[UserWork] = {
    DB.withConnection { implicit connection =>
      import DBConstants.UserWork._
      parseUserWork(SQL (
       s"""
          |SELECT * FROM $TableName
          |WHERE $RunIDKey = {run} AND $PeriodIDKey = {period};
        """.stripMargin
      ) on (
        "run"       -> run,
        "period"    -> period
      ))
    }
  }

  def getUserWork(run: String, period: String, user: String) : Seq[UserWork] = {
    DB.withConnection { implicit connection =>
      import DBConstants.UserWork._
      parseUserWork(SQL (
       s"""
          |SELECT * FROM $TableName
          |WHERE $RunIDKey = {run} AND $PeriodIDKey = {period} AND $UserIDKey = {user};
        """.stripMargin
      ) on (
        "run"       -> run,
        "period"    -> period,
        "user"      -> user
      ))
    }
  }

  private def parseUserWork(sql: SimpleSql[Row])(implicit connection: java.sql.Connection) : Seq[UserWork] = {
    import DBConstants.UserWork._
    sql as {
      long(IDKey) ~ timestamp(TimestampKey) ~ str(RunIDKey) ~ str(PeriodIDKey) ~ str(UserIDKey) ~
        str(TypeKey) ~ str(DataKey) ~ str(MetadataKey) ~ str(DescriptionKey) map {
        case id ~ timestamp ~ run ~ period ~ user ~ typ ~ data ~ metadata ~ description =>
          UserWork(Option(id), timestamp, run, period, user, typ, data, Array(), metadata, description,
                   getWorkSupplementsByRefID(id), getWorkCommentsByRefID(id))
        case _ => raiseDBAccessException
      } *
    }
  }

  def getWorkCommentsByRefID(workRefID: Long) : Seq[UserWorkComment] = {
    DB.withConnection { implicit connection =>
      import DBConstants.UserWorkComments._
      SQL (
       s"""
          |SELECT * FROM $TableName
          |WHERE $RefIDKey = {refID};
        """.stripMargin
      ) on (
        "refID" -> workRefID
      ) as {
        long(IDKey) ~ long(RefIDKey) ~ timestamp(TimestampKey) ~ str(UserIDKey) ~ str(CommentKey) map {
          case id ~ refID ~ timestamp ~ user ~ comment => UserWorkComment(Option(id), Option(refID), timestamp, user, comment)
          case _ => raiseDBAccessException
        } *
      }
    }
  }

  def getWorkSupplementsByRefID(workRefID: Long) : Seq[UserWorkSupplement] = {
    DB.withConnection { implicit connection =>
      import DBConstants.UserWorkSupplements._
      SQL (
       s"""
          |SELECT * FROM $TableName
          |WHERE $RefIDKey = {refID};
        """.stripMargin
      ) on (
        "refID" -> workRefID
      ) as {
        long(IDKey) ~ long(RefIDKey) ~ str(TypeKey) ~ str(DataKey) ~ str(MetadataKey) map {
          case id ~ refID ~ typ ~ data ~ metadata => UserWorkSupplement(Option(id), Option(refID), typ, data, Array(), metadata)
          case _ => raiseDBAccessException
        } *
      }
    }
  }

  def getTypeBundleByName(name: String) : ValidationNel[String, TypeBundle] = {
    DB.withConnection { implicit connection =>
      import DBConstants.TypeBundles._
      val opt = SQL (
       s"""
          |SELECT * FROM $TableName
          |WHERE $NameKey = {name};
        """.stripMargin
      ) on (
        "name" -> name
      ) as {
        str(NameKey) ~ str(ActionJSKey) ~ str(PresentationJSKey) ~ str(FileExtensionKey) map {
          case name ~ action ~ presentation ~ ext => TypeBundle(name, action, presentation, ext)
          case _                                  => raiseDBAccessException
        } *
      } headOption;
      opt map (_.successNel[String]) getOrElse (s"No type bundle found with name $name".failNel)
    }
  }

  def getOrCreateTypeBundleByName(name: String) : ValidationNel[String, TypeBundle] = {
    getTypeBundleByName(name) orElse {
      submit(TypeBundle(name, "", "", "")) flatMap (_ => getTypeBundleByName(name))
    }
  }

  def submit[T <% Submittable](submission: T) : ValidationNel[String, Long] = submission.submit
  def update[T <% Updatable]  (update: T)                                   { update.update() }

}

sealed trait Submittable {
  def submit : ValidationNel[String, Long]
}

private object Submittable {

  import AnormExtras.tryInsert

  implicit def userWork2Submittable(userWork: UserWork) = new Submittable {
    override def submit : ValidationNel[String, Long] = DB.withConnection { implicit connection =>

      import DBConstants.UserWork._
      val sql = SQL (
       s"""
          |INSERT INTO $TableName
          |($TimestampKey, $RunIDKey, $PeriodIDKey, $UserIDKey, $TypeKey, $DataKey, $MetadataKey, $DescriptionKey) VALUES
          |({timestamp}, {runID}, {periodID}, {userID}, {type}, {data}, {metadata}, {description});
        """.stripMargin
      ) on (
        "timestamp"   -> userWork.timestamp,
        "runID"       -> userWork.runID,
        "periodID"    -> userWork.periodID,
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
    override def submit : ValidationNel[String, Long] = DB.withConnection { implicit connection =>

      import DBConstants.UserWorkComments._
      val sql = SQL (
       s"""
          |INSERT INTO $TableName
          |($RefIDKey, $TimestampKey, $UserIDKey, $CommentKey) VALUES
          |({refID}, {timestamp}, {userID}, {comment});
        """.stripMargin
      ) on (
        "refID"     -> workComment.refID,
        "timestamp" -> workComment.timestamp,
        "userID"    -> workComment.userID,
        "comment"   -> workComment.comment
      )

      tryInsert(sql)(_.get.successNel[String])

    }
  }

  implicit def workSupplement2Submittable(workSupplement: UserWorkSupplement) = new Submittable {
    override def submit : ValidationNel[String, Long] = DB.withConnection { implicit connection =>

      import DBConstants.UserWorkSupplements._
      val sql = SQL (
       s"""
          |INSERT INTO $TableName
          |($RefIDKey, $TypeKey, $DataKey, $MetadataKey) VALUES
          |({refID}, {type}, {data}, {metadata});
        """.stripMargin
      ) on (
        "refID"    -> workSupplement.refID,
        "type"     -> workSupplement.typ,
        "data"     -> workSupplement.data,
        "metadata" -> workSupplement.metadata
      )

      tryInsert(sql)(_.get.successNel[String])

    }
  }

  implicit def typeBundle2Submittable(bundle: TypeBundle) = new Submittable {
    override def submit : ValidationNel[String, Long] = DB.withConnection { implicit connection =>

      import DBConstants.TypeBundles._
      val sql = SQL (
       s"""
          |INSERT INTO $TableName
          |($NameKey, $ActionJSKey, $PresentationJSKey, $FileExtensionKey) VALUES
          |({name}, {action_js}, {presentation_js}, {file_extension});
        """.stripMargin
      ) on (
        "name"            -> bundle.name,
        "action_js"       -> bundle.actionJS,
        "presentation_js" -> bundle.presentationJS,
        "file_extension"  -> bundle.fileExtension
      )

      // It makes no sense to get an ID back here, since the unique key for these is their already-known names
      tryInsert(sql)(_ => 0L.successNel[String])

    }
  }

}

sealed trait Updatable {
  def update()
}

private object Updatable {

  implicit def userWork2Updatable(userWork: UserWork) = new Updatable {
    override def update() { DB.withConnection { implicit connection =>

      import DBConstants.UserWork._
      val sql = SQL (
       s"""
          |UPDATE $TableName
          |SET $TimestampKey={timestamp}, $RunIDKey={run_id}, $PeriodIDKey={period_id}, $UserIDKey={user_id},
          |    $TypeKey={type}, $DataKey={data}, $MetadataKey={metadata}, $DescriptionKey={description}
          |WHERE $IDKey={id};
        """.stripMargin
      ) on (
        "id"          -> userWork.id,
        "timestamp"   -> userWork.timestamp,
        "run_id"      -> userWork.runID,
        "period_id"   -> userWork.periodID,
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

      import DBConstants.UserWorkSupplements._
      val sql = SQL (
       s"""
          |UPDATE $TableName
          |SET $RefIDKey={ref_id}, $TypeKey={type}, $DataKey={data}, $MetadataKey={metadata}
          |WHERE $IDKey={id};
        """.stripMargin
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

      import DBConstants.TypeBundles._
      val sql = SQL (
       s"""
          |UPDATE $TableName
          |SET $ActionJSKey={action_js}, $PresentationJSKey={presentation_js}, $FileExtensionKey={file_extension}
          |WHERE $NameKey={name};
        """.stripMargin
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
  def tryInsert(sql: SimpleSql[Row])(f: (Option[Long]) => ValidationNel[String, Long])
               (implicit connection: java.sql.Connection) : ValidationNel[String, Long] = {
    try sql.executeInsert() match { case x => f(x) }
    catch {
      case ex: MySQLIntegrityConstraintViolationException => s"SQL constraint violated: ${ex.getMessage}".failNel
    }
  }
}

private object DBConstants {

  trait Table {
    def TableName: String
  }

  object TypeBundles extends Table {

    override val TableName = "type_bundles"

    val ActionJSKey       = "action_js"
    val FileExtensionKey  = "file_extension"
    val NameKey           = "name"
    val PresentationJSKey = "presentation_js"

  }

  object UserWork extends Table {

    override val TableName = "user_work"

    val DataKey        = "data"
    val DescriptionKey = "description"
    val IDKey          = "id"
    val MetadataKey    = "metadata"
    val PeriodIDKey    = "period_id"
    val RunIDKey       = "run_id"
    val TimestampKey   = "timestamp"
    val TypeKey        = "type"
    val UserIDKey      = "user_id"

  }

  object UserWorkComments extends Table {

    override val TableName = "user_work_comments"

    val CommentKey   = "comment"
    val IDKey        = "id"
    val RefIDKey     = "ref_id"
    val TimestampKey = "timestamp"
    val UserIDKey    = "user_id"

  }

  object UserWorkSupplements extends Table {

    override val TableName = "user_work_supplements"

    val DataKey     = "data"
    val IDKey       = "id"
    val MetadataKey = "metadata"
    val RefIDKey    = "ref_id"
    val TypeKey     = "type"

  }

}
