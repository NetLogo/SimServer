package models.submission

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/23/12
 * Time: 2:00 PM
 */

trait Submission {
  def id: Option[Long]
}

trait Association extends Submission {
  def refId : Option[Long]
}

trait Entry extends Submission



//@ Do this as some point
sealed trait UserWorkSupplement extends Association

case class UserWork(override val id:          Option[Long] = None,
                                 timestamp:   Long = System.currentTimeMillis(),
                                 periodID:    String,
                                 runID:       String,
                                 userID:      String,
                                 data:        String,
                                 metadata:    String,
                                 description: String,
                                 supplements: Seq[UserWorkSupplement],
                                 comments:    Seq[UserWorkComment]) extends Entry {

  def addComments   (newComments:    UserWorkComment*)    = this.cloneWith(comments = this.comments ++ newComments)
  def addSupplements(newSupplements: UserWorkSupplement*) = this.cloneWith(supplements = this.supplements ++ newSupplements)

  def cloneWith(id:          Option[Long]            = this.id,
                timestamp:   Long                    = this.timestamp,
                periodID:    String                  = this.periodID,
                runID:       String                  = this.runID,
                userID:      String                  = this.userID,
                data:        String                  = this.data,
                metadata:    String                  = this.metadata,
                description: String                  = this.description,
                supplements: Seq[UserWorkSupplement] = this.supplements,
                comments:    Seq[UserWorkComment]    = this.comments) =
    UserWork(id, timestamp, periodID, runID, userID, data, metadata, description, supplements, comments)

}

case class UserWorkComment(override val id:        Option[Long],
                           override val refId:     Option[Long],
                                        timestamp: Long = System.currentTimeMillis(),
                                        userId:    String,
                                        comment:   String) extends Association
