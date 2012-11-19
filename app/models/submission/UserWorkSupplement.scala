package models.submission

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/26/12
 * Time: 12:36 PM
 */

case class UserWorkSupplement(override val id:       Option[Long],
                              override val refID:    Option[Long],
                                           typ:      String,
                                           data:     String,
                                           metadata: String) extends Association {

  def cloneWith(id:       Option[Long]    = this.id,
                refID:    Option[Long]    = this.refID,
                typ:      String          = this.typ,
                data:     String          = this.data,
                metadata: String          = this.metadata) =
    UserWorkSupplement(id, refID, typ, data, metadata)

}

object UserWorkSupplement extends FromMapParser {

  import scalaz.{ Scalaz, ValidationNEL }, Scalaz._

  override protected type Target    = UserWorkSupplement
  override protected type ConsTuple = (Option[Long], Option[Long], String, String, String)

  override def fromMap(implicit params: MapInput) : Output = {

    val RefIDKey    = "ref_id"
    val DataKey     = "data"

    // Not required
    val TypeKey     = "type"
    val MetadataKey = "metadata"

    val valueTupleMaybe = (fetch(RefIDKey) |@| fetch(DataKey)) {
      (refID, data) =>
        val metadata = params.getOrElse(MetadataKey, "")
        val typ      = params.getOrElse(TypeKey, SupplementMetadata.fromString(metadata).fold((_ => ""), (_.getType)))
        (refID, typ, data, metadata)
    }

    valueTupleMaybe flatMap (validate _).tupled map (UserWorkSupplement.apply _).tupled

  }

  protected def validate(refID: String, typ: String, data: String, metadata: String) : ValidationNEL[FailType, ConsTuple] = {

    val refIDMaybe = Validator.validateRefID(refID)
    val typeMaybe  = Validator.accept(typ)
    val dataMaybe  = Validator.accept(data)
    val metaMaybe  = Validator.accept(metadata)

    (refIDMaybe |@| typeMaybe |@| dataMaybe |@| metaMaybe) {
      (refID, typ: String, data: String, metadata: String) =>
        (None, Option(refID), typ, data, metadata)
    }

  }

}
