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

  override def fromMap(params: MapInput) : Output = {

    val RefIDKey    = "ref_id"
    val DataKey     = "data"
    val Keys        = List(RefIDKey, DataKey)

    val TypeKey     = "type"
    val MetadataKey = "metadata"

    val valueMaybes = Keys map {
      key => params.get(key) map (_.succeed) getOrElse ("No item with key '%s' passed in".format(key).failNel) map (List(_))
    } // We `map` the `Success`es into lists so that `append` (called below) will give me something pattern-matchable --JAB

    val valueTupleMaybe = valueMaybes reduce (_ append _) map {
      case refID :: data :: Nil =>
        val metadata = params.getOrElse(MetadataKey, "")
        val typ      = params.getOrElse(TypeKey, SupplementMetadata.fromString(metadata).fold((_ => ""), (_.getType)))
        (refID, typ, data, metadata)
      case _ =>
        throw new IllegalArgumentException("Broken Supplement validation format!")
    }

    valueTupleMaybe flatMap (validate _).tupled map (UserWorkSupplement.apply _).tupled

  }

  protected def validate(refID: String, typ: String, data: String, metadata: String) : ValidationNEL[F, ConsTuple] = {

    val refIDMaybe = Validator.validateRefID(refID)
    val typeMaybe  = typ.succeed
    val dataMaybe  = data.succeed
    val metaMaybe  = metadata.succeed
    val maybes     = List(refIDMaybe, typeMaybe, dataMaybe, metaMaybe) map (_ map (List(_)))

    maybes reduce (_ append _) map {
      case (refID: Long) :: (typ: String) :: (data: String) :: (metadata: String) :: Nil =>
        (None, Option(refID), typ, data, metadata)
      case _ =>
        throw new IllegalArgumentException("Broken Supplement constructor validation format!")
    }

  }

}
