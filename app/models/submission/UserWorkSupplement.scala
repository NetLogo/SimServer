package models.submission

import play.api.libs.json._

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
                                           metadata: String) extends Association with JsonWritable {

  override def toJsonObj : JsObject = {
    val typeTuple     = ("type",     JsString(typ))
    val dataTuple     = ("data",     JsString(data))
    val metadataTuple = ("metadata", Json.parse(metadata))
    val tuples         = Seq(typeTuple, dataTuple, metadataTuple)
    JsObject(tuples)
  }

}

object UserWorkSupplement extends FromMapParser {

  import models.datastructure.FullFailValidationList.vsl2Enhanced
  import scalaz.{ Failure, Success, Validation }

  override protected type Target    = UserWorkSupplement
  override protected type ConsTuple = (Option[Long], Option[Long], String, String, String)

  override def fromMap(params: MapInput) : Output = {

    val RefIDKey    = "ref_id"
    val DataKey     = "data"
    val Keys        = List(RefIDKey, DataKey)

    val TypeKey     = "type"
    val MetadataKey = "metadata"

    val valueMaybes = Keys map {
      key => params.get(key) map (Success(_)) getOrElse (Failure("No item with key '%s' passed in\n".format(key))) map (List(_))
    } // We `map` the `Success`es into lists so that `append` (called below) will give me something pattern-matchable --JAB

    val valueTupleMaybe = valueMaybes reduce (_ fullFailAppend _) map {
      case refID :: data :: Nil =>
        val metadata = params.getOrElse(MetadataKey, "")
        val typ      = params.getOrElse(TypeKey, SupplementMetadata.fromString(metadata).fold((_ => ""), (_.getType)))
        (refID, typ, data, metadata)
      case _ =>
        throw new IllegalArgumentException("Broken Supplement validation format!")
    }

    valueTupleMaybe flatMap (validate _).tupled map (UserWorkSupplement.apply _).tupled

  }

  protected def validate(refID: String, typ: String, data: String, metadata: String) : Validation[String, ConsTuple] = {

    val refIDMaybe = Validator.validateRefID(refID)
    val typeMaybe  = Success(typ)
    val dataMaybe  = Success(data)
    val metaMaybe  = Success(metadata)
    val maybes     = List(refIDMaybe, typeMaybe, dataMaybe, metaMaybe) map (_ map (List(_)))

    maybes reduce (_ fullFailAppend _) map {
      case (refID: Long) :: (typ: String) :: (data: String) :: (metadata: String) :: Nil =>
        (None, Option(refID), typ, data, metadata)
      case _ =>
        throw new IllegalArgumentException("Broken Supplement constructor validation format!")
    }

  }

}
