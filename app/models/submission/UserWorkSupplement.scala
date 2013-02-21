package models.submission

import
  models.util.ParamBundle

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
                                           rawData:  Array[Byte],
                                           metadata: String) extends Association

object UserWorkSupplement extends FromMapParser with DataFromBundleParser {

  import scalaz.{ Scalaz, ValidationNEL }, Scalaz._

  override protected type Target      = UserWorkSupplement
  override protected type ConsTuple   = (Option[Long], Option[Long], String, String, Array[Byte], String)
  override protected type ParsedTuple = (String, String, Array[Byte], String)

  override protected def fromBundleHelper(bundle: ParamBundle) : Output = {
    constructFrom(parseFromMap(bundle.stringParams) flatMap {
      case (refID, typ, rawData, metadata) if (rawData.isEmpty) =>
        byteFetch(DataKey)(bundle.byteParams) map {
          realRawData => (refID, typ, realRawData, metadata)
        }
      case x =>
        x.successNel[FailType]
    })
  }

  override protected def parseFromMap(implicit params: MapInput) : Parsed = {

    val RefIDKey    = "ref_id"

    // Not required
 // val DataKey
    val TypeKey     = "type"
    val MetadataKey = "metadata"

    fetch(RefIDKey) map {
      refID =>
        val metadata = params.getOrElse(MetadataKey, "")
        val typ      = params.getOrElse(TypeKey, Metadata.fromString(metadata).fold((_ => ""), (_.getType)))
        val rawData  = params.getOrElse(DataKey, "").getBytes
        (refID, typ, rawData, metadata)
    }

  }

  protected def validate(refID: String, typ: String, rawData: Array[Byte], metadata: String) : ValidationNEL[FailType, ConsTuple] = {

    val refIDMaybe = Validator.validateRefID(refID)
    val typeMaybe  = Validator.accept(typ)
    val rawMaybe   = Validator.accept(rawData)
    val metaMaybe  = Validator.accept(metadata)

    (refIDMaybe |@| typeMaybe |@| rawMaybe |@| metaMaybe) {
      (refID, typ: String, rawData: Array[Byte], metadata: String) =>
        (None, Option(refID), typ, "", rawData, metadata)
    }

  }

  override protected def constructFrom(parsed: Parsed) : Output =
    parsed flatMap (validate _).tupled map (UserWorkSupplement.apply _).tupled

}
