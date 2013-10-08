package models.submission

import
  scalaz.{ Scalaz, ValidationNel},
    Scalaz.ToValidationV

import
  models.util.ParamBundle

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/26/12
 * Time: 12:16 PM
 */

// Unless I wanna go all `shapeless` on this thing's ass, there's not really a good way to enforce having a `validate` method... --JAB
private[submission] trait Parser {

  protected type Target
  protected type ConsTuple   <: Product
  protected type ParsedTuple <: Product
  protected type FailType    =  String
  protected type Parsed      =  ValidationNel[FailType, ParsedTuple]
  protected type Output      =  ValidationNel[FailType, Target]

  protected def constructFrom(parsed: Parsed) : Output

}

private[submission] trait FromBundleParser extends Parser {
  protected type ByteMapInput = Map[String, Array[Byte]]
  def byteFetch(key: String)(implicit params: ByteMapInput) = // Converts keys to `Validation`s
    params.get(key) map (_.successNel[String]) getOrElse (s"No item with key '$key' passed in".failNel)
  def fromBundle(bundle: ParamBundle) : Output
}

private[submission] trait DataFromBundleParser extends FromBundleParser {

  self: FromMapParser =>

  protected val DataKey = "data"

  protected def fromBundleHelper(bundle: ParamBundle) : Output

  override def fromBundle(bundle: ParamBundle) : Output = {
    if (bundle.stringParams.contains(DataKey))
      fromMap(bundle.stringParams)
    else
      fromBundleHelper(bundle)
  }

}

private[submission] trait FromMapParser extends Parser {

  protected type MapInput = Map[String, String]

  protected def parseFromMap(implicit params: MapInput) : Parsed

  def fromMap(implicit params: MapInput) : Output   = constructFrom(parseFromMap(params))
  def fetch(key: String)(implicit params: MapInput) = // Converts keys to `Validation`s
    params.get(key) map (_.successNel[String]) getOrElse (s"No item with key '$key' passed in".failNel)

  // But not _too_ hard! --JAB (10/8/13)
  protected def tryHarderToGetNested(innerMapName: String)(implicit params: MapInput) : String = {
    val KeyRegex = s"""$innerMapName\\[(.*)\\]""".r
    val q        = "\""
    params collect { case (key @ KeyRegex(innerKey), value) => s"$q$innerKey$q: $q$value$q" } mkString ("{ ", ", ", " }")
  }

}

private[submission] trait FromStringParser extends Parser {
  def fromString(str: String) : Output
}
