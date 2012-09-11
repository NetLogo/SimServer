package models.web

import play.api.libs.json.{ JsValue, Reads }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 9/10/12
 * Time: 1:47 PM
 */

// Modeling necessary information for interpreting input parameters (which could be getting interpreted as either JSON or `Map`s)
// Essentially, models how something with a parameter key can become realized parameters/`ParamBox`s
class Param[T] private (val key: String, jsFunc: (JsValue) => Option[T], val pathDescriptor: String, defaulter: Option[() => T] = None) {

  private lazy val default = defaulter.get() // BOOM!

  private def unpeel(opt: Option[T]) = opt orElse (if (this.hasDefault) Option(default) else None)

  def apply(js: JsValue)    : ParamBox[T] = ParamBox(key, unpeel(jsFunc(js)))
  def unpeelJs(js: JsValue) : T           = unpeel(jsFunc(js)) get // BOOM!
  def hasDefault            : Boolean     = !defaulter.isEmpty

}

object Param {

  private val ClassNameMatcher = "(?:.*\\.)?(.*)".r // Capture the part after the last '.' (if there even are any dots)

  private def extractClassName(c: Class[_])                    = c.getName match { case ClassNameMatcher(name) => name }
  private def standardJsonPathFormat(className: String)        = "<root> -> <%s>".format(className.toLowerCase)
  private def standardJsonExtractor(key: String)(js: JsValue)  = js \ key

  // Oh, boy... the trouble that I went through to make this what-turned-out-to-be-trashy factory API...
  // You can't reasonably do `jsFunc` and `pathDescriptor` as default arguments
  // PROTIP: You probably don't want to get me started on this.
  def apply[T : Reads : ClassManifest](key: String) : Param[T] = {
    val jsFunc         = standardJsonExtractor(key)(_: JsValue).asOpt[T]
    val pathDescriptor = standardJsonPathFormat(extractClassName(classManifest[T].erasure))
    new Param(key, jsFunc, pathDescriptor, None)
  }

  def apply[T](key: String, jsFunc: (String) => (JsValue) => Option[T], descriptor: String, defaulter: Option[() => T] = None) =
    new Param(key, jsFunc(key), descriptor, defaulter)

}


/* Models a realized parameter--a parameter that has been had extraction attempted on it.
 * If the extraction succeeded, it will be a `SomeParam`; if not, it will be a `NoneParam`.
 * `ParamBox` can be thought of as a gimped-in-functionality `Option` that always carries
 * with it a `key` value, which tells what parameter is being represented in the box (this
 * is helpful for cases where you don't want to immediately return a `Validation` monad,
 * and you want something else down the line to do the validation of all the parameters,
 * and be able to report specifically _which_ parameters failed to validate)
 */
sealed abstract class ParamBox[+T] {

  def key: String

  def get     : T
  def isEmpty : Boolean

  def flatMap  [U]     (f: T => ParamBox[U]) : ParamBox[U] = if (isEmpty) NoneParam(key) else f(this.get)
  def getOrElse[U >: T](default: => U)       : U           = if (isEmpty) default        else this.get
  def map      [U]     (f: T => U)           : ParamBox[U] = if (isEmpty) NoneParam(key) else SomeParam(key, f(this.get))

}

object ParamBox {
  def apply[T](key: String, opt: Option[T]) : ParamBox[T] = opt map (value => SomeParam(key, value)) getOrElse NoneParam(key)
}

// I wanted to use an `OptionProxy`, but, apparently, one doesn't exist... :(
case class SomeParam[T](override val key: String, value: T) extends ParamBox[T] {
  override def get     = value
  override def isEmpty = false
}

case class NoneParam(override val key: String) extends ParamBox[Nothing] {
  override def get     = throw new NoSuchElementException("NoneParam.get")
  override def isEmpty = true
}