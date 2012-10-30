package views

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/26/12
 * Time: 3:06 PM
 */

//@ This whole system has gotten really gross, really quick... --JAB
object ItemBasedHtmlGenerator {

  // Return HTML from this... OR DIE!  --JAB
  def apply(implicit p: models.submission.Presentable) : String = {
    val AssetsPath       = "/assets/" //@ FIX!!!!
    val ImageNotFoundURL = AssetsPath + "images/not_found.png"
    p.typ match {
      case t @ "export_world" =>
        ensuring[models.submission.UserWork, String] {
          work =>
            val imageOpt    = work.supplements find (s => s.typ == "export_interface" || s.typ == "export_view")
            val imageURLOpt = imageOpt map (s => "%s/uploads/%s/%s".format(AssetsPath, s.typ, s.data))
            val modelName   = (play.api.libs.json.Json.parse(work.metadata) \ "model").as[String] //@ Validate better
            val tag         = """<img src="%s" class="work_image" />""".format(imageURLOpt getOrElse ImageNotFoundURL)
            basicWrap(tag, t, """{ path: "%s/export_world/%s", model: "%s" }""".format(AssetsPath, work.data, modelName))
        }
      case t =>
        basicWrap("No presentation available for type '%s'".format(t), t)
    }
  }

  protected def ensuring[T, U](f: T => U)(implicit that: models.submission.Presentable) = {
    try f(that.asInstanceOf[T])
    catch {
      case cc: ClassCastException => throw new AssertionError("Unexpected type: " + that.getClass.getName, cc)
    }
  }

  // Creates a fake link that runs custom JavaScript on-click
  protected def basicWrap(str: String, typ: String, arg: String ="") =
    "<a href='javascript:void(0)' onclick='do_custom_%s('%s')'>%s</a>".format(typ, arg, str)

}
