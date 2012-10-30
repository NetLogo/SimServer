package views

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/26/12
 * Time: 3:06 PM
 */

object ItemBasedHtmlGenerator {

  // Return HTML from this... OR DIE!  --JAB
  def apply(implicit p: models.submission.Presentable) : String = {
    val AssetsPath = "/assets/" //@ FIX!!!!
    p.typ match {
      case t @ "export_world" =>
        val urlOpt = ensuring[models.submission.UserWork, Option[String]] {
          _.supplements find (s => s.typ == "export_interface" || s.typ == "export_view") map
                             (s => AssetsPath + "uploads/%s/%s".format(s.typ, s.data))
        }
        val out = """<img src="%s" class="work_image" />""".format(urlOpt getOrElse (AssetsPath + "images/not_found.png"))
        basicWrap(out, t)
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
  protected def basicWrap(str: String, typ: String) = "<a href='javascript:void(0)' onclick='do_custom_%s()'>%s</a>".format(typ, str)

}
