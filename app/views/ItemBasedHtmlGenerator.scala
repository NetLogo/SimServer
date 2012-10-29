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
    p.typ match {
      case t =>
        basicWrap("No presentation available for type '%s'".format(t), t)
    }
  }

  protected def ensuring[T](f: T => String)(implicit that: models.submission.Presentable) = {
    try f(that.asInstanceOf[T])
    catch {
      case cc: ClassCastException => throw new AssertionError("Unexpected type: " + that.getClass.getName, cc)
    }
  }

  // Creates a fake link that runs custom JavaScript on-click
  protected def basicWrap(str: String, typ: String) = "<a href='javascript:void(0)' onclick='do_custom_%s()'>%s</a>".format(typ, str)

}
