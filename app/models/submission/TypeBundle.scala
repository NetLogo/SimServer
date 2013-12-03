package models.submission

import
  scala.collection.mutable.{ Map => MMap }

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 10/29/12
 * Time: 2:56 PM
 */

case class TypeBundle(name: String, actionJS: String, presentationJS: String, fileExtension: String)

object TypeBundleCache {

  private val cache = MMap[String, TypeBundle]()

  def byName(name: String): TypeBundle =
    cache.getOrElseUpdate(name, SubmissionDBManager getTypeBundleByName name getOrElse TypeBundle(name, "", "", ""))

}
