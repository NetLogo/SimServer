package models.util

import java.io.File
import FileUtil.{ dropExt, ModelFileFilter }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 5:43 PM
 */

object ModelMapper {

  private val fsPathToHubNet     = "./public/misc/models/hubnet/"
  private val aliasToFilenameMap = Map[String, String]() //@ This would benefit from being a `BiHashMap`...
  private val filenameToAliasMap = aliasToFilenameMap map { case (k, v) => (v, k) }

  //@ I'm debating whether or not this should be recreated each time it's queried for... --JAB (8/29/12)
  private def nameToFileMap = (new File(fsPathToHubNet)).listFiles(ModelFileFilter) map (file => (dropExt(file.getName), file)) toMap

  def apply(fn: String)           : File        = get(fn) getOrElse (throw new NoSuchElementException("Could not find file: " + fn))
  def contains(modelName: String) : Boolean     = !get(modelName).isEmpty
  def modelNames                  : Seq[String] = (nameToFileMap map { case (k, _) => filenameToAliasMap.getOrElse(k, k) } toSeq).sortBy(_.toLowerCase)

  def get(fn: String) : Option[File] = {
    def getFileOptByAlias(alias: String) = aliasToFilenameMap get (alias) flatMap (nameToFileMap get (_))
    nameToFileMap get (fn) orElse (getFileOptByAlias(fn))
  }

  def unalias(alias: String) : String =
    nameToFileMap get (alias) map (_ => alias) getOrElse (aliasToFilenameMap.getOrElse(alias, alias))

}
