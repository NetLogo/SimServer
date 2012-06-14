package models.util

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 5:43 PM
 */

object ModelMap {
  private val nameToFilenameMap = Map("Critters" -> "Critter%20Designers")
  def apply   (modelName: String) : String      = nameToFilenameMap(modelName)
  def contains(modelName: String) : Boolean     = nameToFilenameMap.contains(modelName)
  def listModels                  : Seq[String] = nameToFilenameMap.keys.toSeq
}
