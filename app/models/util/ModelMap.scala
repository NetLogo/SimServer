package models.util

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 5:43 PM
 */

object ModelMap {
  private val nameToFileNameMap = Map("Critters" -> "Critter%20Designers")
  def apply(modelName: String) : String = nameToFileNameMap(modelName)
}
