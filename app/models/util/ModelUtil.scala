package models.util

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 5:43 PM
 */

//@ Simple and stupid.  ...Or simply stupid?
object ModelUtil {

  private val RootURLFormat = "http://129.105.107.206:9000/assets/misc/models/%s.nlogo".format((_: String)) //@ THIS SUUUUUUUCKS

  private val nameToFileNameMap = Map("Critters" -> "Critter%20Designers")

  def getURLFromName(modelName: String) : String = {
    RootURLFormat(nameToFileNameMap(modelName))
  }

}
