package models.util

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 5:43 PM
 */

//@ Simple and stupid.  ...Or simply stupid?
object ModelUtil {

  //@ This suuuuuucks!
  private val RootURLFormat = "http://" + "DERP" /* We need to acquire IP and port somehow... */ + "/assets/misc/models/%s.nlogo".format((_: String))

  private val nameToFileNameMap = Map("Critters" -> "Critter%20Designers")

  def getURLFromName(modelName: String) : String = {
    RootURLFormat(nameToFileNameMap(modelName))
  }

}
