package models.filemanager

import akka.util.duration._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 1:00 PM
 */

object TempFileManager extends FileManager {
  override val MyFolderName = "gen"
  protected override val LifeSpan = 1 minute
  protected override val SystemName = "TempGen"
}
