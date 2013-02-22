package models.filemanager

import
  scala.concurrent.duration._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 1:00 PM
 */

object TempFileManager extends FileManager {

            override val MyFolderName = "temp"
  protected override val LifeSpan     = 1 hour
  protected override val SystemName   = "TempGen"

  override def formatFilePath(fileNameBasis: String, fileExt: String) : String =
    s"$MyFolderName/${fileNameBasis.##.abs}.$fileExt"

}
