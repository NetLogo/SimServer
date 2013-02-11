package models.filemanager

import
  scala.concurrent.duration._

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 8/22/12
 * Time: 1:01 PM
 */

object PermFileManager extends FileManager {
            override val MyFolderName = "perm"
  protected override val LifeSpan     = 365 days // Permanent enough for me...
  protected override val SystemName   = "PermanentFiles"
}
