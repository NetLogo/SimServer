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
            override lazy val MyFolderName = "perm"
  protected override lazy val LifeSpanOpt  = None
  protected override lazy val SystemName   = "PermanentFiles"
}
