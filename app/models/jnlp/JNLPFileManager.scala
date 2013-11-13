package models.jnlp

import
  scala.concurrent.duration._

import
  models.filemanager.FileManager

/**
 * Created with IntelliJ IDEA.
 * User: jason
 * Date: 2/22/13
 * Time: 3:59 PM
 */

object JNLPFileManager extends FileManager {
            override lazy val MyFolderName = "gen"
  protected override lazy val LifeSpan     = 1000 days
  protected override lazy val SystemName   = "JNLPFiles"
}
