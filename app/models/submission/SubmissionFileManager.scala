package models.submission

import
  scala.concurrent.duration._

import
  models.filemanager.FileManager

/**
 * Created with IntelliJ IDEA.
 * User: Jason
 * Date: 11/1/12
 * Time: 1:52 PM
 */

object SubmissionFileManager extends FileManager {

  override           def MyFolderName = "uploads"
  override protected def LifeSpanOpt  = None
  override protected def SystemName   = "SubmissionFiles"

  def formatFilePath(fileNameBasis: String, bundle: TypeBundle): String =
    s"$MyFolderName/${bundle.name}/$fileNameBasis.${bundle.fileExtension}"

  def registerFile(contents: Array[Byte], fileNameBasis: String, bundle: TypeBundle): String = {
    val filename = formatFilePath(fileNameBasis, bundle)
    saveFile(contents, filename) dropWhile (_ != '/') drop 1 // Toss out the "assets/"
  }

}
