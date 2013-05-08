package models.util

import
  java.io.{ File, FilenameFilter, FileOutputStream, PrintWriter }

import
  org.apache.commons.codec.binary.Base64

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 1:24 PM
 */

object FileUtil {

  private val ImageExtensions = List("jpeg", "jpg", "png", "bmp", "gif")

  val NetLogoFileExt  = "nlogo"
  val ModelFileFilter = extFilter(NetLogoFileExt)

  def extFilter(ext: String) = new FilenameFilter() { def accept(file: File, name: String) = name.toLowerCase.endsWith("." + ext) }

  def dropExt(filename: String) : String = {
    val index = filename.lastIndexOf(".")
    if (index >= 0) filename.substring(0, index) else filename
  }

  def printTextToFile(f: File)(op: PrintWriter => Unit) {
    using(new PrintWriter(f))(op)
  }

  def printBytesToFile(filename: String)(data: Array[Byte]) {
    val bytes = {
      if (ImageExtensions.exists(filename.endsWith(_)))
        decodeBase64(data)
      else
        data
    }
    using (new FileOutputStream(filename))(_.write(bytes))
  }

  def using[A <: { def close() }, B](param: A)(f: A => B) : B = {
    try { f(param) } finally { param.close() }
  }

  private def decodeBase64(bytes: Array[Byte]) = Base64.decodeBase64(bytes)

}
