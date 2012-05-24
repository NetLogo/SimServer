package models.util

import java.io.{FileWriter, File, PrintWriter}


/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/24/12
 * Time: 1:24 PM
 */

object FileUtil {

  def printToFile(filename: String)(data: String) {
    using (new FileWriter(filename))(fileWriter => fileWriter.write(data))
  }

  private def using[A <: { def close() }, B](param: A)(f: A => B) : B = {
    try { f(param) } finally { param.close() }
  }

}
