package models.util

import
  java.io.File

import
  org.bizzle.datastructure.mutable.BiHashMap

import
  FileUtil.{ dropExt, ModelFileFilter }

/**
 * Created by IntelliJ IDEA.
 * User: Jason
 * Date: 5/31/12
 * Time: 5:43 PM
 */

object ModelMapper {

  private val fsPathToHubNet         = "./public/models/hubnet/"
  private val filenameAliasBijection = BiHashMap(
    Alias("Dice Stalagmite")        -> "Dice Stalagmite HubNet",
    Alias("Memory")                 -> "Memory HubNet",
    Alias("Tragedy of the Commons") -> "Tragedy of the Commons HubNet"
  )

  // I'm hoping it will be fine to be able to reload and sort the file list each time it's queried for --JAB (8/29/12)
  // Thus far, it has been entirely fine to do so --JAB (4/7/13)
  private def nameToFileMap = (new File(fsPathToHubNet)).listFiles(ModelFileFilter) map (file => (dropExt(file.getName), file)) toMap

  def apply(fn: String)           : File    = get(fn) getOrElse (throw new NoSuchElementException("Could not find file: " + fn))
  def contains(modelName: String) : Boolean = !get(modelName).isEmpty

  def modelNames : Seq[String] =
    (nameToFileMap map { case (k, _) => filenameAliasBijection get k map (_.id) getOrElse k } toSeq).sortBy(_.toLowerCase)

  def get(fn: String) : Option[File] = {
    def getFileOptByAlias(alias: Alias) = filenameAliasBijection get alias flatMap (nameToFileMap get (_))
    nameToFileMap get (fn) orElse (getFileOptByAlias(Alias(fn)))
  }

  def unalias(alias: String) : String =
    nameToFileMap get (alias) map (_ => alias) getOrElse (filenameAliasBijection.getOrElse(Alias(alias), alias))

}

// We need to wrap either aliases or filenames in some other class so that `BiHashMap` can tell which one we want just by the types
private case class Alias(id: String)
