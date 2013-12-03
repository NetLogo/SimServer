package models.submission

import
  java.{ io, nio, util },
    io.{ ByteArrayInputStream, ByteArrayOutputStream, FileInputStream, InputStream },
    nio.charset.Charset,
    util.zip.{ ZipEntry, ZipOutputStream }


import
  org.joda.time.{ DateTime, format },
    format.DateTimeFormat

object WorkZipper {

  private val CharSet = Charset.forName("ISO-8859-1")

  def apply(works: UserWork*): Array[Byte] = {

    val runBundles = works groupBy (_.runID) mapValues (_ groupBy (_.periodID) mapValues (_ groupBy (_.userID)))
    val baos       = new ByteArrayOutputStream
    val out        = new ZipOutputStream(baos, CharSet)

    runBundles foreach {
      case (runID, periodBundles) =>
        out.putNextEntry(new ZipEntry(s"$runID/"))
        periodBundles foreach {
          case (periodID, userBundles) =>
            out.putNextEntry(new ZipEntry(s"$runID/$periodID/"))
            userBundles foreach {
              case (userID, workBundles) =>
                val prefix = s"$runID/$periodID/$userID/"
                out.putNextEntry(new ZipEntry(prefix))
                workBundles foreach zipWork(prefix, out)
            }
        }
    }

    out.close()

    baos.toByteArray

  }

  private def zipWork(prefix: String, out: ZipOutputStream)(work: UserWork): ZipOutputStream = {
    val newPrefix = s"$prefix${work.id.get}/"
    out.putNextEntry(new ZipEntry(newPrefix))
    implicit val p = Prefix(newPrefix)
    (writeData(work) _ andThen writeMetadata(work) andThen writeComments(work.comments) andThen writeSupplements(work.supplements))(out)
  }

  private def writeData(work: UserWork)(out: ZipOutputStream)(implicit prefix: Prefix): ZipOutputStream = {
    val fileExt = TypeBundleCache.byName(work.typ).fileExtension
    val fis     = new FileInputStream(s"./public/${work.data}")
    writeEntry(fis, s"work.$fileExt")(out)
  }

  private def writeMetadata(work: UserWork)(out: ZipOutputStream)(implicit prefix: Prefix): ZipOutputStream = {

    val unJson: PartialFunction[String, String] = {
      case s if (s.startsWith("\"") && s.endsWith("\"")) => s drop 1 dropRight 1
      case s => s
    }

    val predefKeys = Seq("id",                 "type",   "time",                      "description")
    val predefVals = Seq(work.id.get.toString, work.typ, work.timestamp.toTimeString, work.description)

    val metaPairs = new JsonMetadata(work.metadata).toMap

    val pairs       = ((predefKeys zip predefVals) ++ metaPairs).toMap mapValues unJson
    val metadataStr = pairs map { case (key, value) => s"$key: $value" } mkString "\n"

    writeEntry(metadataStr, "metadata.txt")(out)

  }

  private def writeComments(comments: Seq[UserWorkComment])(out: ZipOutputStream)(implicit prefix: Prefix): ZipOutputStream = {

    val commentsStr =
      comments map {
        case UserWorkComment(_, _, timestamp, userID, comment) => s"[${timestamp.toTimeString}] $userID: $comment"
      } mkString (
        "\n"
      )

    writeEntry(commentsStr, "comments.txt")(out)

  }

  private def writeSupplements(supplements: Seq[UserWorkSupplement])(out: ZipOutputStream)(implicit prefix: Prefix): ZipOutputStream = {
    supplements foreach {
      supp =>
        val fileExt = TypeBundleCache.byName(supp.typ).fileExtension
        val fis     = new FileInputStream(s"./public/${supp.data}")
        writeEntry(fis, s"supplement-${supp.typ}-${supp.id.get}.$fileExt")(out)
    }
    out
  }

  private def writeEntry[T <% PimpyStream](convertible: T, name: String)(out: ZipOutputStream)(implicit prefix: Prefix): ZipOutputStream = {

    out.putNextEntry(new ZipEntry(s"${prefix.value}$name"))

    val b     = new Array[Byte](1024)
    val in    = convertible.toInputStream
    var count = in.read(b)

    while (count > 0) {
      out.write(b, 0, count)
      count = in.read(b)
    }

    in.close()
    out.closeEntry()

    out

  }

  private implicit class LongAndPimpy(timestamp: Long) {
    def toTimeString = DateTimeFormat.forPattern("MM/dd/yy @ hh:mma").print(new DateTime(timestamp))
  }

  private trait PimpyStream {
    def toInputStream: InputStream
  }

  private implicit class PimpyIS(is: InputStream) extends PimpyStream {
    override def toInputStream = is
  }

  private implicit class PimpyString(str: String) extends PimpyStream {
    override def toInputStream = new ByteArrayInputStream(str.getBytes(CharSet))
  }

  private case class Prefix(value: String) extends AnyVal

}
