/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */



package scala.tools.partest
package nest

import java.io.{ FilenameFilter, IOException,
                FileInputStream, FileOutputStream, BufferedReader,
                FileReader, PrintWriter, FileWriter}
import java.net.URI
import scala.collection.mutable

trait FileManager {
  def testRootDir: Directory
  def testRootPath: String

  var JAVACMD: String
  var JAVAC_CMD: String

  var CLASSPATH: String
  var LATEST_LIB: String
  var LATEST_REFLECT: String
  var LATEST_COMP: String
  var LATEST_PARTEST: String
  var LATEST_ACTORS: String

  var updateCheck = false
  var failed = false

  var SCALAC_OPTS: Seq[String] = words(PartestDefaults.scalacOpts)
  var JAVA_OPTS                 = PartestDefaults.javaOpts

  def universalJavaOpts   = words(JAVA_OPTS)
  def universalScalacOpts = SCALAC_OPTS
  def universalClasspath  = CLASSPATH

  def updatePluginPath(options: List[String]): List[String] = {
    def absolutize(path: String) = Path(path) match {
      case x if x.isAbsolute  => x.path
      case x                  => (testRootDir / x).toAbsolute.path
    }
    options partition (_ startsWith "-Xplugin:") match {
      case (Nil, _)     => options
      case (opt1, opt2) =>
        val plugins      = opt1 map (_ stripPrefix "-Xplugin:") flatMap (_ split pathSeparator) map absolutize
        val pluginOption = "-Xplugin:" + (plugins mkString pathSeparator)

        opt2 :+ pluginOption
    }
  }

  /** Only when --debug is given. */
  lazy val testTimings = new mutable.HashMap[String, Long]
  def recordTestTiming(name: String, milliseconds: Long) =
    synchronized { testTimings(name) = milliseconds }

  def getLogFile(dir: File, fileBase: String, kind: String): File =
    new File(dir, fileBase + "-" + kind + ".log")

  def getLogFile(file: File, kind: String): File = {
    val dir      = file.getParentFile
    val fileBase = basename(file.getName)

    getLogFile(dir, fileBase, kind)
  }

  def logFileExists(file: File, kind: String) =
    getLogFile(file, kind).canRead

  def overwriteFileWith(dest: File, file: File) =
    dest.isFile && copyFile(file, dest)

  def copyFile(from: File, dest: File): Boolean = {
    if (from.isDirectory) {
      assert(dest.isDirectory, "cannot copy directory to file")
      val subDir:Directory = Path(dest) / Directory(from.getName)
      subDir.createDirectory()
      from.listFiles.toList forall (copyFile(_, subDir))
    }
    else {
      val to = if (dest.isDirectory) new File(dest, from.getName) else dest
      try { to writeAll from.fileContents ; true }
      catch { case _: IOException => false }
    }
  }
}
