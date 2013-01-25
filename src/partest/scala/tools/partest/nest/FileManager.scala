/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io._
import java.net.URI
import scala.tools.nsc.io.{ Path, Directory, File => SFile }
import scala.sys.process._
import scala.collection.mutable
import scala.tools.nsc.io
import scala.Some
import java.util.Locale

trait FileUtil {
  /**
   * Compares two files using a Java implementation of the GNU diff
   * available at http://www.bmsi.com/java/#diff.
   *
   * @param  f1  the first file to be compared
   * @param  f2  the second file to be compared
   * @return the text difference between the compared files
   */
  def compareFiles(f1: File, f2: File): String = {
    val diffWriter = new StringWriter
    val args = Array(f1.getAbsolutePath(), f2.getAbsolutePath())

    DiffPrint.doDiff(args, diffWriter)
    val res = diffWriter.toString
    if (res startsWith "No") "" else res
  }
  def compareContents(lines1: Seq[String], lines2: Seq[String]): String = {
    val xs1 = lines1.toArray[AnyRef]
    val xs2 = lines2.toArray[AnyRef]

    val diff   = new Diff(xs1, xs2)
    val change = diff.diff_2(false)
    val writer = new StringWriter
    val p      = new DiffPrint.NormalPrint(xs1, xs2, writer)

    p.print_script(change)
    val res = writer.toString
    if (res startsWith "No ") ""
    else res
  }
}
object FileUtil extends FileUtil { }

object TestKind extends Enumeration {
  val Scalacheck   = Value("scalacheck")
  val Pos          = Value("pos")
  val Neg          = Value("neg")
  val Run          = Value("run")
  val Jvm          = Value("jvm")
  val Specialized  = Value("specialized")
  val Instrumented = Value("instrumented")
  val Presentation = Value("presentation")
  val Ant          = Value("ant")
  val Buildmanager = Value("buildmanager")
  val Res          = Value("res")
  val Shootout     = Value("shootout")
  val Scalap       = Value("scalap")
  val Script       = Value("script")

  def expectsFailure(v: Value) = v match {
    case Neg => true
    case _   => false
  }
}

class TestContext(val testFile: File, val kind: TestKind.Value) {

  def this(testFile: File, kind: String) = this(testFile, TestKind.withName(kind))

  val parent  = testFile.getParentFile
  val dir: File = parent
  val fileBase: String = basename(testFile.getName)

  val logFile = new File(dir, s"$fileBase-$kind.log")
  private def createLogStream = SFile(logFile).printStream()
  private var createdLogFile = false
  private var logStream: Option[PrintStream] = None
  def log     = logStream.get  // FIXME Writing into this will never fail, not even after close(). Replace with something more bloody.
  val outDir  = new File(parent, s"$fileBase-$kind.obj")

  def expectFailure = TestKind.expectsFailure(kind)

  def createOutputDir(): File = {
    outDir.mkdirs()
    outDir
  }

  def flagsFile = {
    val logFileBasename = basename(logFile.getName)
    new File(parent, "%s.flags" format (logFileBasename.substring(0, logFileBasename.lastIndexOf("-"))))
  }

  def logFileExists: Boolean = logFile.canRead

  def createLogFile() {
    require(logStream.isEmpty)
    require(!createdLogFile)
    createdLogFile = true
    logStream = Some(createLogStream)
  }

  def closeLog() {
    require(logStream.isDefined)
    require(createdLogFile)
    if (log.checkError()) NestUI.warning("There were errors with the log handling.\n")

    SFile.closeQuietly(log)

    // normalize line endings
    // System.getProperty("line.separator") should be "\n" here
    // so reading a file and writing it back should convert all CRLFs to LFs
    SFile(logFile).printlnAll(SFile(logFile).lines.toList: _*)

    logStream = None
  }

  def isLogClosed = createdLogFile && logStream.isEmpty

  def checkFile: File = ???
  def checkFileExists: Boolean = ???

  def getCheckFilePath(kind: TestKind.Value): SFile = getCheckFilePath(kind.toString)
  def getCheckFilePath(suffix: String = ""): SFile = {
    def chkFile(s: String) = (Directory(dir) / s"${fileBase}$s.check").toFile

    if (chkFile("").isFile || suffix == "") chkFile("")
    else chkFile("-" + suffix)
  }

  private def getCheckFile(dir: File) = Some(getCheckFilePath(kind)) filter (_.canRead)

  /**
   * This will print the contents of the log file to the console after closing [[scala.tools.partest.nest.TestContext#log]]
   */
  def outputLogFile() {
    log.close()
    val lines = SFile(logFile).lines
    if (lines.nonEmpty) {
      NestUI.normal("Log file '" + logFile + "': \n")
      lines foreach (x => NestUI.normal(x + "\n"))
    }
  }
}

trait FileManager extends FileUtil {

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

  var showDiff = false
  var updateCheck = false
  var showLog = false
  var failed = false

  var SCALAC_OPTS = PartestDefaults.scalacOpts.split(' ').toSeq
  var JAVA_OPTS   = PartestDefaults.javaOpts
  var timeout     = PartestDefaults.timeout
  // how can 15 minutes not be enough? What are you doing, run/lisp.scala?
  // You complete in 11 seconds on my machine.
  var oneTestTimeout = 60 * 60 * 1000

  /** Only when --debug is given. */
  lazy val testTimings = new mutable.HashMap[String, Long]
  def recordTestTiming(name: String, milliseconds: Long) =
    synchronized { testTimings(name) = milliseconds }
  def showTestTimings() {
    testTimings.toList sortBy (-_._2) foreach { case (k, v) => println("%s: %s".format(k, v)) }
  }

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

      try {
        SFile(to) writeAll SFile(from).slurp()
        true
      }
      catch { case _: IOException => false }
    }
  }

  def mapFile(file: File, replace: String => String) {
    val f = SFile(file)

    f.printlnAll(f.lines.toList map replace: _*)
  }
}
