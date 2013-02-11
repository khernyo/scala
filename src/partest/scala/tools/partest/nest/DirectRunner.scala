/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */



package scala.tools.partest
package nest

import java.io.File
import scala.tools.nsc.util.{ ScalaClassLoader, Exceptional }
import java.util.concurrent._

case class TestRunParams(val scalaCheckParentClassLoader: ScalaClassLoader)

trait DirectRunner {
  def fileManager: FileManager

  import PartestDefaults.numThreads

  Thread.setDefaultUncaughtExceptionHandler(
    new Thread.UncaughtExceptionHandler {
      def uncaughtException(thread: Thread, t: Throwable) {
        val t1 = Exceptional unwrap t
        System.err.println(s"Uncaught exception on thread $thread: $t1")
        t1.printStackTrace()
      }
    }
  )
  def runTestsForFiles(kindFiles: List[File], kind: String): List[TestState] = {
    System.setProperty("line.separator", "\n")

    // @partest maintainer: we cannot create a fresh file manager here
    // since the FM must respect --buildpath and --classpath from the command line
    // for example, see how it's done in ReflectiveRunner
    //val consFM = new ConsoleFileManager
    //import consFM.{ latestCompFile, latestLibFile, latestPartestFile }
    val latestCompFile    = new File(fileManager.LATEST_COMP)
    val latestReflectFile = new File(fileManager.LATEST_REFLECT)
    val latestLibFile     = new File(fileManager.LATEST_LIB)
    val latestPartestFile = new File(fileManager.LATEST_PARTEST)
    val latestActorsFile  = new File(fileManager.LATEST_ACTORS)
    val scalacheckURL     = PathSettings.scalaCheck.toURL
    val scalaCheckParentClassLoader = ScalaClassLoader.fromURLs(
      scalacheckURL :: (List(latestCompFile, latestReflectFile, latestLibFile, latestActorsFile, latestPartestFile).map(_.toURI.toURL))
    )

    NestUI.resetTestNumber()

    val pool      = Executors.newFixedThreadPool(numThreads)
    val manager   = new RunnerManager(kind, fileManager, TestRunParams(scalaCheckParentClassLoader))
    val futures   = kindFiles map (f => pool submit callable(manager runTest f))

    pool.shutdown()
    try if (!pool.awaitTermination(4, TimeUnit.HOURS))
      NestUI.warning("Thread pool timeout elapsed before all tests were complete!")
    catch { case t: InterruptedException =>
      NestUI.warning("Thread pool was interrupted")
      t.printStackTrace()
    }

    futures map (_.get)
  }
}
