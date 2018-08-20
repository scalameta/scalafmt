package docs

import java.nio.file.Paths
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

final class Docusaurus(getProcess: () => Process, executor: ExecutorService) {
  private def process = getProcess()
  private def pid(): Int = {
    val cls = process.getClass.getName
    assert(cls.contains("UNIXProcess"), cls)
    val field = process.getClass.getDeclaredField("pid")
    field.setAccessible(true)
    field.get(process).asInstanceOf[Int]
  }
  def kill(): Unit = {
    Runtime.getRuntime.exec(s"kill -2 ${pid()}").destroy()
    executor.shutdown()
    process.destroy()
  }

}

object Docusaurus {

  def start(): Docusaurus = {
    val npm = Executors.newFixedThreadPool(1)
    var process: Process = null
    npm.submit(new Runnable {
      override def run(): Unit = {
        val cwd = Paths.get("website").toFile
        process = new java.lang.ProcessBuilder("npm", "start")
          .inheritIO()
          .directory(cwd)
          .start()
      }
    })
    new Docusaurus(() => process, npm)
  }
}
