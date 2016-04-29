package ro.asalajan.reactivefile

import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.charset.Charset
import java.nio.file.{Path, Paths, StandardOpenOption}
import java.nio.{ByteBuffer, CharBuffer}
import java.util.concurrent.atomic.AtomicInteger


import rx.lang.scala.{Observable, Subscription}

object Main {

  def main(args: Array[String]): Unit = {
    val fileURL = Thread.currentThread().getContextClassLoader.getResource("myFile.txt")
    val filePath: Path = Paths.get(fileURL.toURI)

    ReactiveFile.from(filePath)
      .subscribe(
      ch => print(ch),
      t => print(t)
    )

    Thread.sleep(3000);

  }


}
