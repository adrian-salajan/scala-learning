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

    toObservable(filePath)
      .subscribe(
      ch => print(ch),
      t => print(t)
    )


    Thread.sleep(3000);

  }

  def toObservable(filePath: Path): Observable[Char] = {

    Observable.create(observer => {
      val fileChannel: AsynchronousFileChannel = AsynchronousFileChannel.open(filePath, StandardOpenOption.READ)


      val byteBuffer = ByteBuffer.allocate(8)


      fileChannel.read(byteBuffer, 0, new AtomicInteger(0), new CompletionHandler[Integer, AtomicInteger] {
        override def completed(result: Integer, position: AtomicInteger): Unit = {
          val decoder = Charset.forName("UTF-8").newDecoder();


          byteBuffer.flip();
          val chars: CharBuffer = decoder.decode(byteBuffer)
          while (chars.hasRemaining) {
            observer.onNext(chars.get())
          }
          byteBuffer.clear()

          if (result.intValue() == byteBuffer.capacity()) {
            //continue reading
            val newPosition = position.addAndGet(result)
            fileChannel.read(byteBuffer, newPosition.toLong, position, this)
          }
        }

        override def failed(exc: Throwable, position: AtomicInteger): Unit = {

        }
      })
      Subscription()
    })


  }
}
