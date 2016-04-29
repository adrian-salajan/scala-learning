package ro.asalajan.reactivefile

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.charset.Charset
import java.nio.file.{Path, StandardOpenOption}
import java.util.concurrent.atomic.AtomicInteger

import rx.lang.scala.{Observable, Observer, Subscription}

object ReactiveFile {
  def from(filePath: Path): Observable[Char] = {

    Observable.create(observer => {
      try {
        readAsync(filePath, observer)
      } catch {
        case e: Exception => observer.onError(e)
      }
      Subscription()
    })
  }

  def readAsync(filePath: Path, observer: Observer[Char]): Unit = {
    val fileChannel: AsynchronousFileChannel = AsynchronousFileChannel.open(filePath, StandardOpenOption.READ)
    val byteBuffer = ByteBuffer.allocate(8)
    val decoder = Charset.forName("UTF-8").newDecoder();

    fileChannel.read(byteBuffer, 0, new AtomicInteger(0), new CompletionHandler[Integer, AtomicInteger] {
      override def completed(result: Integer, position: AtomicInteger): Unit = {

        byteBuffer.flip();
        //prepare for reading
        val chars: CharBuffer = decoder.decode(byteBuffer)
        while (chars.hasRemaining) {
          observer.onNext(chars.get())
        }
        byteBuffer.clear()

        if (result.intValue() == byteBuffer.capacity()) {
          //continue reading
          val newPosition = position.addAndGet(result)
          fileChannel.read(byteBuffer, newPosition.toLong, position, this)
        } else {
          //complete the stream
          observer.onCompleted()
        }
      }

      override def failed(throwable: Throwable, position: AtomicInteger): Unit = {
        throw throwable
      }
    })
  }
}
