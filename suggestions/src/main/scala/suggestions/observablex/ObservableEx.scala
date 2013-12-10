package suggestions
package observablex

import scala.concurrent.{Future, ExecutionContext}
import scala.util._
import scala.util.Success
import scala.util.Failure
import java.lang.Throwable
import rx.lang.scala.Observable
import rx.lang.scala.Scheduler
import rx.lang.scala.subjects.{AsyncSubject, ReplaySubject}

object ObservableEx {

  /** Returns an observable stream of values produced by the given future.
    * If the future fails, the observable will fail as well.
    *
    * @param f future whose values end up in the resulting observable
    * @return an observable completed after producing the value of the future, or with an exception
    */
  def apply[T](f: Future[T])(implicit execContext: ExecutionContext): Observable[T] = {
    val channel = ReplaySubject[T]()
    f.onComplete {
      case Success(v) => {
        channel.onNext(v); channel.onCompleted()
      }
      case Failure(e) => channel.onError(e)
    }
    channel
  }
}