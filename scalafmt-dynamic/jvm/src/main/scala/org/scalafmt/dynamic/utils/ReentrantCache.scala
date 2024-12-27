package org.scalafmt.dynamic.utils

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration._
import scala.util.Try

class ReentrantCache[K, V] {
  private[this] var cache: Map[K, Future[V]] = Map.empty

  @tailrec
  final def getOrAddToCache(key: K, shouldEvict: V => Boolean = _ => false)(
      get: () => V,
  ): V = synchronized( // try to exit quickly from synchronized block
    cache.get(key) match {
      case Some(fut) => Right(fut)
      case None =>
        val p = Promise[V]()
        cache += key -> p.future
        Left(p)
    },
  ) match {
    case Right(fut) =>
      // we set the timeout to 10 minutes because
      // we can't expect everybody to have the same internet connection speed.
      //
      // get or wait for other thread to finish download
      val result = Await.result(fut, 10.minute)

      if (shouldEvict(result)) {
        synchronized(cache -= key)
        getOrAddToCache(key, shouldEvict)(get)
      } else result
    case Left(p) =>
      val result = Try(get())
      p.complete(result)
      result.get
  }

  def clear(): Iterable[Future[V]] = synchronized {
    val oldValues = cache.values
    cache = Map.empty
    oldValues
  }

  def getFromCache(key: K): Option[V] = cache.get(key)
    .map(Await.result(_, 10.minute))
}
object ReentrantCache {
  def apply[K, V](): ReentrantCache[K, V] = new ReentrantCache[K, V]
}
