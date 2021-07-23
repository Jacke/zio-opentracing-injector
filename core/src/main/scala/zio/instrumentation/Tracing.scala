package zio.instrumentation

import zio._

class Tracing[Span](val tracing: Propagator[Span]) extends Serializable

class Propagator[Span](backend: Backend[Span], ref: FiberRef[Span]) {
  def get: UIO[Span] = ref.get
  def useChild[R, E, A](operationName: String, use: ZIO[R, E, A]): ZIO[R, E, A] =
    ZIO.bracket(get.flatMap(backend.child(operationName, _)))(backend.close)(span => ref.locally(span)(use))
}
object Propagator {
  // Called by HTTP Frameworks for every request
  def make[Span](backend: Backend[Span], initial: Span): UIO[Propagator[Span]] =
    FiberRef.make[Span](initial).map(new Propagator(backend, _))
}

trait Backend[Span] {
  def root(operationName: String): UIO[Span]
  def child(operationName: String, parent: Span): UIO[Span]
  def close(span: Span): UIO[Unit]
}
