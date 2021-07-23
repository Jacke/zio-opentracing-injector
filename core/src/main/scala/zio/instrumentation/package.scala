package zio

import zio._

package object instrumentation {
  implicit class InstrumentedZioSyntax[-R, +E, +A](val self: ZIO[R, E, A]) extends AnyVal {
    def instrumented[Span: Tag](operationName: String): ZIO[R with Has[Tracing[Span]], E, A] =
      ZIO.access[Has[Tracing[Span]]](_.get.tracing).flatMap(_.useChild(operationName, self))
  }
}
