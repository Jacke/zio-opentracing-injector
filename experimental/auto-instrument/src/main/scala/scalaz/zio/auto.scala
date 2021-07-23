package zio // to access private ZIO tags

import _root_.zio.instrumentation._
import io.opentracing.Span

object experimental {
  object Experimental {
    def apply[R, E, A](self: ZIO[R, E, A]): Experimental[R, E, A] =
      new Experimental(self)
  }
  class Experimental[-R, +E, +A](val self: ZIO[R, E, A]) extends AnyVal {
    def auto(): ZIO[R with Has[Tracing[Span]], E, A] = auto(self)

    def auto[R, E, A, X1](self: ZIO[R, E, A]): ZIO[R with Has[Tracing[Span]], E, A] = {
      implicit val spanTag = izumi.reflect.Tag[Span]
      self.tag match {
        case ZIO.Tags.FlatMap =>
          val flatMap = self.asInstanceOf[ZIO.FlatMap[R, E, X1, A]]
          for {
            x      <- auto(flatMap.zio)
            ztrace <- ZIO.trace
            a      <- flatMap.k(x).instrumented("FlatMap " + ztrace.stackTrace.drop(1).head.prettyPrint)
          } yield a
        case ZIO.Tags.Access =>
          val read = self.asInstanceOf[ZIO.Read[R, E, A]]
          new ZIO.Read((r: R) => auto(read.k(r))).instrumented("Read")
        case ZIO.Tags.Succeed =>
          self.instrumented("Succeed")
        case ZIO.Tags.EffectTotal =>
          for {
            ztrace <- ZIO.trace
            a      <- self.instrumented("EffectTotal" + ztrace.parentTrace.get.stackTrace.head.prettyPrint)
          } yield a
        case ZIO.Tags.Fail            => ???
        case ZIO.Tags.Fold            => self
        case ZIO.Tags.InterruptStatus => ???
        case ZIO.Tags.CheckInterrupt  => ???
        case ZIO.Tags.EffectPartial   => ???
        case ZIO.Tags.EffectAsync     => ???
        case ZIO.Tags.Fork            => ???
        case ZIO.Tags.Supervise       => ???
        case ZIO.Tags.Descriptor      => ???
        //  case ZIO.Tags.Lock            => ???
        //    case ZIO.Tags.SuspendWith    => ???
        case ZIO.Tags.Yield          => ???
        case ZIO.Tags.Provide        => ???
        case ZIO.Tags.FiberRefNew    => ???
        case ZIO.Tags.FiberRefModify => ???
        case ZIO.Tags.Trace          => ???
        case ZIO.Tags.TracingStatus  => ???
        case ZIO.Tags.CheckTracing   => ???
        case _                       => self
      }
    }
  }

}
