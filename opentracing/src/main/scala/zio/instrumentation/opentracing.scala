package zio.instrumentation

import zio._
import io.opentracing._

object opentracing {
  type F[A] = ZIO[Has[zio.instrumentation.Tracing[Span]], Nothing, A]

  private def ffiA[A](f: Span => A): F[A] =
    ZIO.accessM[Has[zio.instrumentation.Tracing[Span]]](_.get.tracing.get).flatMap(span => UIO(f(span)))

  private def ffi(f: Span => Any): F[Unit] =
    ffiA(f).unit

  def log(fields: (String, TracingValue)*): F[Unit] = {
    val jFields = new java.util.HashMap[String, Any]()
    fields.foreach {
      case (key, TracingValue.StringTracingValue(value))  => jFields.put(key, value)
      case (key, TracingValue.NumberTracingValue(value))  => jFields.put(key, value)
      case (key, TracingValue.BooleanTracingValue(value)) => jFields.put(key, value)
    }
    ffi(_.log(jFields))
  }

  def tag(key: String, value: TracingValue): F[Unit] = value match {
    case TracingValue.StringTracingValue(value)  => ffi(_.setTag(key, value))
    case TracingValue.NumberTracingValue(value)  => ffi(_.setTag(key, value))
    case TracingValue.BooleanTracingValue(value) => ffi(_.setTag(key, value))
  }

  object baggage {
    def set(key: String, value: String): F[Unit] = ffi(_.setBaggageItem(key, value))
    def get(key: String): F[Option[String]]      = ffiA(span => Option(span.getBaggageItem(key)))
  }
}

/**
 * Copied from Puretracing
 * https://github.com/tabdulradi/puretracing
 */
sealed trait TracingValue
object TracingValue {
  final case class StringTracingValue(value: String)   extends TracingValue
  final case class NumberTracingValue(value: Number)   extends TracingValue
  final case class BooleanTracingValue(value: Boolean) extends TracingValue

  implicit def tracingValueFromString(value: String): TracingValue         = StringTracingValue(value)
  implicit def tracingValueFromBoolean(value: Boolean): TracingValue       = BooleanTracingValue(value)
  implicit def tracingValueFromInt(value: Int): TracingValue               = NumberTracingValue(value)
  implicit def tracingValueFromDouble(value: Double): TracingValue         = NumberTracingValue(value)
  implicit def tracingValueFromBigDecimal(value: BigDecimal): TracingValue = NumberTracingValue(value)
}

class OpenTracingBackend(tracer: Tracer) extends Backend[Span] {
  def root(operationName: String): UIO[Span] =
    UIO(tracer.buildSpan(operationName).start())

  def child(operationName: String, parent: Span): UIO[Span] =
    UIO(tracer.buildSpan(operationName).asChildOf(parent).start())

  def close(span: Span): UIO[Unit] = UIO(span.finish())
}
