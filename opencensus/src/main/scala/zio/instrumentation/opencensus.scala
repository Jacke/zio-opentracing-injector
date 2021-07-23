package zio.instrumentation

import zio._
import io.opencensus.trace._
import scala.collection.JavaConverters._

object opencensus {
  type F[A] = ZIO[Has[zio.instrumentation.Tracing[Span]], Nothing, A]

  private def ffiA[A](f: Span => A): F[A] =
    ZIO.accessM[Has[zio.instrumentation.Tracing[Span]]](_.get.tracing.get).flatMap(span => UIO(f(span)))

  private def ffi(f: Span => Any): F[Unit] =
    ffiA(f).unit

  private def attributesFromScala(
    attributes: Seq[(String, ZAttributeValue)]
  ): java.util.Map[String, io.opencensus.trace.AttributeValue] =
    attributes.toMap.mapValues(_.underlying).toMap.asJava

  def annotate(description: String, attributes: (String, ZAttributeValue)*): F[Unit] =
    ffi(_.addAnnotation(Annotation.fromDescriptionAndAttributes(description, attributesFromScala(attributes))))
}

final case class ZAttributeValue(underlying: AttributeValue) extends AnyVal
object ZAttributeValue {
  import AttributeValue._

  implicit def attributeValueFromString(value: String): ZAttributeValue =
    ZAttributeValue(stringAttributeValue(value))

  implicit def attributeValueFromBoolean(value: Boolean): ZAttributeValue =
    ZAttributeValue(booleanAttributeValue(value))

  implicit def attributeValueFromDouble(value: Double): ZAttributeValue =
    ZAttributeValue(doubleAttributeValue(value))

  implicit def attributeValueFromLong(value: Long): ZAttributeValue =
    ZAttributeValue(longAttributeValue(value))
}

class OpenCensusBackend(tracer: Tracer) extends Backend[Span] {
  def root(operationName: String): UIO[Span] =
    child(operationName, null)

  def child(operationName: String, parent: Span): UIO[Span] =
    UIO(tracer.spanBuilderWithExplicitParent(operationName, parent).startSpan())

  def close(span: Span): UIO[Unit] = UIO(span.end())
}
