import zio._
import zio.instrumentation._
import java.io.IOException

object OpencensusSimpleExample extends App {
  import io.opencensus.trace.{ BlankSpan, Span }

  import io.opencensus.exporter.trace.jaeger._
  import io.opencensus.trace.samplers.Samplers

  JaegerTraceExporter.createAndRegister(
    JaegerExporterConfiguration
      .builder()
      .setThriftEndpoint("http://127.0.0.1:14268/api/traces")
      .setServiceName("zio-opencensus-demo")
      .build()
  )

  val traceConfig = io.opencensus.trace.Tracing.getTraceConfig()

  traceConfig.updateActiveTraceParams(
    traceConfig
      .getActiveTraceParams()
      .toBuilder
      .setSampler(Samplers.alwaysSample())
      .build()
  )

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    (for {
      tracer     <- UIO(io.opencensus.trace.Tracing.getTracer())
      propagator <- Propagator.make[Span](new OpenCensusBackend(tracer), BlankSpan.INSTANCE)
      r          = Has(new Tracing(propagator)) ++ Has(zio.console.Console.Service.live)
      _          <- myAppLogic.provide(r)
      _          <- UIO(io.opencensus.trace.Tracing.getExportComponent.shutdown())
    } yield ()).exitCode

  def myAppLogic(implicit T: Tag[Span]): ZIO[Has[Tracing[Span]] with zio.console.Console, IOException, Unit] =
    for {
      _      <- doSomething(1 + 9000000).instrumented("shithole")
      fiber1 <- doSomething(2).instrumented("parent2").fork
      fiber2 <- doSomething(3).instrumented("parent3").fork
      _      <- doSomething(4).instrumented("parent4")
      _      <- fiber1.join
      _      <- fiber2.join
      _      <- doSomething(5).instrumented("parent5")
    } yield ()

  def doSomething(n: Int)(implicit T: Tag[Span]): ZIO[Has[Tracing[Span]] with zio.console.Console, IOException, Unit] =
    for {
      _ <- console.putStrLn(s"Child 1 of Parent $n").instrumented(s"child1-of-parent$n")
      _ <- opencensus.annotate("HELLO", "n" -> n, "isFoo" -> true)
      _ <- console.putStrLn(s"Child 2 of Parent $n").instrumented(s"child2-of-parent$n")
    } yield ()
}
