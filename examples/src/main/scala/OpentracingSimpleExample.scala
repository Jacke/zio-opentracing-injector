import zio._
import zio.instrumentation._
import java.io.IOException
import io.opentracing.Span

object OpentracingSimpleExample extends App {
  import io.jaegertracing.Configuration, Configuration.SamplerConfiguration, Configuration.ReporterConfiguration

  val tracerConf =
    new Configuration("zio-opentracing-demo")
      .withSampler(SamplerConfiguration.fromEnv().withType("const").withParam(1))
      .withReporter(ReporterConfiguration.fromEnv().withLogSpans(true))

  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    (for {
      tracer     <- UIO(tracerConf.getTracer)
      rootSpan   <- UIO(tracer.buildSpan("REQUEST-SHITHOLE").start())
      propagator <- Propagator.make[Span](new OpenTracingBackend(tracer), rootSpan)
      r          = Has(new Tracing(propagator)) ++ Has(zio.console.Console.Service.live)
      _          <- myAppLogic.provide(r)
      _          <- UIO(rootSpan.finish())
    } yield ()).exitCode

  def myAppLogic(implicit T: Tag[Span]): ZIO[Has[Tracing[Span]] with zio.console.Console, IOException, Unit] =
    for {
      _      <- doSomething(1).instrumented("shit_hole1")
      fiber1 <- doSomething(2000).instrumented("shit_hole2").fork
      fiber2 <- doSomething(3).instrumented("parent3").fork
      _      <- doSomething(4).instrumented("parent4")
      _      <- fiber1.join
      _      <- fiber2.join
      _      <- doSomething(5).instrumented("parent5")
    } yield ()

  def doSomething(n: Int)(implicit T: Tag[Span]): ZIO[Has[Tracing[Span]] with zio.console.Console, IOException, Unit] =
    for {
      _ <- opentracing.log()
      _ <- console.putStrLn(s"Child 1 of Parent $n").instrumented(s"child1-of-parent$n")
      _ <- opentracing.log("LEVEL" -> "DEBUG", "n" -> n, "isShithole" -> true)
      _ <- console.putStrLn(s"Child 2 of Parent $n").instrumented(s"child2-of-parent$n")
    } yield ()
}
