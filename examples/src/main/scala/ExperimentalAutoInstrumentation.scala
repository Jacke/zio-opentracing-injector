import zio._
import _root_.zio.experimental
import zio.instrumentation._
import io.opentracing.Span
import java.io.IOException

object ExperimentalAutoInstrumentation extends App {
  import io.jaegertracing.Configuration, Configuration.SamplerConfiguration, Configuration.ReporterConfiguration

  val tracerConf =
    new Configuration("zio-opentracing-demo")
      .withSampler(SamplerConfiguration.fromEnv().withType("const").withParam(1))
      .withReporter(ReporterConfiguration.fromEnv().withLogSpans(true))

  // Fixme: Instantiation is a bit cumbersome
  def run(args: List[String]) =
    (for {
      tracer     <- UIO(tracerConf.getTracer)
      rootSpan   <- UIO(tracer.buildSpan("REQUEST-ROOT").start())
      propagator <- Propagator.make[io.opentracing.Span](new OpenTracingBackend(tracer), rootSpan)
      r          = Has(new Tracing(propagator)) ++ Has(zio.console.Console.Service.live)
      _          <- experimental.Experimental(myAppLogic).auto().provide(r)
      _          <- UIO(rootSpan.finish())
    } yield ()).exitCode

  def myAppLogic(implicit T: Tag[Span]): ZIO[Has[Tracing[Span]] with zio.console.Console, IOException, Unit] =
    for {
      _      <- doSomething(1)
      fiber1 <- doSomething(2).fork
      fiber2 <- doSomething(3).fork
      _      <- doSomething(4)
      _      <- fiber1.join
      _      <- fiber2.join
      _      <- doSomething(5)
    } yield ()

  def doSomething(n: Int)(implicit T: Tag[Span]): ZIO[Has[Tracing[Span]] with zio.console.Console, IOException, Unit] =
    for {
      _ <- console.putStrLn(s"Child 1 of Parent $n")
      _ <- console.putStrLn(s"Child 2 of Parent $n")
    } yield ()
}
