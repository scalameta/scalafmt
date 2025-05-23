import scala.scalanative.build._
//import scala.util.Properties

object Extensions {

  implicit class ImplicitNativeConfig(private val cfg: NativeConfig)
      extends AnyVal {
    def withGC: NativeConfig =
//      if (Properties.isWin) cfg.withGC(GC.commix) else
        cfg.withGC(GC.immix)
    def forTest: NativeConfig = withGC.withMode(Mode.debug)
    def forRelease: NativeConfig = withGC.withMode(Mode.releaseFull)
  }

}
