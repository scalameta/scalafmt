package org.scalafmt.interfaces

/** A classloader that shares only scalafmt-interfaces classes from the parent
  * classloader.
  *
  * This classloader is intended to be used as a parent when class-loading
  * scalafmt-dynamic. By using this classloader as a parent, it's possible to
  * cast runtime instances from the scalafmt-dynamic classloader into
  * `org.scalafmt.interfaces.Scalafmt` from this classloader.
  */
class ScalafmtClassLoader(var parent: ClassLoader) extends ClassLoader(null) {
  @throws[ClassNotFoundException]
  override protected def findClass(name: String) =
    if (name.startsWith("org.scalafmt.interfaces")) parent.loadClass(name)
    else super.findClass(name)
}
