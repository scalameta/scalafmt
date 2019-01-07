package org.scalafmt.interfaces;

/**
 * A classloader that shares only scalafmt-interfaces classes from the parent classloader.
 *
 * This classloader is intended to be used as a parent when class-loading scalafmt-dynamic.
 * By using this classloader as a parent, it's possible to cast runtime instances from
 * the scalafmt-dynamic classloader into `org.scalafmt.interfaces.Scalafmt` from this classlaoder.
 */
public class ScalafmtClassLoader extends ClassLoader {

    private ClassLoader parent;
    public ScalafmtClassLoader(ClassLoader parent) {
        super(null);
        this.parent = parent;
    }

    @Override
    protected Class<?> findClass(String name) throws ClassNotFoundException {
        if (name.startsWith("org.scalafmt.interfaces")) {
            return parent.loadClass(name);
        }  else {
            return super.findClass(name);
        }
    }
}
