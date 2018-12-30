package org.scalafmt.interfaces;

import java.nio.file.*;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.ServiceLoader;

public abstract class Scalafmt {

    public abstract String format(Path file, String code);

    public static ScalafmtBuilder newBuilder(ClassLoader loader) {
        Iterator<ScalafmtBuilder> builders = ServiceLoader.load(ScalafmtBuilder.class, loader).iterator();
        if (builders.hasNext()) {
            return builders.next();
        }
        throw new NoSuchElementException(
                ScalafmtBuilder.class.getName() +
                        ". To fix this problem, make sure that the provided classloader contains the scalafmt-cli module."
        );
    }

}