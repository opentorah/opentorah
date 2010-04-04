/* $Id: Function.java,v 1.1 2006/07/20 15:06:27 dub Exp $ */

package org.podval.calendar.moon;


public interface Function<S, T> {

    public T apply(S arg);
}
