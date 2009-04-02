/* $Id: TestAngle.java,v 1.1 2006/07/20 15:06:48 dub Exp $ */

package org.podval.moon;

import junit.framework.*;


public class TestAngle extends TestCase {

    public void testAngle() {
        testFromValue(5, 34);
        testFromValue(54, 34);
        testFromValue(154, 59);
        testFromValue(254, 0);
    }


    private void testFromValue(int degrees, int minutes) {
        Angle angle = new Angle(degrees, minutes);
        assertEquals(degrees, angle.getDegrees());
        assertEquals(minutes, angle.getMinutes());
    }
}
