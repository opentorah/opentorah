/* $Id: TestAngle.java,v 1.1 2006/07/20 15:06:48 dub Exp $ */

package org.podval.calendar.moon;

import org.junit.Test;
import org.junit.Assert;


public class TestAngle {

    @Test
    public void angleTest() {
        testFromValue(5, 34);
        testFromValue(54, 34);
        testFromValue(154, 59);
        testFromValue(254, 0);
    }


    private void testFromValue(int degrees, int minutes) {
        Angle angle = new Angle(degrees, minutes);
        Assert.assertEquals(degrees, angle.getDegrees());
        Assert.assertEquals(minutes, angle.getMinutes());
    }
}
