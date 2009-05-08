/*
 *  Copyright 2009 dub.
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  under the License.
 */

package org.podval.ical;

import java.io.FileOutputStream;
import org.podval.calendar.Date;
import org.podval.calendar.GregorianCalendar;
import org.podval.calendar.GregorianMonth;

import java.io.OutputStream;


public final class iCalGenerator {

    private void writeYear(final int year, final OutputStream os) {
        final iCalWriter out = new iCalWriter(os);
        out.beginCalendar("-//Podval Group//NONSGML Jewish Calendar//EN", "Jewish Dates", "Jewish Dates, Events and Schedules");

        Date gDate = Date.create(year, GregorianMonth.January, 1);
        while (true) {
            final Date jDate = gDate.toJewish();
            final String summary = jDate.getMonth().name + " " + jDate.getDay();

            out.beginEvent(false);
            out.writeSummary(summary);
            out.writeFullDayDuration(gDate);
            out.endEvent();

            gDate = GregorianCalendar.getInstance().dateFromDays(gDate.getDays()+1);
            if (gDate.getYear() != year) {
                break;
            }
        }

        out.endCalendar();
    }


    public static void main(final String[] args) throws Exception {
        new iCalGenerator().writeYear(2009, new FileOutputStream("/tmp/jc.ics"));
    }
}
