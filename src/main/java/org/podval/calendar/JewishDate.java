/*
 *  Copyright 2010 dub.
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

package org.podval.calendar;


/**
 *
 * @author dub
 */
public final class JewishDate extends Date<JewishMonth, JewishDate> {

    public static JewishDate create(final int year, final JewishMonth month, final int day) {
        return JewishCalendar.getInstance().dateFromDate(year, month, day);
    }


    public JewishDate(
        final int days,
        final int year,
        final Month<JewishMonth> month,
        final int day)
    {
        super(days, year, month, day);
    }


    private JewishDate(
        final int days,
        final int year,
        final Month<JewishMonth> month,
        final int day,
        final int hour,
        final int minute,
        final int parts)
    {
        super(days, year, month, day, hour, minute, parts);
    }


    @Override
    protected JewishDate create(final int days, final int year, final Month<JewishMonth> month, final int day, final int hour, final int minute, final int parts) {
        return new JewishDate(days, year, month, day, hour, minute, parts);
    }


    public GregorianDate toGregorian() {
        boolean isAfterMidnight = getHour() >= 6;
        return GregorianCalendar.getInstance()
            .dateFromDays(getDays() + (isAfterMidnight ? 1 : 0))
            .setTime((getHour()-6) % 24, getMinute(), getParts());
    }
}
