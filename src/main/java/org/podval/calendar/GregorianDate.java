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
public class GregorianDate extends Date<GregorianMonth, GregorianDate> {

    public static GregorianDate create(final int year, final GregorianMonth month, final int day) {
        return GregorianCalendar.getInstance().dateFromDate(year, month, day);
    }


    public GregorianDate(
        final int days,
        final int year,
        final Month<GregorianMonth> month,
        final int day)
    {
        super(days, year, month, day);
    }


    private GregorianDate(
        final int days,
        final int year,
        final Month<GregorianMonth> month,
        final int day,
        final int hour,
        final int minute,
        final int parts)
    {
        super(days, year, month, day, hour, minute, parts);
    }


    @Override
    protected GregorianDate create(final int days, final int year, final Month<GregorianMonth> month, final int day, final int hour, final int minute, final int parts) {
        return new GregorianDate(days, year, month, day, hour, minute, parts);
    }


    public JewishDate toJewish() {
        boolean isAfterShkia = getHour() >= 18;
        return JewishCalendar.getInstance()
            .dateFromDays(getDays() + (isAfterShkia ? 0 : -1))
            .setTime((getHour()+6) % 24, getMinute(), getParts());
    }
}
