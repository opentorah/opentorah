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
import org.podval.calendar.GregorianCalendar;
import org.podval.calendar.GregorianDate;
import org.podval.calendar.GregorianMonth;
import org.podval.calendar.JewishMonth;
import org.podval.calendar.JewishDate;

import java.io.OutputStream;


public final class ICalGenerator {

    private ICalGenerator(final OutputStream os) {
        this.out = new ICalWriter(os);
    }


    private void writeYear(final int year) {
        out.beginCalendar("-//Podval Group//NONSGML Jewish Calendar//EN", "Jewish Dates", "Jewish Dates, Events and Schedules");

        GregorianDate gDate = GregorianDate.create(year, GregorianMonth.January, 1);
        while (true) {
            writeDay(gDate);

            gDate = GregorianCalendar.getInstance().dateFromDays(gDate.getDays()+1);
            if (gDate.getYear() != year) {
                break;
            }
        }

        out.endCalendar();
    }


    private static final String ICON_URL = "http://calendar.podval.org/icon.gif";


    private void writeDay(final GregorianDate gDate) {
        final JewishDate jDate = gDate.toJewish();
        final String monthName = monthName(jDate.getMonth().month);
        final String summary = monthName + " " + jDate.getDay();
        final String url = "http://calendar.podval.org/day/" + jDate.getYear() + "/" + jDate.getMonth().name + "/" + jDate.getDay();

        out.beginEvent();
        out.writeSummary(summary);
        out.writeFullDayDuration(gDate);
        out.addGoggleContent(summary, ICON_URL, url, 200, 200);
        out.endEvent();
    }


    private String monthName(final JewishMonth month) {
        String result = null;
        switch (month) {
        case Tishri: result = "Тишрей"; break;
        case MarHeshvan: result = "Мар-Хешван"; break;
        case Kislev: result = "Кислев"; break;
        case Tevet: result = "Тевес"; break;
        case Shevat: result = "Шват"; break;
        case Adar: result = "Адар"; break;
        case AdarI: result = "Адар I"; break;
        case AdarII: result = "Адар II"; break;
        case Nissan: result = "Нисан"; break;
        case Iyyar: result = "Ияр"; break;
        case Sivan: result = "Сиван"; break;
        case Tammuz: result = "Таммуз"; break;
        case Av: result = "Ав"; break;
        case Elul: result = "Элул"; break;
        }
        return result;
    }


    public static void main(final String[] args) throws Exception {
        new ICalGenerator(new FileOutputStream("/tmp/jc.ics")).writeYear(2009);
    }


    private final ICalWriter out;
}
