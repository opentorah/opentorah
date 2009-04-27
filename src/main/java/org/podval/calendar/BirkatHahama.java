package org.podval.calendar;


public final class BirkatHahama {

    public final int FIRST = Calendar.getJewish().dateFromDate(1, JewishMonth.Adar, 21).getDays();


    public Date<JewishMonth> getDate(final int number) {
        return Calendar.getJewish().dateFromDays(FIRST + number * (28 * 365 + 7));
    }


    public void print() {
        System.out.println("||Cycle||Jewish||Secular||");
        for (int cycle = 0; cycle <= 214; cycle++) {
            final Date<JewishMonth> jDate = getDate(cycle);
            final Date<GregorianMonth> gDate = Calendar.getGregorian().dateFromDays(jDate.getDays());
            System.out.println("||" + cycle + "||" + jDate + "||" + gDate + "||");
        }
    }


    public void tabulate() {
        final int[] adar = new int[31];
        final int[] nissan = new int[31];

        for (int cycle = 0; cycle <= 214; cycle++) {
            final Date<JewishMonth> date = getDate(cycle);
            if ((date.getMonth().month == JewishMonth.Adar) || (date.getMonth().month == JewishMonth.AdarII)) {
                adar[date.getDay()]++;
            } else if (date.getMonth().month == JewishMonth.Nissan) {
                nissan[date.getDay()]++;
            } else {
                throw new Error();
            }
        }

        tabulateMonth("Adar", adar);
        tabulateMonth("Nissan", nissan);
    }


    private void tabulateMonth(final String name, final int[] month) {
        System.out.println(name);
        System.out.println("||Day||Times||");
        for (int day = 1; day <= 30; day++) {
            System.out.println("||" + day + "||" + month[day] + "||");
        }
    }


    public static void main(final String[] args) {
        new BirkatHahama().print();
        new BirkatHahama().tabulate();
    }
}
