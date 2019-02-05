---
---
# Research Questions #

## Fixed Calendar ##

### Calculate Meton's cycle ###

Show why 19-years cycle is the best.
Calculate optimal distribution of the leap years.

### New moon from 6:7 ###

Rambam mentiones Nisan conjunction on Sunday, 5 hours and 107 units parts after sunrise.
He doesn't give the year, and there seems to be no such year...

### RoshHaShono corrections ###

- are there meaningful names for the corrections?
- are corrections explained by the desire to not have Yom Kippur on Sunday etc.?
- calculate third correction from the maximum length of a year and the first correction
- KH 7:8 says that postponement of RoshHaShonoh is done to align the calendar better
with the true molad; analyze the statistics of distances between mean and true molad
and RoshHashono.

There is an argument concerning some of the Rosh Hashono delays involving r. Saadia Gaon:
https://he.wikipedia.org/wiki/%D7%9E%D7%97%D7%9C%D7%95%D7%A7%D7%AA_%D7%A8%D7%91_%D7%A1%D7%A2%D7%93%D7%99%D7%94_%D7%92%D7%90%D7%95%D7%9F_%D7%95%D7%91%D7%9F_%D7%9E%D7%90%D7%99%D7%A8
Спор о размерах "старого молада", когда он отодвигает Рош ашана на следующий день.
По нашей шите от Расага и дальше - 18 часов
По шите Аарона Бен Меира - 18 часов и 642/1080 частей часа

make possible calculating the calendar according to the other opinion and compare the results.

Молад года творения человека выходит в пятницу.
Это и было Рош а-Шана второго года, ведь тогда принцип ли аду Рош не применялся.
Тогда понятней спор Рамбама и Райвада о причине сдвига Рош а-Шана ло аду.
Рамбама настаивает, что оно нужно для подсчёта по кибуц эмцаи,
а Райвад на него из Талмуда возмущается.
Теоретически можно сделать календарь на время до установления календаря без сдвига Рош а-Шана.

### Sun Cycle ###

Where and when was the Sun created?
Compare with Rambam's epoch from the astronomical calendar.
Where and how is date of birchat hachamo defined?
How would it look according to the other opinion? 

### Seasons ###

Rav Ada's tkufos started a week later than Shmuel's! Isn't it readily observable?
Analyze the difference in historic periods...


## Astronomical Calendar ##

### Epoch ###
- when was the Sun created?
- introduce "creation" epoch.
- compare calculations based on it and on the Rambam's epoch.
- make possible calculations for a Moment, not just a Day (at least for the sun);

### Math.findZero() ###
- finish and test the code
- use two separate lengths (precisions)?

### Seasons ###
- calculate true seasons in SeasonsAstronomical
- KH 10:7 says that real vernal equinox is approximately two days before the mean one; add a test.

### Exactification ###
- Tzikuni gives the algorithm of such reconstruction:
 add to the remainder as many times 360 as there were full rotations in given time period,
 and then divide... find the page for exact reference;
- use it to calculate intervals for Rambam's values of the angular velocities  
  
### Angular movement for 29 days ###

Are Rambam's values reconstructible if value for 29 days is calculated as 3*10-1, not 2*10+9?
For any tables other than SunLongitudeMean?

### Angular speeds from periods ###

Convert each year length mentioned in the fixed calendar to angular speed.
Angular speed of the moon = 360 / (1/tropical month + 1/solar year).
Moon.meanLunarPeriod - what is it called? Tropical?

### Distance between the mean and true new moons ###

KH 5:1-2 says that the distance is no more than a day.
Calculate this difference for all months;
Rambam's epoch - two days after molad?! (Petya Ofman).

### Day of sighting ###

How to find the day of sighting given a month?


## Schedule ##

### Shulchan Aruch ###
Magen Avraham 428:4 (6): When Pesach falls on Shabbat, so that residents of the Land of Israel
read Shmini on the Shabbat following it, there are those that split Tazria and Metzora,
and there are those that split Behar and Bechukotai, and if the year is leap, they split
Matot and Masai.

### Baladi (Yemenite) ###
instead of Matot and Masai combine Chukat and Balak

### Daradaim ###
(following Rabbi Saadia Gaon): instead of combining Matot and Masai,
add to Korach Chukat to 20:21, and next Shabbos read the rest of Chukkat and Balak

### 3 small aliyot ###
for 2/5/Shabbos day: look into and implement differences in customs  

### 3-year cycle ###
Is anybody orthodox completing Torah cycle in three years?
If yes, maybe we should generate that schedule too.
Here is the list of Haftarot: https://www.google.com/url?q=https://faculty.biu.ac.il/~ofery/papers/haftarot3.pdf&sa=D&source=hangouts&ust=1535496504151000&usg=AFQjCNEMKEf2NJqydV6avPacDQhGBEqLDQ
