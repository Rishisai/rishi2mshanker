/*Objective function*/

/*m= month number
r1 = remaining items sold in month 1 after 200
s1= storage costs in month 1
b1 = binary values */

min : 12 m1 + 14 m2 + 16 m3 + 18 m4 + 10 r1 + 12 r2 + 14 r3 + 16 r4 + 2s1 + 2s2 + 2s3 + 2s4;

/*constraints*/

m1 + r1 - s1 = 150;
s1 + m2 + r2 - s2 = 200;
s2 + m3 + r3 - s3 = 250;
s3 + m4 + r4 - s4 = 150;
m1 - 200b1 >= 0;
m2 - 200b2 >= 0;
m3 - 200b3 >= 0;
m4 - 200b4 >= 0;
r1 - 10000b1 <= 0;
r2 - 10000b2 <= 0;
r3 - 10000b3 <= 0;
r4 - 10000b4 <= 0;

0 <= m1 <= 200;
0 <= m2 <= 200;
0 <= m3 <= 200;
0 <= m4 <= 200;

bin b1 ;
bin b2 ; 
bin b3 ;
bin b4 ;