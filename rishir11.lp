/*Objective function*/
max: +385 Product1 +330 Product2 +275 Product3;

/*Constraints*/
Plant1: +20 Product1Plant1 +15 Product2Plant1 +12 Product3Plant1 <= 13000;
Plant2: +20 Product1Plant2 +15 Product2Plant2 +12 Product3Plant2 <= 12000;
Plant3: +20 Product1Plant3 +15 Product2Plant3 +12 Product3Plant3 <= 5000;

Product1Plant1 + Produtct2Plant1 + Produtct3Plant1 <= 750;
Product1Plant2 + Produtct2Plant2 + Produtct3Plant2 <= 900;
Product1Plant3 + Produtct2Plant3 + Produtct3Plant3 <= 450;

Product1 <= 900;
Product2 <= 1200;
Product3 <= 750;
