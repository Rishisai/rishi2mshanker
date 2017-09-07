/*Objective function*/
max: 385 Product1 +330 Product2 +275 Product3;

/*Constraints*/
Plant1: +500 Product1 +250 Product3 <= 13000;
Plant2: +300 Product1 +500 Product3 <= 12000;
Plant3: +100 Product1 +200 Product2 <= 5000;
Product1 = 20;
Product2 = 15;
Product3 = 12;
