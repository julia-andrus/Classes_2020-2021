##### 
#25



Greater93 <- c(100, 100, 60, 135, 195, 195, 125, 135, 95, 42, 75, 72)
Less89 <- c(80, 75, 75, 85, 75, 35, 85, 65, 45, 100, 28, 38, 50, 28)

qqnorm(Greater93-Less89);qqline(Greater93-Less89)

mean(Greater93)
sd(Greater93)
boxplot(Greater93, horizontal = T)
t.test(Greater93, Less89, conf.level = .95)



mean(Less89)
sd(Less89)
boxplot(Less89, horizontal = T)

money <- list(">=93"=c(100, 100, 60, 135, 195, 195, 125, 135, 95, 42, 75, 72), 
              "<=89"=c(80, 75, 75, 85, 75, 35, 85, 65, 45, 100, 28, 38, 50, 28))
View(money)



boxplot(money, horizontal = T)




#####
#37

Indoor <- c(.07, .08, .09, .12, .12, .12, .13, .14, .15,
                       .15, .17, .17, .18, .18, .18, .18, .19,
                       .20, .22, .22, .23, .23, .25, .26, .28,
                       .28, .29, .34, .39, .40, .45, .54, .62)
              
Outdoor <- c(.29, .68, .47, .54, .97, .35, .49, .84, .86,
                      .28, .32, .32, 1.55, .66, .29, .21, 1.02,
                      1.59, .90, .52, .12, .54, .88, .49, 1.24,
                      .48, .27, .37, 1.26, .70, .76, .99, .36)

#Normal Porbability

qqnorm(Indoor-Outdoor);qqline(Indoor-Outdoor)

t.test(Indoor, Outdoor, paired= T)



z.test(House, level= 0.95)

pnorm(-3.66)

z.test(House, .95)
1-0.0001261076
pnorm(3.66)
1- 0.9998739

pnorm(3.74)
1-0.999908

pnorm(3.85)
1-0.9999

pnorm(-.63)


one <- c(29, 34, 33, 27, 28, 32, 31, 34, 32, 27)
mean(one)
sd(one)

two <- c(18, 15, 23, 13, 12)
mean(two)
sd(two)
