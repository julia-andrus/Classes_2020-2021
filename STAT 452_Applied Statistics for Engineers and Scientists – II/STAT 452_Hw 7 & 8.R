# 10.4

rm(Refri_four, Refri_one)
one <- c(30.4, 29.2)
two <- c(27.7, 27.1)
three <- c(27.1, 24.8)
four <- c(25.5, 28.8)

##combine these together

All_four <- data.frame(cbind( one, two, three, four))

View((All_four))
summary(All_four)

##Stacking groups

Four <- stack(All_four)

# Getting ANOVA command
View(Four)


Anova_Results <- aov(values ~ ind, data=Four)

summary(Anova_Results)



#10.6

Carb. <- c(20.5, 28.1, 27.8, 27.0, 28.0,
           25.2, 25.3, 27.1, 20.5, 31.3)

Sili. <- c(26.3, 24.0, 26.2, 20.2, 23.7, 
           34.0, 17.1, 26.8, 23.7, 24.9)

Mag. <-  c(29.5, 34.0, 27.5, 29.4, 27.9,
          26.2, 29.9, 29.5, 30.0, 35.6)

Herm. <- c(36.5, 44.2, 34.1, 30.3, 31.4, 
           33.1, 34.1, 32.9, 36.3, 25.5)

Formations_All <-data.frame(cbind(Carb., Sili., Mag., Herm.))
summary(Formations_All)

All_Formations <- stack(Formations_All)
summary(All_Formations)

ANOVA_Summary <- aov(values ~ ind, data = All_Formations)
summary(ANOVA_Summary)

#10.18a)

Hormone_1 <- c(13, 17, 7, 14)
Hormone_2 <-  c(21, 13, 20, 17)
Hormone_3 <- c(18, 15, 20, 17)
Hormone_4 <- c(7, 11, 18, 10)
Hormone_5 <- c(6, 11, 15, 8)
sum(Hormone_4)
grand.mean(Hormone_1, Hormone_2)
Hormones <- data.frame(cbind(Hormone_1, Hormone_5, Hormone_2, Hormone_3, Hormone_4))
summary(Hormones)

Hormones_all <- stack(Hormones)

((51)^2+(71)^2+(70)^2+(46)^2)


Anova_Hormones <- aov(values ~ ind, data = Hormones_all)
summary(Anova_Hormones)




# 10.18b)

Tukey_Hormones <- TukeyHSD(aov(values ~ ind, data = Hormones_all))
$ ` ind `

# 37

Motor_1 <- c(13.1, 15.0, 14.0, 14.4, 14.0, 11.6)
Motor_2 <- c(16.3, 15.7, 17.2, 14.9, 14.4, 17.2)
Motor_3 <- c(13.7, 13.9, 12.4, 13.8, 14.9, 13.3)
Motor_4 <- c(15.7, 13.7, 14.4, 16.0, 13.9, 14.7)
Motor_5 <- c(13.5, 13.4, 13.2, 12.7, 13.4, 12.3)

Motors <- data.frame(cbind(Motor_1, Motor_2, Motor_3, Motor_4, Motor_5))

boxplot(Motors, horizontal= F)

All_Motors <- stack(Motors)

ANOVA_Motors <- aov(values ~ ind, data = All_Motors)
summary(ANOVA_Motors)

TukeyHSD(ANOVA_Motors)

plot(TukeyHSD(ANOVA_Motors), ylabel= "1")












# 42 a) Unequl sample sizes

Brown <- c(26.8, 27.9, 23.7, 25.0, 26.3, 24.8, 25.7, 24.5)
Green <- c(26.4, 24.2, 28.0, 26.9, 29.1)
Blue <- c(25.7, 27.2, 29.9, 28.5, 29.4, 28.3)

Iris_Color <- data.frame(cbind(length(Brown), length(Green), length(Blue))

                         summary(Iris_Color)
rm(Iris_Color)

Iris_data <- data.frame(Y=c(Brown, Green, Blue),
                        Groups=factor(rep(c("Brown", "Green", "Blue"), times= c(length(Brown), length(Green), length(Blue)))))
summary(Iris_data)
View(Iris_data)

boxplot(Iris_data)

Iris_Anova <- aov(Y ~ Groups, data= Iris_data)
summary(Iris_Anova)

Tuckey_Iris <- TukeyHSD(Iris_Anova)
View(Tuckey_Iris)

#boxplot of the differences of mean
plot(Tuckey_Iris)
View(plot(Tuckey_Iris, title("Iris Groups")))


# 10.22)

EC_1.6 <- c(59.5, 53.3, 56.8, 63.1, 58.7)
EC_3.8 <- c(55.2, 59.1, 52.8, 54.5)
EC_6.0 <- c(51.7, 48.8, 53.9, 49.0)
EC_10.2 <- c(44.6, 48.5, 41.0, 47.3, 46.1)

EC_data <- data.frame(Y=c(EC_1.6, EC_3.8, EC_6.0, EC_10.2),
                        Groups=factor(rep(c("EC_1.6", "EC_3.8", "EC_6.0", "EC_10.2" ),
                            times= c(length(EC_1.6), length(EC_3.8), length(EC_6.0), length(EC_10.2)))))

EC_ANOVA <- aov(Y ~ Groups, data = EC_data)

summary((EC_ANOVA))
help(mean)
