# 12.16

Rainfall_volume_x <- c(5, 12, 14, 17, 23, 30, 40, 47, 55, 67, 72, 81, 96, 112, 127 )

sum(Rainfall_volume_x )

Runoff_volume_y <- c(4, 10, 13, 15, 15, 25, 27, 46, 38, 46, 53, 70, 82, 99, 100)

4^2+ 10^2+ 13^2+ 15^2+ 15^2+ 25^2+ 27^2+ 46^2+ 38^2+ 46^2+ 53^2+ 70^2+ 82^2+ 99^2+ 100^2

sum(Runoff_volume_y)

sum(Rainfall_volume_x * Runoff_volume_y)

41999-(-1.1283 * 643)-(0.826973 * 51232)

Rainfall_Runoff <- data.frame(c(Rainfall_volume, Runoff_volume))

plot(Runoff_volume_y ~ Rainfall_volume_x, main="Water Volume")

abline(lm( Runoff_volume_y ~ Rainfall_volume_x))


mean(Rainfall_volume)
mean(Runoff_volume)

length(Rainfall_volume)






# 12.20 

Pressure <- c(0, 0, 0, .1, .1, .1, .2, .2, .2, .3, .3, .3, .4, 
              .4, .4, .5, .5, .5, .6, .6, .6 )

sum(Pressure)

Ratio <- c(0.123, 0.100, 0.101, 0.172, 0.133, 0.107, 0.217, 
           0.172, 0.151, 0.263, 0.227, 0.252, 0.310, 0.365,
           0.239, 0.365, 0.319, 0.312, 0.394, 0.386, 0.320)
0.123^2+0.100^2+0.101^2+0.172^2+0.133^2+0.107^2+0.217^2+0.172^2+0.151^2+0.263^2+0.227^2+0.252^2+0.310^2+0.365^2+ 0.239^2 + 0.365^2 +0.319^2 + 0.312^2 + 0.394^2 + 0.386^2 + 0.320^2 

sum(Ratio * Pressure)
sum(Ratio)

Bond_Behavior <- data.frame(Pressure, Ratio)
summary(Bond_Behavior)
View(Bond_Behavior)

Linear_Bond <- lm(Ratio ~ Pressure, data = Bond_Behavior)

View(Linear_Bond)


plot(Ratio ~ Pressure, main = "Bond_Behavior")

#Regressions Line
abline(lm(Ratio ~ Pressure))

summary(lm(Ratio ~ Pressure))






#12.34

all <- data.frame(Air_Void_X, Dielectric_Y)

Air_Void_X <- c(4.35, 4.79, 5.57, 5.20, 5.07, 5.79, 5.36, 
                  6.40, 5.66, 5.90, 6.49, 5.70, 6.49, 6.37, 6.51, 7.88, 6.74, 7.08)

Dielectric_Y <- c(4.55, 4.49, 4.50, 4.47, 4.47, 4.45, 4.40, 4.34, 4.43, 4.43, 
                  4.42, 4.40, 4.33, 4.44, 4.40, 4.26, 4.32, 4.34)

4.55^2+4.49^2+ 4.50^2+ 4.47^2+ 4.47^2+ 4.45^2+ 4.40^2+ 4.34^2+ 4.43^2+ 4.43^2+ 4.42^2+ 4.40^2+ 4.33^2+ 4.44^2+ 4.40^2+ 4.26^2+ 4.32^2+ 4.34^2



4.35^2+ 4.79^2+ 5.57^2+ 5.20^2+ 5.07^2+ 5.79^2+ 5.36^2+ 6.40^2+5.66^2+ 5.90^2+ 6.49^2+ 5.70^2+ 6.49^2+ 6.37^2+ 6.51^2+ 7.88^2+ 6.74^2+ 7.08^2


View(lm(all))


sum(Air_Void_X * Dielectric_Y)


sum(Air_Void_X)
sum(Dielectric_Y)

mean(Dielectric_Y)
mean(Air_Void_X)

350.6868-(4.85 * 79.44)-(-.07466 * 472.8149 )








#12.52 

Chlorine_X <- c(1.5, 1.5, 2.0, 2.5, 2.5, 3.0, 3.5, 3.5, 4.0)

sum(Chlorine_X)

1.5^2+ 1.5^2+ 2.0^2+ 2.5^2+ 2.5^2+ 3.0^2+ 3.5^2+ 3.5^2+ 4.0^2


Etch_rate_Y <- c(23.0, 24.5, 25.0, 30.0, 33.5, 40.0, 40.5, 47.0, 49.0)
sum(Etch_rate_Y)




sum(Chlorine_X * Etch_rate_Y)

summary(lm(Chlorine_X ~ Etch_rate_Y))

sum(Etch_rate_Y)
23.0^2+ 24.5^2+ 25.0^2+ 30.0^2+ 33.5^2+ 40.0^2+ 40.5^2+ 47.0^2+49.0^2

plot(Chlorine_X, Etch_rate_Y, main = "Chlorine Flow")

lm(Chlorine_X  ~ Etch_rate_Y)

0.8925123-(1-  0.9750021)

pnorm(1.96)





# 12.68

Height_X <- c(12, 14, 14, 15, 15, 16, 18, 22, 22, 24,24, 26, 26, 27, 28, 30, 30, 33, 36)

12^2+ 14^2+ 14^2+ 15^2+ 15^2+ 16^2+ 18^2+ 22^2+ 22^2+ 24^2+24^2+ 26^2+ 26^2+ 27^2+ 28^2+ 30^2+30^2+ 33^2+ 36^2

mean(Height_X)
sum(Height_X)

Price_Y <- c(35.53, 37.82, 36.90, 40.00, 38.00, 37.50, 41.00, 48.50, 47.00, 47.50, 
             46.20, 50.35, 49.13, 48.07, 50.90, 54.78, 54.32, 57.17, 57.45)
mean(Price_Y)
sum(Price_Y)

35.53^2+ 37.82^2+ 36.90^2+ 40.00^2+38.00^2+ 37.50^2+ 41.00^2+ 48.50^2+ 47.00^2+ 47.50^2+ 46.20^2+ 50.35^2+ 49.13^2+ 48.07^2+ 50.90^2+ 54.78^2+ 54.32^2+ 57.17^2+ 57.45^2


sum(Height_X * Price_Y)
plot( Price_Y ~ Height_X, main= "Truss Height and Sale Pricee")
abline(lm( Price_Y ~ Height_X))

summary(lm(Height_X ~ Price_Y))

summary(lm(Price_Y ~ Height_X ))
