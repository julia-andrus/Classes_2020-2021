#38

Sub <- list("slide" = c(30, 35, 40, 25, 20, 30, 35, 62, 40, 51, 25, 42, 33),
            "digital"= c(25, 16, 15, 15, 10, 20, 7, 16, 15, 13, 11, 19, 19))



boxplot(Sub, horizontal = T)

t.test(slide, digital, paired = T)



#37 b
t.test(slide, digital, paired = T)


M <- c(82.6, 87.1, 89.5, 88.8, 94.3, 80.0, 86.7, 92.5, 97.8, 90.4, 94.6, 91.6)

LD <- c(86.9, 87.3, 92.0, 89.3, 91.4, 85.9, 89.4, 91.8, 94.3, 92.0, 93.1, 91.3 )

t.test(M, LD, paired = T)




# 9.46 a)

Min <- c(10490, 16620, 17300, 15480, 12970, 17260, 13400, 13900,
         13630, 13260, 14370, 11700, 15470, 17840, 14070, 14760)

weeks <- c(9110, 13250, 14720, 12740, 10120, 14570, 11220, 11100,
           11420,10910, 12110, 8620,  12590, 15090, 10550, 12230)

qqnorm(Min);qqline(Min)

qqnorm(weeks);qqline(weeks)

#######9.46b)
t.test(Min, weeks, paired= T)
#t=20.727
#p-value=1.88e-12=0.000012
#If significance level=0.05 or 0.01, we can reject null hyp.

#######9.46c) 
var.test(Min, weeks)
#F = 1.1682,  df=15
#p-value=0.7673
#F-left critical value=2.862093
qf(0.975, 15, 15, lower.tail = T)
#F-right critical value=2.862093
qf(0.025, 15, 15, lower.tail = F)
#F=1.1682 < 2.862093=F_{left}, then we can rejecy H_{0}

pnorm(.025)



difference <- c( 5, 19, 25, 10, 10, 10, 28,
                 46, 25, 38, 14, 23, 14)


mean(difference)

sd(difference)

11.96255/3.60555

20.53846/3.317816

sum(difference^(2))

2^(2)

#32

old <- c(length=28, mean=801, SD=117)

young <- c(length=16, mean=780, SD=72)

t.test(old, young, paired= F)







# 9.64

Energizer <- c(8.65, 8.74, 8.91, 8.72, 8.85, 8.52, 8.62, 8.68, 8.86)
#n = 9
length(Energizer)
#mean = 8.727778
mean(Energizer)
# Variance = 0.01611944
var(Energizer)

Ultracell <- c(8.76, 8.81, 8.81, 8.70, 8.73, 8.76, 8.68, 8.64, 8.79)
#n = 9
length(Ultracell)
#mean = 8.742222
mean(Ultracell)
# variance = 0.003544444
var(Ultracell)

#F-test= 4.547806, df=8
var.test(Energizer, Ultracell)
#F-left critical value=0.2255676
qf(0.975, 8, 8, lower.tail = T)
#F-right critical value=4.43326
qf(0.025, 8, 8, lower.tail = F)

pnorm(.14, lower.tail = F)

pnorm(-2.14)

pnorm(2.67, lower.tail = F)

