
# For homework assignment 4: Data from the book 
#####

ALD <- c(1.38, 0.44, 1.09, 0.75,0.66, 1.28, 0.51,
         0.39, 0.70, 0.46, 0.54, 0.83, 0.58, 0.64,
         1.30, 0.57, 0.43, 0.62, 1.00, 1.05, 0.82,
         1.10, 0.65, 0.99, 0.56, 0.56, 0.64, 0.45,
         0.82, 1.06, 0.41, 0.58, 0.66, 0.54, 0.83,
         0.59, 0.51, 1.04, 0.85, 0.45, 0.52, 0.58,
         1.11, 0.34, 1.25, 0.38, 1.44, 1.28, 0.51)

sort(ALD
MVSD.ALD <- data.frame(mean(ALD), var(ALD), sd(ALD))
MVSD.ALD
length(ALD)


# Visual Data: Histogram, Curve line, and Boxplot
#####


hist(ALD, density=60, breaks=6, prob=T, xlab="ALD", ylab="units of pixe dimensions", main = "Hist of ALD")

curve(dnorm(x,mean=mean(ALD),sd=sd(ALD)), add=TRUE,col="black")

boxplot(ALD, main="Data_ALD", horizontal = T)

abline(v=quantile(ALD, c(0.25, 0.75)), col= "blue")



# Quick Info about the data
######

min(ALD)
median(ALD)
max(ALD)
quantile(ALD, 0.25)
quantile(ALD, 0.75)


#Confidence Interval
#####

t.test(ALD)
t.test(var(ALD, 0.95)


