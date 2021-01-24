
AC_voltage <-c(62, 50, 53, 57, 41, 53, 55, 61,
               59, 64,50, 53, 64, 62, 50, 68, 
               54, 55, 57, 50, 55, 50, 56, 55, 
               46, 55, 53, 54, 52, 47, 47, 55, 
               57, 48, 63, 57, 57, 55,53, 59,
               53, 52, 50, 55, 60, 50, 56, 58)
boxplot(AC_voltage, main="AC Voltage(kV) Testing of Insulation Liquids", ylab="Votage (kV)")

M_voltage <- mean(AC_voltage)
M_voltage
median(AC_voltage)
var(AC_voltage)##variance
sqrt(var(AC_voltage)) #Standard Deviation(sd())
5.230672/48
1.96 * 0.1089723
M_voltage + 0.2135857
M_voltage - 0.2135857

