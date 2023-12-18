#UAS Probability & Statistics CE319-A
#Jackson Lawrence (00000070612)
#Rich Marvin Lim (00000079061)


#Set Up Dataset
install.packages("tidyverse")
library(tidyverse)
data <- read_csv("C:/Users/hp/Downloads/datasheetAsia10Year2.csv")
gdp <- data$`gdp_per_capita`
energy <- data$`Primary energy consumption per capita (kWh/person)`


#View Data
selected_data <- data.frame(Entity = data$Entity, Year = data$Year, GDP = gdp, Energy = energy)
View(selected_data)


#Function untuk menghitung modus
modus <- function(x) {
  x <- na.omit(x) 
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}


#Statistika Deskriptif Variabel GDP Per Kapita
mean(gdp, na.rm = TRUE)
median(gdp, na.rm = TRUE)
modus(gdp)
var(gdp, na.rm = TRUE)
sd(gdp, na.rm = TRUE)
quantile(gdp, na.rm = TRUE)
IQR(gdp, na.rm = TRUE)
range(gdp, na.rm = TRUE)
paste("Range: ", max(gdp, na.rm = TRUE)-min(gdp, na.rm = TRUE))
summary(gdp)


#Statistika Deskriptif Variabel Primary Energy Consumption
mean(energy, na.rm = TRUE)
median(energy, na.rm = TRUE)
modus(energy)
var(energy, na.rm = TRUE)
sd(energy, na.rm = TRUE)
quantile(energy, na.rm = TRUE)
IQR(energy, na.rm = TRUE)
range(energy, na.rm = TRUE)
paste("Range: ", max(energy, na.rm = TRUE)-min(energy, na.rm = TRUE))
summary(energy)


library(moments)
#Skewness & Kurtosis Variabel GDP Per Kapita
skewness(gdp, na.rm = TRUE)
kurtosis(gdp, na.rm = TRUE)


#Skewness & Kurtosis Variabel Primary Energy Consumption
skewness(energy, na.rm = TRUE)
kurtosis(energy, na.rm = TRUE)


gdp[is.na(gdp)] <- 0
energy[is.na(energy)] <- 0
#Covariance & Correlation
cov(gdp, energy)
cor(gdp, energy)


#Boxplot
boxplot(gdp, main = "Boxplot of GDP Per Kapita", notch = TRUE, col="steelblue")
boxplot(energy, main = "Boxplot of Primary Energy Consumption Per Kapita", notch = TRUE, col="steelblue", cex.axis = 0.6)


#Histogram
options(scipen = 999) #Untuk menghindari nilai berbentuk pangkat
hist(gdp, main = "Histogram  of GDP Per Kapita", xlim = c(0,100000), col="steelblue", xlab="Value", ylab="Frequency")
hist(energy, main = "Histogram of Primary Energy Consumption Per Kapita", xlim = c(0,300000), ylim = c(0, 250), col="steelblue", xlab="Value", ylab="Frequency")


#Dot Chart 
dotchart(gdp, pch = 21, bg="steelblue", main="Dot Chart of GDP Per Kapita", pt.cex = 1.5)
dotchart(energy, pch = 21, bg="steelblue", main="Dot Chart of Primary Energy Consumption Per Kapita", pt.cex = 1.5)


#Scatter Plot 
plot(gdp, energy, col="steelblue", main="Scatter Plot")


#Q-Q Plot
gdp_df <- data.frame(gdp)
for (col in names(gdp_df)) {
  qqnorm(gdp_df[[col]], main="Q-Q Plot of GDP Per Kapita", col="steelblue")
  qqline(gdp_df[[col]])
}
energy_df <- data.frame(energy)
for (col in names(energy_df)) {
  qqnorm(energy_df[[col]], main="Q-Q Plot of Primary Energy Consumption Per Kapita", col="steelblue")
  qqline(energy_df[[col]])
}


#Shapiro-Wilk Test
shapiro.test(gdp)
shapiro.test(energy)


#-----------------------------------------------------------------------------------------------------------------------------------------------------------
#Transformasi log
gdp_log <- log10(gdp)
energy_log <- log10(energy)
hist(gdp_log, main = "Histogram of GDP Per Kapita (Log Transformation)", xlim = c(2.5,5.0), ylim = c(0,60), col="steelblue")
hist(energy_log, main = "Histogram of Primary Energy Consumption Per Kapita (Log Transformation)", xlim = c(2.5,5.5), ylim = c(0,100), col="steelblue")
gdp_log_df <- data.frame(gdp_log)
for (col in names(gdp_log_df)) {
  qqnorm(gdp_log_df[[col]], main="Q-Q Plot of GDP Per Kapita (Log Transformation)", col="steelblue", ylim = c(0,10))
  qqline(gdp_log_df[[col]])
}
energy_log_df <- data.frame(energy_log)
for (col in names(energy_log_df)) {
  qqnorm(energy_log_df[[col]], main="Q-Q Plot of Primary Energy Consumption Per Kapita (Log Transformation)", col="steelblue")
  qqline(energy_log_df[[col]])
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------


#t-test one sample
t.test(gdp_log, mu=50, conf.level = 0.95, alternative = "two.sided")
t.test(energy_log, mu=50, conf.level = 0.95, alternative = "two.sided")


#Nilai t-statistics untuk one sample
qt(0.05, 368, lower.tail = FALSE)
qt(0.05, 369, lower.tail = FALSE)


#t-test two sample
t.test(gdp_log, energy_log, mu=0, conf.level = 0.95, alternative = "two.sided", paired = TRUE)


#Nilai t-statistics untuk two sample
qt(0.05, 368, lower.tail = FALSE)


#Regresi Linear
gdp_regresilinear <- gdp_log
energy_regresilinear <- energy_log
linearregretion = lm(energy_regresilinear~gdp_regresilinear)
print(linearregretion)
summary(linearregretion)
est <- c(8,5,3)
a <- data.frame(gdp_regresilinear=est)
result = predict(linearregretion, a)
print(result)


#Regression Line
plot(gdp_log, energy_log, main = "Regression Line", xlab = "GDP", ylab = "Energy")
abline(linearregretion, col = "steelblue") 


#Residu
residual = resid(linearregretion)
print(residual)
summary(residual)


#Plot Residu
plot(fitted(linearregretion), residual, col = "steelblue")
abline(0,0)


#Q-Q Plot Residu
qqnorm(residual, col = "steelblue")
qqline(residual)


#Normalitas Residu
plot(density(residual), col = "steelblue")
shapiro.test(residual)


#Chi Square
chisquare <- matrix(c(0.5, 6.9, 8.4, 20.0, 98.4, 4.4, 20.3,6.7,11.0,10.5,4.7,13.5,0.8,1.5,7.9,9.6,67.6,14.9,26.4,28.9,41.6,43.4,52.1,0.5,5.3,6.7,23.1,84.4,3.7,14.3,4.4,9.7,9.0,3.8,12.7,1.0,1.6,8.4,9.0,81.8,19.3,23.4,28.4,48.2,47.0,54.7), nrow = 23, ncol=2)
colnames(chisquare) <- c("2011-2015", "2016-2020")
rownames(chisquare) <- c("Agriculture, fishing, mining and quarrying", "Manufacturing", "Electricity, gas and water supply, and waste management", "Construction", "Import and export trade", "Wholesale", "Retail trade", "Accommodation services", "Food and beverage services", "Land transport", "Water transport", "Air transport", "Warehousing and other transportation services", "Postal and courier services", "Telecommunications", "Other information and communications services", "Financing", "Insurance", "Real estate", "Professional and business services", "Public administration", "Social and personal services", "Ownership of premises") #Economic Activity
View(chisquare)
print(chisquare)
chi_square_test <- chisq.test(chisquare)
print(chi_square_test)