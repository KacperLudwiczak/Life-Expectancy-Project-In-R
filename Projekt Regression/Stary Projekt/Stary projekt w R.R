setwd("C:/")
#use automatic import function to read the golf data
library(readxl)
Zestawienie <- read_excel("C:\Users\Kacper\Desktop\Moje dane w R\Zestawienie.xlsx")
View(Zestawienie)
names(Zestawienie)
print(Zestawienie$"High-tech exports")
plot(Zestawienie$"High-tech exports")
summary(Zestawienie$"High-tech exports")
hist(Zestawienie$"High-tech exports")
plot(Zestawienie$"High-tech exports"~Zestawienie$"Employment in high- and medium-high")
plot(Zestawienie$"High-tech exports"~Zestawienie$"High-tech patent") 
#########################################################
#Linear Regression
Model1<- lm(Zestawienie$"High-tech exports"~Zestawienie$"High-tech patent")
abline(Model1$coef,lty=5)
#########################################################
#Multiple Regression
Zestawienie_num <- as.numeric(Zestawienie)
cor(Zestawienie)
colnames(Dane)<-c("Price","Years","Mileage","Power","Capacity")
cor(Dane)
Model <- lm(Price~Years+Mileage+Power+Capacity,data=Dane)
summary(Model)
#Diagnostic
plot(Model$res,ylab="Residuals",main="Index plot of residuals")
plot(Model$fit,Model$res,xlab="Fitted",ylab="Residuals")
abline(h=0)
#Normality test
shapiro.test(Model$res)
