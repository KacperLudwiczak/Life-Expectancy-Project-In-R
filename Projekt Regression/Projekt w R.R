#Regression Model Projekt Kacper Ludwiczak

#Instalacja pakietu 'readxl', oraz zaznaczenie pakietu przez funkcje library
library(readxl)

#Przydzielenie bazy danych do wektora 'Dane'
Dane <-  read_excel("C:/Users/Kacper/Desktop/Projekt Regression/Dane do R.xlsx")
View(Dane)
names(Dane)
print(Dane$"Life expectancy" )

#Zmianna nazw zmiennych
colnames(Dane)<-c("Life","Adult_M","Infant_D","H_B","Measles","Under_D","Polio","Dipht","GDP","Popl","Income")

#Przedstawienie zmiennej zaleznej na wykresie
plot(Dane$"Life")

#Przedstawienie nastepujacych danych ze zmiennej zaleznej: Minimalna wartosc, Pierwszy kwartal, Mediana, Srednia, Trzeci kwartal, Maksymalna wartosc
summary(Dane$"Life" )

#Przedstawienie histogramy zmiennej zaleznej
hist(Dane$"Life" )

#Przedstawienie korelacji wszystkich zmiennych
cor(Dane)

#Współliniowość 
round(cor(Dane[,-1]),3)

matrix <- as.matrix(Dane[,-1])

matrix_eigen <- eigen(t(matrix) %*% matrix)
matrix_eigen$val

sqrt(matrix_eigen$val[1]/matrix_eigen$val)

#Linear Regression

#Przedstawienie wykresu ze zmienna zaleznom wertykalnie i zmienna 'Adult_M' horyzontalnie
plot(Dane$"Life"~Dane$"Adult_M")

#Tworzenie modelu simple regression za pomoca funkcji lm(linear model). Jako zmienna zaleznom zastosowalem "Life", natomiast zamienna niezalezna to "Adult_M". 
SimpleModel<- lm(Dane$"Life"~Dane$"Adult_M")
summary(SimpleModel)

#Przedstawienie na wykresie niebieskiej lini najbardziej dopasowanej do danych za pomoca wspolczynnika
abline(SimpleModel$coef,col="blue")

#Multiple Regression

#Tworzenie modelu multiple regression, 'Life' jako zmienna zalezna.
Model <- lm(Life~Adult_M+Infant_D+H_B+Measles+Under_D+Polio+Dipht+GDP+Popl+Income,data=Dane)
summary(Model)

#W modelu drugim usuwam 'Measles', dajac spadek wartosci p dla zmiennej 'Under_D'.
Model2 <- lm(Life~Adult_M+Infant_D+H_B+Under_D+Polio+Dipht+GDP+Popl+Income,data=Dane)
summary(Model2)

#W modelu trzecim usuwam 'H_B', dajac spadek wartosci p dla zmiennej 'Dipht'.
Model3 <- lm(Life~Adult_M+Infant_D+Under_D+Polio+Dipht+GDP+Popl+Income,data=Dane)
summary(Model3)

#W modelu czwartym usuwam 'Income'.
Model4 <- lm(Life~Adult_M+Infant_D+Under_D+Polio+Dipht+GDP+Popl,data=Dane)
summary(Model4)

#W modelu piatym usuwam 'Popl', daje wzrost wartosci 'Under_D' i 'Polio', jest niepozadany rezultat, dlatego wybieram model czwarty
Model5 <- lm(Life~Adult_M+Infant_D+Under_D+Polio+Dipht+GDP,data=Dane)
summary(Model5)

#Wybrany model
Model <- lm(Life~Adult_M+Infant_D+Under_D+Polio+Dipht+GDP+Popl,data=Dane)
summary(Model)

#W celu potwierdzenia wyboru uzylem funkcji backward i both, ktore daly identyczny wynik.
Model_backward<-step(lm(Life~Adult_M+Infant_D+H_B+Measles+Under_D+Polio+Dipht+GDP+Popl+Income,data=Dane),direction="backward")
summary(Model_backward)
Model_both<-step(lm(Life~Adult_M+Infant_D+H_B+Measles+Under_D+Polio+Dipht+GDP+Popl+Income,data=Dane),direction="both")
summary(Model_both) 


#Outliers

(RS <- rstudent(Model))

plot(RS,ylab="R-Student residual",main="R-Student residual")

RS[abs(RS)==max(abs(RS))]

#test statistics
dim(Dane)
dim(Dane)[1]
0.05/dim(Dane)[1]*2
dim(Dane)[1]-4-1
qt(0.05/(dim(Dane)[1]*2),(dim(Dane)[1]-4-1))

(-abs(RS[abs(RS)==max(abs(RS))])<qt(0.05/(dim(Dane)[1]*2),(dim(Dane)[1]-4-1)))

#Zidentyfikowanie wpływowych danych, stosujemy statystyki Cooka.
(cooks_dis <- cooks.distance(Model))
plot(cooks_dis,ylab="Cooks distances")

Model_no_outliers <-lm(Life~Adult_M+Infant_D+Under_D+Polio+Dipht+GDP+Popl,data=Dane,subset=(cooks_dis<max(cooks_dis)))

summary(Model_no_outliers)

#Diagnostic
par(mfrow=c(2,1))
plot(Model$res,ylab="Residuals",main="Index plot of residuals")
abline(h=0,col="red")
plot(Model$fit,Model$res,xlab="Fitted",ylab="Residuals")
abline(h=0,col="red")

par(mfrow=c(2,1))
plot(Model_no_outliers$res,ylab="Residuals",main="Index plot of residuals")
abline(h=0,col="red")
plot(Model_no_outliers$fit,Model_no_outliers$res,xlab="Fitted",ylab="Residuals")
abline(h=0,col="red")

#Normality test

boxplot(Dane$Life)
qqnorm(Dane$"Life" )
qqline(Dane$"Life" )

boxplot(Dane$Adult_M)
qqnorm(Dane$"Adult_M" )
qqline(Dane$"Adult_M" )

boxplot(Dane$Infant_D)
qqnorm(Dane$"Infant_D")
qqline(Dane$"Infant_D")

boxplot(Dane$Under_D)
qqnorm(Dane$"Under_D")
qqline(Dane$"Under_D")

boxplot(Dane$Polio)
qqnorm(Dane$"Polio")
qqline(Dane$"Polio")

boxplot(Dane$Dipht)
qqnorm(Dane$"Dipht")
qqline(Dane$"Dipht")

boxplot(Dane$GDP)
qqnorm(Dane$"GDP")
qqline(Dane$"GDP")

boxplot(Dane$Popl)
qqnorm(Dane$"Popl")
qqline(Dane$"Popl") 

library("ggpubr")
ggqqplot(Dane$Life, ylab = "Life")
ggqqplot(Dane$Adult_M, ylab = "Adult_M")
ggqqplot(Dane$Infant_D, ylab = "Infant_D")
ggqqplot(Dane$Under_D, ylab = "Under_D")
ggqqplot(Dane$Polio, ylab = "Polio")
ggqqplot(Dane$Dipht, ylab = "Dipht")
ggqqplot(Dane$GDP, ylab = "GDP")
ggqqplot(Dane$Popl, ylab = "Popl")

par(mfrow=c(1,1))
qqnorm(Model$res,ylab="Residuals")
qqline(Model$res, col="blue")

par(mfrow=c(1,1))
qqnorm(Model_no_outliers$res,ylab="Residuals")
qqline(Model_no_outliers$res, col="blue")

qqnorm(rstudent(Model),ylab="Studentized residuals")
abline(0,1)

qqnorm(rstudent(Model_no_outliers),ylab="Studentized residuals")
abline(0,1)

shapiro.test(Model$res)
shapiro.test(Model2$res)
shapiro.test(Model3$res)
shapiro.test(Model4$res)
shapiro.test(Model5$res)
shapiro.test(Model_no_outliers$res)

#Testing of independence
library(randtests)

runs.test(Model$res)
runs.test(Model_no_outliers$res)

#Calculate VIF for the first variable
explanatory<-as.matrix(Dane[,-1])
summary(lm(explanatory[,1]~explanatory[,-1]))
summary(lm(explanatory[,1]~explanatory[,-1]))$r.squared

#Automatic calculation of the VIF values
library(car)
vif(Model)

#Przedstawienie wspołczynnikow wartosci.
Model$coefficients

#Przypisanie wspolczynnikow do nowych zmiennych
beta_0<-Model$coefficients['(Intercept)']
beta_0
beta_adultm<-Model$coefficients['Adult_M']
beta_adultm
beta_infantd<-Model$coefficients['Infant_D']
beta_infantd
beta_underd<-Model$coefficients['Under_D']
beta_underd
beta_polio<-Model$coefficients['Polio']
beta_polio
beta_dipht<-Model$coefficients['Dipht']
beta_dipht
beta_gdp<-Model$coefficients['GDP']
beta_gdp
beta_popl<-Model$coefficients['Popl']
beta_popl


#Forecast

(forecast<-beta_0+beta_adultm*100+beta_infantd*56+beta_underd*80+beta_polio*70+beta_dipht*80+beta_gdp*300+beta_popl*10000000)
(forecast2<-beta_0+beta_adultm*500+beta_infantd*86+beta_underd*90+beta_polio*73+beta_dipht*94+beta_gdp*456+beta_popl*9080000)
(forecast3<-beta_0+beta_adultm*60+beta_infantd*47+beta_underd*52+beta_polio*64+beta_dipht*45+beta_gdp*291+beta_popl*13768000)


#Package do czytania "read_csv"
library(tidyverse)

#Przydzielenie bazy danych do wektora 'Dane_nowe'
Dane_nowe <-  read_csv("C:/Users/Kacper/Desktop/Projekt Regression/Dane z 2015.csv")
View(Dane_nowe)
names(Dane_nowe)

#Usunięcie
library("dplyr")
Dane_nowe <- Dane_nowe %>% select(-c(Country, Year, Status, Alcohol, `percentage expenditure`, `Total expenditure`))
View(Dane_nowe)
names(Dane_nowe)

#Nowe nazwy
colnames(Dane_nowe)<-c("Life","Adult_M","Infant_D","H_B","Measles","BMI", "Under_D","Polio","Dipht","H/A", "GDP","Popl", "t_1-19", "t_5-9", "Income", "Schooling")

#Zastepowanie ubytków
Dane_nowe$H_B = ifelse(is.na(Dane_nowe$H_B),
                       ave(Dane_nowe$H_B, FUN = function(x) mean(x, na.rm = TRUE)),
                       Dane_nowe$H_B)
Dane_nowe$BMI = ifelse(is.na(Dane_nowe$BMI ),
                       ave(Dane_nowe$BMI , FUN = function(x) mean(x, na.rm = TRUE)),
                       Dane_nowe$BMI )
Dane_nowe$GDP = ifelse(is.na(Dane_nowe$GDP ),
                       ave(Dane_nowe$GDP  , FUN = function(x) mean(x, na.rm = TRUE)),
                       Dane_nowe$GDP  )
Dane_nowe$Popl = ifelse(is.na(Dane_nowe$Popl  ),
                       ave(Dane_nowe$Popl   , FUN = function(x) mean(x, na.rm = TRUE)),
                       Dane_nowe$Popl   )
Dane_nowe$`t_1-19`  = ifelse(is.na(Dane_nowe$`t_1-19`   ),
                        ave(Dane_nowe$`t_1-19`   , FUN = function(x) mean(x, na.rm = TRUE)),
                        Dane_nowe$`t_1-19`   )
Dane_nowe$`t_5-9` = ifelse(is.na(Dane_nowe$`t_5-9`   ),
                        ave(Dane_nowe$`t_5-9`    , FUN = function(x) mean(x, na.rm = TRUE)),
                        Dane_nowe$`t_5-9`    )
Dane_nowe$Income = ifelse(is.na(Dane_nowe$Income    ),
                           ave(Dane_nowe$Income    , FUN = function(x) mean(x, na.rm = TRUE)),
                           Dane_nowe$Income    )
Dane_nowe$Schooling = ifelse(is.na(Dane_nowe$Schooling   ),
                          ave(Dane_nowe$Schooling    , FUN = function(x) mean(x, na.rm = TRUE)),
                          Dane_nowe$Schooling    )

#Współliniowość
matrix_nowy <- as.matrix(Dane_nowe[,-1])

matrix_eigen_nowy <- eigen(t(matrix_nowy) %*% matrix_nowy)
matrix_eigen_nowy$val

sqrt(matrix_eigen_nowy$val[1]/matrix_eigen_nowy$val)

#Model
Model_nowy <- lm(Life~Adult_M+Infant_D+H_B+Measles+BMI+Under_D+Polio+Dipht+`H/A`+GDP+Popl+`t_1-19`+`t_5-9`+Income+Schooling,data=Dane_nowe)
summary(Model_nowy)

#Poprawa modelu
Model_nowy_backward<-step(lm(Life~Adult_M+Infant_D+H_B+Measles+BMI+Under_D+Polio+Dipht+`H/A`+GDP+Popl+`t_1-19`+`t_5-9`+Income+Schooling,data=Dane_nowe),direction="backward")
summary(Model_nowy_backward)
Model_nowy <- Model_nowy_backward
summary(Model_nowy)

#Wartości odstające
cooks_dis <- cooks.distance(Model_nowy)
plot(cooks_dis,ylab="Cooks distances")
Model_nowy_no_outliers <-lm(Life~Adult_M+Infant_D+H_B+Under_D+`H/A`+`t_1-19`+Income,data=Dane_nowe,subset=(cooks_dis<max(cooks_dis)))
summary(Model_nowy_no_outliers)
summary(Model_nowy)

#Test normalności
shapiro.test(Model_nowy$res)
shapiro.test(Model_nowy_no_outliers$res)

#Porównanie
summary(Model)
summary(Model_nowy)

summary(Model_no_outliers)
summary(Model_nowy_no_outliers)

#Podzial danych na training i test
library(caTools)
set.seed(123)
split = sample.split(Dane$Life, SplitRatio = 0.8)
training_set = subset(Dane, split == TRUE)
test_set = subset(Dane, split == FALSE)


regressor = lm(formula = Life ~ .,
               data = training_set)

summary(regressor)

y_pred = predict(regressor, newdata = test_set)
y_pred

split_nowy = sample.split(Dane_nowe$Life, SplitRatio = 0.8)
training_set_nowy = subset(Dane_nowe, split == TRUE)
test_set_nowy = subset(Dane_nowe, split == FALSE)


regressor_nowy = lm(formula = Life ~ .,
               data = training_set_nowy)

summary(regressor_nowy)
summary(regressor)

y_pred_nowy = predict(regressor_nowy, newdata = test_set_nowy)
y_pred_nowy

#Porównanie
y_pred = predict(regressor, newdata = test_set)
y_pred
y_pred_nowy = predict(regressor_nowy, newdata = test_set_nowy)
y_pred_nowy

