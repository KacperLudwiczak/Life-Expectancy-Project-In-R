setwd("C:/")
#use automatic import function to read the golf data
library(readxl)
Dane <-  read_excel("C:/Users/Kacper/Desktop/Projekt Regression/Dane czyste.xlsx")
View(Dane)
names(Dane)
print(Dane$"Life expectancy" )
plot(Dane$"Life expectancy" )
summary(Dane$"Life expectancy" )
hist(Dane$"Life expectancy" )
plot(Dane$"Life expectancy"~Dane$"Adult Mortality")
plot(Dane$"Life expectancy"~Dane$"Schooling" )
#########################################################
#Linear Regression
Model1<- lm(Dane$"Life expectancy"~Dane$"Adult Mortality")
abline(Model1$coef,lty=5)
#########################################################
#Multiple Regression
cor(Dane)
colnames(Dane)<-c("Life","Adult_M","Infant_D","H_B","Measles","BMI","Polio","Diph","HIV","GDP","Popl","Thinness","Income","Schooling")
cor(Dane)
Model <- lm(Life~Adult_M+Infant_D+H_B+Measles+BMI+Polio+Diph+HIV+GDP+Popl+Thinness+Income+Schooling,data=Dane)
summary(Model)
#------------------------------------------------
#Forecast
names(Model)
Model$coefficients
beta_0<-Model$coefficients['(Intercept)']
beta_adultm<-Model$coefficients['Adult_M']
beta_infantd<-Model$coefficients['Infant_D']
beta_h_b<-Model$coefficients['H_B']
beta_measles<-Model$coefficients['Measles']
beta_bmi<-Model$coefficients['BMI']
beta_polio<-Model$coefficients['Polio']
beta_diph<-Model$coefficients['Diph']
beta_hiv<-Model$coefficients['HIV']
beta_gdp<-Model$coefficients['GDP']
beta_popl<-Model$coefficients['Popl']
beta_thinness<-Model$coefficients['Thinness']
beta_income<-Model$coefficients['Income']
beta_schooling<-Model$coefficients['Schooling']

(our_forecast<-beta_0+beta_adultm*100+beta_infantd*56+beta_h_b*60+beta_measles*200+beta_bmi*30+beta_polio*70+beta_diph*80+beta_hiv*0.5+beta_gdp*300+beta_popl*10000000+beta_thinness*10+beta_schooling*13)
(our_forecast<-beta_0+beta_adultm*300+beta_infantd*66+beta_h_b*70+beta_measles*250+beta_bmi*30+beta_polio*80+beta_diph*80+beta_hiv*0.3+beta_gdp*480+beta_popl*10000000+beta_thinness*11+beta_schooling*12)

#------------------------------------------------
#Outliers
jack <- rstudent(Model)
plot(jack,ylab="Jacknife Residuals",main="Jacknife Residuals")
jack[abs(jack)==max(abs(jack))]
#test statistics
qt(0.05/(dim(Dane)[1]*2),(dim(Dane)[1]-4-1))
#Bacause of
(-abs(jack[abs(jack)==max(abs(jack))])<qt(0.05/(dim(Dane)[1]*2),(dim(Dane)[1]-4-1)))
#we reject the hypothesis the value is a regular one, so it is an outlier

#Influential data
#To identify influential data we apply the Cook statistics.
cook <- cooks.distance(Model)
plot(cook,ylab="Cooks distances")
identify(1:dim(Dane)[1],cook,row.names(Dane))

Model_subset <-lm(Life~Adult_M+Infant_D+H_B+Measles+BMI+Polio+Diph+HIV+GDP+Popl+Thinness+Income+Schooling,data=Dane,subset=(cook<max(cook)))

#Compare the results of the full model with the reduced one
summary(Model_subset)
summary(Model)

#Alternative calculations
# number_cook<-which(cook==max(cook))
# Model_subset_alternative <- lm(Price~Years+Mileage+Power+Capacity,data=Dane,subset=(row.names(Dane)!=number_cook))
# summary(Model_subset_alternative)
#------------------------------------------------

#Diagnostic
par(mfrow=c(2,1))
plot(Model$res,ylab="Residuals",main="Index plot of residuals")
abline(h=0,col="red")
plot(Model$fit,Model$res,xlab="Fitted",ylab="Residuals")
abline(h=0,col="red")

#Normality test
par(mfrow=c(1,1))
qqnorm(Model$res,ylab="Residuals")
qqline(Model$res)

qqnorm(rstudent(Model),ylab="Studentized residuals")
abline(0,1)

shapiro.test(Model$res)

#Testing of independence
library(randtests)
?runs.test
runs.test(Model$res)

#Collinearity
#One of the motivation: There are explanatory variables with large p-values. However, all of them might be expected to affect the endogeneous variable. Why aren't they significant? Check the collinearity problem. First step - analyse correlation matrix)
round(cor(Dane[,-1]),3)
#Let's analyse condition numbers?
matrix <- as.matrix(Dane[,-1])
e_matrix <- eigen(t(matrix) %*% matrix)
e_matrix$val
sqrt(e_matrix$val[1]/e_matrix$val)
#Calculate VIF for the first variable
explanatory<-as.matrix(Dane[,-1])
summary(lm(explanatory[,1]~explanatory[,-1]))$r.squared

#Automatic calculation of the VIF values
library(car)
vif(Model)






#------------------------------------------------
#Selection of independent variables - stepwise method
#RSS=Residual sum-of-squares of a fitted model
Model_forward<-step(lm(Price~Years+Mileage+Power+Capacity,data=Dane),direction="forward")
Model_forward
names(Model_forward)
Model_forward$formula
Model_backward<-
  Model_backward
Model_both<-
  Model_both
summary(Model_both)
#Choose the best model
