library(dslabs)
library(tidyverse)
library(wbstats)
library(knitr)

##DATA PREPARATION
#Defining the list of countries
countrylist <- c(Mauritius = "MUS", Malta = "MLT", "Singapore" = "SGP")

#Grouping all economic indicators into one vector
my_indicators <- c("GDP" = "NY.GDP.MKTP.KD", "Domestic Investment" = "NE.GDI.TOTL.ZS", "FDI" = "BX.KLT.DINV.WD.GD.ZS", "Manufacturing" = "NV.IND.MANF.ZS", "Current Account" = "BN.CAB.XOKA.GD.ZS", "Working Pop" = "SP.POP.1564.TO.ZS")

#downloading the data 
df <- wb_data(indicator = my_indicators, country = countrylist, start_date = 1976, end_date = 2019)

#Saving downloaded data
df <- save(df, file = "coyvcheongtook.RData")

#descriptive statistics of the data frame, df
summary(df)

##DATA WRANGLING
#Data Splitting Into Countries
#Singapore
df_singapore <- df %>%
  filter(country == "Singapore")

#Malta Data
df_malta <- df %>%
  filter(country == "Malta")

#Mauritius
df_mauritius <- df %>%
  filter(country == "Mauritius")

##PRE PROCESSING OF DATA
##SINGAPORE

#Descriptive Statistics of Singapore data
summary(df_singapore)
attach(df_singapore)

##DEEP DIVE ANALYSIS
#Number of years manufacturing sector contribution is greater or equal to 20%
df_singapore_manf <- df_singapore %>%
  filter(Manufacturing >= 20)
nrow(df_singapore_manf)

#Number of years Domestic Investment exceeded or equalled to 35%
df_singapore_dominv <- df_singapore %>%
  filter(`Domestic Investment` >= 35)
nrow(df_singapore_dominv)

#Number of years Current Account deficit exceeds or equals to 5%
df_singapore_currentacc <- df_singapore %>%
  filter(`Current Account` <= -5)
nrow(df_singapore_currentacc)

##Exploratory Graphics of Independent Variables with GDP
#Domestic Investment against GDP
plot(df_singapore$`Domestic Investment`, df_singapore$GDP)
lines(lowess(df_singapore$`Domestic Investment`, df_singapore$GDP), col = "blue")

#FDI against GDP
plot(df_singapore$FDI, df_singapore$GDP)
lines(lowess(df_singapore$FDI, df_singapore$GDP), col = "red")

#Current Account against GDP
plot(df_singapore$`Current Account`, df_singapore$GDP)
lines(lowess(df_singapore$`Current Account`, df_singapore$GDP), col = "green")

#Working Population against GDP
plot(df_singapore$`Working Pop`, df_singapore$GDP)
lines(lowess(df_singapore$`Working Pop`, df_singapore$GDP), col = "grey")

#Manufacturing against GDP
plot(df_singapore$Manufacturing, df_singapore$GDP)
lines(lowess(df_singapore$Manufacturing, df_singapore$GDP), col = "blue")


##CORRELATION TESTS - GDP
cor(FDI, GDP, method = "spearman")
cor(`Domestic Investment`, GDP, method = "spearman")
cor(Manufacturing, GDP, method = "spearman")
cor(`Working Pop`, GDP, method = "spearman")
cor(`Current Account`, GDP, method = "spearman")

##Relationship of FDI and Manufacturing, Domestic Investment and Manufacturing
plot(df_singapore$FDI, df_singapore$Manufacturing)
lines(lowess(df_singapore$FDI, df_singapore$Manufacturing), col = "grey")

plot(df_singapore$`Domestic Investment`, df_singapore$Manufacturing)
lines(lowess(df_singapore$`Domestic Investment`, df_singapore$Manufacturing), col = "red")


##Correlation Test - FDI and Manufacturing, Domestic Investment and Manufacturing
attach(df_singapore)

#FDI and Manufacuring
cor(FDI, Manufacturing, method = "spearman")

#Domestic Investment and Manufacturing
cor(`Domestic Investment`, Manufacturing, method = "spearman")


##REGRESSION ANALYSIS
#Model 1 - With all indicators
fit_singapore <- lm(GDP ~ FDI + `Domestic Investment` + Manufacturing + `Current Account` + `Working Pop`, data = df_singapore)
summary(fit_singapore)

##ASSESSMENT OF MULTICOLLINEARITY
library(broom)
library(car)

#ACF
broom::glance(fit_singapore)

#Variance Inflation Factor (VIF)
vif(fit_singapore)

#Correlation Matrix
cor_matrix_sing <- cor(df_singapore[c("FDI", "Domestic Investment", "Manufacturing", "Working Pop", "Current Account")])
cor(cor_matrix_sing)

##Model 2 with Manufacturing and Domestic Investment only
fit_singapore.model2 <- lm(GDP ~ log(`Domestic Investment`) + log(Manufacturing), data = df_singapore)

summary(fit_singapore.model2)

broom::glance(fit_singapore.model2)

##Model 3 - Introducing An Interaction Term
#Creating an interaction term - FDI and Current Account
df_singapore$FDI.CurrAcc <- interaction(FDI, `Current Account`)

#Regression  with the interaction term
fit_singapore_interaction <- lm(GDP ~ FDI + `Current Account` + FDI.CurrAcc, data = df_singapore)

summary(fit_singapore_interaction)

#Model 4 - Polynomial Regression
quad.sing.3 <- GDP ~ FDI + I(FDI^2) + log(Manufacturing) + log(`Domestic Investment`)
fit_singapore_quadratic <- lm(quad.sing.3, data = df_singapore)

summary(fit_singapore_quadratic)

broom::glance(fit_singapore_quadratic)

##Test for Sequential Correlation
acf(residuals(fit_singapore_quadratic))

plot(predict(fit_singapore_quadratic), residuals(fit_singapore_quadratic))


##MALTA

#Descriptive Statistics of Maltese data
summary(df_malta)

#Boxplot of FDI - identifying outliers
boxplot(df_malta$FDI)

#Removing outliers from the Maltese data
df_malta_clean <- df_malta %>%
  filter(FDI <= 100)

#Descriptive statistics of cleaned data
summary(df_malta_clean)

##DEEP DIVE ANALYSIS
#Number of years Malta's manufacturing sector contribution equals to or exceeds 20%
df_malta_clean_manf <- df_malta_clean %>%
  filter(Manufacturing >= 20)
nrow(df_malta_clean_manf)

#Number of years that Domestic Investment exceeds or equals to 25%
df_malta_clean_dominv <- df_malta_clean %>%
  filter(`Domestic Investment` >= 25)
nrow(df_malta_clean_dominv)

#Number of years that Current Account deficit equals to or exceeds 5%
df_malta_clean_currentacc <- df_malta_clean %>%
  filter(`Current Account` <= -5)
nrow(df_malta_clean_currentacc)

##Exploratory Graphics Of Independent Variables With GDP

#Plot of FDI against GDP
plot(df_malta_clean$FDI, df_malta_clean$GDP)
lines(lowess(df_malta_clean$FDI, df_malta_clean$GDP), col = "blue")

#Plot Manufacturing against GDP
plot(Manufacturing, GDP)
lines(lowess(Manufacturing, GDP), col = "darkblue")

#Plot Domestic Investment against GDP
plot(`Domestic Investment`, GDP)
lines(lowess(`Domestic Investment`, GDP), col = "red")

#Plot Current Account against GDP
plot(`Current Account`, GDP)
lines(lowess(`Current Account`, GDP), col = "green")

#Plot Working Population against GDP
plot(`Working Pop`, GDP)
lines(lowess(`Working Pop`, GDP), col = "darkblue")

attach(df_malta_clean)

#CORRELATION TESTS - GDP
cor(FDI, GDP, method = "spearman")
cor(`Domestic Investment`, GDP, method = "spearman")
cor(Manufacturing, GDP, method = "spearman")
cor(`Working Pop`, GDP, method = "spearman")
cor(`Current Account`, GDP, method = "spearman")

##Relationship of FDI and Manufacturing, Domestic Investment and Manufacturing
#Plot FDI against Manufacturing
plot(FDI, Manufacturing)
lines(lowess(FDI, Manufacturing), col = "darkgrey")

#Plot Domestic Investment against Manufacturing
plot(`Domestic Investment`, Manufacturing)
lines(lowess(`Domestic Investment`, Manufacturing), col = "darkred")

#Correlation Test - FDI and Manufacturing
cor(FDI, Manufacturing, method = "spearman")

#Correlation Test - Domestic Investment and Manufacturing
cor(`Domestic Investment`, Manufacturing, method = "spearman")

##REGRESSION ANALYSIS
#Model 1 - With all indicators
fit_malta <- lm(GDP ~ FDI + `Working Pop` + `Current Account` + Manufacturing + `Domestic Investment`, data = df_malta_clean)
summary(fit_malta)

##ASSESSMENT OF MULITCOLLINEARITY

#Variance Inflation Factor
vif(fit_malta)

#Correlation Matrix
cor_matrix_malta <- cor(df_malta_clean[c("FDI", "Domestic Investment", "Manufacturing", "Working Pop", "Current Account")])
cor(cor_matrix_malta)

#ACF
broom::glance(fit_malta)

#Model 2 - With one indicator - Manufacturing
fit_malta_1 <- lm(GDP ~ Manufacturing, data = df_malta_clean)

summary(fit_malta_1)

broom::glance(fit_malta_1)

#Model 3 - With two indicators - Manufacturing and Current Account
fit_malta_2 <- lm(GDP ~ Manufacturing + `Current Account`, data = df_malta_clean)

summary(fit_malta_2)

broom::glance(fit_malta_2)

##Assessment of Model 5
#Plot of fitted values against residuals
plot(predict(fit_malta_2), residuals(fit_malta_2))

#Model 4 - with an interaction term using FDI and Manufacturing
#Introducing an interaction term - FDI and Manufacturing
df_malta_clean$FDIManf <- interaction(df_malta_clean$FDI, df_malta_clean$Manufacturing)

#Regression with the interaction term
fit_malta_interaction <- lm(GDP ~ FDI + Manufacturing + FDIManf, data = df_malta_clean)

summary(fit_malta_interaction)

broom::glance(fit_malta_interaction)

#Model 5 - polynomial regression
attach(df_malta_clean)
quad.malta2 <- GDP ~ FDI + I(FDI^2) + `Current Account`

fit_malta_quadratic <- lm(quad.malta2, data = df_malta_clean)

summary(fit_malta_quadratic)

broom::glance(fit_malta_quadratic)

##Assessment of Sequential Correlation - Model 3
acf(residuals(fit_malta_2))


##MAURITIUS
#Exploratory Data Analysis of Mauritius dataset
summary(df_mauritius)

##DEEP DIVE ANALYSIS
#No of years the Manufacturing sector contribution equal to or exceeds 20%
df_mauritius_manf <- df_mauritius %>%
  filter(Manufacturing >= 20)
nrow(df_mauritius_manf)
#No of years Domestic Investment level equals to or exceeds 25%
df_mauritius_dominv <- df_mauritius %>%
  filter(`Domestic Investment` >= 25)
nrow(df_mauritius_dominv)
#No of years Current Account Deficit equals to or exceeds 5%
df_mauritius_currentacc <- df_mauritius %>%
  filter(`Current Account` <= -5)
nrow(df_mauritius_currentacc)

##Exploratory Graphics Of Independent Variables With GDP
attach(df_mauritius)

# Plot Manufacturing against GDP
plot(Manufacturing, GDP)
lines(lowess(Manufacturing, GDP), col = "blue")

#Plot FDI against GDP
plot(FDI, GDP)
lines(lowess(FDI, GDP), col = "red")

#Plot Working Population against GDP
plot(`Working Pop`, GDP)
lines(lowess(`Working Pop`, GDP), col = "green")

#Plot Current Account against GDP
plot(`Current Account`, GDP)
lines(lowess(`Working Pop`, GDP), col = "grey")

#Plot Domestic Investment against GDP
plot(`Domestic Investment`, GDP)
scatterplot(`Domestic Investment` ~ GDP, data = df_mauritius, smooth = FALSE, grid = FALSE)


#CORRELATION TESTS - GDP
cor(FDI, GDP, method = "spearman")
cor(`Domestic Investment`, GDP, method = "spearman")
cor(Manufacturing, GDP, method = "spearman")
cor(`Working Pop`, GDP, method = "spearman")
cor(`Current Account`, GDP, method = "spearman")


##Relationship of FDI and Manufacturing, Domestic Investment and Manufacturing
#FDI and Manufacturing
plot(FDI, Manufacturing)
lines(lowess(FDI, Manufacturing), col = "darkgreen")

#Domestic Investment and Manufacturing
plot(`Domestic Investment`, Manufacturing)
lines(lowess(`Domestic Investment`, Manufacturing), col = "black")


#Correlation Test - FDI and Manufacturing
cor(FDI, Manufacturing, method = "spearman")

#Correlation Test - Domestic Investment and Manufacturing
cor(`Domestic Investment`, Manufacturing, method = "spearman")


#REGRESSION ANALYSIS
#Model 1 - With all indicators
fit_mauritius <- lm(GDP ~ Manufacturing + `Current Account` + `Domestic Investment` + FDI + `Working Pop`, data = df_mauritius)

summary(fit_mauritius)

broom::glance(fit_mauritius)

##ASSESSMENT OF MULTICOLLINEARITY

#Variance Inflation Factor
vif(fit_mauritius)

#Correlation Matrix
cor_matrix_mtius <- cor(df_mauritius[c("FDI", "Working Pop", "Current Account", "Domestic Investment", "Manufacturing")])
cor(cor_matrix_mtius)

#Model 2 - Polynomial Regression with FDI
quad.mtius1 <- GDP ~ FDI + I(FDI^2)

fit_mauritius_quadratic <- lm(quad.mtius1, data = df_mauritius)

summary(fit_mauritius_quadratic)

broom::glance(fit_mauritius_quadratic)


#Model 3 - With 2 indicators: Working Population and Current Account
fit_mauritius2 <- lm(GDP ~ `Current Account` + `Working Pop`, data = df_mauritius)

summary(fit_mauritius2)

broom::glance(fit_mauritius2)


#Model 4  - Excluding FDI
fit_mauritius.excFDI <- lm(GDP ~ `Working Pop` + `Current Account` + `Domestic Investment` + Manufacturing, data = df_mauritius)

summary(fit_mauritius.excFDI)

broom::glance(fit_mauritius.excFDI)

##Assessment of Sequential Correlation - Model 4
acf(fit_mauritius.excFDI)

#Plot fitted values against residuals
plot(predict(fit_mauritius.excFDI), residuals(fit_mauritius.excFDI))

##Analysis of Variance - Model 3, Model 4, Model 1
anova(fit_mauritius2, fit_mauritius.excFDI, fit_mauritius)

##CROSS VALIDATION OF RESULTS
#Singapore
attach(df_singapore)
library(boot)
boot.fn_sing <- function(data, index)
  return(coef(lm(quad.sing.3, data = df_singapore)))

boot.fn_sing(df_singapore, 1:44)

boot(df_singapore, boot.fn_sing, 44)











