# Read in File
Experiment.df <- read.csv("Experiment.csv")

# Load in Packages
library(tidyverse)
library(ggplot2)
library(glmnet)
library(latex2exp)
library(readr)

# View and Explore Experiment.df
view(Experiment.df)
nrow(Experiment.df)
names(Experiment.df)

## Plot Data (no model)
# Force in vs Connecting Tube length
plot(Experiment.df$Tube.Length..mm., Experiment.df$Input.Average..N., xlab = 'Connecting Tube Length (mm)', ylab = "Force Input (N)")
# Force in vs Press Area
plot(Experiment.df$Press.Area..mm.2., Experiment.df$Input.Average..N., xlab = 'Press Area (mm^2)', ylab = "Force Input (N)")
# Force in vs Liquid Used
ggplot(Experiment.df) +
  geom_point(aes(x = Liquid.Viscocity, y = Input.Average..N.))
# Force in vs Liquid Contamination 
ggplot(Experiment.df) +
  geom_point(aes(x = Liquid.Contamination, y = Input.Average..N.))
#---------------------------------------------------------------------------------------------------------------------------------------------------

#### Assumption Testing For simple ANOVA tests as well as for Simple Linear regression 
### Assumption Testing For Liquid Type used
AOVFIT_LU <- lm(Experiment.df$Input.Average..N. ~ Experiment.df$Liquid.Viscocity)
summary(AOVFIT_LU)
# Checking Independence, Constant Variance and Normality 
par(mfrow = c(2,2))
plot(AOVFIT_LU$fitted.values,AOVFIT_LU$residuals,
     xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(AOVFIT_LU$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(AOVFIT_LU$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(AOVFIT_LU$residuals)
qqline(AOVFIT_LU$residuals)
# Test for homogeneity of variances
Levene <- function(y, group){
  group <- as.factor(group)  # precautionary
  meds <- tapply(y, group, median)
  resp <- abs(y - meds[group])
  anova(lm(resp ~ group))[1, 4:5]
}
Levene(Experiment.df$Input.Average..N., Experiment.df$Liquid.Viscocity)

### Assumption Testing For Contamination 
AOVFIT_LC <- lm(Experiment.df$Input.Average..N. ~ Experiment.df$Liquid.Contamination)
summary(AOVFIT_LC)
# Checking Independence, Constant Variance and Normality 
par(mfrow = c(2,2))
plot(AOVFIT_LC$fitted.values,AOVFIT_LC$residuals,
     xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(AOVFIT_LC$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(AOVFIT_LC$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(AOVFIT_LC$residuals)
qqline(AOVFIT_LC$residuals)
# Test for homogeneity of variances
Levene <- function(y, group){
  group <- as.factor(group)  # precautionary
  meds <- tapply(y, group, median)
  resp <- abs(y - meds[group])
  anova(lm(resp ~ group))[1, 4:5]
}
Levene(Experiment.df$Input.Average..N., Experiment.df$Liquid.Contamination)

### Assumption Testing For Press Area 
## Simple linear regression
# Linearity (for continuous variables) and Constant variance
SRPA.w <- lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2.)
summary(SRPA.w)
par(mfrow = c(2,2))
plot(SRPA.w$fitted.values,SRPA.w$residuals,
     xlab="Fitted Values",ylab="Residuals",
     main="Residuals vs Fitted Values")
abline(h=0,col='red')
# Independence
plot(SRPA.w$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
# Normality
hist(SRPA.w$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(SRPA.w$residuals)
qqline(SRPA.w$residuals)
## ANOVA model
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
AOVFIT_PA <- lm(Experiment.df$Input.Average..N. ~ Experiment.df$Press.Area..mm.2.)
summary(AOVFIT_PA)
# Checking Independence, Constant Variance and Normality 
par(mfrow = c(2,2))
plot(AOVFIT_PA$fitted.values,AOVFIT_PA$residuals,
     xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(AOVFIT_PA$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(AOVFIT_PA$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(AOVFIT_PA$residuals)
qqline(AOVFIT_PA$residuals)
# Test for homogeneity of variances
Levene <- function(y, group){
  group <- as.factor(group)  # precautionary
  meds <- tapply(y, group, median)
  resp <- abs(y - meds[group])
  anova(lm(resp ~ group))[1, 4:5]
}
Levene(Experiment.df$Input.Average..N., Experiment.df$Press.Area..mm.2.)

### Assumption Testing For Tube Length
## Simple linear regression
# Linearity (for continuous variables) and Constant variance
SRTL.w <- lm(data = Experiment.df, Input.Average..N. ~ Tube.Length..mm.)
summary(SRTL.w)
par(mfrow = c(2,2))
plot(SRTL.w$fitted.values,SRTL.w$residuals,
     xlab="Fitted Values",ylab="Residuals",
     main="Residuals vs Fitted Values")
abline(h=0,col='red')
# Independence
plot(SRTL.w$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
# Normality
hist(SRTL.w$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(SRTL.w$residuals)
qqline(SRTL.w$residuals)
## ANOVA model
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
AOVFIT_TL <- lm(Experiment.df$Input.Average..N. ~ Experiment.df$Tube.Length..mm.)
summary(AOVFIT_TL)
# Checking Independence, Constant Variance and Normality 
par(mfrow = c(2,2))
plot(AOVFIT_TL$fitted.values,AOVFIT_TL$residuals,
     xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(AOVFIT_TL$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(AOVFIT_TL$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(AOVFIT_TL$residuals)
qqline(AOVFIT_TL$residuals)
# Test for homogeneity of variances
Levene <- function(y, group){
  group <- as.factor(group)  # precautionary
  meds <- tapply(y, group, median)
  resp <- abs(y - meds[group])
  anova(lm(resp ~ group))[1, 4:5]
}
Levene(Experiment.df$Input.Average..N., Experiment.df$Tube.Length..mm.)
#---------------------------------------------------------------------------------------------------------------------------------------------------

## Transform the data for Pump area and Liquid viscosity.
# Log transformation for Y
array = as.array(Experiment.df$Input.Average..N.)
LogInputAv = log(array)

#### Assumption Testing For simple ANOVA tests as well as for Simple Linear regression using the log(InputForceAverage)
### Assumption Testing For Liquid Type used
AOVFIT_LU <- lm(LogInputAv ~ Experiment.df$Liquid.Viscocity)
summary(AOVFIT_LU)
# Checking Independence, Constant Variance and Normality 
par(mfrow = c(2,2))
plot(AOVFIT_LU$fitted.values,AOVFIT_LU$residuals,
     xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(AOVFIT_LU$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(AOVFIT_LU$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(AOVFIT_LU$residuals)
qqline(AOVFIT_LU$residuals)
# Test for homogeneity of variances
Levene <- function(y, group){
  group <- as.factor(group)  # precautionary
  meds <- tapply(y, group, median)
  resp <- abs(y - meds[group])
  anova(lm(resp ~ group))[1, 4:5]
}
Levene(LogInputAv, Experiment.df$Liquid.Viscocity)

### Assumption Testing For Contamination 
AOVFIT_LC <- lm(LogInputAv ~ Experiment.df$Liquid.Contamination)
summary(AOVFIT_LC)
# Checking Independence, Constant Variance and Normality 
par(mfrow = c(2,2))
plot(AOVFIT_LC$fitted.values,AOVFIT_LC$residuals,
     xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(AOVFIT_LC$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(AOVFIT_LC$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(AOVFIT_LC$residuals)
qqline(AOVFIT_LC$residuals)
# Test for homogeneity of variances
Levene <- function(y, group){
  group <- as.factor(group)  # precautionary
  meds <- tapply(y, group, median)
  resp <- abs(y - meds[group])
  anova(lm(resp ~ group))[1, 4:5]
}
Levene(LogInputAv, Experiment.df$Liquid.Contamination)

### Assumption Testing For Press Area 
## Simple linear regression
# Linearity (for continuous variables) and Constant variance
SRPA.w <- lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2.)
summary(SRPA.w)
par(mfrow = c(2,2))
plot(SRPA.w$fitted.values,SRPA.w$residuals,
     xlab="Fitted Values",ylab="Residuals",
     main="Residuals vs Fitted Values")
abline(h=0,col='red')
# Independence
plot(SRPA.w$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
# Normality
hist(SRPA.w$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(SRPA.w$residuals)
qqline(SRPA.w$residuals)
## ANOVA model
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
AOVFIT_PA <- lm(LogInputAv ~ Experiment.df$Press.Area..mm.2.)
summary(AOVFIT_PA)
# Checking Independence, Constant Variance and Normality 
par(mfrow = c(2,2))
plot(AOVFIT_PA$fitted.values,AOVFIT_PA$residuals,
     xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(AOVFIT_PA$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(AOVFIT_PA$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(AOVFIT_PA$residuals)
qqline(AOVFIT_PA$residuals)
# Test for homogeneity of variances
Levene <- function(y, group){
  group <- as.factor(group)  # precautionary
  meds <- tapply(y, group, median)
  resp <- abs(y - meds[group])
  anova(lm(resp ~ group))[1, 4:5]
}
Levene(LogInputAv, Experiment.df$Press.Area..mm.2.)

### Assumption Testing For Tube Length
## Simple linear regression
# Linearity (for continuous variables) and Constant variance
SRTL.w <- lm(data = Experiment.df, Input.Average..N. ~ Tube.Length..mm.)
summary(SRTL.w)
par(mfrow = c(2,2))
plot(SRTL.w$fitted.values,SRTL.w$residuals,
     xlab="Fitted Values",ylab="Residuals",
     main="Residuals vs Fitted Values")
abline(h=0,col='red')
# Independence
plot(SRTL.w$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
# Normality
hist(SRTL.w$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(SRTL.w$residuals)
qqline(SRTL.w$residuals)
## ANOVA model
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
AOVFIT_TL <- lm(LogInputAv ~ Experiment.df$Tube.Length..mm.)
summary(AOVFIT_TL)
# Checking Independence, Constant Variance and Normality 
par(mfrow = c(2,2))
plot(AOVFIT_TL$fitted.values,AOVFIT_TL$residuals,
     xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(AOVFIT_TL$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(AOVFIT_TL$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(AOVFIT_TL$residuals)
qqline(AOVFIT_TL$residuals)
# Test for homogeneity of variances
Levene <- function(y, group){
  group <- as.factor(group)  # precautionary
  meds <- tapply(y, group, median)
  resp <- abs(y - meds[group])
  anova(lm(resp ~ group))[1, 4:5]
}
Levene(LogInputAv, Experiment.df$Tube.Length..mm.)

#---------------------------------------------------------------------------------------------------------------------------------------------------

### Simple Models, Model Force Input for each Factor and simple ANOVA Tests
## Simple Linear Regression Using Press Area
SRPA.w <- lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2.)
summary(SRPA.w)
plot(Experiment.df$Press.Area..mm.2.,Experiment.df$Input.Average..N.)
abline(SRPA.w, col = 'red')
## ANOVA Using Press Area and Tukeys Test
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
APA.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Press.Area..mm.2.)
summary(APA.aov)
# Tukey's HSD
TUKAPA <- TukeyHSD(APA.aov)
TUKAPA
plot(TUKAPA)

## Simple Linear Regression Using Connecting Tube Length
SRTL.w <- lm(data = Experiment.df, Input.Average..N. ~ Tube.Length..mm.)
summary(SRTL.w)
plot(Experiment.df$Tube.Length..mm.,Experiment.df$Input.Average..N.)
abline(SRTL.w, col = 'red')
## ANOVA Using Connecting Tube Length and Tukeys Test
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
ATL.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Tube.Length..mm. )
summary(ATL.aov)
# Tukey's HSD
TUKATL <- TukeyHSD(ATL.aov)
TUKATL
plot(TUKATL)

## ANOVA Using Liquid Used and Tukeys Test
ALU.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Liquid.Viscocity)
summary(ALU.aov)
# Tukey's HSD
TUKALU <- TukeyHSD(ALU.aov)
TUKALU
plot(TUKALU)

## ANOVA Using Liquid Contamination and Tukeys Test
ALC.aov <- aov(data = Experiment.df, Input.Average..N. ~ Liquid.Contamination)
summary(ALC.aov)
# Tukey's HSD
TUKALC <- TukeyHSD(ALC.aov)
TUKALC
plot(TUKALC)


#---------------------------------------------------------------------------------------------------------------------------------------------------

#### ANCOVA Assumption Testing

### The assumption of "The value of the covariate does not depend on treatment" is met since the press area
### and the tube length was measured before the experiment was conducted

### Assumption testing For Liquid contamination with press area covariate 
## Fitting the linear model 
Experiment.df$Liquid.Contamination <- as.factor(Experiment.df$Liquid.Contamination) 
LCPA.lm <- lm(Input.Average..N. ~ Press.Area..mm.2. + Liquid.Contamination, data = Experiment.df)
## Linearity, independence, constant variance and normality checks
Experiment.df$Liquid.Contamination <- as.factor(Experiment.df$Liquid.Contamination) 
par(mfrow = c(2,3))
plot(Experiment.df$Press.Area..mm.2.,LCPA.lm$residuals,xlab="Press Area (mm^2)",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(Experiment.df$Liquid.Contamination,LCPA.lm$residuals,xlab="Liquid Contamination",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(LCPA.lm$fitted.values,LCPA.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(LCPA.lm$residuals,type = "l",xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(LCPA.lm$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(LCPA.lm$residuals)
qqline(LCPA.lm$residuals)
## Assumption check for the interaction
ancova_checkLCPA <- aov(Input.Average..N. ~ Press.Area..mm.2.*Liquid.Contamination, data = Experiment.df)
summary(ancova_checkLCPA)
## Testing For Unusual Observations(linearity)
X <- model.matrix(Input.Average..N. ~ Press.Area..mm.2. + Liquid.Contamination, data = Experiment.df)
k <- NCOL(X) - 1
n <- nrow(X)
# Creating the data frame of metrics for unusual observations
Unusual_Observations <- data.frame(leverage = hat(X),
                                   standardised = rstandard(LCPA.lm),
                                   studentised = rstudent(LCPA.lm),
                                   cooks = cooks.distance(LCPA.lm))
# Identifying unusual observations
which(Unusual_Observations$leverage>(2*(k+1)/n))
which(abs(Unusual_Observations$standardised)>2)
which(abs(Unusual_Observations$studentised)>2)
which(Unusual_Observations$cooks>1)
# Outliers are
X[11,]
X[12,]

### Assumption testing For Liquid contamination with Tube Length covariate 
## Fitting the linear model 
Experiment.df$Liquid.Contamination <- as.factor(Experiment.df$Liquid.Contamination) 
LCTL.lm <- lm(Input.Average..N. ~ Tube.Length..mm. + Liquid.Contamination, data = Experiment.df)
## Linearity, independence, constant variance and normality checks
par(mfrow = c(2,3))
plot(Experiment.df$Tube.Length..mm.,LCTL.lm$residuals,xlab="Tube Length (mm)",ylab="Residuals",main="Residuals vs Tube Length")
abline(h=0,col='red')
plot(Experiment.df$Liquid.Contamination,LCTL.lm$residuals,xlab="Liquid Contamination",ylab="Residuals",main="Residuals vs Tube Length")
abline(h=0,col='red')
plot(LCTL.lm$fitted.values,LCTL.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(LCTL.lm$residuals,type = "l",xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(LCTL.lm$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(LCTL.lm$residuals)
qqline(LCTL.lm$residuals)
## Assumption check for the interaction
ancova_checkLCTL <- aov(Input.Average..N. ~ Tube.Length..mm.*Liquid.Contamination, data = Experiment.df)
summary(ancova_checkLCTL)
## Testing For Unusual Observations(linearity)
X <- model.matrix(Input.Average..N. ~ Tube.Length..mm. + Liquid.Contamination, data = Experiment.df)
k <- NCOL(X) - 1
n <- nrow(X)
# Creating the data frame of metrics for unusual observations
Unusual_Observations <- data.frame(leverage = hat(X),
                                   standardised = rstandard(LCTL.lm),
                                   studentised = rstudent(LCTL.lm),
                                   cooks = cooks.distance(LCTL.lm))
# Identifying unusual observations
which(Unusual_Observations$leverage>(2*(k+1)/n))
which(abs(Unusual_Observations$standardised)>2)
which(abs(Unusual_Observations$studentised)>2)
which(Unusual_Observations$cooks>1)
# Outliers are
X[9,]
X[10,]
X[11,]
X[12,]


### Assumption testing For Liquid Used with press area covariate
## Fitting the linear model 
Experiment.df$Liquid.Viscocity <- as.factor(Experiment.df$Liquid.Viscocity) 
LUPA.lm <- lm(Input.Average..N. ~ Press.Area..mm.2. + Liquid.Viscocity, data = Experiment.df)
## Linearity, independence, constant variance and normality checks
par(mfrow = c(2,3))
plot(Experiment.df$Press.Area..mm.2.,LUPA.lm$residuals,xlab="Press Area (mm^2)",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(Experiment.df$Liquid.Viscocity,LUPA.lm$residuals,xlab="Liquid Viscocity",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(LUPA.lm$fitted.values,LUPA.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(LUPA.lm$residuals,type = "l",xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(LUPA.lm$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(LUPA.lm$residuals)
qqline(LUPA.lm$residuals)
## Assumption check for the interaction
ancova_checkLUPA <- aov(Input.Average..N. ~ Press.Area..mm.2.*Liquid.Viscocity, data = Experiment.df)
summary(ancova_checkLUPA)
## Testing For Unusual Observations(linearity)
X <- model.matrix(Input.Average..N. ~ Press.Area..mm.2. + Liquid.Viscocity, data = Experiment.df)
k <- NCOL(X) - 1
n <- nrow(X)
# Creating the data frame of metrics for unusual observations
Unusual_Observations <- data.frame(leverage = hat(X),
                                   standardised = rstandard(LUPA.lm),
                                   studentised = rstudent(LUPA.lm),
                                   cooks = cooks.distance(LUPA.lm))
# Identifying unusual observations
which(Unusual_Observations$leverage>(2*(k+1)/n))
which(abs(Unusual_Observations$standardised)>2)
which(abs(Unusual_Observations$studentised)>2)
which(Unusual_Observations$cooks>1)
# Outliers are
X[11,]
X[12,]

### Assumption testing For Liquid Used with Tube Length covariate 
## Fitting the linear model 
Experiment.df$Liquid.Viscocity <- as.factor(Experiment.df$Liquid.Viscocity) 
LUTL.lm <- lm(Input.Average..N. ~ Tube.Length..mm. + Liquid.Viscocity, data = Experiment.df)
## Linearity, independence, constant variance and normality checks
par(mfrow = c(2,3))
plot(Experiment.df$Tube.Length..mm.,LUTL.lm$residuals,xlab="Tube Length (mm)",ylab="Residuals",main="Residuals vs Tube Length")
abline(h=0,col='red')
plot(Experiment.df$Liquid.Viscocity,LUTL.lm$residuals,xlab="Liquid Viscocity",ylab="Residuals",main="Residuals vs Tube Length")
abline(h=0,col='red')
plot(LUTL.lm$fitted.values,LUTL.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(LUTL.lm$residuals,type = "l",xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(LUTL.lm$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(LUTL.lm$residuals)
qqline(LUTL.lm$residuals)
## Assumption check for the interaction
ancova_checkLUTL <- aov(Input.Average..N. ~ Tube.Length..mm.*Liquid.Viscocity, data = Experiment.df)
summary(ancova_checkLUTL)
## Testing For Unusual Observations(linearity)
X <- model.matrix(Input.Average..N. ~ Tube.Length..mm. + Liquid.Viscocity, data = Experiment.df)
k <- NCOL(X) - 1
n <- nrow(X)
# Creating the data frame of metrics for unusual observations
Unusual_Observations <- data.frame(leverage = hat(X),
                                   standardised = rstandard(LUTL.lm),
                                   studentised = rstudent(LUTL.lm),
                                   cooks = cooks.distance(LUTL.lm))
# Identifying unusual observations
which(Unusual_Observations$leverage>(2*(k+1)/n))
which(abs(Unusual_Observations$standardised)>2)
which(abs(Unusual_Observations$studentised)>2)
which(Unusual_Observations$cooks>1)
# Outliers are
X[11,]
X[12,]

### Assumption testing For Press Area (treating it as Catagorical Variabel) with Tube Length covariate 
## Fitting the linear model 
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
PATL.lm <- lm(Input.Average..N. ~ Tube.Length..mm. + Press.Area..mm.2., data = Experiment.df)
## Linearity, independence, constant variance and normality checks
par(mfrow = c(2,3))
plot(Experiment.df$Tube.Length..mm.,PATL.lm$residuals,xlab="Tube Length (mm)",ylab="Residuals",main="Residuals vs Tube Length")
abline(h=0,col='red')
plot(Experiment.df$Press.Area..mm.2.,PATL.lm$residuals,xlab="Press Area (mm^2)",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(PATL.lm$fitted.values,PATL.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(PATL.lm$residuals,type = "l",xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(PATL.lm$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(PATL.lm$residuals)
qqline(PATL.lm$residuals)
## Assumption check for the interaction
ancova_checkPATL <- aov(Input.Average..N. ~ Tube.Length..mm.*Press.Area..mm.2., data = Experiment.df)
summary(ancova_checkPATL)
## Testing For Unusual Observations(linearity)
X <- model.matrix(Input.Average..N. ~ Tube.Length..mm. + Press.Area..mm.2., data = Experiment.df)
k <- NCOL(X) - 1
n <- nrow(X)
# Creating the data frame of metrics for unusual observations
Unusual_Observations <- data.frame(leverage = hat(X),
                                   standardised = rstandard(PATL.lm),
                                   studentised = rstudent(PATL.lm),
                                   cooks = cooks.distance(PATL.lm))
# Identifying unusual observations
which(Unusual_Observations$leverage>(2*(k+1)/n))
which(abs(Unusual_Observations$standardised)>2)
which(abs(Unusual_Observations$studentised)>2)
which(Unusual_Observations$cooks>1)
# Outliers are
X[11,]
X[12,]
X[33,]

### Assumption testing For Tube Length (treating it as Catagorical Variabel) with Press Area covariate 
## Fitting the linear model 
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
TLPA.lm <- lm(Input.Average..N. ~ Press.Area..mm.2. + Tube.Length..mm., data = Experiment.df)
## Linearity, independence, constant variance and normality checks
par(mfrow = c(2,3))
plot(Experiment.df$Press.Area..mm.2.,TLPA.lm$residuals,xlab="Press Area (mm^2)",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(Experiment.df$Tube.Length..mm.,TLPA.lm$residuals,xlab="Tube Length (mm)",ylab="Residuals",main="Residuals vs Tube Length")
abline(h=0,col='red')
plot(TLPA.lm$fitted.values,TLPA.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(TLPA.lm$residuals,type = "l",xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(TLPA.lm$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(TLPA.lm$residuals)
qqline(TLPA.lm$residuals)
## Assumption check for the interaction
ancova_checkTLPA <- aov(Input.Average..N. ~ Press.Area..mm.2.*Tube.Length..mm., data = Experiment.df)
summary(ancova_checkTLPA)
## Testing For Unusual Observations(linearity)
X <- model.matrix(Input.Average..N. ~ Press.Area..mm.2. + Tube.Length..mm., data = Experiment.df)
k <- NCOL(X) - 1
n <- nrow(X)
# Creating the data frame of metrics for unusual observations
Unusual_Observations <- data.frame(leverage = hat(X),
                                   standardised = rstandard(TLPA.lm),
                                   studentised = rstudent(TLPA.lm),
                                   cooks = cooks.distance(TLPA.lm))
# Identifying unusual observations
which(Unusual_Observations$leverage>(2*(k+1)/n))
which(abs(Unusual_Observations$standardised)>2)
which(abs(Unusual_Observations$studentised)>2)
which(Unusual_Observations$cooks>1)
# Outliers are
X[11,]
X[12,]
X[33,]

#--------------------------------------------------------------------------------------------------------------------------------------------------- 

### ANCOVA Tests (fitting the model) and More Complex Tests (Single Blocking Factor, not k-way ANOVA)
## ANCOVA Model Using Liquid Contamination, Blocking For Pump Area
ANC_LCPA.acv <- aov(Input.Average..N. ~ Press.Area..mm.2. + Liquid.Contamination, data = Experiment.df)
summary(ANC_LCPA.acv)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_LCPA.acv)
tukey_res
plot(tukey_res)
## ANCOVA Model Using Liquid Contamination, Blocking For Connecting Tube Length
ANC_LCTL.acv <- aov(Input.Average..N. ~ Tube.Length..mm. + Liquid.Contamination, data = Experiment.df)
summary(ANC_LCTL.acv)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_LCTL.acv)
tukey_res
plot(tukey_res)
## ANOVA Model Using Liquid Contamination, Blocking For Liquid Used
A_LCLU.aov <- aov(data = Experiment.df, Input.Average..N. ~ Liquid.Contamination*Liquid.Viscocity)
summary(A_LCLU.aov)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_LCLU.aov)
tukey_res
plot(tukey_res)

## ANCOVA Model Using Liquid Used, Blocking For Pump Area
ANC_LUPA.acv <- aov(Input.Average..N. ~ Press.Area..mm.2. + Liquid.Viscocity, data = Experiment.df)
summary(ANC_LUPA.acv)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_LUPA.acv)
tukey_res
plot(tukey_res)
## ANCOVA Model Using Liquid Used, Blocking For Connecting Tube Length
ANC_LUTL.acv <- aov(Input.Average..N. ~ Tube.Length..mm. + Liquid.Viscocity, data = Experiment.df)
summary(ANC_LUTL.acv)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_LUTL.acv)
tukey_res
plot(tukey_res)
## ANOVA Model Using Liquid Used, Blocking For Liquid Contamination (Same as LC blocking for LU)
A_LULC.aov <- aov(data = Experiment.df, Input.Average..N. ~ Liquid.Viscocity*Liquid.Contamination)
summary(A_LULC.aov)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_LULC.aov)
tukey_res
plot(tukey_res)

## ANCOVA Model Using Press Area, Blocking For Connecting Tube Length
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
ANC_PATL.acv <- aov(Input.Average..N. ~ Tube.Length..mm. + Experiment.df$Press.Area..mm.2., data = Experiment.df)
summary(ANC_PATL.acv)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_PATL.acv)
tukey_res
plot(tukey_res)
## ANOVA Model Using Press Area, Blocking For Liquid Contamination 
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
A_PALC.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Press.Area..mm.2.*Liquid.Contamination)
summary(A_PALC.aov)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_PALC.aov)
tukey_res
plot(tukey_res)
## ANOVA Model Using Press Area, Blocking For Liquid Used
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
A_PALU.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Press.Area..mm.2.*Liquid.Viscocity)
summary(A_PALU.aov)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_PALU.aov)
tukey_res
plot(tukey_res)

## ANCOVA Model Using Connecting Tube Length, Blocking For Press Area
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
ANC_TLPA.acv <- aov(Input.Average..N. ~ Press.Area..mm.2. + Experiment.df$Tube.Length..mm., data = Experiment.df)
summary(ANC_TLPA.acv)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_TLPA.acv)
tukey_res
plot(tukey_res)
## ANOVA Model Using Connecting Tube Length, Blocking For Liquid Contamination 
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
A_TLLC.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Tube.Length..mm.*Liquid.Contamination)
summary(A_TLLC.aov)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_TLLC.aov)
tukey_res
plot(tukey_res)
## ANOVA Model Using Connecting Tube Length, Blocking For Liquid Used
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
A_TLLU.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Tube.Length..mm.*Liquid.Viscocity)
summary(A_TLLU.aov)
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_TLLU.aov)
tukey_res
plot(tukey_res)

#---------------------------------------------------------------------------------------------------------------------------------------------------

## 4 Way ANOVA (Treating Both Press Area and Tube Length as Categorical variables)
Experiment.df$Press.Area..mm.2.<- as.factor(Experiment.df$Press.Area..mm.2.)
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
Fullaov_int <- aov(Experiment.df$Input.Average..N. ~ Experiment.df$Liquid.Contamination*Experiment.df$Liquid.Viscocity
                     *Experiment.df$Press.Area..mm.2.*Experiment.df$Tube.Length..mm.)
summary(Fullaov_noint)

Fullaov_noint <- aov(Experiment.df$Input.Average..N. ~ Experiment.df$Liquid.Contamination+Experiment.df$Liquid.Viscocity
                   +Experiment.df$Press.Area..mm.2.+Experiment.df$Tube.Length..mm.)
summary(Fullaov_noint)

#---------------------------------------------------------------------------------------------------------------------------------------------------

## Multiple Linear Regression Models without Interaction (Remember to run this without having run any of the code for ANOVA tests since it turns our
## Continuous Variables into Categorical ones) 
# Full Multiple Linear Regression Model
FMR.w <- lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination)
summary(FMR.w)

# Forward Selection of model
step(lm(Experiment.df$Input.Average..N.~1),
     scope=list(lower=~1, upper=~Experiment.df$Liquid.Contamination + Experiment.df$Liquid.Viscocity
                + Experiment.df$Press.Area..mm.2. + Experiment.df$Tube.Length..mm.),
     direction="forward")

# Backward elimination of model
step(lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination),
     direction="backward")

# Stepwise selection of model (starting with full)
step(lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination),
     direction="both")

# Stepwise selection of model (starting with empty)
step(lm(Experiment.df$Input.Average..N.~1),
     scope=list(lower=~1, upper=~Experiment.df$Liquid.Contamination + Experiment.df$Liquid.Viscocity
                + Experiment.df$Press.Area..mm.2. + Experiment.df$Tube.Length..mm.),
     direction="both")

## All Models are the same so there will only be 2 partial F tests, one for the full model without and with interaction
# Partial F test with full model without interaction 
reduced_model <- lm(data = Experiment.df, Input.Average..N. ~ Experiment.df$Liquid.Contamination + Experiment.df$Liquid.Viscocity + Experiment.df$Press.Area..mm.2.)
anova(reduced_model,FMR.w)
# Partial F test with full model with interaction 
anova(reduced_model,FMRI.w)

#---------------------------------------------------------------------------------------------------------------------------------------------------

## Multiple Linear Regression Models with Interaction (Remember to run this without having run any of the code for ANOVA tests since it turns our
## Continuous Variables into Categorical ones) 
# Full Multiple Linear Regression Model
FMRI.w <- lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination)
summary(FMRI.w)

# Forward Selection of model
step(lm(Experiment.df$Input.Average..N.~1),
     scope=list(lower=~1, upper=~Experiment.df$Liquid.Contamination*Experiment.df$Liquid.Viscocity
                *Experiment.df$Press.Area..mm.2.*Experiment.df$Tube.Length..mm.),
     direction="forward")

# Backward elimination of model
step(lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination),
     direction="backward")

# Stepwise selection of model (starting with full)
step(lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination),
     direction="both")

# Stepwise selection of model (starting with empty)
step(lm(Experiment.df$Input.Average..N.~1),
     scope=list(lower=~1, upper=~Experiment.df$Liquid.Contamination*Experiment.df$Liquid.Viscocity
                *Experiment.df$Press.Area..mm.2.*Experiment.df$Tube.Length..mm.),
     direction="both")

## There exists only one model that is not the full model therefore there will only be one partial F test
# Partial F test with full model with interaction
reducedint_model = lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2. + 
                        Liquid.Viscocity + Liquid.Contamination + 
                        Tube.Length..mm. + Press.Area..mm.2.:Liquid.Viscocity)
anova(reducedint_model,FMRI.w)
# Partial F test with full model without interaction 
# (p value must be interpreted differently since reduced model with interaction has more added to it, i.e. p > 0.05
# favors full model without interaction)
anova(FMR.w, reducedint_model)

### We can conclude that the best multi linear regression model to use is one with the reduced model with the interaction
summary(reducedint_model)

#---------------------------------------------------------------------------------------------------------------------------------------------------

## Assumption Testing for chosen multi linear regression model
# Linearity (for continuous variables) and Constant variance
par(mfrow = c(2,3))
plot(reducedint_model$fitted.values,reducedint_model$residuals,
     xlab="Fitted Values",ylab="Residuals",
     main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(Experiment.df$Press.Area..mm.2.,reducedint_model$residuals,
     xlab="Press Area (mm^2)",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(Experiment.df$Tube.Length..mm.,reducedint_model$residuals,
     xlab="Tube Length (mm)",ylab="Residuals",main="Residuals vs Tube Length")
abline(h=0,col='red')
# Independence
plot(reducedint_model$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
# Normality
hist(reducedint_model$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(reducedint_model$residuals)
qqline(reducedint_model$residuals)
# No multicollinearity 
plot(Experiment.df$Press.Area..mm.2.,Experiment.df$Tube.Length..mm.,
     xlab="Press Area (mm^2)",ylab="Tube Length (mm)",
     main="Press Area vs Tube Length")


## Testing For Unusual Observations For the chosen multi linear regression model (linearity)
X <- model.matrix(Input.Average..N. ~ Press.Area..mm.2. + Liquid.Viscocity + Liquid.Contamination + 
                    Tube.Length..mm. + Press.Area..mm.2.:Liquid.Viscocity, data = Experiment.df)
k <- NCOL(X) - 1
n <- nrow(X)

# Creating the data frame of metrics for unusual observations
Unusual_Observations <- data.frame(leverage = hat(X),
                                   standardised = rstandard(reducedint_model),
                                   studentised = rstudent(reducedint_model),
                                   cooks = cooks.distance(reducedint_model))

# Identifying unusual observations
which(Unusual_Observations$leverage>(2*(k+1)/n))
which(abs(Unusual_Observations$standardised)>2)
which(abs(Unusual_Observations$studentised)>2)
which(Unusual_Observations$cooks>1)

# Outliers are
X[20,]
X[21,]
X[22,]

#--------------------------------------------------------------------------------------------------------------------------------------------

## Penalised Regression for chosen model


#--------------------------------------------------------------------------------------------------------------------------------------------

# Bootstrap Sampling 

#--------------------------------------------------------------------------------------------------------------------------------------------