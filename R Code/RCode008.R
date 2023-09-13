# Read in File
ExperimentE.df <- read.csv("ExperimentE.csv")

# Load in Packages
library(tidyverse)
library(ggplot2)
library(glmnet)
library(latex2exp)
library(readr)

# View and Explore ExperimentE.df
view(ExperimentE.df)
nrow(ExperimentE.df)
names(ExperimentE.df)

## Transform the data for Pump area and Liquid viscosity.
# Log transformation for Y
array = as.array(ExperimentE.df$Input..N.)
Input = array

#--------------------------------------------------------------------------------------------------------------------------------------

#### Assumption Testing For simple ANOVA tests as well as for Simple Linear regression using the log(InputForceAverage)
### Assumption Testing For Liquid Type used
AOVFIT_LU <- lm(Input ~ ExperimentE.df$Liquid.Viscocity)
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
Levene(Input, ExperimentE.df$Liquid.Viscocity)

### Assumption Testing For Contamination 
AOVFIT_LC <- lm(Input ~ ExperimentE.df$Liquid.Contamination)
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
Levene(Input, ExperimentE.df$Liquid.Contamination)

### Assumption Testing For Press Area 
## Simple linear regression
# Linearity (for continuous variables) and Constant variance
SRPA.w <- lm(data = ExperimentE.df, Input ~ Press.Area..mm.2.)
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

### Assumption Testing For Tube Length
## ANOVA model
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
AOVFIT_TL <- lm(Input ~ ExperimentE.df$Tube.Length..mm.)
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
Levene(Input, ExperimentE.df$Tube.Length..mm.)

#--------------------------------------------------------------------------------------------------------------------------------------
# Formal Simple Tests and Plot 
## Simple Linear Regression Using Press Area
SRPA.w <- lm(data = ExperimentE.df, Input ~ Press.Area..mm.2.)
summary(SRPA.w)
plot(ExperimentE.df$Press.Area..mm.2.,Input)
abline(SRPA.w, col = 'red')

## ANOVA Using Connecting Tube Length and Tukeys Test
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
ATL.aov <- aov(data = ExperimentE.df, Input ~ ExperimentE.df$Tube.Length..mm. )
summary(ATL.aov)
ggplot(ExperimentE.df, aes(Tube.Length..mm., Input)) +
  geom_boxplot()
# Tukey's HSD
TUKATL <- TukeyHSD(ATL.aov)
TUKATL
plot(TUKATL)

## ANOVA Using Contamination and Tukeys Test
ALC.aov <- aov(data = ExperimentE.df, Input ~ ExperimentE.df$Liquid.Contamination )
summary(ALC.aov)
ggplot(ExperimentE.df, aes(Liquid.Contamination, Input)) +
  geom_boxplot()
# Tukey's HSD
TUKALC <- TukeyHSD(ALC.aov)
TUKALC
plot(TUKALC)

## ANOVA Using Liquid Used and Tukeys Test
ALU.aov <- aov(data = ExperimentE.df, Input ~ ExperimentE.df$Liquid.Viscocity )
summary(ALU.aov)
ggplot(ExperimentE.df, aes(Liquid.Viscocity, Input)) +
  geom_boxplot()
# Tukey's HSD
TUKALU <- TukeyHSD(ALU.aov)
TUKALU
plot(TUKALU)

#--------------------------------------------------------------------------------------------------------------------------------------

#### ANCOVA Assumption Testing (using Press area as covariate)

### The assumption of "The value of the covariate does not depend on treatment" is met since the press area
### and the tube length was measured before the experiment was conducted

### Assumption testing For Liquid contamination with press area covariate 
## Fitting the linear model 
ExperimentE.df$Liquid.Contamination <- as.factor(ExperimentE.df$Liquid.Contamination) 
LCPA.lm <- lm(Input ~ Press.Area..mm.2. + Liquid.Contamination, data = ExperimentE.df)
## Linearity, independence, constant variance and normality checks
ExperimentE.df$Liquid.Contamination <- as.factor(ExperimentE.df$Liquid.Contamination) 
par(mfrow = c(2,3))
plot(ExperimentE.df$Press.Area..mm.2.,LCPA.lm$residuals,xlab="Press Area (mm^2)",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(ExperimentE.df$Liquid.Contamination,LCPA.lm$residuals,xlab="Liquid Contamination",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(LCPA.lm$fitted.values,LCPA.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(LCPA.lm$residuals,type = "l",xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(LCPA.lm$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(LCPA.lm$residuals)
qqline(LCPA.lm$residuals)
## Assumption check for the interaction
ancova_checkLCPA <- aov(Input ~ Press.Area..mm.2.*Liquid.Contamination, data = ExperimentE.df)
summary(ancova_checkLCPA)
## Testing For Unusual Observations(linearity)
X <- model.matrix(Input ~ Press.Area..mm.2. + Liquid.Contamination, data = ExperimentE.df)
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


### Assumption testing For Liquid Used with press area covariate
## Fitting the linear model 
ExperimentE.df$Liquid.Viscocity <- as.factor(ExperimentE.df$Liquid.Viscocity) 
LUPA.lm <- lm(Input ~ Press.Area..mm.2. + Liquid.Viscocity, data = ExperimentE.df)
## Linearity, independence, constant variance and normality checks
par(mfrow = c(2,3))
plot(ExperimentE.df$Press.Area..mm.2.,LUPA.lm$residuals,xlab="Press Area (mm^2)",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(ExperimentE.df$Liquid.Viscocity,LUPA.lm$residuals,xlab="Liquid Viscocity",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(LUPA.lm$fitted.values,LUPA.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(LUPA.lm$residuals,type = "l",xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(LUPA.lm$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(LUPA.lm$residuals)
qqline(LUPA.lm$residuals)
## Assumption check for the interaction
ancova_checkLUPA <- aov(Input ~ Press.Area..mm.2.*Liquid.Viscocity, data = ExperimentE.df)
summary(ancova_checkLUPA)
## Testing For Unusual Observations(linearity)
X <- model.matrix(Input ~ Press.Area..mm.2. + Liquid.Viscocity, data = ExperimentE.df)
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


### Assumption testing For Tube Length (treating it as Catagorical Variabel) with Press Area covariate 
## Fitting the linear model 
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
TLPA.lm <- lm(Input ~ Press.Area..mm.2. + Tube.Length..mm., data = ExperimentE.df)
## Linearity, independence, constant variance and normality checks
par(mfrow = c(2,3))
plot(ExperimentE.df$Press.Area..mm.2.,TLPA.lm$residuals,xlab="Press Area (mm^2)",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(ExperimentE.df$Tube.Length..mm.,TLPA.lm$residuals,xlab="Tube Length (mm)",ylab="Residuals",main="Residuals vs Tube Length")
abline(h=0,col='red')
plot(TLPA.lm$fitted.values,TLPA.lm$residuals,xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(TLPA.lm$residuals,type = "l",xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
hist(TLPA.lm$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(TLPA.lm$residuals)
qqline(TLPA.lm$residuals)
## Assumption check for the interaction
ancova_checkTLPA <- aov(Input ~ Press.Area..mm.2.*Tube.Length..mm., data = ExperimentE.df)
summary(ancova_checkTLPA)
## Testing For Unusual Observations(linearity)
X <- model.matrix(Input ~ Press.Area..mm.2. + Tube.Length..mm., data = ExperimentE.df)
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

#--------------------------------------------------------------------------------------------------------------------------------------

### Assumption Testing For ANOVA models and Linear Model using Liquid Used as a Blocking Factor

## Assumption Testing For Contamination using Liquid Used as a Blocking Variable
AOVFIT_LC <- lm(Input ~ ExperimentE.df$Liquid.Contamination*ExperimentE.df$Liquid.Viscocity)
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
Levene(Input, AOVFIT_LC$fitted.values)

## Assumption Testing For Press Area Using Liquid Used as a Blocking Variable
SRPA.w <- lm(data = ExperimentE.df, Input ~ Press.Area..mm.2.*Liquid.Viscocity)
summary(SRPA.w)
# Linearity and Constant Variance
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

## Assumption Testing For Tube Length using Liquid Used as a Blocking Variable
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
AOVFIT_TL <- lm(Input ~ ExperimentE.df$Tube.Length..mm.*ExperimentE.df$Liquid.Viscocity)
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
Levene(Input, AOVFIT_TL$fitted.values)

#--------------------------------------------------------------------------------------------------------------------------------------
# Formal Comparisons for variable using variables with a significant effect on force input as a blocking factor 

### ANCOVA for Liquid Contamination, Liquid Used and Tube Length using Press Area as a blocking factor
## ANCOVA Model Using Liquid Contamination, Blocking For Pump Area
ANC_LCPA.acv <- aov(Input ~ Press.Area..mm.2. + Liquid.Contamination, data = ExperimentE.df)
summary(ANC_LCPA.acv)
ggplot(ExperimentE.df, aes(as.factor(Press.Area..mm.2.), Input)) +
  geom_boxplot(aes(fill = Liquid.Contamination)) +
  theme_classic()
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_LCPA.acv)
tukey_res
plot(tukey_res)

## ANCOVA Model Using Liquid Used, Blocking For Pump Area
ANC_LUPA.acv <- aov(Input ~ Press.Area..mm.2. + Liquid.Viscocity, data = ExperimentE.df)
summary(ANC_LUPA.acv)
ggplot(ExperimentE.df, aes(as.factor(Press.Area..mm.2.), Input)) +
  geom_boxplot(aes(fill = Liquid.Viscocity)) +
  theme_classic()
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_LUPA.acv)
tukey_res
plot(tukey_res)

## ANCOVA Model Using Connecting Tube Length, Blocking For Press Area
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
ANC_TLPA.acv <- aov(Input ~ Press.Area..mm.2. + ExperimentE.df$Tube.Length..mm., data = ExperimentE.df)
summary(ANC_TLPA.acv)
ggplot(ExperimentE.df, aes(as.factor(Press.Area..mm.2.), Input)) +
  geom_boxplot(aes(fill = Tube.Length..mm.)) +
  theme_classic()
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_TLPA.acv)
tukey_res
plot(tukey_res)

#--------------------------------------------------------------------------------------------------------------------------------------
### ANOVA and Linear regression for Liquid Contamination, Press Area and Tube Length using Liquid used as a blocking factor

## ANOVA Model Using Liquid Contamination, Blocking For Liquid Used
A_LCLU.aov <- aov(data = ExperimentE.df, Input ~ Liquid.Contamination*Liquid.Viscocity)
summary(A_LCLU.aov)
ggplot(ExperimentE.df, aes(as.factor(Liquid.Viscocity), Input)) +
  geom_boxplot(aes(fill = Liquid.Contamination)) +
  theme_classic()
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_LCLU.aov)
tukey_res
plot(tukey_res)

## Linear regression Model Using Press Area, Blocking For Liquid Used
SRPA.w <- lm(data = ExperimentE.df, Input ~ Press.Area..mm.2.*Liquid.Viscocity)
summary(SRPA.w)
presfac = as.factor(ExperimentE.df$Press.Area..mm.2.)
ggplot(ExperimentE.df, aes(as.factor(Liquid.Viscocity), Input)) +
  geom_boxplot(aes(fill = presfac)) +
  theme_classic()

## ANOVA Model Using Connecting Tube Length, Blocking For Liquid Used
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
A_TLLU.aov <- aov(data = ExperimentE.df, Input ~ ExperimentE.df$Tube.Length..mm.*Liquid.Viscocity)
summary(A_TLLU.aov)
ggplot(ExperimentE.df, aes(as.factor(Liquid.Viscocity), Input)) +
  geom_boxplot(aes(fill = Tube.Length..mm.)) +
  theme_classic()
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_TLLU.aov)
tukey_res
plot(tukey_res)

#--------------------------------------------------------------------------------------------------------------------------------------

## Multiple Linear Regression Models without Interaction
# Full Multiple Linear Regression Model
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
FMR.w <- lm(data = ExperimentE.df, Input ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination)
summary(FMR.w)

# Forward Selection of model
step(lm(Input~1),
     scope=list(lower=~1, upper=~ExperimentE.df$Liquid.Contamination + ExperimentE.df$Liquid.Viscocity
                + ExperimentE.df$Press.Area..mm.2. + ExperimentE.df$Tube.Length..mm.),
     direction="forward")

# Backward elimination of model
step(lm(data = ExperimentE.df, Input ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination),
     direction="backward")

# Stepwise selection of model (starting with full)
step(lm(data = ExperimentE.df, Input ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination),
     direction="both")

# Stepwise selection of model (starting with empty)
step(lm(Input~1),
     scope=list(lower=~1, upper=~ExperimentE.df$Liquid.Contamination + ExperimentE.df$Liquid.Viscocity
                + ExperimentE.df$Press.Area..mm.2. + ExperimentE.df$Tube.Length..mm.),
     direction="both")

# No F test since Full model is the chosen model


## Multiple Linear Regression Models with Interaction (Remember to run this without having run any of the code for ANOVA tests since it turns our
## Continuous Variables into Categorical ones) 
# Full Multiple Linear Regression Model
FMRI.w <- lm(data = ExperimentE.df, Input ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination)
summary(FMRI.w)

# Forward Selection of model
step(lm(Input~1),
     scope=list(lower=~1, upper=~ExperimentE.df$Liquid.Contamination*ExperimentE.df$Liquid.Viscocity
                *ExperimentE.df$Press.Area..mm.2.*ExperimentE.df$Tube.Length..mm.),
     direction="forward")

# Backward elimination of model
step(lm(data = ExperimentE.df, Input ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination),
     direction="backward")

# Stepwise selection of model (starting with full)
step(lm(data = ExperimentE.df, Input ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination),
     direction="both")

# Stepwise selection of model (starting with empty)
step(lm(Input~1),
     scope=list(lower=~1, upper=~ExperimentE.df$Liquid.Contamination*ExperimentE.df$Liquid.Viscocity
                *ExperimentE.df$Press.Area..mm.2.*ExperimentE.df$Tube.Length..mm.),
     direction="both")

# There are three models firstly, two are the full models (interaction and no interaction)
# Third is,
reducedint_model = lm(Input ~ ExperimentE.df$Press.Area..mm.2. + 
                        ExperimentE.df$Liquid.Viscocity + ExperimentE.df$Liquid.Contamination + 
                        ExperimentE.df$Tube.Length..mm. + ExperimentE.df$Press.Area..mm.2.:ExperimentE.df$Liquid.Contamination)
summary(reducedint_model)
# Do partial F test for both reduced int model anddd Full model with interaciton
anova(reducedint_model,FMRI.w)

# Do partial F test for both reducedd int model andd Full model without interaction (this will be the reduced version in the test)
anova(FMR.w, reducedint_model)

# Can conlcude best model is full model wihtout interaction
summary(FMR.w)

#---------------------------------------------------------------------------------------------------------------------------------------------------

## Assumption Testing for chosen multi linear regression model
# Linearity (for continuous variables) and Constant variance
par(mfrow = c(2,3))
plot(FMR.w$fitted.values,FMR.w$residuals,
     xlab="Fitted Values",ylab="Residuals",
     main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(ExperimentE.df$Press.Area..mm.2.,FMR.w$residuals,
     xlab="Press Area (mm^2)",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
# Independence
plot(FMR.w$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
# Normality
hist(FMR.w$residuals,xlab="Residuals",main="Histogram of Residuals")
qqnorm(FMR.w$residuals)
qqline(FMR.w$residuals)
# No multicollinearity 
# There are no multiple continoues variables

## Testing For Unusual Observations For the chosen multi linear regression model (linearity)
X <- model.matrix(Input ~ Press.Area..mm.2. + Liquid.Viscocity + Liquid.Contamination + 
                    Tube.Length..mm., data = ExperimentE.df)
k <- NCOL(X) - 1
n <- nrow(X)

# Creating the data frame of metrics for unusual observations
Unusual_Observations <- data.frame(leverage = hat(X),
                                   standardised = rstandard(FMR.w),
                                   studentised = rstudent(FMR.w),
                                   cooks = cooks.distance(FMR.w))

# Identifying unusual observations
which(Unusual_Observations$leverage>(2*(k+1)/n))
which(abs(Unusual_Observations$standardised)>2)
which(abs(Unusual_Observations$studentised)>2)
which(Unusual_Observations$cooks>1)

#---------------------------------------------------------------------------------------------------------------------------------------------------

## Interaction Plots 
# Interaction between Liquid Used and Press Area
interaction.plot(x.factor = ExperimentE.df$Press.Area..mm.2., #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Viscocity, #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Press Area (mm^2)",
                 main = "Interaction of Liquid Viscosity and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Used")
interaction.plot(x.factor = ExperimentE.df$Liquid.Viscocity, #x-axis variable
                 trace.factor = ExperimentE.df$Press.Area..mm.2., #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Liquid Used",
                 main = "Interaction of Liquid Viscosity and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Press Area")

# Interaction between Liquid Contamination and Press Area
interaction.plot(x.factor = ExperimentE.df$Press.Area..mm.2., #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Contamination, #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Press Area",
                 main = "Interaction of Liquid Contamination and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Contamination")
interaction.plot(x.factor = ExperimentE.df$Liquid.Contamination, #x-axis variable
                 trace.factor = ExperimentE.df$Press.Area..mm.2., #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Liquid Contamination",
                 main = "Interaction of Liquid Contamination and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Press Area")

# Interaction between Tube Length and Press Area
interaction.plot(x.factor = ExperimentE.df$Press.Area..mm.2., #x-axis variable
                 trace.factor = ExperimentE.df$Tube.Length..mm., #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Press Area",
                 main = "Interaction of Tube Length and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Tube Length")
interaction.plot(x.factor = ExperimentE.df$Tube.Length..mm., #x-axis variable
                 trace.factor = ExperimentE.df$Press.Area..mm.2., #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Tube Length",
                 main = "Interaction of Tube Length and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Press Area")

# Interaction between Liquid Contamination and Liquid Used
interaction.plot(x.factor = ExperimentE.df$Liquid.Viscocity, #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Contamination, #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Liquid Used",
                 main = "Interaction of Liquid Contamination and Liquid Viscosity",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Contamination")
interaction.plot(x.factor = ExperimentE.df$Liquid.Contamination, #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Viscocity, #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Liquid Contamination",
                 main = "Interaction of Liquid Contamination and Liquid Viscosity",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Used")

# Interaction between Tube Length and Liquid Used
interaction.plot(x.factor = ExperimentE.df$Liquid.Viscocity, #x-axis variable
                 trace.factor = ExperimentE.df$Tube.Length..mm., #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 main = "Interaction of Tube Length and Liquid Viscosity",
                 xlab = "Liquid Used",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Tube Length")
interaction.plot(x.factor = ExperimentE.df$Tube.Length..mm., #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Viscocity, #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Tube Length",
                 main = "Interaction of Tube Length and Liquid Viscosity",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Used")

# Interaction between Tube Length and Liquid Contamination
interaction.plot(x.factor = ExperimentE.df$Liquid.Contamination, #x-axis variable
                 trace.factor = ExperimentE.df$Tube.Length..mm., #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Liquid Contamination",
                 main = "Interaction of Tube Length and Liquid Contamination",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Tube Length")
interaction.plot(x.factor = ExperimentE.df$Tube.Length..mm., #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Contamination, #variable for lines
                 response = Input, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Force Input (Newtons)",
                 xlab = "Tube Length",
                 main = "Interaction of Tube Length and Liquid Contamination",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Contamination")

#--------------------------------------------------------------------------------------------------------------------------------------------

## Penalised Regression for chosen model
df = subset(ExperimentE.df, select = c(-Input..N., -RunOrder))
df$Liquid.Water = df$Liquid.Viscocity 
df$Liquid.Oil = df$Liquid.Water
# df = subset(df, select = c(-Liquid.Viscocity))

df$Liquid.Contamination[df$Liquid.Contamination == "No"] <- 0
df$Liquid.Contamination[df$Liquid.Contamination == "Yes"] <- 1

df$Liquid.Water[df$Liquid.Water == "Water"] <- 1
df$Liquid.Water[df$Liquid.Water == "Oil"] <- 0
df$Liquid.Water[df$Liquid.Water == "Detergent"] <- 0

df$Liquid.Oil[df$Liquid.Oil == "Water"] <- 0
df$Liquid.Oil[df$Liquid.Oil == "Oil"] <- 1
df$Liquid.Oil[df$Liquid.Oil == "Detergent"] <- 0

X <- data.matrix(df)

X[X == 1] <- 0
X[X == 2] <- 1

Y <- data.matrix(Input)

# Getting the plot of coefficients versus log lambda.
set.seed(5)
fit <- glmnet(X, Y, alpha = 1)
plot(fit, xvar = "lambda",label=TRUE)
mtext("Number of Non-Zero Coefficients", side=3, line = 2.5)

#Lasso 
set.seed(5)
cvfit <- cv.glmnet(X, Y, alpha=1)
coef(cvfit, s = "lambda.1se")
plot(cvfit)

# Get our residuals for our assumption checks
Yhat <- predict(cvfit, newx = X, s = "lambda.1se")
residuals <- Y - Yhat

# R-squared 
1 - sum(residuals^2)/sum((Yhat - mean(Yhat))^2)

# variance of the residuals
mean(residuals^2)

# Residuals versus fitted values and each x
par(mfrow=c(3,3))
plot(Yhat,residuals) # Residuals vs fitted values
for (j in 1:5){
  plot(X[,j],residuals)
}

# Multicollinearity
pairs(X)

# QQ plot to assess normality (not really
# necessary since we're not looking
# at confidence intervals or p-values)
qqnorm(residuals)
qqline(residuals)
