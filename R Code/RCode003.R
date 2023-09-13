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

# Create input average (in Newtons (N)) column in Experiment data frame
Experiment.df$Input.Average..N. = rowMeans(Experiment.df[,c(6,7,8)])
names(Experiment.df)

## Transform the data for Pump area and Liquid viscosity.
# Log transformation for Y
array = as.array(Experiment.df$Input.Average..N.)
LogInputAv = log(array)

#--------------------------------------------------------------------------------------------------------------------------------------

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
SRPA.w <- lm(data = Experiment.df, LogInputAv ~ Press.Area..mm.2.)
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

#--------------------------------------------------------------------------------------------------------------------------------------
# Formal Simple Tests and Plot 
## Simple Linear Regression Using Press Area
SRPA.w <- lm(data = Experiment.df, LogInputAv ~ Press.Area..mm.2.)
summary(SRPA.w)
plot(Experiment.df$Press.Area..mm.2.,LogInputAv)
abline(SRPA.w, col = 'red')

## ANOVA Using Connecting Tube Length and Tukeys Test
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
ATL.aov <- aov(data = Experiment.df, LogInputAv ~ Experiment.df$Tube.Length..mm. )
summary(ATL.aov)
ggplot(Experiment.df, aes(Tube.Length..mm., LogInputAv)) +
  geom_boxplot()
# Tukey's HSD
TUKATL <- TukeyHSD(ATL.aov)
TUKATL
plot(TUKATL)

## ANOVA Using Contamination and Tukeys Test
ALC.aov <- aov(data = Experiment.df, LogInputAv ~ Experiment.df$Liquid.Contamination )
summary(ALC.aov)
ggplot(Experiment.df, aes(Liquid.Contamination, LogInputAv)) +
  geom_boxplot()
# Tukey's HSD
TUKALC <- TukeyHSD(ALC.aov)
TUKALC
plot(TUKALC)

## ANOVA Using Liquid Used and Tukeys Test
ALU.aov <- aov(data = Experiment.df, LogInputAv ~ Experiment.df$Liquid.Viscocity )
summary(ALU.aov)
ggplot(Experiment.df, aes(Liquid.Viscocity, LogInputAv)) +
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
Experiment.df$Liquid.Contamination <- as.factor(Experiment.df$Liquid.Contamination) 
LCPA.lm <- lm(LogInputAv ~ Press.Area..mm.2. + Liquid.Contamination, data = Experiment.df)
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
ancova_checkLCPA <- aov(LogInputAv ~ Press.Area..mm.2.*Liquid.Contamination, data = Experiment.df)
summary(ancova_checkLCPA)
## Testing For Unusual Observations(linearity)
X <- model.matrix(LogInputAv ~ Press.Area..mm.2. + Liquid.Contamination, data = Experiment.df)
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
Experiment.df$Liquid.Viscocity <- as.factor(Experiment.df$Liquid.Viscocity) 
LUPA.lm <- lm(LogInputAv ~ Press.Area..mm.2. + Liquid.Viscocity, data = Experiment.df)
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
ancova_checkLUPA <- aov(LogInputAv ~ Press.Area..mm.2.*Liquid.Viscocity, data = Experiment.df)
summary(ancova_checkLUPA)
## Testing For Unusual Observations(linearity)
X <- model.matrix(LogInputAv ~ Press.Area..mm.2. + Liquid.Viscocity, data = Experiment.df)
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
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
TLPA.lm <- lm(LogInputAv ~ Press.Area..mm.2. + Tube.Length..mm., data = Experiment.df)
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
ancova_checkTLPA <- aov(LogInputAv ~ Press.Area..mm.2.*Tube.Length..mm., data = Experiment.df)
summary(ancova_checkTLPA)
## Testing For Unusual Observations(linearity)
X <- model.matrix(LogInputAv ~ Press.Area..mm.2. + Tube.Length..mm., data = Experiment.df)
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
AOVFIT_LC <- lm(LogInputAv ~ Experiment.df$Liquid.Contamination*Experiment.df$Liquid.Viscocity)
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
Levene(LogInputAv, AOVFIT_LC$fitted.values)

## Assumption Testing For Press Area Using Liquid Used as a Blocking Variable
SRPA.w <- lm(data = Experiment.df, LogInputAv ~ Press.Area..mm.2.*Liquid.Viscocity)
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
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
AOVFIT_TL <- lm(LogInputAv ~ Experiment.df$Tube.Length..mm.*Experiment.df$Liquid.Viscocity)
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
Levene(LogInputAv, AOVFIT_TL$fitted.values)

#--------------------------------------------------------------------------------------------------------------------------------------
# Formal Comparisons for variable using variables with a significant effect on force input as a blocking factor 

### ANCOVA for Liquid Contamination, Liquid Used and Tube Length using Press Area as a blocking factor
## ANCOVA Model Using Liquid Contamination, Blocking For Pump Area
ANC_LCPA.acv <- aov(LogInputAv ~ Press.Area..mm.2. + Liquid.Contamination, data = Experiment.df)
summary(ANC_LCPA.acv)
ggplot(Experiment.df, aes(as.factor(Press.Area..mm.2.), LogInputAv)) +
  geom_boxplot(aes(fill = Liquid.Contamination)) +
  theme_classic()
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_LCPA.acv)
tukey_res
plot(tukey_res)

## ANCOVA Model Using Liquid Used, Blocking For Pump Area
ANC_LUPA.acv <- aov(LogInputAv ~ Press.Area..mm.2. + Liquid.Viscocity, data = Experiment.df)
summary(ANC_LUPA.acv)
ggplot(Experiment.df, aes(as.factor(Press.Area..mm.2.), LogInputAv)) +
  geom_boxplot(aes(fill = Liquid.Viscocity)) +
  theme_classic()
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_LUPA.acv)
tukey_res
plot(tukey_res)

## ANCOVA Model Using Connecting Tube Length, Blocking For Press Area
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
ANC_TLPA.acv <- aov(LogInputAv ~ Press.Area..mm.2. + Experiment.df$Tube.Length..mm., data = Experiment.df)
summary(ANC_TLPA.acv)
ggplot(Experiment.df, aes(as.factor(Press.Area..mm.2.), LogInputAv)) +
  geom_boxplot(aes(fill = Tube.Length..mm.)) +
  theme_classic()
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(ANC_TLPA.acv)
tukey_res
plot(tukey_res)

#--------------------------------------------------------------------------------------------------------------------------------------
### ANOVA and Linear regression for Liquid Contamination, Press Area and Tube Length using Liquid used as a blocking factor

## ANOVA Model Using Liquid Contamination, Blocking For Liquid Used
A_LCLU.aov <- aov(data = Experiment.df, LogInputAv ~ Liquid.Contamination*Liquid.Viscocity)
summary(A_LCLU.aov)
ggplot(Experiment.df, aes(as.factor(Liquid.Viscocity), LogInputAv)) +
  geom_boxplot(aes(fill = Liquid.Contamination)) +
  theme_classic()
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_LCLU.aov)
tukey_res
plot(tukey_res)

## Linear regression Model Using Press Area, Blocking For Liquid Used
SRPA.w <- lm(data = Experiment.df, LogInputAv ~ Press.Area..mm.2.*Liquid.Viscocity)
summary(SRPA.w)
presfac = as.factor(Experiment.df$Press.Area..mm.2.)
ggplot(Experiment.df, aes(as.factor(Liquid.Viscocity), LogInputAv)) +
  geom_boxplot(aes(fill = presfac)) +
  theme_classic()

## ANOVA Model Using Connecting Tube Length, Blocking For Liquid Used
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
A_TLLU.aov <- aov(data = Experiment.df, LogInputAv ~ Experiment.df$Tube.Length..mm.*Liquid.Viscocity)
summary(A_TLLU.aov)
ggplot(Experiment.df, aes(as.factor(Liquid.Viscocity), LogInputAv)) +
  geom_boxplot(aes(fill = Tube.Length..mm.)) +
  theme_classic()
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_TLLU.aov)
tukey_res
plot(tukey_res)

#--------------------------------------------------------------------------------------------------------------------------------------

## Multiple Linear Regression Models without Interaction
# Full Multiple Linear Regression Model
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
FMR.w <- lm(data = Experiment.df, LogInputAv ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination)
summary(FMR.w)

# Forward Selection of model
step(lm(LogInputAv~1),
     scope=list(lower=~1, upper=~Experiment.df$Liquid.Contamination + Experiment.df$Liquid.Viscocity
                + Experiment.df$Press.Area..mm.2. + Experiment.df$Tube.Length..mm.),
     direction="forward")

# Backward elimination of model
step(lm(data = Experiment.df, LogInputAv ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination),
     direction="backward")

# Stepwise selection of model (starting with full)
step(lm(data = Experiment.df, LogInputAv ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination),
     direction="both")

# Stepwise selection of model (starting with empty)
step(lm(LogInputAv~1),
     scope=list(lower=~1, upper=~Experiment.df$Liquid.Contamination + Experiment.df$Liquid.Viscocity
                + Experiment.df$Press.Area..mm.2. + Experiment.df$Tube.Length..mm.),
     direction="both")

# No F test since Full model is the chosen model


## Multiple Linear Regression Models with Interaction (Remember to run this without having run any of the code for ANOVA tests since it turns our
## Continuous Variables into Categorical ones) 
# Full Multiple Linear Regression Model
FMRI.w <- lm(data = Experiment.df, LogInputAv ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination)
summary(FMRI.w)

# Forward Selection of model
step(lm(LogInputAv~1),
     scope=list(lower=~1, upper=~Experiment.df$Liquid.Contamination*Experiment.df$Liquid.Viscocity
                *Experiment.df$Press.Area..mm.2.*Experiment.df$Tube.Length..mm.),
     direction="forward")

# Backward elimination of model
step(lm(data = Experiment.df, LogInputAv ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination),
     direction="backward")

# Stepwise selection of model (starting with full)
step(lm(data = Experiment.df, LogInputAv ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination),
     direction="both")

# Stepwise selection of model (starting with empty)
step(lm(LogInputAv~1),
     scope=list(lower=~1, upper=~Experiment.df$Liquid.Contamination*Experiment.df$Liquid.Viscocity
                *Experiment.df$Press.Area..mm.2.*Experiment.df$Tube.Length..mm.),
     direction="both")

# There are only two chosen models for loginput force which is the full no int and full int
# Partial F test with full model without interaction and full model with interaction
anova(FMR.w, FMRI.w)

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
plot(Experiment.df$Press.Area..mm.2.,FMR.w$residuals,
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
X <- model.matrix(LogInputAv ~ Press.Area..mm.2. + Liquid.Viscocity + Liquid.Contamination + 
                    Tube.Length..mm., data = Experiment.df)
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
interaction.plot(x.factor = Experiment.df$Press.Area..mm.2., #x-axis variable
                 trace.factor = Experiment.df$Liquid.Viscocity, #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Used")
interaction.plot(x.factor = Experiment.df$Liquid.Viscocity, #x-axis variable
                 trace.factor = Experiment.df$Press.Area..mm.2., #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Liquid Used",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Press Area")

# Interaction between Liquid Contamination and Press Area
interaction.plot(x.factor = Experiment.df$Press.Area..mm.2., #x-axis variable
                 trace.factor = Experiment.df$Liquid.Contamination, #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Contamination")
interaction.plot(x.factor = Experiment.df$Liquid.Contamination, #x-axis variable
                 trace.factor = Experiment.df$Press.Area..mm.2., #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Liquid Contamination",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Press Area")

# Interaction between Tube Length and Press Area
interaction.plot(x.factor = Experiment.df$Press.Area..mm.2., #x-axis variable
                 trace.factor = Experiment.df$Tube.Length..mm., #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Tube Length")
interaction.plot(x.factor = Experiment.df$Tube.Length..mm., #x-axis variable
                 trace.factor = Experiment.df$Press.Area..mm.2., #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Tube Length",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Press Area")

# Interaction between Liquid Contamination and Liquid Used
interaction.plot(x.factor = Experiment.df$Liquid.Viscocity, #x-axis variable
                 trace.factor = Experiment.df$Liquid.Contamination, #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Liquid Used",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Contamination")
interaction.plot(x.factor = Experiment.df$Liquid.Contamination, #x-axis variable
                 trace.factor = Experiment.df$Liquid.Viscocity, #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Liquid Contamination",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Used")

# Interaction between Tube Length and Liquid Used
interaction.plot(x.factor = Experiment.df$Liquid.Viscocity, #x-axis variable
                 trace.factor = Experiment.df$Tube.Length..mm., #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Liquid Used",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Tube Length")
interaction.plot(x.factor = Experiment.df$Tube.Length..mm., #x-axis variable
                 trace.factor = Experiment.df$Liquid.Viscocity, #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Tube Length",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Used")

# Interaction between Tube Length and Liquid Contamination
interaction.plot(x.factor = Experiment.df$Liquid.Contamination, #x-axis variable
                 trace.factor = Experiment.df$Tube.Length..mm., #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Liquid Contamination",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Tube Length")
interaction.plot(x.factor = Experiment.df$Tube.Length..mm., #x-axis variable
                 trace.factor = Experiment.df$Liquid.Contamination, #variable for lines
                 response = LogInputAv, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Force Input",
                 xlab = "Tube Length",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Contamination")
