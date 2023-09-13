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
LogInput = log(array)

#--------------------------------------------------------------------------------------------------------------------------------------

#### Assumption Testing For simple ANOVA tests as well as for Simple Linear regression using the log(InputForceAverage)
### Assumption Testing For Liquid Type used
AOVFIT_LU <- aov(LogInput ~ ExperimentE.df$Liquid.Viscocity)
summary(AOVFIT_LU)
# Checking Independence, Constant Variance and Normality 
par(mfrow = c(2,2))
plot(AOVFIT_LU$fitted.values,AOVFIT_LU$residuals,
     xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted Values (Liquid Viscosity)")
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
Levene(LogInput, ExperimentE.df$Liquid.Viscocity)

### Assumption Testing For Contamination 
AOVFIT_LC <- aov(LogInput ~ ExperimentE.df$Liquid.Contamination)
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
qqnorm(AOVFIT_LC$residuals, main="Normal Q-Q Plot (Liquid Contamination)")
qqline(AOVFIT_LC$residuals)
# Test for homogeneity of variances
Levene <- function(y, group){
  group <- as.factor(group)  # precautionary
  meds <- tapply(y, group, median)
  resp <- abs(y - meds[group])
  anova(lm(resp ~ group))[1, 4:5]
}
Levene(LogInput, ExperimentE.df$Liquid.Contamination)

### Assumption Testing For Press Area 
## Simple linear regression
# Linearity (for continuous variables) and Constant variance
SRPA.w <- lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2.)
summary(SRPA.w)
par(mfrow = c(2,2))
plot(SRPA.w$fitted.values,SRPA.w$residuals,
     xlab="Fitted Values",ylab="Residuals",
     main="Residuals vs Fitted Values (Press Area)")
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
AOVFIT_TL <- aov(LogInput ~ ExperimentE.df$Tube.Length..mm.)
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
qqnorm(AOVFIT_TL$residuals, main="Normal Q-Q Plot (Tube Length)")
qqline(AOVFIT_TL$residuals)
# Test for homogeneity of variances
Levene <- function(y, group){
  group <- as.factor(group)  # precautionary
  meds <- tapply(y, group, median)
  resp <- abs(y - meds[group])
  anova(lm(resp ~ group))[1, 4:5]
}
Levene(LogInput, ExperimentE.df$Tube.Length..mm.)

#--------------------------------------------------------------------------------------------------------------------------------------
# Formal Simple Tests and Plot 
## Simple Linear Regression Using Press Area
SRPA.w <- lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2.)
summary(SRPA.w)
plot(ExperimentE.df$Press.Area..mm.2.,LogInput, xlab = "Press Area (mm^2)", ylab = "Log Input Force (Newtons)")
abline(SRPA.w, col = 'red')

## ANOVA Using Connecting Tube Length and Tukeys Test
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
ATL.aov <- aov(data = ExperimentE.df, LogInput ~ ExperimentE.df$Tube.Length..mm. )
summary(ATL.aov)
ggplot(ExperimentE.df, aes(Tube.Length..mm., LogInput)) +
  geom_boxplot() +
  xlab("Tube Length (mm)") +
  ylab("Log Input Force (Newtons)")
# Tukey's HSD
TUKATL <- TukeyHSD(ATL.aov)
TUKATL
plot(TUKATL)

## ANOVA Using Contamination and Tukeys Test
ALC.aov <- aov(data = ExperimentE.df, LogInput ~ ExperimentE.df$Liquid.Contamination )
summary(ALC.aov)
ggplot(ExperimentE.df, aes(Liquid.Contamination, LogInput)) +
  geom_boxplot() +
  xlab("Liquid Contamination") +
  ylab("Log Input Force (Newtons)")
# Tukey's HSD
TUKALC <- TukeyHSD(ALC.aov)
TUKALC
plot(TUKALC)

## ANOVA Using Liquid Used and Tukeys Test
ALU.aov <- aov(data = ExperimentE.df, LogInput ~ ExperimentE.df$Liquid.Viscocity )
summary(ALU.aov)
ggplot(ExperimentE.df, aes(Liquid.Viscocity, LogInput)) +
  geom_boxplot() +
  xlab("Liquid Viscosity") +
  ylab("Log Input Force (Newtons)")
# Tukey's HSD
TUKALU <- TukeyHSD(ALU.aov)
TUKALU
plot(TUKALU)

#--------------------------------------------------------------------------------------------------------------------------------------

### Assumption Testing For ANOVA models and Linear Model using Press Area as a Blocking Factor
# Make Press Area a Catagorical variable for the purposes of making it a blocking factor
presfac = as.factor(ExperimentE.df$Press.Area..mm.2.)

## Assumption Testing For Contamination using Liquid Used as a Blocking Variable
AOVFIT_LC <- lm(LogInput ~ ExperimentE.df$Liquid.Contamination*presfac)
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
Levene(LogInput, AOVFIT_LC$fitted.values)

## Assumption Testing For Liquid Used Using Press Area as a Blocking Variable
AOVFIT_LU <- lm(data = ExperimentE.df, LogInput ~ Liquid.Viscocity*presfac)
summary(AOVFIT_LU)
# Linearity and Constant Variance
par(mfrow = c(2,2))
plot(AOVFIT_LU$fitted.values,AOVFIT_LU$residuals,
     xlab="Fitted Values",ylab="Residuals",
     main="Residuals vs Fitted Values")
abline(h=0,col='red')
# Independence
plot(AOVFIT_LU$residuals,type = "l",
     xlab="Observation Number",ylab="Residuals",main="Independence Plot")
abline(h=0,col='red')
# Normality
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
Levene(LogInput, AOVFIT_LU$fitted.values)


## Assumption Testing For Tube Length using Press Area as a Blocking Variable
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
AOVFIT_TL <- lm(LogInput ~ ExperimentE.df$Tube.Length..mm.*presfac)
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
Levene(LogInput, AOVFIT_TL$fitted.values)

#--------------------------------------------------------------------------------------------------------------------------------------

### Assumption Testing For ANOVA models and Linear Model using Liquid Used as a Blocking Factor

## Assumption Testing For Contamination using Liquid Used as a Blocking Variable
AOVFIT_LC <- lm(LogInput ~ ExperimentE.df$Liquid.Contamination*ExperimentE.df$Liquid.Viscocity)
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
Levene(LogInput, AOVFIT_LC$fitted.values)

## Assumption Testing For Press Area Using Liquid Used as a Blocking Variable
SRPA.w <- lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2.*Liquid.Viscocity)
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
AOVFIT_TL <- lm(LogInput ~ ExperimentE.df$Tube.Length..mm.*ExperimentE.df$Liquid.Viscocity)
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
Levene(LogInput, AOVFIT_TL$fitted.values)

#--------------------------------------------------------------------------------------------------------------------------------------
# Formal Comparisons for variable using variables with a significant effect on force input as a blocking factor 

### ANOVA and Linear regression for Liquid Contamination, Liquid used and Tube Length using press area as a blocking factor
presfac = as.factor(ExperimentE.df$Press.Area..mm.2.)
## ANOVA Model Using Liquid Contamination, Blocking For Press Area
A_LCPA.aov <- aov(data = ExperimentE.df, LogInput ~ Liquid.Contamination*presfac)
summary(A_LCPA.aov)
ggplot(ExperimentE.df, aes(as.factor(presfac), LogInput)) +
  geom_boxplot(aes(fill = Liquid.Contamination)) +
  theme_classic() +
  xlab("Press Area (mm^2)") +
  ylab("Log Input Force (Newtons)") 
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_LCPA.aov)
tukey_res
plot(tukey_res)

## ANOVA Model Using Liquid Used, Blocking For Press Area
A_LUPA.aov <- aov(data = ExperimentE.df, LogInput ~ Liquid.Viscocity*presfac)
summary(A_LUPA.aov)
ggplot(ExperimentE.df, aes(as.factor(presfac), LogInput)) +
  geom_boxplot(aes(fill = Liquid.Viscocity)) +
  theme_classic() +
  xlab("Press Area (mm^2)") +
  ylab("Log Input Force (Newtons)") 
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_LUPA.aov)
tukey_res
plot(tukey_res)

## ANOVA Model Using Connecting Tube Length, Blocking For Press Area
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
A_TLPA.aov <- aov(data = ExperimentE.df, LogInput ~ ExperimentE.df$Tube.Length..mm.*presfac)
summary(A_TLPA.aov)
ggplot(ExperimentE.df, aes(as.factor(presfac), LogInput)) +
  geom_boxplot(aes(fill = Tube.Length..mm.)) +
  theme_classic() +
  xlab("Press Area (mm^2)") +
  ylab("Log Input Force (Newtons)")
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_TLPA.aov)
tukey_res
plot(tukey_res)

#--------------------------------------------------------------------------------------------------------------------------------------
### ANOVA and Linear regression for Liquid Contamination, Press Area and Tube Length using Liquid used as a blocking factor

## ANOVA Model Using Liquid Contamination, Blocking For Liquid Used
A_LCLU.aov <- aov(data = ExperimentE.df, LogInput ~ Liquid.Contamination*Liquid.Viscocity)
summary(A_LCLU.aov)
ggplot(ExperimentE.df, aes(as.factor(Liquid.Viscocity), LogInput)) +
  geom_boxplot(aes(fill = Liquid.Contamination)) +
  theme_classic() +
  xlab("Liquid Viscosity") +
  ylab("Log Input Force (Newtons)") 
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_LCLU.aov)
tukey_res
plot(tukey_res)

## Linear regression Model Using Press Area, Blocking For Liquid Used
SRPA.w <- lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2.*Liquid.Viscocity)
summary(SRPA.w)
presfac = as.factor(ExperimentE.df$Press.Area..mm.2.)
ggplot(ExperimentE.df, aes(as.factor(Liquid.Viscocity), LogInput)) +
  geom_boxplot(aes(fill = presfac)) +
  theme_classic() +
  xlab("Liquid Viscosity") +
  ylab("Log Input Force (Newtons)")

## ANOVA Model Using Connecting Tube Length, Blocking For Liquid Used
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
A_TLLU.aov <- aov(data = ExperimentE.df, LogInput ~ ExperimentE.df$Tube.Length..mm.*Liquid.Viscocity)
summary(A_TLLU.aov)
ggplot(ExperimentE.df, aes(as.factor(Liquid.Viscocity), LogInput)) +
  geom_boxplot(aes(fill = Tube.Length..mm.)) +
  theme_classic() +
  xlab("Liquid Viscosity") +
  ylab("Log Input Force (Newtons)")
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_TLLU.aov)
tukey_res
plot(tukey_res)

#--------------------------------------------------------------------------------------------------------------------------------------

## Multiple Linear Regression Models without Interaction
# Full Multiple Linear Regression Model
ExperimentE.df$Tube.Length..mm. <- factor(ExperimentE.df$Tube.Length..mm.)
ExperimentE.df$Liquid.Contamination <- factor(ExperimentE.df$Liquid.Contamination)
ExperimentE.df$Liquid.Viscocity <- factor(ExperimentE.df$Liquid.Viscocity)
FMR.w <- lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination)
summary(FMR.w)

# Forward Selection of model
step(lm(LogInput~1),
     scope=list(lower=~1, upper=~ExperimentE.df$Liquid.Contamination + ExperimentE.df$Liquid.Viscocity
                + ExperimentE.df$Press.Area..mm.2. + ExperimentE.df$Tube.Length..mm.),
     direction="forward")

# Backward elimination of model
step(lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination),
     direction="backward")

# Stepwise selection of model (starting with full)
step(lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination),
     direction="both")

# Stepwise selection of model (starting with empty)
step(lm(LogInput~1),
     scope=list(lower=~1, upper=~ExperimentE.df$Liquid.Contamination + ExperimentE.df$Liquid.Viscocity
                + ExperimentE.df$Press.Area..mm.2. + ExperimentE.df$Tube.Length..mm.),
     direction="both")

# No F test since Full model is the chosen model


## Multiple Linear Regression Models with Interaction (Remember to run this without having run any of the code for ANOVA tests since it turns our
## Continuous Variables into Categorical ones) 
# Full Multiple Linear Regression Model
FMRI.w <- lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination)
summary(FMRI.w)

# Forward Selection of model
step(lm(LogInput~1),
     scope=list(lower=~1, upper=~ExperimentE.df$Liquid.Contamination*ExperimentE.df$Liquid.Viscocity
                *ExperimentE.df$Press.Area..mm.2.*ExperimentE.df$Tube.Length..mm.),
     direction="forward")

# Backward elimination of model
step(lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination),
     direction="backward")

# Stepwise selection of model (starting with full)
step(lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2.*Tube.Length..mm.*Liquid.Viscocity*Liquid.Contamination),
     direction="both")

# Stepwise selection of model (starting with empty)
step(lm(LogInput~1),
     scope=list(lower=~1, upper=~ExperimentE.df$Liquid.Contamination*ExperimentE.df$Liquid.Viscocity
                *ExperimentE.df$Press.Area..mm.2.*ExperimentE.df$Tube.Length..mm.),
     direction="both")

# There are three models firstly, two are the full models (interaction and no interaction)
# Third is,
reducedint_model = lm(LogInput ~ ExperimentE.df$Press.Area..mm.2. + 
                        ExperimentE.df$Liquid.Viscocity + ExperimentE.df$Liquid.Contamination + 
                        ExperimentE.df$Tube.Length..mm. + ExperimentE.df$Press.Area..mm.2.:ExperimentE.df$Liquid.Contamination)
summary(reducedint_model)
# Do partial F test for both reduced int model and Full model with interaction
anova(reducedint_model,FMRI.w)

# Do partial F test for both reduced int model and Full model without interaction (this will be the reduced version in the test)
anova(FMR.w, reducedint_model)

# Can conclude best model is full model without interaction
summary(FMR.w)

#---------------------------------------------------------------------------------------------------------------------------------------------------

## Assumption Testing for chosen multi linear regression model
# Linearity (for continuous variables) and Constant variance
FMR.w <- lm(data = ExperimentE.df, LogInput ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination)
summary(FMR.w)
par(mfrow = c(2,3))
plot(FMR.w$fitted.values,FMR.w$residuals,
     xlab="Fitted Values",ylab="Residuals",
     main="Residuals vs Fitted Values")
abline(h=0,col='red')
plot(ExperimentE.df$Press.Area..mm.2.,FMR.w$residuals,
     xlab="Press Area (mm^2)",ylab="Residuals",main="Residuals vs Press Area")
abline(h=0,col='red')
plot(ExperimentE.df$Liquid.Viscocity,FMR.w$residuals,xlab="Liquid Viscosity",ylab="Residuals",main="Residuals vs Liquid Viscosity")
abline(h=0,col='red')
plot(ExperimentE.df$Liquid.Contamination,FMR.w$residuals,xlab="Liquid Contamination",ylab="Residuals",main="Residuals vs Liquid Contamination")
abline(h=0,col='red')
plot(ExperimentE.df$Tube.Length..mm.,FMR.w$residuals,xlab="Tube Length (mm)",ylab="Residuals",main="Residuals vs Tube Length (mm)")
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
X <- model.matrix(LogInput ~ Press.Area..mm.2. + Liquid.Viscocity + Liquid.Contamination + 
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

## Confidence Interval for each predictor
# Press Area
confint(FMR.w, 'Press.Area..mm.2.', level = 0.95)
# Liquid Viscosity
confint(FMR.w, 'Liquid.ViscocityOil', level = 0.95)
confint(FMR.w, 'Liquid.ViscocityWater', level = 0.95)
# Liquid Contamination
confint(FMR.w, 'Liquid.ContaminationYes', level = 0.95)
# Tube Length
confint(FMR.w, 'Tube.Length..mm.900', level = 0.95)

#---------------------------------------------------------------------------------------------------------------------------------------------------

## Interaction Plots 
# Interaction between Liquid Used and Press Area
interaction.plot(x.factor = ExperimentE.df$Press.Area..mm.2., #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Viscocity, #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Press Area (mm^2)",
                 main = "Interaction of Liquid Viscosity and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Used")
interaction.plot(x.factor = ExperimentE.df$Liquid.Viscocity, #x-axis variable
                 trace.factor = ExperimentE.df$Press.Area..mm.2., #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Liquid Used",
                 main = "Interaction of Liquid Viscosity and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Press Area (mm^2)")

# Interaction between Liquid Contamination and Press Area
interaction.plot(x.factor = ExperimentE.df$Press.Area..mm.2., #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Contamination, #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Press Area (mm^2)",
                 main = "Interaction of Liquid Contamination and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Contamination")
interaction.plot(x.factor = ExperimentE.df$Liquid.Contamination, #x-axis variable
                 trace.factor = ExperimentE.df$Press.Area..mm.2., #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Liquid Contamination",
                 main = "Interaction of Liquid Contamination and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Press Area (mm^2)")

# Interaction between Tube Length and Press Area
interaction.plot(x.factor = ExperimentE.df$Press.Area..mm.2., #x-axis variable
                 trace.factor = ExperimentE.df$Tube.Length..mm., #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Press Area (mm^2)",
                 main = "Interaction of Tube Length and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Tube Length (mm)")
interaction.plot(x.factor = ExperimentE.df$Tube.Length..mm., #x-axis variable
                 trace.factor = ExperimentE.df$Press.Area..mm.2., #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Tube Length (mm)",
                 main = "Interaction of Tube Length and Press Area",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Press Area (mm^2)")

# Interaction between Liquid Contamination and Liquid Used
interaction.plot(x.factor = ExperimentE.df$Liquid.Viscocity, #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Contamination, #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Liquid Used",
                 main = "Interaction of Liquid Contamination and Liquid Viscosity",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Contamination")
interaction.plot(x.factor = ExperimentE.df$Liquid.Contamination, #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Viscocity, #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Liquid Contamination",
                 main = "Interaction of Liquid Contamination and Liquid Viscosity",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Used")

# Interaction between Tube Length and Liquid Used
interaction.plot(x.factor = ExperimentE.df$Liquid.Viscocity, #x-axis variable
                 trace.factor = ExperimentE.df$Tube.Length..mm., #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Liquid Used",
                 main = "Interaction of Tube Length and Liquid Viscosity",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Tube Length (mm)")
interaction.plot(x.factor = ExperimentE.df$Tube.Length..mm., #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Viscocity, #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Tube Length (mm)",
                 main = "Interaction of Tube Length and Liquid Viscosity",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Used")

# Interaction between Tube Length and Liquid Contamination
interaction.plot(x.factor = ExperimentE.df$Liquid.Contamination, #x-axis variable
                 trace.factor = ExperimentE.df$Tube.Length..mm., #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Liquid Contamination",
                 main = "Interaction of Tube Length and Liquid Contamination",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Tube Length (mm)")
interaction.plot(x.factor = ExperimentE.df$Tube.Length..mm., #x-axis variable
                 trace.factor = ExperimentE.df$Liquid.Contamination, #variable for lines
                 response = LogInput, #y-axis variable
                 fun = median, #metric to plot
                 ylab = "Mean Log Force Input (Newtons)",
                 xlab = "Tube Length (mm)",
                 main = "Interaction of Tube Length and Liquid Contamination",
                 col = c("red", "blue"),
                 lty = 1, #line type
                 lwd = 2, #line width
                 trace.label = "Liquid Contamination")
#--------------------------------------------------------------------------------------------------------------------------------------------

# ANOVA to see if there is interaction between liquid contamination and tube length
## ANOVA Model Using Connecting Tube Length, Blocking For Liquid Used
ExperimentE.df$Tube.Length..mm. <- as.factor(ExperimentE.df$Tube.Length..mm.)
A_TLLC.aov <- aov(data = ExperimentE.df, LogInput ~ ExperimentE.df$Tube.Length..mm.*ExperimentE.df$Liquid.Contamination)
summary(A_TLLC.aov)
ggplot(ExperimentE.df, aes(as.factor(Liquid.Contamination), LogInput)) +
  geom_boxplot(aes(fill = Tube.Length..mm.)) +
  theme_classic() +
  xlab("Liquid Viscosity") +
  ylab("Log Input Force (Newtons)")
# Post-hoc analysis for the ANCOVA model
tukey_res <- TukeyHSD(A_TLLC.aov)
tukey_res
plot(tukey_res)

#--------------------------------------------------------------------------------------------------------------------------------------------

## Penalised Regression for chosen model
df = subset(ExperimentE.df, select = c(-Input..N., -RunOrder))
df$Liquid.Water = df$Liquid.Viscocity 
df$Liquid.Oil = df$Liquid.Water
df = subset(df, select = c(-Liquid.Viscocity))

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

Y <- data.matrix(LogInput)

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

