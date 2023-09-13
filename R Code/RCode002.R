# Read in File
Experiment.df <- read.csv("C:/Users/User/Documents/Uni/Year 2/SEM 1/MXB242 - Regression and Design/Group Project/MXB242 GroupProject/R Code/Experiment.csv")

# Load in Packages
library(tidyverse)
library(ggplot2)


# View and Explore Experiment.df
view(Experiment.df)
nrow(Experiment.df)
names(Experiment.df)

# Create input average (in Newtons (N)) column in Experiment data frame
Experiment.df$Input.Average..N. = rowMeans(Experiment.df[,c(6,7,8)])
names(Experiment.df)

## Plot Data (no model)
# Force in vs Connecting Tube length
par(mfrow = c(2,2))
plot(Experiment.df$Tube.Length..mm., Experiment.df$Input.Average..N., xlab = 'Connecting Tube Length (mm)', ylab = "Force Input (N)")
# Force in vs Press Area
plot(Experiment.df$Press.Area..mm.2., Experiment.df$Input.Average..N., xlab = 'Press Area (mm^2)', ylab = "Force Input (N)")
# Force in vs Liquid Used
ggplot(Experiment.df) +
  geom_point(aes(x = Liquid.Viscocity, y = Input.Average..N.))
# Force in vs Liquid Contamination 
ggplot(Experiment.df) +
  geom_point(aes(x = Liquid.Contamination, y = Input.Average..N.))

#-------------------------------------------------------------------------------------------------------------------------------------

### Simple Models, Model Force Input for each Factor and simple ANOVA Tests
## Simple Linear Regression Using Press Area
SRPA.w <- lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2.)
summary(SRPA.w)
plot(Experiment.df$Press.Area..mm.2.,Experiment.df$Input.Average..N.)
abline(SRPA.w, col = 'red')
## ANOVA Using Press Area
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
APA.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Press.Area..mm.2.)
summary(APA.aov)
## Tukey's HSD with ANOVA using Press Area
TukeyAPA <- TukeyHSD(APA.aov)
TukeyAPA
plot(TukeyAPA)

## Simple Linear Regression Using Connecting Tube Length
SRTL.w <- lm(data = Experiment.df, Input.Average..N. ~ Tube.Length..mm.)
summary(SRTL.w)
plot(Experiment.df$Tube.Length..mm.,Experiment.df$Input.Average..N.)
abline(SRTL.w, col = 'red')
## ANOVA Using Connecting Tube Length
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
ATL.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Tube.Length..mm. )
summary(ATL.aov)

## ANOVA Using Liquid Used 
ALU.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Liquid.Viscocity)
summary(ALU.aov)

## Tukey's HSD with ANOVA using liquid used
TukeyALU <- TukeyHSD(ALU.aov)
TukeyALU
plot(TukeyALU)

## ANOVA Using Liquid Contamination 
ALC.aov <- aov(data = Experiment.df, Input.Average..N. ~ Liquid.Contamination)
summary(ALC.aov)

#-------------------------------------------------------------------------------

### ANCOVA Tests and More Complex Tests (Single Blocking Factor, not k-way ANOVA)
## ANCOVA Model Using Liquid Contamination, Blocking For Pump Area
ANC_LCPA.acv <- aov(Input.Average..N. ~ Press.Area..mm.2. + Liquid.Contamination, data = Experiment.df)
summary(ANC_LCPA.acv)
## ANCOVA Model Using Liquid Contamination, Blocking For Connecting Tube Length
ANC_LCTL.acv <- aov(Input.Average..N. ~ Tube.Length..mm. + Liquid.Contamination, data = Experiment.df)
summary(ANC_LCTL.acv)
## ANOVA Model Using Liquid Contamination, Blocking For Liquid Used
A_LCLU.aov <- aov(data = Experiment.df, Input.Average..N. ~ Liquid.Contamination*Liquid.Viscocity)
summary(A_LCLU.aov)

## ANCOVA Model Using Liquid Used, Blocking For Pump Area
ANC_LUPA.acv <- aov(Input.Average..N. ~ Press.Area..mm.2. + Liquid.Viscocity, data = Experiment.df)
summary(ANC_LUPA.acv)
## ANCOVA Model Using Liquid Used, Blocking For Connecting Tube Length
ANC_LUTL.acv <- aov(Input.Average..N. ~ Tube.Length..mm. + Liquid.Viscocity, data = Experiment.df)
summary(ANC_LUTL.acv)
## ANOVA Model Using Liquid Used, Blocking For Liquid Contamination (Same as LC blocking for LU)
A_LULC.aov <- aov(data = Experiment.df, Input.Average..N. ~ Liquid.Viscocity*Liquid.Contamination)
summary(A_LULC.aov)

## ANCOVA Model Using Press Area, Blocking For Connecting Tube Length
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
ANC_PATL.acv <- aov(Input.Average..N. ~ Tube.Length..mm. + Experiment.df$Press.Area..mm.2., data = Experiment.df)
summary(ANC_PATL.acv)
## ANOVA Model Using Press Area, Blocking For Liquid Contamination 
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
A_PALC.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Press.Area..mm.2.*Liquid.Contamination)
summary(A_PALC.aov)
## ANOVA Model Using Press Area, Blocking For Liquid Used
Experiment.df$Press.Area..mm.2. <- as.factor(Experiment.df$Press.Area..mm.2.)
A_PALU.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Press.Area..mm.2.*Liquid.Viscocity)
summary(A_PALU.aov)

## ANCOVA Model Using Connecting Tube Length, Blocking For Press Area
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
ANC_TLPA.acv <- aov(Input.Average..N. ~ Press.Area..mm.2. + Experiment.df$Tube.Length..mm., data = Experiment.df)
summary(ANC_TLPA.acv)
## ANOVA Model Using Connecting Tube Length, Blocking For Liquid Contamination 
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
A_TLLC.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Tube.Length..mm.*Liquid.Contamination)
summary(A_TLLC.aov)
## ANOVA Model Using Connecting Tube Length, Blocking For Liquid Used
Experiment.df$Tube.Length..mm. <- as.factor(Experiment.df$Tube.Length..mm.)
A_TLLU.aov <- aov(data = Experiment.df, Input.Average..N. ~ Experiment.df$Tube.Length..mm.*Liquid.Viscocity)
summary(A_TLLU.aov)
#-------------------------------------------------------------------------------------------------------------------------------------


## Multiple Linear Regression Models (Remember to run this without having run any of the code for ANOVA tests since it turns our
## Continuous Variables into Categorical ones) 
# Full Multiple Linear Regression Model
FMR.w <- lm(data = Experiment.df, Input.Average..N. ~ Press.Area..mm.2. + Tube.Length..mm. + Liquid.Viscocity + Liquid.Contamination)
summary(FMR.w)
