###########################################################
###########################################################
#         Homework Assignment PSYP13 Philip Howlett       #
###########################################################
###########################################################

# Creating models that predict the pain ratings of patients after surgery

###########################################################
#                                                         #
#                 Loading packages                        #
#                                                         #
###########################################################

library(psych) # for describe
library(lm.beta) # for lm.beta
library(car) # for scatter3d
library(ggplot2) # for ggplot
library(rgl) # for scatter3d
library (dplyr)
library(lmtest)
library(sandwich)



###########################################################
#                                                         #
#                 Data management and descriptives        #
#                                                         #
###########################################################

#Load the data set

data_original = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv")
### Check the dataset

View(data_original)

# Display simple descriptive statistics and plots.
describe(data_original)
describe(data_original$pain)
describe(data_original$STAI_trait)
describe(data_original$pain_cat)
describe(data_original$cortisol_serum)
describe(data_original$cortisol_saliva)
describe(data_original$mindfulness)

# histograms
hist(data_original$pain, breaks = 20)
hist(data_original$STAI_trait, breaks = 10)
hist(data_original$pain_cat, breaks = 10)
hist(data_original$mindfulness, breaks = 6)
hist(data_original$cortisol_saliva, breaks = 10)
hist(data_original$cortisol_serum, breaks = 20)

# scatterplot
plot(cortisol_serum ~ cortisol_saliva, data = data_original)
plot(pain ~ pain_cat, data = data_original)

# use the table() function for categorical data
table(data_original$sex)

#exclude or correct data that is not valid
data_improved = data_original # create a copy of the data where which will be cleaned

data_improved = data_improved[!data_improved$mindfulness <= 1, ]
data_improved$age[data_improved$age==222]=0
data_improved = data_improved[!data_improved$age <= 1, ]



###########################################################
#                                                         #
#                   Multiple regression                   #
#                                                         #
###########################################################

#Models
model1 = lm(pain ~ age + sex, data = data_improved)

model2 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness,
            data = data_improved)

#Summary
model1

model2

#Leverage
plot(data_improved$pain ~ data_improved$sex)
abline(lm(pain ~ sex, data = data_improved))

plot(data_improved$pain ~ data_improved$age)
abline(lm(pain ~ age, data = data_improved))

plot(data_improved$pain ~ data_improved$STAI_trait)
abline(lm(pain ~ STAI_trait, data = data_improved))

plot(data_improved$pain ~ data_improved$pain_cat)
abline(lm(pain ~ pain_cat, data = data_improved))

plot(data_improved$pain ~ data_improved$cortisol_serum)
abline(lm(pain ~ cortisol_serum, data = data_improved))

plot(data_improved$pain ~ data_improved$cortisol_saliva)
abline(lm(pain ~ cortisol_saliva, data = data_improved))

plot(data_improved$pain ~ data_improved$mindfulness)
abline(lm(pain ~ mindfulness, data = data_improved))

plot(model1, which = 5)
plot(model2, which = 5)

#Cook's distance
plot(x = model1, which = 4)
plot(x = model2, which = 4)

4/nrow(data_improved)

#Normality of Residuals
hist(x = residuals(model1), xlab = "Value of residual", main = "", breaks = 20)
hist(x = residuals(model2), xlab = "Value of residual", main = "", breaks = 20)

plot( x = model1, which = 2 )
plot( x = model2, which = 2 )

describe(residuals(model1))
describe(residuals(model2))

residualsdata_improved1 <- residuals(model1)
shapiro.test(residualsdata_improved1)
residualsdata_improved2 <- residuals (model2)
shapiro.test(residualsdata_improved2)


#Linearity 
linear1 <- fitted.values(object = model1)
plot(x = linear1, y=data_improved$age, xlab = "Fitted Values", ylab = "Observed values")
plot(x = linear1, y=data_improved$sex, xlab = "Fitted Values", ylab = "Observed values")
linear2 <- fitted.values(object = model2)
plot(x = linear2, y=data_improved$age, xlab = "Fitted Values", ylab = "Observed values", main = "Age")
plot(x = linear2, y=data_improved$sex, xlab = "Fitted Values", ylab = "Observed values", main = "Gender")
plot(x = linear2, y=data_improved$pain, xlab = "Fitted Values", ylab = "Observed values", main = "Pain")
plot(x = linear2, y=data_improved$STAI_trait, xlab = "Fitted Values", ylab = "Observed values", main = "STAI")
plot(x = linear2, y=data_improved$pain_cat, xlab = "Fitted Values", ylab = "Observed values", main = "Pain Cats")
plot(x = linear2, y=data_improved$cortisol_serum, xlab = "Fitted Values", ylab = "Observed values", main = "Cortisol Serum")
plot(x = linear2, y=data_improved$cortisol_saliva, xlab = "Fitted Values", ylab = "Observed values", main = "Cortisol saliva")
plot(x = linear2, y=data_improved$mindfulness, xlab = "Fitted Values", ylab = "Observed values", main = "Mindfulness")

residualPlots(model = model1)
residualPlots(model = model2)

#Homogenity of Variance
plot(x = model1, which = 3)
plot(x = model2, which = 3)

ncvTest(model1)
ncvTest(model2)

bptest(model1)
bptest(model2)

#Colineraity
vif(mod = model1)
vif(mod = model2)

##########################################################
##########################################################
###                 Removing cortisol_saliva          ####
##########################################################
##########################################################

model3 = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness,
            data = data_improved)

#Cook's distance
plot(x = model3, which = 4)


#Residuals
hist(x = residuals(model3), xlab = "Value of residual", main = "", breaks = 20)
plot( x = model3, which = 2 )
describe(residuals(model3))

#Linearity
linear3 <- fitted.values(object = model3)
plot(x = linear3, y=data_improved$age, xlab = "Fitted Values", ylab = "Observed values", main = "Age")
plot(x = linear3, y=data_improved$sex, xlab = "Fitted Values", ylab = "Observed values", main = "Gender")
plot(x = linear3, y=data_improved$pain, xlab = "Fitted Values", ylab = "Observed values", main = "Pain")
plot(x = linear3, y=data_improved$STAI_trait, xlab = "Fitted Values", ylab = "Observed values", main = "STAI")
plot(x = linear3, y=data_improved$pain_cat, xlab = "Fitted Values", ylab = "Observed values", main = "Pain Cats")
plot(x = linear3, y=data_improved$cortisol_serum, xlab = "Fitted Values", ylab = "Observed values", main = "Cortisol Serum")
plot(x = linear3, y=data_improved$mindfulness, xlab = "Fitted Values", ylab = "Observed values", main = "Mindfulness")

residualPlots(model = model3)

#Homogenity of Variance
plot(x = model3, which = 3)

ncvTest(model3)

bptest(model3)

#Colineraity
vif(mod = model3)



############################################################
############################################################
##               COMPARE MODELS                            #
############################################################
#############################################################

# Adj. R squared statistic

summary(model1)$adj.r.squared
summary(model3)$adj.r.squared

# ANOVA and AIC

anova(model1, model3)

AIC(model1)
AIC(model3)

#P Values, confidence intervals, coefficients

summary(model1)
confint(model1)
lm.beta(model1)

summary(model3)
confint(model3)
lm.beta(model3)


# the regression equation: Y = b0 + b1*X1 + b2*X2

######################################################################
######################################################################
######      A RESEARCHER CHALLENGES YOU                  #############
######################################################################
######################################################################

modelother = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight,
            data = data_improved)

modelother

#Leverage
plot(data_improved$pain ~ data_improved$sex)
abline(lm(pain ~ sex, data = data_improved))

plot(data_improved$pain ~ data_improved$age)
abline(lm(pain ~ age, data = data_improved))

plot(data_improved$pain ~ data_improved$STAI_trait)
abline(lm(pain ~ STAI_trait, data = data_improved))

plot(data_improved$pain ~ data_improved$pain_cat)
abline(lm(pain ~ pain_cat, data = data_improved))

plot(data_improved$pain ~ data_improved$cortisol_serum)
abline(lm(pain ~ cortisol_serum, data = data_improved))

plot(data_improved$pain ~ data_improved$cortisol_saliva)
abline(lm(pain ~ cortisol_saliva, data = data_improved))

plot(data_improved$pain ~ data_improved$mindfulness)
abline(lm(pain ~ mindfulness, data = data_improved))

plot(modelother, which = 5)

#Cook's distance
plot(x = modelother, which = 4)

4/nrow(data_improved)

#Residuals
hist(x = residuals(modelother), xlab = "Value of residual", main = "", breaks = 20)

plot( x = modelother, which = 3 )

residualsdata_improvedother <- residuals (modelother)
shapiro.test(residualsdata_improvedother)


#Linearity 
linear4 <- fitted.values(object = modelother)
plot(x = linear4, y=data_improved$age, xlab = "Fitted Values", ylab = "Observed values", main = "Age")
plot(x = linear4, y=data_improved$sex, xlab = "Fitted Values", ylab = "Observed values", main = "Gender")
plot(x = linear4, y=data_improved$pain, xlab = "Fitted Values", ylab = "Observed values", main = "Pain")
plot(x = linear4, y=data_improved$pain_cat, xlab = "Fitted Values", ylab = "Observed values", main = "Pain Cats")
plot(x = linear4, y=data_improved$cortisol_serum, xlab = "Fitted Values", ylab = "Observed values", main = "Cortisol Serum")
plot(x = linear4, y=data_improved$weight, xlab = "Fitted Values", ylab = "Observed values", main = "Weight")
plot(x = linear4, y=data_improved$mindfulness, xlab = "Fitted Values", ylab = "Observed values", main = "Mindfulness")
plot(x = linear4, y=data_improved$STAI_trait, xlab = "Fitted Values", ylab = "Observed values", main = "STAI")

residualPlots(model = modelother)

#Homogenity of Variance
plot(x = modelother, which = 3)

ncvTest(modelother)

bptest(modelother)

#Colineraity
vif(mod = modelother)

##############################################################
##############################################################
################      GO BACKWARDS               #############
##############################################################
##############################################################

step(object = modelother, diretion = "backward")

#########################################################
############ BACKWARDS MODEL ############################
#########################################################
#########################################################

backwardmodel = lm(pain ~ age + sex + pain_cat + cortisol_serum + mindfulness,
                   data = data_improved)

backwardmodel

summary(backwardmodel)
confint(backwardmodel)
lm.beta(backwardmodel)
summary(modelother)
confint(modelother)
lm.beta(modelother)

summary(modelother)$adj.r.squared
summary(backwardmodel)$adj.r.squared

AIC(modelother)
AIC(backwardmodel)

anova(backwardmodel, modelother)


#####ANOVA
theorymodel = model3

theorymodel

anova(backwardmodel, theorymodel)

AIC(backwardmodel)
AIC(theorymodel)


##############################################################
##############################################################
#######      NEW DATA SET       ##############################
##############################################################
##############################################################

new_original = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv")
### Check the dataset

View(new_original)

########exclude data
new_improved = new_original
new_improved = new_improved[!new_improved$mindfulness <= 1, ]

#########predict

pred_theorymodel <- predict(theorymodel, new_improved)
pred_theorymodel

pred_backwardmodel <- predict(backwardmodel, new_improved)
pred_backwardmodel

RSS_test_theory = sum((new_improved[, "pain"] - pred_theorymodel)^2)
RSS_test_back = sum((new_improved[, "pain"] - pred_backwardmodel)^2)

RSS_test_theory
RSS_test_back

##########################################################################
##########################################################################
##########          TIME BECOMES AN ISSUE   ##############################
##########################################################################
##########################################################################


### Data management and descriptive statistics

## Loading packages

library(psych) # for describe
library(reshape2) # for melt function
library(ggplot2) # for ggplot
library(cAIC4) # for cAIC
library(r2glmm) # for r2beta
library(influence.ME) # for influence (this will also load the lme4 package)
library(lattice) # for qqmath


## Custom functions

# This is a function to extract standardized beta coefficients from linear mixed models.
# This function was adapted from: https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model


stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}


##Loading data

last_original = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv")

# ID as factor
last_original$ID = factor(last_original$ID)

View(last_original)


## Check the dataset

# descriptives
describe(last_original)
table(last_original)

# histograms
hist(last_original$pain1)
hist(last_original$pain2)
hist(last_original$pain3)
hist(last_original$pain4)
hist(last_original$STAI_trait)
hist(last_original$pain_cat)
hist(last_original$cortisol_serum)
hist(last_original$cortisol_saliva)
hist(last_original$mindfulness)
hist(last_original$weight)


### Repeated measures analysis using linear mixed models

#  repeated varibales
repeated_variables = c("pain1", "pain2", "pain3",	"pain4")

# correlation of repeated variables
cor(last_original[,repeated_variables])

## Reshape dataframe

last_long = melt(last_original, measure.vars=repeated_variables, variable.name = "day", value.name = "pain_rating")

# order data frame by participant ID
last_long$ID = as.numeric(last_long$ID)
last_long = last_long[order(last_long[,"ID"]),]

# change the time variable to a numerical vector
last_long$day = as.numeric(last_long$day)

View(last_long)


## Building the linear mixed model


lastint = lmer(pain_rating ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + day + (1|ID), data = last_long)
lastslope = lmer(pain_rating ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + day + (day|ID), data = last_long)

#########
sum(residuals(lastint)^2)
sum(residuals(lastslope)^2)

## Plotting models
actualdata = ggplot(last_long, aes(y = pain_rating, x = day, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pain_rating, x=day))+
  facet_wrap( ~ ID, ncol = 5)
actualdata


int_plot = ggplot(lastint, aes(y = pain_rating, x = day, group = ID))+
  geom_point(aes(color = ID), size = 4)+
  geom_smooth(aes(color = ID), method = "lm", formula = "y ~ x", fill = NA, fullrange = TRUE)
int_plot


slope_plot = ggplot(lastslope, aes(y = pain_rating, x = day,group = ID))+
  geom_point(aes(color = ID), size = 4)+
  geom_smooth(aes(color = ID), method = "lm", formula  = "y ~ x", fill = NA, fullrange = TRUE)+
  xlim(-1, 50)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)
slope_plot


# Model comparison

last_longwithpreds = last_long
last_longwithpreds$pred_int = predict(lastint)
last_longwithpreds$pred_slope = predict(lastslope)

# random intercept model
ggplot(last_longwithpreds, aes(y = pain_rating, x = day, group = ID))+
  geom_point(size = 3, colour = 'darkblue')+
  geom_line(color='darkorange', aes(y=pred_int, x=day))+
  facet_wrap( ~ ID, ncol = 5)

# random slope and intercept model
ggplot(last_longwithpreds, aes(y = pain_rating, x = day, group = ID))+
  geom_point(size = 3, colour='darkblue')+
  geom_line(color='darkorange', aes(y=pred_slope, x=day))+
  facet_wrap( ~ ID, ncol = 5)


# cAIC

cAIC(lastint)$caic
cAIC(lastslope)$caic

anova(lastint, lastslope)
r2beta(lastslope, method = "nsj", data = last_long)

## Adding the quadratic term of time to the model

lastslopequad = lmer(pain_rating ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + day + I(day^2) + (day|ID), data = last_long)

# And add the predictions 
last_longwithpreds$pred_quad = predict(lastslopequad)


# Compare

last_longwithpreds$pred_quad = predict(lastslopequad)

plot_quad = ggplot(last_longwithpreds, aes(y = pain_rating, x = day, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_quad, x=day))+
  facet_wrap( ~ ID, ncol = 5)


plot_quad

sum(residuals(lastslope)^2)
sum(residuals(lastslopequad)^2)

cAIC(lastslope)$caic
cAIC(lastslopequad)$caic

anova(lastslope, lastslopequad)


# Refit model
last_longdaycentered = last_long
last_longdaycentered$day_centered = last_longdaycentered$day - mean(last_longdaycentered$day)


lastslopequad = lmer(pain_rating ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + day + I(day_centered^2) + (day|ID), data = last_longdaycentered)

# Stats

# Marginal R squared
r2beta(lastslopequad, method = "nsj", data = last_longdaycentered)

# Conditional AIC
cAIC(lastslopequad)

# Model coefficients
summary(lastslopequad)

# Confidence intervals for the coefficients
confint(lastslopequad)

# standardized Betas
stdCoef.merMod(lastslopequad)


anova(lastslopequad)

###########################################################
#              Model diagnostics of mixed models          #
###########################################################


## Loading packages

# You will need the following packages for this exercise.

library(psych) # for pairs.panels
library(ggplot2) # for ggplot
library(reshape2) # for melt function
library(influence.ME) # for influence (this will also load the lme4 package)
library(lattice) # for qqmath

## Load wound healing data

last_original = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv")



# designate which are the repeated varibales
repeated_variables = c("pain1", "pain2", "pain3",	"pain4")

# asign ID and location as factors
last_original$ID = factor(last_original$ID)

# 'long format'.

last_long = melt(last_original, measure.vars=repeated_variables, variable.name = "day", value.name = "pain_rating")
last_long$ID = as.numeric(last_long$ID)
last_long = last_long[order(last_long[,"ID"]),]
last_long$day = as.numeric(last_long$day)

# center the variable 'time' .

last_longdaycentered = last_long
last_longdaycentered$day_centered = last_longdaycentered$day - mean(last_longdaycentered$day)


# sqared time as a new variable.  

last_longdaycentered$day_centered_2 = last_longdaycentered$day_centered^2

## building the model


lastslopequad = lmer(pain_rating ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + day + I(day_centered^2) + (day|ID), data = last_longdaycentered)

last_resid = last_longdaycentered
last_resid$resid = residuals(lastslopequad)

##################
####################
####################

## Influential outliers

influence_observation = influence(lastslopequad, obs = T)$alt.fixed # this can take a minute or so
influence_group = influence(lastslopequad, group = "ID")$alt.fixed


boxplot(influence_observation[,"day_centered"])


pred_names = colnames(influence_group)

par(mfrow=c(1, length(pred_names)))
for(i in 1:length(pred_names)){
  boxplot(influence_observation[,pred_names[i]], main = pred_names[i])
}


for(i in 1:length(pred_names)){
  boxplot(influence_group[,pred_names[i]], main = pred_names[i])
}
par(mfrow=c(2,4))


## Normality

dev.off()
qqmath(lastslopequad, id=0.05)


qqmath(ranef(lastslopequad))

## Linearity

dev.off()
plot(lastslopequad, arg = "pearson")

plot(resid ~ day_centered, data = last_resid)
plot(resid ~ day_centered_2, data = last_resid)
plot(resid ~ age, data = last_resid)
plot(resid ~ sex, data = last_resid)
plot(resid ~ STAI_trait, data = last_resid)
plot(resid ~ pain_cat, data = last_resid)
plot(resid ~ cortisol_serum, data = last_resid)
plot(resid ~ mindfulness, data = last_resid)

## Homoscedasticity


plot(lastslopequad, arg = "pearson")

homosced_mod = lm(last_resid$resid^2 ~ last_resid$ID)
summary(homosced_mod)

# caluclate interquartile range within each cluster
IQR_of_residuals_by_participant = sapply(split(last_resid, f = last_resid$ID), function(x) IQR(x$resid))

# rank ordering them
rank = rank(IQR_of_residuals_by_participant)
# adding rank to the dataframe containing the residuals
last_resid$rank = rep(rank, each = length(repeated_variables))
# creating a vector of participant IDs ordered based on the rank
IDforplot = unique(last_resid$ID[order(last_resid$rank)])

# create the cyclone plot
ggplot(last_resid, aes(y = resid, x = factor(rank), labels = ID))+
  geom_boxplot()+
  scale_x_discrete(labels=IDforplot)+
  coord_flip()



## Multicollinearity

pairs.panels(last_longdaycentered[,c("age", "sex", "STAI_trait", "pain_cat", "cortisol_serum", "mindfulness",  "day_centered", "day_centered_2")], col = "red", lm = T)
