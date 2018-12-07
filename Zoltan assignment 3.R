rm(list=ls(all=TRUE)) # clears the workspace
graphics.off()  # clears graphics

##############################
# Load the packages
##############################

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


#####################################
# Load the data
#####################################

data3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv")

data3.backup = data3 # Backup of the data.


#####################################
# Descriptives and normality checks
#####################################

describe(data3)

table(data3$sex)

hist(data3$pain1, breaks = 30)
hist(data3$pain2, breaks = 30)
hist(data3$pain3, breaks = 30)
hist(data3$pain4, breaks = 30)
hist(data3$age, breaks = 30)
hist(data3$STAI_trait, breaks = 30)
hist(data3$pain_cat, breaks = 30)
hist(data3$cortisol_serum, breaks = 30)
hist(data3$cortisol_saliva, breaks = 30)
hist(data3$mindfulness, breaks = 30)
hist(data3$weight, breaks = 30)
# Data appears to be normal and without any obvious errors.

#######################################################
# Repeated measures analysis using linear mixed models
#######################################################

# Rename the dependent variables.
colnames(data3)[colnames(data3)=="pain1"] <- "pain_1"
colnames(data3)[colnames(data3)=="pain2"] <- "pain_2"
colnames(data3)[colnames(data3)=="pain3"] <- "pain_3"
colnames(data3)[colnames(data3)=="pain4"] <- "pain_4"

# designate which are the repeated variables
repeated_variables = c("pain_1", "pain_2", "pain_3",	"pain_4")

# correlation of repeated variables
cor(data3[,repeated_variables])
# The repeated variables are highly correlated, which is good since repeated measures requires clustered data.

# Reshape dataset from wide form to long form.
data3_long = melt(data3, measure.vars=repeated_variables, variable.name = "time", value.name = "pain_rating")

data3_long = data3_long[order(data3_long[,"ID"]),] # sort data by ID.
data3_long$time = as.numeric(data3_long$time) # Changes the time variable to a numerical vector.

# Creating the linear mixed models.

mod_rep_int = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + time + (1|ID), data = data3_long)
mod_rep_slope = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + time + (time|ID), data = data3_long)

############################################
# Model comparison
############################################

data3_long_withpred = data3_long # save data3_long as a backup.

# Add predictions from the two models into two respective columns.
data3_long_withpred$pred_int = predict(mod_rep_int)
data3_long_withpred$pred_slope = predict(mod_rep_slope)

# Random intercept model
ggplot(data3_long_withpred, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x=time))+
  facet_wrap( ~ ID, ncol = 5)+
  xlab("Time (days)")+
  ylab("Pain experience")

# Random slope and intercept model
ggplot(data3_long_withpred, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope, x=time))+
  facet_wrap( ~ ID, ncol = 5)+
  xlab("Time (days)")+
  ylab("Pain experience")
# mod_rep_slope seems to fit the data points better, judging by the graphs.

# Compare the cAIC (conditional AIC). Lower score = better model.
cAIC(mod_rep_int)$caic
cAIC(mod_rep_slope)$caic
# The models have the exact same cAIC, meaning that they should be exactly as good.

# Comparison by likelihood rating test.
anova(mod_rep_int, mod_rep_slope)
# mod_rep_slope is significantly better according to the test.

# New model with quadratic time, to account for the somewhat curvilinear shape of the plots.
mod_slope_quadtime = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + time + I(time^2) + (time|ID), data = data3_long)

# Add predictions using quadratic model.
data3_long_withpred$pred_slope_quadtime = predict(mod_slope_quadtime)

# Plot quadratic model.
plot_quad = ggplot(data3_long_withpred, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope_quadtime, x=time))+
  facet_wrap( ~ ID, ncol = 5)+
  xlab("Time (days)")+
  ylab("Pain experience")
plot_quad

# Compare the models.
cAIC(mod_rep_slope)$caic
cAIC(mod_slope_quadtime)$caic
# Quadratic model has lower cAIC --> better model.

anova(mod_rep_slope, mod_slope_quadtime)
# Quadratic model significantly better than non-quadratic model with random slope.

# Center the time variable to remove any correlation between time and time^2.
data3_long_centered_time = data3_long
data3_long_centered_time$time_centered = data3_long_centered_time$time - mean(data3_long_centered_time$time)

# New model with centered time.
mod_slope_quadtime2 = lmer(pain_rating ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + time_centered + I(time_centered^2) + (time_centered|ID), data = data3_long_centered_time)

cAIC(mod_slope_quadtime)$caic
cAIC(mod_slope_quadtime2)$caic # Double-check to ensure the value is the same.


r2beta(mod_slope_quadtime2, method = "nsj", data = data3_long_centered_time) # Marginal R squared

cAIC(mod_slope_quadtime2)$caic # Conditional AIC

summary(mod_slope_quadtime2) # Model coefficients

confint(mod_slope_quadtime2) # Confidence intervals for the coefficients

stdCoef.merMod(mod_slope_quadtime2) # Standardized Betas


#######################################
# Model diagnostics
#######################################

# Adding a column with squared time as a fixed variable.
data3_long_centered_time$time_centered_square = data3_long_centered_time$time_centered^2


data3_long_resid = data3_long_centered_time # Data backup
data3_long_resid$residuals = residuals(mod_slope_quadtime2) # Adding residuals to dataset.

# Checking for outliers
influence_observation = influence(mod_slope_quadtime2, obs = T)$alt.fixed
influence_group = influence(mod_slope_quadtime2, group = "ID")$alt.fixed


pred_names = colnames(influence_group)

for(i in 1:length(pred_names)){
  boxplot(influence_observation[,pred_names[i]], main = pred_names[i])
}


for(i in 1:length(pred_names)){
  boxplot(influence_group[,pred_names[i]], main = pred_names[i])
}
# Some data points might be outliers, but personally I don't think they appear to be very influential.
# I will therefore treat them as non-outliers.

#############################

# Check normality
qqmath(mod_slope_quadtime2) # QQ plot for the residuals of the quadratic model.

qqmath(ranef(mod_slope_quadtime2)) # QQ plot for the random effects.
# Some deviations from normality might be detected for the random effects.

hist(data3_long_resid$residuals) # Judging by the histogram, the residuals look somewhat normally distributed though.
shapiro.test((data3_long_resid$residuals)) # Also indicates a normal distribution of residuals. (not sure if applicable)

##############################

# Linearity

plot(mod_slope_quadtime2, arg = "pearson", xlab = "Fitted", ylab = "Residuals (Pearson)") # Might be somewhat linear.


plot(residuals ~ age, data = data3_long_resid)
plot(residuals ~ sex, data = data3_long_resid)
plot(residuals ~ STAI_trait, data = data3_long_resid)
plot(residuals ~ pain_cat, data = data3_long_resid)
plot(residuals ~ cortisol_serum, data = data3_long_resid)
plot(residuals ~ mindfulness, data = data3_long_resid)
plot(residuals ~ time_centered, data = data3_long_resid, xlab = "Time (centered)", ylab = "Residuals")
plot(residuals ~ time_centered_square, data = data3_long_resid)
# All residuals look somewhat linear, except for time_centered that looks slightly curved.

###############################

# Homoscedasticity on the observation level

plot(mod_slope_quadtime2, arg = "pearson") # Variance seems somewhat evenly distributed.


# Homoscedasticity across clusters (using a cyclone plot)

# caluclate interquartile range within each cluster
IQR_of_residuals_by_participant = sapply(split(data3_long_resid, f = data3_long_resid$ID), function(x) IQR(x$resid))
# rank ordering them
rank = rank(IQR_of_residuals_by_participant)
# adding rank to the dataframe containing the residuals
data3_long_resid$rank = rep(rank, each = length(repeated_variables))
# creating a vector of participant IDs ordered based on the rank, this will be used as labels
IDforplot = unique(data3_long_resid$ID[order(data3_long_resid$rank)])

# create the cyclone plot
ggplot(data3_long_resid, aes(y = residuals, x = factor(rank), labels = ID))+
  geom_boxplot()+
  scale_x_discrete(labels=IDforplot)+
  coord_flip()
# The cyclone plot shows some deviations, specifically for ID 11 and 12. We might suspect heteroscedasticity.

# Significance test of homoscedasticity across clusters by fitting a linear model where we predict residuals using participant ID (clustering variable).
homosced_mod = lm(data3_long_resid$resid^2 ~ data3_long_resid$ID)
summary(homosced_mod)
# F-test reveals that p > .05, meaning that heteroscedasticity should not be problematic in this case.

###############################

# Multicollinearity

pairs.panels(data3_long_centered_time[,c("sex", "age", "STAI_trait", "pain_cat", "cortisol_serum", "mindfulness", "time_centered", "time_centered_square")], col = "red", lm = T)
# No signs of problematic multicollinearity.
