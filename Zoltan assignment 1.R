rm(list=ls(all=TRUE)) # clears the workspace
graphics.off()  # clears graphics

##############################
# Load the packages
##############################
require(psych)
require(lsr)
library(dplyr) # for data management
library(gsheet) # to read data from google sheets
library(ggplot2) # for ggplot
require(car)

error_plotter <- function(mod, col = "black", x_var = NULL){
  mod_vars = as.character(mod$call[2])
  data = eval(parse(text = as.character(mod$call[3])))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))
  
  data$pred = predict(mod)
  
  if(x == "1" & is.null(x_var)){x = "response_ID"
  data$response_ID = 1:nrow(data)} else if(x == "1"){x = x_var}
  
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)
  abline(mod)
  
  for(i in 1:nrow(data)){
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))
    abline(v = data[i,x], lty = 2, col = col)
  }
  
}

#####################################
# Load the data
#####################################


data1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv")

data.backup = data1 # backup

#####################################
# Descriptives and normality checks
#####################################

describe(data1)

table(data1$sex)

hist(data1$pain, breaks = 30)
hist(data1$age, breaks = 30)
hist(data1$STAI_trait, breaks = 30)
hist(data1$pain_cat, breaks = 30)
hist(data1$cortisol_serum, breaks = 30)
hist(data1$cortisol_saliva, breaks = 30)
hist(data1$mindfulness, breaks = 30)
hist(data1$weight, breaks = 30)

# all variables appear to be normally distributed.

data1 = data1[-28,] # removed outlier whose age was 222 years.

# Checking the dataset for multivariate outliers with Mahalanobis distances.

data1$num = c(1:159) # Creating a dummy variable to predict in a linear model.

mod.check = lm(formula = num ~ pain + sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness + weight, data = data1)
summary(mod.check)

lev.check=hat(model.matrix(mod.check)) # leverage
plot(lev.check)
# Calculate Mahalanobis distance
N = nrow(data1)
mahad.check=(N-1)*(lev.check - 1/N)
tail(sort(mahad.check),5)
order(mahad.check,decreasing=T)[c(5,4,3,2,1)] # shows the 5 data points with the highest Mahalanobis distance.
# Using a chi^2 table with df = 9 and alpha = .001, we get a cut-off score of 27.877. 
# Thus, no entries are significant outliers according to this procedure.

######################################################
# Multiple regression
######################################################

mod1 = lm(formula = pain ~ sex + age, data = data1)
summary(mod1)

mod2 = lm(formula = pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data1)
summary(mod2)

##########################################################
# Checking for multivariate outliers in regression models
##########################################################

lev.mod2=hat(model.matrix(mod2)) # leverage for model 2
plot(lev.mod2)
# Calculate Mahalanobis distance
mahad.mod2=(N-1)*(lev.mod2 - 1/N)
tail(sort(mahad.mod2),5)
order(mahad.mod2,decreasing=T)[c(5,4,3,2,1)]
# Using a chi^2 table with df = 7 and alpha = .001, we get a cut-off score of 24.322. 
# Row 89 is identified as an outlier with a score of 25.89, and is therefore excluded.
data1 = data1[-89,] # removed outlier.

mod1 = update(mod1) # Updated model 1 after removing the outlier.

lev.mod1=hat(model.matrix(mod1)) # leverage for updated model 1
plot(lev.mod1)
N2 = nrow(data1) # Updated N value to account for the outlier.
mahad.mod1=(N2-1)*(lev.mod1 - 1/N2)
tail(sort(mahad.mod1),5)
order(mahad.mod1,decreasing=T)[c(5,4,3,2,1)]
# Using a chi^2 table with df = 2 and alpha = .001, we get a cut-off score of 13.816. 
# Row 43 is identified as an outlier with a score of 14.76, and is therefore excluded.
data1 = data1[-43,] # removed outlier.

mod1 = update(mod1) # Updated model 1 after removing the second outlier.
mod2 = update(mod2) # Updated model 2 after removing the second outlier.

lev.mod1=hat(model.matrix(mod1)) # leverage for updated model 1
plot(lev.mod1)
N3 = nrow(data1) # Updated N value to account for the outlier.
mahad.mod1=(N3-1)*(lev.mod1 - 1/N3)
tail(sort(mahad.mod1),5)
order(mahad.mod1,decreasing=T)[c(5,4,3,2,1)]

lev.mod2=hat(model.matrix(mod2)) # leverage for updated model 2
plot(lev.mod2)
# Calculate Mahalanobis distance
mahad.mod2=(N3-1)*(lev.mod2 - 1/N3)
tail(sort(mahad.mod2),5)
order(mahad.mod2,decreasing=T)[c(5,4,3,2,1)]

# No more outliers are found in the models.

###################################################
# Checking the assumptions of the regression model
###################################################

hist(residuals(mod2))
boxplot(residuals(mod2))
shapiro.test(residuals(mod2)) # Normality test. Null hypothesis = normal distribution.
# Fails to reject the null hypothesis, i.e., the residuals are normally distributed.

par(mfrow=c(2,2))  # set 2 rows and 2 columns plot layout
plot(mod2) # Straight lines in the upper left and lower left plots are indicative of homoscedasticity.
ncvTest(mod2) # Tests for heterogeneity of variance. Null hypothesis = homogeneity of variance.
# p > .05, meaning that the assumption of homogeneity of variance has not been violated.

par(mfrow=c(1,1))  # set 1 row and 1 column plot layout

dataCor= data1
dataCor = dataCor[4:10]
dataCor$pain = data1$pain
cor(dataCor) # Correlation matrix for the variables used in mod2. cortisol_serum and _saliva are highly correlated, r = 0.88.

vif(mod2) # Tests the model for multicollinearity. The variance inflation factor (VIF) identifies correlation between
# independent variables and the strength of that correlation. VIF > 5 means critical levels of multicollinearity.
# cortisol_saliva has a VIF of 5.45. Therefore, a new model is created without this variable.

mod3 = lm(formula = pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data1)
summary(mod3)
vif(mod3) # much lower values than for model 2.

lev.mod3=hat(model.matrix(mod3)) # leverage for model 3
plot(lev.mod3)
# Calculate Mahalanobis distance
mahad.mod3=(N3-1)*(lev.mod3 - 1/N3)
tail(sort(mahad.mod3),5)
order(mahad.mod3,decreasing=T)[c(5,4,3,2,1)]
# No signs of multivariate outliers in model 3. (df=6, alpha=.001 gives a cut-off score of 22.458)

hist(residuals(mod3))
boxplot(residuals(mod3))
shapiro.test(residuals(mod3)) # Normality test.
# Fails to reject the null hypothesis, i.e., the residuals are normally distributed.

par(mfrow=c(2,2))  # set 2 rows and 2 columns plot layout
plot(mod3)
ncvTest(mod3) # Tests for heterogeneity of variance. 
# p > .05, meaning that the assumption of homogeneity of variance has not been violated.
par(mfrow=c(1,1))  # set 1 row and 1 column plot layout

###################################################
# Comparison between model 1 and 3
###################################################

summary(mod1)
summary(mod3)

anova(mod1, mod3) # Compare the two regression models to see if the second one is significantly better than the first one.

require(lm.beta)
lm.beta(mod1) # Standardized beta values.
lm.beta(mod3)

confint(mod1) # Confidence intervals
confint(mod3) 

AIC(mod1) # Calculate the Akaike information criterion (AIC) value (the smaller, the better). 
AIC(mod2)
AIC(mod3)

# Double-check to see if the model would be better with cortisol_saliva instead of cortisol_serum.
mod4 = lm(formula = pain ~ sex + age + STAI_trait + pain_cat + cortisol_saliva + mindfulness, data = data1)
summary(mod4)
anova(mod3, mod4)
AIC(mod3)
AIC(mod4) # Higher AIC for model 4, meaning that model 3 is better.