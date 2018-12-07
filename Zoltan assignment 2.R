######################################################################
# Load and run the script from "Zoltan assignment 1" before starting
######################################################################

mod5 = lm(formula = pain ~ sex + age + weight + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data1)

lev.mod5=hat(model.matrix(mod5)) # leverage for model 5
par(mfrow=c(1,1))  # set 1 row and 1 column plot layout
plot(lev.mod5)
# Calculate Mahalanobis distance
mahad.mod5=(N3-1)*(lev.mod5 - 1/N3)
tail(sort(mahad.mod5),5)
order(mahad.mod5,decreasing=T)[c(5,4,3,2,1)]
# Using a chi^2 table with df = 7 and alpha = .001, we get a cut-off score of 24.322. 
# No multivariate outliers were found.

hist(residuals(mod5))
boxplot(residuals(mod5))
shapiro.test(residuals(mod5)) # Normality test. Null hypothesis = normal distribution.
# Fails to reject the null hypothesis, i.e., the residuals are normally distributed.

par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(mod5) # Straight lines in the upper left and lower left plots are indicative of homoscedasticity.
ncvTest(mod5) # Tests for heterogeneity of variance. Null hypothesis = homogeneity of variance.
# p > .05, meaning that the assumption of homogeneity of variance has been satisfied.
par(mfrow=c(1,1))  # set 1 row and 1 column plot layout

dataCor$weight = data1$weight
cor(dataCor) # Correlation matrix for the variables used in mod2. cortisol_serum and _saliva are highly correlated, r = 0.88.
# However, cortisol_saliva is not used in this model.
vif(mod5) # Tests the model for multicollinearity. 
# No variable has a VIF > 5, meaning that the assumption of multicollinearity was satisfied.



step(object = mod5, direction = "backward") # Backward regression using model 5.

mod.back = lm(formula = pain ~ sex + age + pain_cat + cortisol_serum + mindfulness, data = data1) # Backward model

# Test the assumptions of the regression model (normality, etc..)
lev.mod.back=hat(model.matrix(mod.back)) # leverage for backward model
par(mfrow=c(1,1))  # set 1 row and 1 column plot layout
plot(lev.mod.back)
# Calculate Mahalanobis distance
mahad.mod.back=(N3-1)*(lev.mod.back - 1/N3)
tail(sort(mahad.mod.back),5)
order(mahad.mod.back,decreasing=T)[c(5,4,3,2,1)]
# # No signs of multivariate outliers in backward model. (df=5, alpha=.001 gives a cut-off score of 20.515)

hist(residuals(mod.back))
boxplot(residuals(mod.back))
shapiro.test(residuals(mod.back)) # Normality test. Null hypothesis = normal distribution.
# Fails to reject the null hypothesis, i.e., the residuals are normally distributed.

par(mfrow=c(2,2))  # set 2 rows and 2 column plot layout
plot(mod.back) # Straight lines in the upper left and lower left plots are indicative of homoscedasticity.
ncvTest(mod.back) # Tests for heterogeneity of variance. Null hypothesis = homogeneity of variance.
# p > .05, meaning that the assumption of homogeneity of variance has been satisfied.
par(mfrow=c(1,1))  # set 1 row and 1 column plot layout

summary(mod.back)
lm.beta(mod.back)
confint(mod.back)

# Model comparison between backward model and intial model (model submitted to backward regression).
AIC(mod.back, mod5)
anova(mod.back, mod5)

# Model comparison between backward model and model 3.
AIC(mod.back, mod3)
# Backward model is better, because it has lower AIC.
anova(mod.back, mod3)
# The ANOVA does not give any significant diffeence in explained variance, however.

#################################################
# Load the new data on which to make predictions
#################################################

data2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv")

# Check for normality in the dependent variable.
hist(data2$pain)
plot(data2$pain)
shapiro.test(data2$pain)
describe(data2$pain)
# No signs of non-normality.

#################################################
# Prediction and comparison between models
#################################################

# Prediction
pred.mod3 = predict.lm(object = mod3, newdata = data2)
pred.mod.back = predict.lm(object = mod.back, newdata = data2)

plot(pred.mod3, data2$pain, xlab = "Predicted pain experience", ylab = "Actual pain experience", main = "Model 3")
abline(a = 0, b = 1)

plot(pred.mod.back, data2$pain, xlab = "Predicted pain experience", ylab = "Actual pain experience", main = "Backward regression model")
abline(a = 0, b = 1)


# Comparison between the predictions of model 3 and backward model.
RSS.mod3 = sum((data2$pain - pred.mod3)^2) # Residual sum of squared differences (RSS) for mod3.
RSS.mod.back = sum((data2$pain - pred.mod.back)^2) # Residual sum of squared differences (RSS) for backward model.
RSS.mod3
RSS.mod.back # Backward regression model has more error.