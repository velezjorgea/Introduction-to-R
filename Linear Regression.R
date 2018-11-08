# Introduction to R: Linear Regressions

## Multiple Regression Models

#Scateer Plots

"Scatter plots can help visualize any linear relationships between the dependent (response) 
variable and independent (predictor) variables. Ideally, if you are having multiple 
predictor variables, a scatter plot is drawn for each one of them against the response, 
along with the line of best as seen below."

head(cars)
cars[1:10,]
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")

"The scatter plot along with the smoothing line above suggests a linearly increasing 
relationship between the 'dist' and 'speed' variables. This is a good thing, because, 
one of the underlying assumptions in linear regression is that the relationship 
between the response and predictor variables is linear and additive."

##2. BoxPlot - Check for outliers

"Generally, any datapoint that lies outside the 1.5 * interquartile-range (1.5 * IQR) 
is considered an outlier, where, IQR is calculated as the distance between the 25th 
percentile and 75th percentile values for that variable."

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

#3. Density plot 
"Check if the response variable is close to normality"

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")


##4. Correlation
"Correlation is a statistical measure that suggests the level of linear dependence between 
two variables, that occur in pair - just like what we have here in speed and dist. 
Correlation can take values between -1 to +1. If we observe for every instance where speed 
increases, the distance also increases along with it, then there is a high positive correlation 
between them and therefore the correlation between them will be closer to 1. The opposite is true for an inverse relationship, in which case, the correlation between the variables will be close to -1.
A value closer to 0 suggests a weak relationship between the variables. 
A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the 
response variable (Y) is unexplained by the predictor (X), in which case, 
we should probably look for better explanatory variables."

cor(cars$speed, cars$dist)


##5. Build Linear Model
"Now that we have seen the linear relationship pictorially in the scatter plot and by 
computing the correlation, lets see the syntax for building the linear model. 
The function used for building linear models is lm(). The lm() function takes in 
two main arguments, namely: 1. Formula 2. Data. The data is typically a data.frame 
and the formula is a object of class formula. But the most common convention is to
write out the formula directly in place of the argument as written below."

linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)

##6. Linear Regression Diagnostics
"Now the linear model is built and we have a formula that we can use to predict the dist value 
if a corresponding speed is known. Is this enough to actually use this model? NO! Before
using a regression model, you have to ensure that it is statistically significant. 
How do you ensure this? Lets begin by printing the summary statistics for linearMod."

summary(linearMod)  

##7. How to calculate the t Statistic and p-Values?
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

t_value
p_value
f_statistic
f
model_p

##7. AIC and BIC
"The Akaike's information criterion - AIC (Akaike, 1974) and the Bayesian information 
criterion - BIC (Schwarz, 1978) are measures of the goodness of fit of an estimated 
statistical model and can also be used for model selection. Both criteria depend on 
the maximized value of the likelihood function L for the estimated model."
AIC(linearMod) 
BIC(linearMod)

##8. Predicting Linear Models

"So far we have seen how to build a linear regression model using the whole dataset. 
If we build it that way, there is no way to tell how the model will perform with new data.
So the preferred practice is to split your dataset into a 80:20 sample (training:test), 
then, build the model on the 80% sample and then use the model thus built to predict 
the dependent variable on test data.

Doing it this way, we will have the model predicted values for the 20% data (test) as
well as the actuals (from the original dataset). By calculating accuracy measures 
(like min_max accuracy) and error rates (MAPE or MSE), we can find out the prediction 
accuracy of the model. Now, lets see how to actually do this.."

"Step 1: Create the training (development) and test (validation) data samples from 
original data."
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

"Step 2: Develop the model on the training data and use it to predict the distance on test data"
# Build the model on training data -
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

summary (lmMod)  # model summary

"From the model summary, the model p value and predictor's p value are less than the 
significance level, so we know we have a statistically significant model.
Also, the R-Sq and Adj R-Sq are comparative to the original model built on full data."

##Step 4: Calculate prediction accuracy and error rates
"A simple correlation between the actuals and predicted values can be used as a form of 
accuracy measure. A higher correlation accuracy implies that the actuals and predicted 
values have similar directional movement, i.e. when the actuals values increase the 
predicteds also increase and vice-versa."

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
head(actuals_preds)

##Now lets calculate the Min Max accuracy and MAPE (mean absolute percentage error)
apply
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

##k- Fold Cross validation

"Suppose, the model predicts satisfactorily on the 20% split (test data), is that enough 
to believe that your model will perform equally well all the time? It is important to
rigorously test the model's performance as much as possible. One way is to ensure that 
the model equation you have will perform well, when it is 'built' on a different subset 
of training data and predicted on the remaining data.

How to do this is? Split your data into 'k' mutually exclusive random sample portions. 
Keeping each portion as test data, we build the model on the remaining (k-1 portion) 
data and calculate the mean squared error of the predictions. This is done for each of 
the 'k' random sample portions. Then finally, the average of these mean squared errors 
(for 'k' portions) is computed. We can use this metric to compare different linear models.

By doing this, we need to check two things:

If the model's prediction accuracy isn't varying too much for any one particular sample, 
and
If the lines of best fit don't vary too much with respect the the slope and level.
In other words, they should be parallel and as close to each other as possible. 
You can find a more detailed explanation for interpreting the cross validation charts 
when you learn about advanced linear model building."
#install.packages("MASS") 
#install.packages("DAAG")

library(DAAG)
cvResults <- suppressWarnings(CVlm(data = cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
attr(cvResults, 'ms')  # => 251.2783 mean squared error

"In the below plot, Are the dashed lines parallel? Are the small and big symbols are 
not over dispersed for one particular color?

"