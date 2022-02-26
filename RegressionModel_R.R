# Multiple Linear Regression
# Importing the dataset

# The 1st step is setting your working directory "location of your dataset in your device"
dataset = read.csv('bostn.csv')

View(dataset)


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$target, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


View(training_set)
View(test_set)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Note: There is no need to do feature scaling since the package we are using takes care of this for us

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = target ~  NOX+AGE +RAAD,
               data = training_set)
# Note: instead of writing all the variables to stay the profit is a linear combination of them we can write the "dot"
regressor = lm(formula = target ~ .,
               data = training_set)

# For more details on the fitting datasets to linear models
?lm

# In the summary focus on the coefficients section
# Note: the more the stars the more each coefficient is statistically significant 
# Another indication of statistical significance if the p-value is less than 5%

# Note: you can see that R did not fall into the dummy variable trap and removed one of the categorical values
# Note: we can also see that there is only one variable "R&D Spend" that has statistical significance on the prediction
# "R&D Spend" is the only variable that has p-value less than 5% which is reflected by 3 stars ***
# This is why we can change the multiple linear regression into a simple linear regression for valid prediction
# This means we could replace the dataset with only two columns "R&D Spend" and "Profits"

# Note: Significance level interpretation "we have 5 categories:
#  if between 0 - 0.001 it is given 3 stars '***'
#  if between 0.001 - 0.01 it is given 2 stars '**'
#  if between 0.01 - 0.05 it is given 1 star '*'
#  if between 0.05 - 0.1 it is given a dot '.'
#  if between 0.1 - 1 it is given no stars ''
summary(regressor)

# To get the coefficients 
regressor$coefficients

# Predicting the Test set results
y_pred <- predict(regressor, newdata = test_set)
y_pred
y_actual <- test_set$target

error <- y_pred - test_set$target 
percent_error <- abs(error)/y_pred

percent_error <- round(percent_error,2)

df_multi <- data.frame(y_pred, y_actual, error, percent_error)
View(df_multi)

# Lets try building our model with  only two columns "R&D Spend" and "target"

research_spend <- dataset$RM
target <- dataset$target
new_dataset <- data.frame(research_spend, target)

View(new_dataset)

set.seed(123)

split = sample.split(new_dataset$target, SplitRatio = 0.8)
simple_training_set = subset(new_dataset, split == TRUE)
simple_test_set = subset(new_dataset, split == FALSE)

View(simple_training_set)
View(simple_test_set)

simple_regressor = lm(formula = target ~ research_spend,
                      data = simple_training_set)

summary(simple_regressor)

simple_y_pred <- predict(simple_regressor, newdata = simple_test_set)
simple_y_pred

simple_error <- simple_y_pred - simple_test_set$target
simple_percent_error <- abs(simple_error)/simple_y_pred

simple_percent_error <- round(simple_percent_error,2)

comparison_df <- data.frame(df_multi, simple_y_pred, simple_percent_error)
View(comparison_df)

# Visualizing the Training set results

library(ggplot2)
ggplot() +
  geom_point(aes(x = simple_training_set$research_spend, y = simple_training_set$target),
             colour = 'red') +
  geom_line(aes(x = simple_training_set$research_spend, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'blue') +
  ggtitle('target vs Research and Development (Training set)') +
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('Research and Development') +
  ylab('target')

# Visualizing the Test set results

# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = simple_test_set$research_spend, y = simple_test_set$target),
             colour = 'green') +
  geom_line(aes(x = simple_training_set$research_spend, y = predict(simple_regressor, newdata = simple_training_set)),
            colour = 'black') +
  ggtitle('target vs Research and Development (Test set)') + 
  theme(plot.title = element_text(hjust = 0.5))+
  xlab('Research and Development') +
  ylab('target')
