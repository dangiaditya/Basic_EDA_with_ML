# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))


# Splitting the dataset into the Training set and Test set

library(caTools)
set.seed(123)
#Taking 80% of the data for the training and rest 20% for test
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

#Building the optimal model using backward Elimintation 

# we will remove non statistically singnificant variable step by step
#lets take significane value as 0.6(6%)
#now we will take the variable which has highest p value and check whether its greater
#than the significane value or not , if yes then we will remove it and build our 
#regressor again

regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend+State,
               data = dataset)
summary(regressor)

#state has the highest(90%) p value so we will remove it
regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend,
               data = dataset)
summary(regressor)
#Administration has the highest(60%) p value so we will remove it
regressor = lm(formula = Profit ~ R.D.Spend+Marketing.Spend,
               data = dataset)
summary(regressor)
#Now we can observe that the Marketing spend had the P value of 10% and now it has reduced
#to 0.06%

y_pred = predict(regressor, newdata = test_set)


