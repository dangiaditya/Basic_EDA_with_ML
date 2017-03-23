# Multiple Linear Regression
#Multiple Regression done using backward elimination
#Step1- Select aa significane level to stay in model(SL=0.6), we will be taking it as 6 %
#Step2- Fit full model with all possible predictors
#Step3- Consider the predictor with the highest P value.if P>SL, go to step 4 else finish
#Step4- Remove the predictor
#Step5- Fit model again , after removing the value.

#Equation we will be using for our regressor
#Y0 = B0 + B1X1 + ..... B(n-1)X(n-1)


"We have a 50Startups Dataset and we will be pridicting their profit by training our model with all the information we are provided with"
#---------------------------------------------------------------------------------------------
# Importing the dataset
dataset = read.csv('50_Startups.csv')
#Since we have some categorical data , so we will 
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
summary(regressor)
"
Call:
lm(formula = Profit ~ ., data = training_set)

Residuals:
   Min     1Q Median     3Q    Max 
-33128  -4865      5   6098  18065 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      4.965e+04  7.637e+03   6.501 1.94e-07 ***
R.D.Spend        7.986e-01  5.604e-02  14.251 6.70e-16 ***
Administration  -2.942e-02  5.828e-02  -0.505    0.617    
Marketing.Spend  3.268e-02  2.127e-02   1.537    0.134    
State2           1.213e+02  3.751e+03   0.032    0.974    
State3           2.376e+02  4.127e+03   0.058    0.954    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 9908 on 34 degrees of freedom
Multiple R-squared:  0.9499,	Adjusted R-squared:  0.9425 
F-statistic:   129 on 5 and 34 DF,  p-value: < 2.2e-16
"

#we can see the P values in Pr(>|t|) column and the significance code also below the table
#P value is inversely propotional to significance, which means the lower the P value = higher significance



#Building the optimal model using backward Elimintation 

# we will remove non statistically singnificant variable step by step
#lets take significane value as 0.6(6%)
#now we will take the variable which has highest p value and check whether its greater
#than the significane value or not , if yes then we will remove it and build our 
#regressor again

regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend+State,
               data = dataset)
summary(regressor)
#it will have the same summary as given above, since we have not removed any predictor(independent variable)

#state has the highest(90%) p value so we will remove it
regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend,
               data = dataset)
summary(regressor)
#after removing the state value
"
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      5.012e+04  6.572e+03   7.626 1.06e-09 ***
R.D.Spend        8.057e-01  4.515e-02  17.846  < 2e-16 ***
Administration  -2.682e-02  5.103e-02  -0.526    0.602    
Marketing.Spend  2.723e-02  1.645e-02   1.655    0.105 
"

#Administration has the highest(60%) p value so we will remove it
regressor = lm(formula = Profit ~ R.D.Spend+Marketing.Spend,
               data = dataset)
summary(regressor)

"Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)     4.698e+04  2.690e+03  17.464   <2e-16 ***
R.D.Spend       7.966e-01  4.135e-02  19.266   <2e-16 ***
Marketing.Spend 2.991e-02  1.552e-02   1.927     0.06 .
"
#Now we can observe that the Marketing spend had the P value of 10% and now it has reduced
#to 0.06%

y_pred = predict(regressor, newdata = test_set)

y_pred

"  4         5         8        11        16        20        21        24        31 
173441.31 171127.62 160455.74 135011.91 146032.72 115816.42 116650.89 109886.19  99085.22 
       32 
 98314.55 "


