# Install e1071 package which has a Naïve Bayes Classifier
#install.packages("e1071")		

# Add the e1071 package to your R library
library("e1071")			

# Have a look at the iris flower data set
iris					
pairs(iris[1:4], main="Iris Data (red=setosa,green=versicolor,blue=virginica)", pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])

# Train a naïve Bayes model using the iris dataset
nb <- naiveBayes(iris[,1:4], iris[,5])	

# Call back the trained model - The naive Bayes classifier function generates three Gaussian (Normal) distributions for each predictor variable, one for each value of the class variable Species. The first column [,1] is the mean, the 2nd [,2] is the standard deviation.
nb					

# Make predictions on iris data using nb model
pred <- predict(nb, iris[,1:4])

# View the Naïve Bayes predictions
pred

# View the predictions against the actual data
data.frame(pred,iris[,5])

