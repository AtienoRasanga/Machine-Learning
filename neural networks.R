# Install Packages
#install.packages("neuralnet")
library("neuralnet")

# Get data ready for analysis - add 3 columns with TRUE/FALSE for different flower species (3 output nodes)
itrain <- iris
itrain$setosa <- c(itrain$Species == 'setosa')
itrain$versicolor <- c(itrain$Species == 'versicolor')
itrain$virginica <- c(itrain$Species == 'virginica')
itrain$Species <- NULL

# Train the neural network
inet <- neuralnet(setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, itrain, hidden=3, lifesign="full")

# Plot network
plot(inet, rep="best")
# Plot without bias nodes
plot(inet, rep="best", intercept=FALSE)

# Make predictions on origional data
predict <- compute(inet, iris[1:4])

# Look at predictions (see which has the higher value)
predict$net.result

# Identify which values are max then change numbers to name of the species
result<-0
for (i in 1:150) { result[i] <- which.max(predict$net.result[i,]) }
for (i in 1:150) { if (result[i]==1) {result[i] = "setosa"} }
for (i in 1:150) { if (result[i]==2) {result[i] = "versicolor"} }
for (i in 1:150) { if (result[i]==3) {result[i] = "virginica"} }

# Combine and view results against actual data
data.frame(actual = iris[5], results = result)
