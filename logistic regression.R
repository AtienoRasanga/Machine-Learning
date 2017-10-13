#Logistic Regression Example

#Load state data which is available in R base package. The dataset includes data on 50 US
#states such as Population, Illiteracy, Murder Rate, Income 
data <- state.x77

#View the first few records of the data
head(data)

#As an example we will use Population, Illiteracy, Income and Frost to predict the murder
#variable. We first view how the variables correlate to each other
cor(data[,c("Murder", "Population","Illiteracy", "Income", "Frost")])

#Install corrgram package which is used to visualize correlations between variables
#install.packages("corrgram")
library(corrgram)
corrgram(data[,c("Murder", "Population","Illiteracy", "Income", "Frost")])

#Use also pairs() to visualize correlations

pairs(data[,c("Murder", "Population","Illiteracy", "Income", "Frost")])

#Conduct a Multiple Linear Regression Using the following code
#the data set is stored as a matrix, we therefore use as.data.frame() to convert it to a 
#data frame because lm() only handles data frames

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=as.data.frame(data))

summary(fit)

#From these results we see that income and frost are not significant

fit2 <- lm(Murder ~ Population + Illiteracy, data=as.data.frame(data))

summary(fit2)

anova(fit,fit2)

predict(fit2,list(Population=10000,Illiteracy=1.1))


#We can use stepwise regression to select significant predictor variables. We start by
#creating a model using all the variables then remove the least significant variables
fit3 <- lm(Murder ~., data=as.data.frame(data)) 

#Use the step() function to conduct a stepwise regression. 

fit4 <- step(fit3, direction="backward")

summary(fit4)


#Logistic Regression

#Logistic regression is used when the forecast variable is categorical and the predictor
#variables either or both categorical and continous

Titanic <- read.csv("http://www.hodgett.co.uk/titanic.csv", header=TRUE)


#We are going to use survived as the forecast variable and sex, age and fare as the 
#predictor variables, so first we will remove all of the rows where survived is NA using

Titanic <- Titanic[which(!is.na(Titanic$survived)),]

train <- Titanic[1:1000,]

test <- Titanic[1001:1309,]

#use logistic regression on training data set

lfit <- glm(survived ~ sex + age + fare, family=binomial, data=train)

summary(lfit)

#sex and fare are statistically significant with sex having the lowest p-value suggesting a 
#strong association of the sex of the passenger with the probability of having survived

#We can assess the model using the test set. We extract the sex, age and fare columns from
#the data set

test2 <- subset(test,select=c(4,5,9))

results <- predict(lfit, newdata=test2,type='response')

#The returned results are between 0 and 1, but we need 0 and 1. Any value below 0.5
#is 0 while above is assigned 1

results <- ifelse(results > 0.5,1,0)

#Calculate the accuracy of the model

mean(results == test$survived)


