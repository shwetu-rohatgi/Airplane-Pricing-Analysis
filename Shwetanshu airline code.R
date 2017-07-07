# Analysis of Airline Ticket Pricing
# NAME: <Shwetanshu Rohatgi>
# EMAIL: <shwetanshu.rohatgi@gmail.com>
# COLLEGE / COMPANY: <Maharaja Surajmal Institute of Technology> 


data.df <-read.csv(paste("C:/Users/USER/Downloads/SixAirlines.csv", sep=""))#Loading the Data set
attach(data.df)#Attaching the Data ste
View(data.df)#General view of the entire Data frme


#Simple Statistical Analysis using describe funtion 
library(psych)
describe(data.df)

library(ggplot2)
## Loading required package: ggplot2
#Seggregating different flights 
ggplot(data.df, aes(x = AIRLINE, fill = AIRLINE)) + geom_bar()

#Seggregating international and domestic flights
ggplot(data.df, aes(x = INTERNATIONAL))+ geom_bar()

#Prices of Economy and Premium tickets
ggplot(data.df, aes(x = PRICE_ECONOMY)) + geom_density()
ggplot(data.df, aes(x = PRICE_PREMIUM)) + geom_density()



# A Scatterplot of price economy vs flight hours of travel
# ==========
plot(FLIGHT_DURATION,PRICE_ECONOMY, 
     col="blue",
     main="Price economy vs flight hours",
     xlab="Hours", ylab="Price")

# Add the sample means to the Scatterplot
# ==========
abline(h=mean(PRICE_ECONOMY), col="dark blue", lty="dotted")
abline(v=mean(FLIGHT_DURATION), col="dark blue", lty="dotted")

# Add a regression line
# ==========
abline(lm(PRICE_ECONOMY ~ FLIGHT_DURATION))


# A Scatterplot of price premium vs flight hours of travel
# ==========
plot(FLIGHT_DURATION,PRICE_PREMIUM, 
     col="blue",
     main="Price economy vs flight hours",
     xlab="Hours", ylab="Price")

# Add the sample means to the Scatterplot
# ==========
abline(h=mean(PRICE_PREMIUM), col="dark blue", lty="dotted")
abline(v=mean(FLIGHT_DURATION), col="dark blue", lty="dotted")

# Add a regression line
# ==========
abline(lm(PRICE_PREMIUM ~ FLIGHT_DURATION))


#Correlation and Correlation Matrix for Price Economy

library(corrplot)
library(gplots)      # for color interpolation
par(mfrow=c(1, 1))
corrplot.mixed(corr=cor(data.df[ , c(2:6, 8,10,12,15,17)], use="complete.obs"), 
               upper="ellipse", tl.pos="lt", 
               col = colorpanel(50, "red", "gray60", "blue4"))


#Correlation and Correlation Matrix for Price Premium
      
par(mfrow=c(1, 1))
corrplot.mixed(corr=cor(data.df[ , c(2:6, 7,9,11,15,17)], use="complete.obs"), 
               upper="ellipse", tl.pos="lt", 
               col = colorpanel(50, "red", "gray60", "blue4"))



#Scatter Plot Matrix for Price Economy and Price Premium
library(car)
scatterplotMatrix(formula = ~ SEATS_ECONOMY + PITCH_ECONOMY + WIDTH_ECONOMY + PRICE_ECONOMY, cex=0.6,
                  data=data.df, diagonal="histogram")

scatterplotMatrix(formula = ~ SEATS_PREMIUM + PITCH_PREMIUM + WIDTH_PREMIUM + PRICE_PREMIUM, cex=0.6,
                  data=data.df, diagonal="histogram")

#Calculating correlations between Prices of Economy and Premium in correlation to other factors
cor.test(PRICE_ECONOMY, PITCH_ECONOMY)
cor.test(PRICE_ECONOMY, WIDTH_ECONOMY)
cor.test(PRICE_PREMIUM, PITCH_PREMIUM)
cor.test(PRICE_PREMIUM, WIDTH_PREMIUM)

#Using the Boruta package to calcuate the effectiveness of different variables in Calculating the price of Economy class tickets
eco.df <-read.csv(paste("C:/Users/USER/Downloads/Economy.csv", sep=""))
library(Boruta)
set.seed(1234) # for code reproducibility
response <- data.df$PRICE_ECONOMY
bor.results <- Boruta(eco.df,response,maxRuns=101,doTrace=0)
plot(bor.results)

#Using the Boruta package to calcuate the effectiveness of different variables in Calculating the price of Premium class tickets
pre.df <-read.csv(paste("C:/Users/USER/Downloads/prem.csv", sep=""))
library(Boruta)
set.seed(1234) # for code reproducibility
response <- data.df$PRICE_PREMIUM
bor.results <- Boruta(pre.df,response,maxRuns=101,doTrace=0)
plot(bor.results)


#Dividing the Data set into Test and Training Data ste
ratio = sample(1:nrow(data.df), size = 0.25*nrow(data.df))
Test = data.df[ratio,] #Test dataset 25% of total
Training = data.df[-ratio,] #Train dataset 75% of total
dim(Training)
dim(Test)

#Generating A Multi Variable Linear Regressional Model for Price of Economy Flights
linear.mod<- lm(PRICE_ECONOMY~ PITCH_ECONOMY + WIDTH_ECONOMY + FLIGHT_DURATION + QUALITY + PRICE_RELATIVE, data = Training)
summary(linear.mod)

#the t value of Pitch_economy and quality is positive indicating that these predictors are associated with 
#Price_economy. A larger t-value indicates that that it is less likely that the coefficient is not equal to zero purely by chance.
#Again, as the p-value for Flight_Duration and Price_Relative is less than 0.05 they are both statistically significant in the multiple linear regression model for Price_Economy response variable. 
#The model's, p-value: < 2.2e-16 is also lower than the statistical significance level of 0.05, this indicates that we can safely reject the null hypothesis that the value for the coefficient is zero 
#(or in other words, the predictor variable has no explanatory relationship with the response variable).
#The model has a F Statistic of 90, which is considerably high
library(rpart)
library(randomForest)
model.forest <- randomForest(PRICE_ECONOMY~ PITCH_ECONOMY + WIDTH_ECONOMY + FLIGHT_DURATION + QUALITY + PRICE_RELATIVE, data = Training, method = "anova", 
                             ntree = 300,
                             mtry = 2, #mtry is sqrt(6)
                             replace = F,
                             nodesize = 1,
                             importance = T)

varImpPlot(model.forest)
#From the VIF plot we see that Flight Duration and Price Relative are most important factors in predicitng Price Economy.

#We test the model using Random Forest
prediction <- predict(model.forest,Test)
rmse <- sqrt(mean((log(prediction)-log(Test$PRICE_ECONOMY))^2))
round(rmse, digits = 3)

# Evaluation metric function
#A custom root mean Square Function to evaluate the performance of our model
RMSE <- function(x,y)
{
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

#Implementing the Regression Tree Model 
model <- rpart(PRICE_ECONOMY~ PITCH_ECONOMY + WIDTH_ECONOMY + FLIGHT_DURATION + QUALITY + PRICE_RELATIVE, data = Training, method = "anova")
predict <- predict(model, Test)
RMSE1 <- RMSE(predict, Test$PRICE_ECONOMY)
RMSE1 <- round(RMSE1, digits = 3)
RMSE1

#For Premium Class Tickets

#Generating A Multi Variable Linear Regressional Model for Price of Premium Flights
linear.mod<- lm(PRICE_PREMIUM~ PITCH_PREMIUM + WIDTH_PREMIUM + FLIGHT_DURATION + QUALITY + PRICE_RELATIVE, data = Training)
summary(linear.mod)
#The model has an F Statistic of 48.4 which is mediumly high
#the t value of Pitch_premium, width_premium, Price_relative and quality is positive indicating that these predictors are associated with 
#Price_Premium. A larger t-value indicates that that it is less likely that the coefficient is not equal to zero purely by chance.
#Again, as the p-value for Flight_Duration  is less than 0.05 they are both statistically significant in the multiple linear regression model for Price_Economy response variable. 
#The model's, p-value: < 2.2e-16 is also lower than the statistical significance level of 0.05, this indicates that we can safely reject the null hypothesis that the value for the coefficient is zero 
#(or in other words, the predictor variable has no explanatory relationship with the response variable).

library(rpart)
library(randomForest)
model.forest <- randomForest(PRICE_PREMIUM~ PITCH_PREMIUM + WIDTH_PREMIUM + FLIGHT_DURATION + QUALITY + PRICE_RELATIVE, data = Training, method = "anova", 
                             ntree = 300,
                             mtry = 2, #mtry is sqrt(6)
                             replace = F,
                             nodesize = 1,
                             importance = T)

varImpPlot(model.forest)
#From the VIF plot we see that Flight Duration and Price Relative are most important factors in predicitng Price Economy.

# Evaluation metric function
#A custom root mean Square Function to evaluate the performance of our model
RMSE <- function(x,y)
{
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

#Implementing the Regression Tree Model 
model <- rpart(PRICE_ECONOMY~ PITCH_ECONOMY + WIDTH_ECONOMY + FLIGHT_DURATION + QUALITY + PRICE_RELATIVE, data = Training, method = "anova")
predict <- predict(model, Test)
RMSE1 <- RMSE(predict, Test$PRICE_ECONOMY)
RMSE1 <- round(RMSE1, digits = 3)
RMSE1


