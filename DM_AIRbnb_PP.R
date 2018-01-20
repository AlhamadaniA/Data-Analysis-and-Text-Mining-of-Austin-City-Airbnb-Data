set.seed(5889)
library(ggplot2)
library(psych)
library(plotly)
library(rpart)
library(randomForest)
require(caret)
library(rpart.plot)
install.packages('modelr', dependencies = TRUE)
require(modelr)
listings = read.csv('listings.csv', stringsAsFactors = T,na.strings = c("NA",  "NA" ,"N/A"))
summary(listings)
str(listings)

head(listings,n=2)
tail(listings,n=3)
dim(listings)
names(listings)
listings[1,]

df<-as.data.frame( listings[c("host_response_rate", "host_acceptance_rate", "host_is_superhost",
                      "host_listings_count", "zipcode", "property_type","room_type", "accommodates", "bathrooms", "bedrooms", 
                      "beds", "price", "number_of_reviews", "review_scores_rating", "cancellation_policy", 
                      "reviews_per_month")])
names(df)
head(df)
dim(df)
# apply the counting function per columns
NAN_F<- function(x) { sum(is.na(x)) }
###apply sapply function on each column to count number of nan-available data
df_count <- sapply(df, NAN_F, simplify=T)
###Total number of nan_available data in our dataframe
sum(df_count)
is.na(df)
cat("Number of missing data:", sum(is.na(df)), "\n")

#df$host_response_rate<-as.numeric(df$host_response_rate)
listings_df<-na.omit(df)
dim(listings_df)
dim(df)
nrow(df)
nrow(listings_df)
#### % of rows droppped
(nrow(df)-nrow(listings_df))/nrow(df)*100
### = % 45.75835
###Verifying that there is no more missing data within the training datasets
cat("Number of missing data:", sum(is.na(listings_df)), "\n")
is.na(listings_df)
head(listings_df)
class(listings_df$host_response_rate)
head(listings_df)
str(listings_df)
listings_df$host_acceptance_rate<-as.character(listings_df$host_acceptance_rate)
listings_df$price<-as.character(listings_df$price)
listings_df$host_response_rate<-as.character(listings_df$host_response_rate)

# clean data
listings_df$host_acceptance_rate <- gsub("%", "", listings_df$host_acceptance_rate)
listings_df$price <- gsub("[$, ]", "", listings_df$price)
listings_df$host_response_rate <- gsub("%", "", listings_df$host_response_rate)
listings_df$host_response_rate<-as.numeric(listings_df$host_response_rate)
listings_df$price<-as.numeric(listings_df$price)
listings_df$host_acceptance_rate<-as.numeric(listings_df$host_acceptance_rate)

###Create Dummies
tail(listings_df$host_is_superhost)
head(listings_df$host_is_superhost)
class(listings_df$host_is_superhost)
listings_df$host_is_superhost <- as.numeric(listings_df$host_is_superhost=="t")
str(listings_df$host_is_superhost)
listings_df$host_is_superhost
tail(listings_df$host_is_superhost)

#### select non-numeric variables and create dummies
sapply(listings_df[], is.factor)
listings_df$zipcode<-as.factor(listings_df$zipcode)
sapply(listings_df[], is.factor)
str(listings_df$zipcode)
str(listings_df)
head(listings_df$zipcode)
head(listings_df$property_type)
head(listings_df$room_type)
head(listings_df$ cancellation_policy)

dummies<-model.matrix(~zipcode-1+property_type-1+room_type-1+cancellation_policy-1,data =listings_df )
head(dummies)
tail(dummies)
dim(dummies)
dummies<-as.data.frame(dummies)
class(dummies)
### drop non-numeric variables from df2 and add the dummies
listings_df$zipcode<-NULL
listings_df$property_type<-NULL
listings_df$room_type<-NULL
listings_df$ cancellation_policy<-NULL
class(listings_df)
dim(listings_df)
dim(dummies)
listings_df2<-cbind(listings_df, dummies)
dim(listings_df2)
head(listings_df2)
tail(listings_df2)
###summary stats
ggplot(data=listings_df2, aes(listings_df2$price)) + 
    geom_histogram(breaks=seq(1, 1300, by =100), 
                   col="gray2",fill="gray")+labs(title="'Histogram of listing prices") +
  labs(x= "Listing price in $", y="Count")

ggplot(data=listings_df2, aes(listings_df2$price)) + 
  geom_histogram(aes(y =..density..), 
                 col="black", 
                 fill="gray2", 
                 alpha = .2) + 
  geom_density(col=2) + 
  labs(title="Histogram of listing prices") +
  labs(x= "Listing price in $", y="Count")


###scatter plot: price vs number of bedrooms
qplot(price, bedrooms, data = listings_df2, colour = I("blue"),main ="Number of bedrooms vs. prices")
###scatter plot: Number of Reviews vs price
qplot(number_of_reviews, price, data = listings_df2, colour = I("blue"),main ="Reviews vs. prices")
###accommodates: The number of guests a listing can accommodate.
###price: The price (in $US) for a night stay.
qplot(accommodates, price, data = listings_df2, colour = I("blue"),main ="accomodates vs. prices")
###Looks like there are some outliers on both axes
#===========================================================================================================================================
#*******************************************************************************************************************************************
#===========================================================================================================================================

cols<-as.data.frame( listings_df2[c('number_of_reviews','host_acceptance_rate','host_listings_count','accommodates',
                              'bathrooms','bedrooms','beds','price')])
# Basic Scatterplot Matrix
pairs(~number_of_reviews+host_acceptance_rate+host_listings_count+accommodates+bathrooms+bedrooms+beds+price,data=cols, 
      main="Simple Scatterplot Matrix")

install.packages("GGally")
library("GGally") ###is an extenstion of ggplot2
scatter_correlation_plot<-ggpairs(cols, columns = 1:ncol(cols), title = "",
        axisLabels = "show", columnLabels = colnames(cols[, ]))
print(scatter_correlation_plot)
###Correlation matrix can be created using the R function cor() :
cols_correlation <- round(cor(cols),2)
head(cols_correlation)
###The package reshape is required to melt the correlation matrix :
library(reshape2)
cols_correlation_2 <- melt(cols_correlation)
head(cols_correlation_2)
ggplot(data = cols_correlation_2, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()





# Create a ggheatmap
ggheatmap <- ggplot(cols_correlation_2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Important Variables\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
print(ggheatmap)

###Add correlation coefficients on the heatmap
###Use geom_text() to add the correlation coefficients on the graph
###Use a blank theme (remove axis labels, panel grids and background, and axis ticks)
###Use guides() to change the position of the legend title

ggheatmap +  
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

Accommodates<-as.factor(listings_df2$accommodates)
ggplot(listings_df2, aes(x=listings_df2$price, fill=Accommodates)) +
  geom_histogram(bins = 25)

Bedrooms<-as.factor(listings_df2$bedrooms)
ggplot(listings_df2, aes(x=listings_df2$price, fill=Bedrooms)) +
  geom_histogram(bins = 30)
  
### split into test and training data
# Now Selecting 70% of data as sample from total 'n' rows of the data 
set.seed(5889)
###prediction
# define training sample
str(listings_df2)

str(listings_df2)
head(listings_df2,n=1)
sample <- sample.int(n = nrow(listings_df2), size = floor(.7*nrow(listings_df2)), replace = F)
train_listings <- listings_df2[sample, ]
test_listings<- listings_df2[-sample, ]
head(train_listings)
dim(train_listings)
dim(test_listings)
head(train_listings,n=2)
head(test_listings,n=2)
summary(train_listings)
summary(test_listings)
###minimum and maximum of price
min(listings_df$price)
max(listings_df2$price)

train_listings$accommodates
fit_firstlm_model<-lm(price ~ accommodates, data = train_listings)
summary(fit_firstlm_model)
###How does listing price depend on the number of people it accommodates?
### accommodates is statistically signifcant on the price
####there is a strong relationship between the predictor and the response
### The F statistic is larger than 1 and p-value is close to zero. This is evidence against the null hypothesis and implies there is a relationship between price and accommodates 
###As the Adjusted R-squared is 0.3484, 35% of the varinace in price is dependent on accommodates
###As the coeficient of “accommodates” is positive, the relationship is also positive The more guests a listing can accommodate the higher the price goes
as.data.frame(train_listings) %>%
  add_predictions(fit_firstlm_model) %>%
  ggplot(aes(x=accommodates)) +
  geom_point(aes(y=price)) +
  geom_line(aes(y=pred), color='red')
###out-of-sample performance. 
as.data.frame(test_listings) %>%
  add_predictions(fit_firstlm_model) %>%
  ggplot(aes(x=accommodates)) +
  geom_point(aes(y=price)) +
  geom_line(aes(y=pred), color='red')
###Root Mean-squared Error (RMSE)
rmse(fit_firstlm_model,test_listings)
###Mean Absolute Error (MAE): 
mae(fit_firstlm_model,test_listings)

###Find the maximum number of accommodates
NumberofGuests<-sort(train_listings$accommodates,decreasing = T)
Max_NumberofGuests<-max(NumberofGuests)
print(Max_NumberofGuests)
###How well does accommodation size predict price?
###the predicted price associated with a accommodates of 16
predict(fit_firstlm_model, accommodates=16)
predict(fit_firstlm_model, data.frame(accommodates=16), interval = "prediction")
###the predicted price associated with a accommodates of 1
predict(fit_firstlm_model, accommodates=1)
predict(fit_firstlm_model, data.frame(accommodates=1), interval = "prediction")


names(fit_firstlm_model)
# Print the coefficients of fit_firstlm_model
fit_firstlm_model$coefficients





# Scatter Plot
###Use the abline() function to display the least squares regression line 
plot(test_listings$accommodates, test_listings$price, main = "Scatterplot of price vs. accommodates", xlab = "accommodates", ylab = "price", col = "blue")
abline(fit_firstlm_model, col = "red")

fit_secondlm_model<-lm(price ~ ., data = train_listings)
summary(fit_secondlm_model)
rmse(fit_secondlm_model,train_listings) # In-sample
rmse(fit_secondlm_model,test_listings) # Out-of-sample
### Overfitting problem here, meaning that the training error is smaller than the test error. The model is too powerful for the amount of data we have. Note that R recognizes this by giving warnings about a “rank-deficient fit.”

###The F statistic is larger than 1 and p-value is close to zero. This is evidence against the null hypothesis and implies there is a relationship between price and the response variables.
###F-statistic: 35.12 on 74 and 2140 DF,  p-value: < 2.2e-16
###The zipcodes that have statistically very high significant relationship to the response (price) by checking the p-values associated with each predictor’s t-statistic
###78701 Postal code in Austin, Texas
###78732 Postal code in Travis County, Austin, Texas
###78732- The Steiner Ranch community in Northwest Austin is so large, it makes up the majority of this zip code. The exemplary schools, top-notch neighborhood amenities and high-end golf course are just a few of the reasons Austin families choose to call Steiner Ranch home. You'll find a diverse mix of traditional suburban homes and luxury estates in this area. Click to search homes for sale in 78732.
###An improved model

fit_lm_model<- lm(price ~zipcode78701+zipcode78732+accommodates, data = train_listings)

rmse(fit_lm_model,train_listings) # In-sample
rmse(fit_lm_model,test_listings)# Out-of-sample
mae(fit_lm_model,test_listings)

predict(fit_lm_model, zipcode78732=1)


#========================================================================================


###**********************************************************************************************************************
# Create trainControl object(with 5 fold Cross Validation): 

# random forest with out of bag resampling
###RMSE <- sqrt(mean((y-y_pred)^2))



tc <- trainControl("cv",number=10)
Grid <- expand.grid(mtry = seq(4,16,4))
formula<-price~.
fit.rf.cv <- train(formula, data=train_listings , method='rf', trControl=tc,tuneGrid=Grid,metric='RMSE')
# Print maximum ROC statistic
fit.rf.cv

min(fit.rf.cv[["results"]][["RMSE"]])
plot(fit.rf.cv)
fit.lm.cv <- train(formula, data=train_listings , method='lm', trControl=tc,metric='RMSE')
fit.lm.cv
min(fit.lm.cv[["results"]][["RMSE"]])
fit.glm.cv <- train(formula, data=train_listings , method='glm', trControl=tc,metric='RMSE')

fit.glm.cv
min(fit.glm.cv[["results"]][["RMSE"]])







# Create model_list using caret library(compare the models)
model_list <- list(Randomforest = fit.rf.cv, Linearmodel = fit.lm.cv,GLM=fit.glm.cv)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)
resamples
# Summarize the results
summary(resamples)
dotplot(resamples, metric = "RMSE")
bwplot(resamples, metric = "RMSE")
parallelplot(resamples)
parallelplot(resamples , metric = "Rsquared")



#run model against test data set
predict.rf <- predict(fit.rf.cv, test_listings)
head(predict.rf)
tail(predict.rf)
#build a dataframe with our results
submit.rf <- data.frame(Actualprice = test_listings$price, Pred_price=predict.rf)
#write results to .csv for submission
write.csv(submit.rf, file="submit_rf_v1.csv",row.names=FALSE,quote=FALSE)

cor(predict.rf, test_listings$price)

