
####### Analysis of Review Data for Turtle Games

#set your working directory (replace the below with your own)
#setwd('C://Documents/LSE Data Analytics/Course 3/LSE_DA301_assignment_files_new')

#################################
#import the necessary packages
install.packages("DataExplorer")
install.packages("tidyverse")
install.packages("skimr")
install.packages("car")
install.packages("Metrics")
install.packages("RColorBrewer")

#import libraries
library(DataExplorer)
library(tidyverse)
library(skimr)
library(moments)
library(car)
library(Metrics)
library(RColorBrewer)

#################################
#import and explore the dataset
reviews <- read.csv('reviews_clean.csv', header = TRUE)

head(reviews)

View(reviews)

str(reviews)

dim(reviews)

summary(reviews)

#check age is within normal bounds
min(reviews$age)
max(reviews$age)

################################
#Explored here removing outliers of loyalty points and spend score
#Ultimately decided against using this in the main analysis as wanted to investigate higher values of loyalty points and how they came about.
Q1_loyalty <- quantile(reviews$loyalty_points, 0.25)
Q3_loyalty <- quantile(reviews$loyalty_points, 0.75)
IQR_loyalty <- IQR(reviews$loyalty_points)

# Define bounds for outliers
lower_bound_loyalty <- Q1_loyalty - 1.5 * IQR_loyalty
upper_bound_loyalty <- Q3_loyalty + 1.5 * IQR_loyalty

Q1_spend <- quantile(reviews$spend_score, 0.25)
Q3_spend <- quantile(reviews$spend_score, 0.75)
IQR_spend <- IQR(reviews$spend_score)

# Define bounds for outliers
lower_bound_spend <- Q1_spend - 1.5 * IQR_spend
upper_bound_spend <- Q3_spend + 1.5 * IQR_spend

cleaned_reviews <- reviews[reviews$loyalty_points >= lower_bound_loyalty & reviews$loyalty_points <= upper_bound_loyalty
                           & reviews$spend_score >= lower_bound_spend & reviews$spend_score <= upper_bound_spend,]
dim(cleaned_reviews)

#################################
#Visualise and explore the data

#begin with a pair plot of all the numeric data for a general overview
pairs(~age + remuneration + spend_score + loyalty_points, data = reviews)
#only correlation evident is between remuneration and spend score

#visualise the gender split across loyalty points
ggplot(reviews, aes(x=gender, y=loyalty_points, fill= gender)) +
  geom_boxplot() +
  theme_classic()+
  labs(title = 'Loyalty Points by Gender', 
       x = 'Gender',
       y = 'Loyalty Points')+
  scale_fill_manual(values = c("Male" = "lightblue", "Female" = "coral"))
#no noticeable variations, both genders seem to have similar loyalty points, with a slighlty higher outlier in the Female group. 

#visualise the education split across loyalty points
ggplot(reviews, aes(x=education, y=loyalty_points,col = gender)) +
  geom_boxplot() +
  theme_classic()+
  labs(title = 'Loyalty Points by Education and Gender', 
       x = 'Education',
       y = 'Loyalty Points')
#a similar distribution of loyalty points across diploma, graduate, PhD and postgraduate with a slightly higher range of loyalty 
#   points for customers with a basic level of education. The separation by colour of gender does not add much insight so going 
#   forward I will focus on other variables in the dataset.

#to explore further we look at education against spend score
ggplot(reviews, aes(x=education, y=spend_score, fill= education)) +
  geom_boxplot() +
  theme_classic()+
  labs(title = 'Spend Score by Education', 
       x = 'Education',
       y = 'Spend Score')+
  scale_fill_brewer()
#this plot is similar to the one for loyalty points vs education


#view the loyalty points via a histogram
loyalty_hist <- ggplot(reviews, aes(x=loyalty_points)) +
  geom_histogram(binwidth = 250, boundary = 0, position = 'dodge',fill = 'darkgreen', color = 'black' )+
  theme_classic()+
  labs(title = 'Loyalty Points Frequency', 
       x = 'Loyalty Points',
       y = 'Frequency')+
  scale_x_continuous(breaks = seq(0,7000, by = 500))
#view plot interactively
ggplotly(loyalty_hist)
#The most common loyalty point values are between 1000 and 2000 with very few loyalty points over 2500.
mean(reviews$loyalty_points)

#view pertinent data with scatterplots
#loyalty points against spend score split by gender
ggplot(reviews, aes(x=spend_score, y=loyalty_points, col=gender)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()+
  labs(title = 'Spend Score vs Loyalty Points', 
       x = 'Spend Score',
       y = 'Loyalty Points')
#some positive correlation between spend score and loyalty points

#loyalty points against renumeration, colour gradient of spend score
remun_loyal_scatter <- ggplot(reviews, aes(x=remuneration, y=loyalty_points, col=spend_score)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()+
  labs(title = 'Remuneration vs Loyalty Points', 
       x = 'Remuneration',
       y = 'Loyalty Points')
#some positive correlation between remuneration and loyalty points, we can also see spend score increases with loyalty points
ggplotly(remun_loyal_scatter)
# an unexpected insight here is that there are some higher spend scores at the lower end of the remuneration scale

#spend score against remuneration
ggplot(reviews, aes(x=spend_score, y=remuneration)) +
  geom_point() +
  theme_classic()+
  labs(title = 'Spend Score vs Remuneration', 
       x = 'Spend Score',
       y = 'Remuneration')
#We can see from this plot that there are some higher spending customers who are on relatively low remuneration, 
#     indicating that this relationship is not linear but we can see fairly clear clusters.
#     Full clustering analysis is explored in Python.

#loyalty points against age
ggplot(reviews, aes(x=age, y=loyalty_points)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
#there is no correlation evident for these variables


#create a overview report
DataExplorer:: create_report(reviews)

#################################
#Perform statistical analysis
qqnorm(reviews$loyalty_points,
       col = 'blue')
qqline(reviews$loyalty_points,
       col = 'red')
#from this plot the data does not appear to be normally distributed

shapiro.test(reviews$loyalty_points)
#p values < 0.05 so it is again unlikely the data is normally distributed

# Specify the skewness and kurtosis functions.
skewness(reviews$loyalty_points) 
kurtosis(reviews$loyalty_points)

#these values imply data with a sharper peak in the centre and more data in tails than normal distribution, so a higher probability of 
#   extreme values

#to try to work the data into a more normalised distribution we can take the log of loyalty points and analyse this:
#create logged data
reviews$loyalty_points_log = log(reviews$loyalty_points)

qqnorm(reviews$loyalty_points_log,
       col = 'blue')
qqline(reviews$loyalty_points_log,
       col = 'red')
#from this plot the data does not appear to be normally distributed

shapiro.test(reviews$loyalty_points_log)
#p values < 0.05 so it is again unlikely the data is normally distributed

# Specify the skewness and kurtosis functions.
skewness(reviews$loyalty_points_log) 
kurtosis(reviews$loyalty_points_log)

#these values do not provide a much more normally distributed result than the original data, it is just skewed in a 
#     different direction, so we will continue with the non-logged values.


#create a dataset with only numerical values
reviewsnum <- select(reviews, -c(1,6:9))

corPlot(reviewsnum, cex=2)

# Compare loyalty points across different products using ANOVA
anova_results <- aov(loyalty_points ~ remuneration, data = reviews)
summary(anova_results)

#There is strong evidence (p-value < 2e-16 and high f value) to suggest that the remuneration has a statistically 
#significant effect on the loyalty points, meaning there are significant differences in loyalty points 
#between different remuneration levels

###############################################
#Create a Multilinear Regression Model
#   based on the correlation plot and our analysis in Python the mlr will include the spend_score and remuneration columns.

#First we split the data into train and test sets:
#label dataset with ID columns
reviews$ID <- 1:nrow(reviews)
#create train and test sets
training  <- reviews %>% dplyr::sample_frac(0.9) 

testing   <- dplyr::anti_join(reviews,
                              training, by = 'ID') 
#Create the model
modela <- lm(loyalty_points~spend_score+remuneration, data=training)

summary(modela)
#This model shows approximately 83% of variability in loyalty points can be explained by remuneration and spend score
#     Both remuneration and spend score have been marked as significant by R

#check accuracy with test dataset
predictions <- predict(modela, newdata = testing)

# Combine actual and predicted values in  a results dataframe
results <- data.frame(
  Actual = testing$loyalty_points,
  Predicted = predictions
)

# Calculate MSE
rmse_value <- rmse(results$Actual, results$Predicted)

# Calculate R-squared
rsq_value <- 1 - (sum((results$Actual - results$Predicted)^2) / sum((results$Actual - mean(results$Actual))^2))

# Print evaluation metrics
cat("Root Mean Squared Error (RMSE):", rmse_value, "\n")
cat("R-squared:", rsq_value, "\n")
# We have an R-squared value close to 1 which indicates a strong correlation between our independent and 
#     dependent variables but our RMSE is quite high so we will look at the percentage error to put this into context.


mean_loyalty_points <- mean(reviews$loyalty_points)
perc_error=rmse_value/mean_loyalty_points
perc_error
#We have a percentage error of ~32% which is acceptable but we would monitor it's ongoing accuracy and if needed retrain with more data.


#we have previously checked for multicolinearity in Python but as an extra measure we will complete the test in R also
vif_values <- vif(modela)
print(vif_values)
#VIF values are very close to 1 and so there is not indication of multicolinearity in these two variables.




##################################################
#Review data with groupings


####Review the dataset by product to see if there is a relationship between products and loyalty points
#set product column as a factor as it is a categorical variable assigned a random numerical value.
reviews$product <- as.factor(reviews$product)

#Group data and summarise loyalty point data per product
loyalty_product <- reviews %>% group_by(product) %>%
  summarise(loyalty_min=min(loyalty_points),
            loyalty_max=max(loyalty_points),
            loyalty_range = loyalty_max - loyalty_min,
            loyalty_mean=mean(loyalty_points),
            loyalty_count=n(),
            .groups='drop')

#sort by loyalty mean
loyalty_product <- loyalty_product[order(-loyalty_product$loyalty_mean), ]

#view the data
head(loyalty_product)
#From this data we can see that the range of loyalty points accrued from each product sale is very broad and so it is unlikely 
#   that points are based on spend (here we are assuming that each product would cost in each transaction)
#   It may be that customers have bought multiples in one transaction but we do not have this information and as the range is so 
#   broad of over 5000 in some instances we will assume this is not the case.
#   This means other factors must contribute and points are probably not accumulated with a simple ratio (£1 spent =/= 1 point)
names(reviews)

#Review the statistics for loyalty point data
summary_stats <- reviews %>%
  summarise(
    count = n(),
    mean_loyalty = mean(loyalty_points, na.rm = TRUE),
    median_loyalty = median(loyalty_points, na.rm = TRUE),
    sd_loyalty = sd(loyalty_points, na.rm = TRUE),
    min_loyalty = min(loyalty_points, na.rm = TRUE),
    max_loyalty = max(loyalty_points, na.rm = TRUE)
  )

summary_stats


