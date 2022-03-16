#Load required libraries
library(readr)
library(tidyverse)
library(caret)
library(corrplot)
library(caTools)
library(e1071)
library(rpart)
library(rpart.plot)
library(outliers)
library(Boruta)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Import the data set 
data = read_csv("Invistico_Airline.csv")
data = as.data.frame(data)
str(data)

View(data)

##########################################################################
######################Exploratory Data Analysis###########################
##########################################################################

# Copy of the dataframe for EDA
data_EDA = data.frame(data)

# Check variables data type
str(data_EDA)

# Modify data types for visualization 
data_EDA$satisfaction<-as.factor(data_EDA$satisfaction)
data_EDA$Gender<-as.factor(data_EDA$Gender)
data_EDA$Customer.Type<-as.factor(data_EDA$Customer.Type)
data_EDA$Type.of.Travel<-as.factor(data_EDA$Type.of.Travel)
data_EDA$Class<-as.factor(data_EDA$Class)

# Summary statistics 
summary(data_EDA)

# Drop NA values for visualization 
data_EDA = na.omit(data_EDA)

###################Visualizations of continuous variables#################

# Satisfaction per Flight distance
ggplot(data_EDA, aes(x = Flight.Distance, fill = satisfaction)) +
  geom_histogram(binwidth = 10, size = 10) + labs(title = 'Satisfaction per Flight distance') + theme(legend.position = 'bottom')

# Satisfaction per Age
ggplot(data_EDA, aes(x = Age, fill = satisfaction)) +
  geom_histogram(position = position_dodge(preserve = 'single'),binwidth = 2, alpha = 0.8, size = 10) + labs(title = 'Satisfaction per Age') + theme(legend.position = 'bottom')

# Satisfaction per Age
ggplot(data = data_EDA, aes(x = satisfaction, y = Age, fill = satisfaction)) +
  geom_violin(scale = 'area', color = 'blue', alpha=0.5) + labs(title = 'Satisfaction per Age')

###################Visualizations of Satisfaction########################

# Grid Satisfaction gender, Custoper Type, Type of Travel, Class
plot1 <- ggplot(data_EDA, aes(x = Gender, fill = satisfaction)) +
  geom_bar(position = position_dodge(preserve = 'single'), alpha = 0.8, size = 10) + labs(title = 'Satisfaction per Gender')
plot2 <- ggplot(data_EDA, aes(x = Customer.Type, fill = satisfaction)) +
  geom_bar(position = position_dodge(preserve = 'single'), alpha = 0.8, size = 10) + labs(title = 'Satisfaction per Customer type')
plot3 <- ggplot(data_EDA, aes(x = Type.of.Travel, fill = satisfaction)) +
  geom_bar(position = position_dodge(preserve = 'single'), alpha = 0.8, size = 10) + labs(title = 'Satisfaction per Type of travel')
plot4 <- ggplot(data_EDA, aes(x = Class, fill = satisfaction)) +
  geom_bar(position = position_dodge(preserve = 'single'), alpha = 0.8, size = 10) + labs(title = 'Satisfaction per Class')
grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow = 2)

# Grid Satisfaction Seat comfort, Food and drink, Inflight wifi service, Inflight entertainement, Online support, Ease of online booking 
plot5 <- ggplot(data_EDA, aes(x = Seat.comfort, fill = satisfaction)) +
  geom_bar(alpha = 0.8, size = 10) + labs(title = 'Satisfaction per Seat comfort')
plot6 <- ggplot(data_EDA, aes(x = Food.and.drink, fill = satisfaction)) +
  geom_bar(alpha = 0.8, size = 10) + labs(title = 'Satisfaction per Food and drink')
plot7 <- ggplot(data_EDA, aes(x = Inflight.wifi.service, fill = satisfaction)) +
  geom_bar(alpha = 0.8, size = 10) + labs(title = 'Satisfaction per Inflight wifi service')
plot8 <- ggplot(data_EDA, aes(x = Inflight.entertainment, fill = satisfaction)) +
  geom_bar(alpha = 0.8, size = 10) + labs(title = 'Satisfaction per Inflight entertainment')
plot9 <- ggplot(data_EDA, aes(x = Online.support, fill = satisfaction)) +
  geom_bar(alpha = 0.8, size = 10) + labs(title = 'Satisfaction per Online support')
plot10 <- ggplot(data_EDA, aes(x = Ease.of.Online.booking, fill = satisfaction)) +
  geom_bar(alpha = 0.8, size = 10) + labs(title = 'Satisfaction per ease of online booking')
grid.arrange(plot5, plot6, plot7, plot8, plot9, plot10, ncol= 2 , nrow = 3)

# Grid Satisfaction Age and Gender, Age and Customer type, Age and Type of travel, Age and Class
plot17 <- ggplot(data = data_EDA, aes(x = Gender, y = Age, fill = satisfaction)) +
  geom_boxplot(alpha=0.4) + labs(title = 'Satisfaction as per Age and Gender') + theme(legend.position = 'bottom')
plot18 <- ggplot(data = data_EDA, aes(x = Customer.Type, y = Age, fill = satisfaction)) +
  geom_boxplot(alpha=0.5, size = 0.4) + labs(title = 'Satisfaction as per Age and Customer type') + theme(legend.position = 'bottom')
plot19 <- ggplot(data = data_EDA, aes(x = Type.of.Travel, y = Age, fill = satisfaction)) +
  geom_boxplot(alpha=0.5, size = 0.4) + labs(title = 'Satisfaction as per Age and Type of travel') + theme(legend.position = 'bottom')
plot20 <- ggplot(data = data_EDA, aes(x = Class, y = Age, fill = satisfaction)) +
  geom_boxplot(alpha=0.5, size = 0.4) + labs(title = 'Satisfaction as per Age and Class') + theme(legend.position = 'bottom')
grid.arrange(plot17, plot18, plot19, plot20, ncol= 2 , nrow = 2)

###################Visualizations of distributions########################

plot21 <- ggplot(data = data_EDA, aes(y = Age)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Age')
plot22 <- ggplot(data = data_EDA, aes(y = Flight.Distance)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Flight distance')
plot23 <- ggplot(data = data_EDA, aes(y = Seat.comfort)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Seat comfort')
plot24 <- ggplot(data = data_EDA, aes(y = Food.and.drink)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Food and drink')
plot25 <- ggplot(data = data_EDA, aes(y = Inflight.wifi.service)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Inflight wifi service')
plot26 <- ggplot(data = data_EDA, aes(y = Online.support)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Online support')
plot27 <- ggplot(data = data_EDA, aes(y = Ease.of.Online.booking)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Ease of online booking')
plot28 <- ggplot(data = data_EDA, aes(y = On.board.service)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'On board service')
plot29 <- ggplot(data = data_EDA, aes(y = Baggage.handling)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Baggage handling')
plot30 <- ggplot(data = data_EDA, aes(y = Checkin.service)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Checkin service')
plot31 <- ggplot(data = data_EDA, aes(y = Cleanliness)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Cleanliness')
plot32 <- ggplot(data = data_EDA, aes(y = Online.boarding)) +
  geom_boxplot(alpha=0.5, fill = 'dark violet') + labs(title = 'Online boarding')
grid.arrange(plot21, plot22, plot23, plot24, plot25, plot26, plot27, plot28, plot29, plot30, plot31, plot32, ncol= 3 , nrow = 4)


##########################################################################
#############################Data Processing##############################
##########################################################################

#######################Renaming variable names#####################

# Replace spaces and special characters by _
names(data) = str_replace_all(names(data), c(" " = "_", "/" = "_", "-" = "_"))

# Change variable names to lowercase
names(data) = tolower(names(data))

#######################Treating Missing Values######################

# Check for missing values 
sapply(data, function(x) sum(is.na(x)))
sapply(data, function(x) sum(is.null(x)))
mean(is.na(data$arrival_delay_in_minutes)) * 100

# Removing the rows with NA values 
data = na.omit(data)  

#######################Removing Outliers#############################

# Boxplot of the distributions of the continuous numerical variables 
boxplot(data$flight_distance,horizontal=TRUE, las=2, main="flight_distance")
boxplot(data$arrival_delay_in_minutes,horizontal=TRUE, las=2, main="arrival_delay_in_minutes")
boxplot(data$departure_delay_in_minutes,horizontal=TRUE, las=2, main="departure_delay_in_minutes")
boxplot(data$age,horizontal=TRUE, las=2, main="age")

# Function to identify outliers (1.5xIQR) and drop them (NaN)
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "\n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "\n")
  cat("Mean of the outliers:", round(mo, 2), "\n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean with outliers:", round(m1, 2), "\n")
  cat("Mean without outliers:", round(m2, 2), "\n")
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  cat("Outliers successfully removed", "\n")
  return(invisible(dt))
}

#Remove outliers of flight_distance, arrival delay and departure delay
outlierKD(data,flight_distance)
outlierKD(data,arrival_delay_in_minutes)
outlierKD(data,departure_delay_in_minutes)
data = na.omit(data)  

#######################Features Encoding#############################

# Get all the possible values for each categorical variables
unique(unlist(strsplit(as.character(data$satisfaction), ",")))
unique(unlist(strsplit(as.character(data$gender), ",")))
unique(unlist(strsplit(as.character(data$customer_type), ",")))
unique(unlist(strsplit(as.character(data$type_of_travel), ",")))
unique(unlist(strsplit(as.character(data$class), ",")))

# Encoding of satisfaction, gender, customer_type and type_of_travel + rename variable names
data$satisfaction = ifelse(data$satisfaction == "satisfied", 1, 0)
names(data)[names(data) == 'satisfaction'] <- 'satisfied'

data$gender = ifelse(data$gender == "Male", 1, 0)
names(data)[names(data) == 'gender'] <- 'gender_male'

data$customer_type = ifelse(data$customer_type == "Loyal Customer", 1, 0)
names(data)[names(data) == 'customer_type'] <- 'loyal_customer'

data$type_of_travel = ifelse(data$type_of_travel == "Business travel", 1, 0)
names(data)[names(data) == 'type_of_travel'] <- 'business_traveller'

# One-hot encoding of class variable = create 3 dummy variables 
data$class <- as.factor(data$class)
dummy = dummyVars(~class, data=data)
class_encoded = data.frame(predict(dummy, newdata=data))
data$class_eco = class_encoded$class.Eco
data$class_eco_plus = class_encoded$class.Eco.Plus
data$class_business = class_encoded$class.Business
data = subset(data, select = -c(class))


#######################Checking correlation#############################

# Plot a correlation heatmap
corr = cor(data)
corrplot(corr, method="color")

# Identify variables with correlation > 0.75
highlyCorrelated = findCorrelation(corr, cutoff=0.75)
highlyCorrelated

# Check correlation of class_business
cor(data[-1], data$class_business) 

# Remove variable departure_delay_in_minutes and class_business 
data = subset(data, select = -c(class_business))

# Perform a chi-square test
apply(data, 1, chisq.test)


##########################################################################
##########################Features selection##############################
##########################################################################

# Features selection using Boruta
set.seed(123)
features_significance = Boruta(satisfied~., data = data, doTrace = 2)
print(features_significance)

# Plot feature importance
plot(features_significance, xlab="", xaxt="n")
lz = lapply(1:ncol(features_significance$ImpHistory),function(i)features_significance$ImpHistory[is.finite(features_significance$ImpHistory[,i]),i])
names(lz) <- colnames(features_significance$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
       at = 1:ncol(features_significance$ImpHistory), cex.axis = 0.7)

# Dataframe of the selected features
final_vars = getSelectedAttributes(features_significance, withTentative = F)
final_vars.df = attStats(features_significance)
View(final_vars.df)

###########################################################################
##########################Naive Bayes Classification#######################
###########################################################################

# Train/Test split
set.seed(101)
sample = sample.split(data$satisfied, SplitRatio=.80)
train = subset(data, sample==TRUE)
test = subset(data, sample==FALSE)

# Fit the Naive classifier
naive_classifier = naiveBayes(satisfied~., data=train)

# Predictions on the test set
y_pred = predict(naive_classifier, newdata = test)

# Evaluation
confusion_matrix = table(test$satisfied, y_pred)
confusion_matrix

confusionMatrix(confusion_matrix)

###########################################################################
################################Decision Tree##############################
###########################################################################

# Fit the model
decision_tree = rpart(satisfied~., method="class", data=train)
rpart.plot(decision_tree, type=2, extra=1)
summary(decision_tree)

# Find the best complexity
printcp(decision_tree)
plotcp(decision_tree)
print(decision_tree$cptable[which.min(decision_tree$cptable[,4]),1])

# Prune the tree
optimal_decision_tree <- prune(decision_tree, cp=decision_tree$cptable[which.min(decision_tree$cptable[,4]),1])
rpart.plot(optimal_decision_tree, type=2, extra=1)

# Prediction and evaluation 
predictions = predict(optimal_decision_tree, newdata=test, type="class")
confusionMatrix(table(predictions, test$satisfied))
