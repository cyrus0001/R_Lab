
#  1. In R, iris dataset is already available under MASS library. Use iris dataset, 
#    find the following things

#    i.Find the number of row and column of iris dataset.
#   ii.Find the summary of Sepal.Length and Sepal.Width variable
#  iii.Find the types of species and its number.
#   iv.Make a another dataset from iris dataset with size of Petal.Length is grater than 2.


import dplyr
data(iris)
View(iris)


#Problem 1 Part I :-

#dimension of the iris data set
dim(iris)

#Number of rows the iris data set
nrow(iris)

#Number of column of the data set iris
ncol(iris)



#Problem 1 Part II :-

#Summary of the sepal.length of the  iris data set 
summary(iris$Sepal.Length)

#Summary of the sepal.width of the  iris data set 
summary(iris$Sepal.Width)



# Problem 1 Part III :-

#Summary of the species of the iris data set
summary(iris$Species)



#Problem 1 Part IV :-

#Convert all the attribute into lower case
names(iris) <- tolower(names(iris))

#load the library dplyr 
library(dplyr)

#Create a new data set with petal.length > 2
newdataset <- filter(iris,petal.length > 2)

#view the new data set 
View(newdataset)

#2. Create a your own dataset with 5 row and 4 column in R and save this dataset in your system.
#    This dataset must contain at least one categorical variable and one numeric variable(


# Create a data frame called 'mydataset' with four columns: ID, Name, Age, and Gender

mydataset <- data.frame(
  ID = 1:5,                   # numeric variable 
  Name = c("Ram", "Atrav", "Salini", "Salma", "Dhruv"), # categorical value 
  Age = c(25, 30, 22, 18, 22),      # numeric varibale
  Gender = c("Male", "Male", "Female", "Female", "Male") # categorical value 
)


# Print the 'mydataset' data frame to the console
print(mydataset)


# Write the 'mydataset' data frame to a CSV file named 'mydataset.csv'
# The 'row.names = FALSE' argument ensures that row names are not included in the CSV file
write.csv(mydataset, "mydataset.csv", row.names = FALSE)


#3. Use summary function for the following dataset to find the outlier, if any. Also, create
#   a data frame after removing this outlier. Also, add one column for sum of these vectors i.e.
#   X1 + X2 + X3.
#   X1= (2,4,6,10,4,7,12,20,5)
#   X2= (10,5,5,20,4,70,40,12)
#   X3= (2,4,2.5,34,1.6,9.5,6,2)


# Create the Dataset

X1 <- c(2, 4, 6, 10, 4, 7, 12, 20, 5)
X2 <- c(10, 5, 5, 20, 4, 70, 40, 12, NA)  # Adding a missing value
X3 <- c(2, 4, 2.5, 34, 1.6, 9.5, 6, 2, NA)  # Adding a missing value

# Combine the vectors into a data frame
data <- data.frame(X1, X2, X3)
# Summary of the dataset
summary(data)

# Remove rows with missing values
modified_data <- na.omit(data)

#Calculate the sum of X1, X2, and X3

modified_data$Sum <- modified_data$X1 + modified_data$X2 + modified_data$X3

# Print the modified data
print(modified_data)


#Question 4 : 
#    Let’s say you work for a retail company, and you want to create a data frame 
#    and draw a bar chart to visualize your monthly sales data for the past year, 
#     broken down by product categories.



#Create the data frame with the given data set
salesData <- data.frame(
  Month = c("January", "February", "March", "April"),
  Category_A = c(5000, 4500, 5200, 2100),
  Category_B = c(3000, 3200, 3100, 1500),
  Category_C = c(2000, 2300, 1800, 3015)
)
sales_data <-t(salesData)
View(sales_data)

# load the library ggplot2
library(ggplot2)

#load the library reshape2
#Organize the data
library(reshape2)
meltedData <- melt(salesData, id.vars = "Month")

# Create a bar chart using ggplot2
ggplot(meltedData, aes(x = Month, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Monthly Sales by Category",
    x = "Month",
    y = "Sales Amount(Dollar)"
  ) +
  scale_fill_manual(values = c("Category_A" = "red", "Category_B" = "blue", "Category_C" = "green")) +
  theme_minimal()


#  Imports medals total.csv dataset and find the following things as below.
#  i. Total number of gold, silver and bronze model won by India, USA and China.
#  ii.Make two separate bar charts for all three types of medals won by China and UK.
#  iii.Filter the dataset only for five countries as India, USA, Japan, China and Brazil.
#  iv.Use the dataset obtained in (iii) and make a pie chart and label them.
#  v.Find the details of all countries getting at least two golds, five silver and two bronze
#    medals.

#Problem 5 :-

medals_total <- read.csv("C:/Users/Sujit/Downloads/R Lab/medals_total.csv")
print(medals_total)
View(medals_total)

#Problem 5 Part I:

india_data <-filter(medals_total,Country=="India")
usa_data <-filter(medals_total,Country=="United States of America")
china_data <-filter(medals_total,Country=="People's Republic of China")
uk_data <- filter(medals_total,Country=="Great Britain")


#For India

india_gold <- sum(india_data$Gold)
india_silver <- sum(india_data$Silver)
india_bronze <- sum(india_data$Bronze)


cat("India:\n")
cat("Gold:", india_gold, "\n")
cat("Silver:", india_silver, "\n")
cat("Bronze:", india_bronze, "\n")

#For USA 

usa_gold <- sum(usa_data$Gold)
usa_silver <- sum(usa_data$Silver)
usa_bronze <- sum(usa_data$Bronze)

cat("\nUSA:\n")
cat("Gold:", usa_gold, "\n")
cat("Silver:", usa_silver, "\n")
cat("Bronze:", usa_bronze, "\n")

#For china 

china_gold <- sum(china_data$Gold)
china_silver <- sum(china_data$Silver)
china_bronze <- sum(china_data$Bronze)

cat("\nChina:\n")
cat("Gold:", china_gold, "\n")
cat("Silver:", china_silver, "\n")
cat("Bronze:", china_bronze, "\n")

#Problem 5 Part II:


# Create a bar chart for all three types of medals

medal_data <- data.frame(
  Country = c("China", "China", "China", "UK", "UK", "UK"),
  Medal = c("Gold", "Silver", "Bronze", "Gold", "Silver", "Bronze"),
  Count = c(38, 32, 18, 22, 21, 22)
)
# Load the necessary library for plotting (ggplot2)
library(ggplot2)

# Create a bar chart for China
ggplot(subset(medal_data, Country == "China"), aes(x = Medal, y = Count, fill = Medal)) +
  geom_bar(stat = "identity") +
  labs(title = "Medal Counts for China",
       x = "Medal Type",
       y = "Count") +
  scale_fill_manual(values = c("Gold" = "gold", "Silver" = "green", "Bronze" = "peru")) +
  theme_minimal()

# Create a bar chart for the UK
ggplot(subset(medal_data, Country == "UK"), aes(x = Medal, y = Count, fill = Medal)) +
  geom_bar(stat = "identity") +
  labs(title = "Medal Counts for the UK",
       x = "Medal Type",
       y = "Count") +
  scale_fill_manual(values = c("Gold" = "red", "Silver" = "green", "Bronze" = "peru")) +
  theme_minimal()



# Problem 5 Part III:

modified_data <-filter(medals_total,Country=="India"|Country=="United States of America"|Country=="Japan"|Country=="People's Republic of China"|Country=="Brazil")
View(modified_data)

#Problem 5 Part IV:


diff_country <-c(113,88,58,21,7)

pie(diff_country)
colors <- c("green","orange","darkblue","red","violet")
name <- c("USA","China","Japan","Brazil","India");
pie(diff_country,labels = name,col=rainbow(length(diff_country)))
pie(diff_country,col=colors, radius = 1,main="Medel_comparison")

legend("topright",c("USA","China","Japan","Brazil","India"),cex=0.7,fill=colors)


#Problem 5 Part V : 

new_data <- filter(medals_total,Gold.Medal>=2,Silver.Medal>=5,Bronze.Medal >=2)
View(new_data)


#6. Use AirPassengers dataset, which is already available in R and find the following things:
#     i. Find the total number of passengers who travelled from 1949 to 1960.
#    ii. store this AirPassengers dataset in a other dataset and draw a scatter plot between year
#        and number of passengers.
#   iii. Create a boxplot of the number of passengers for each months during entire duration.



#Part I

# Load the AirPassengers dataset and assign it to a different variable name
my_data <- AirPassengers
print(my_data)

# Convert the date ranges to a time window and assign it to a different variable name
time_window <- window(my_data, start = c(1949, 1), end = c(1960, 12))

# Calculate the total number of passengers and assign it to a different variable name
total_passengers <- sum(time_window)

# Print the total number of passengers
cat("Total number of passengers from 1949 to 1960:", total_passengers, "\n")

#Part II :

# Store the AirPassengers dataset in another dataset
dataset_new <- AirPassengers

# Plot a scatter plot between year and the number of passengers
plot(time(dataset_new), dataset_new, type = "l", xlab = "Year", ylab =
       "Number of Passengers", main = "AirPassengers Data")


#Part III:

boxplot(dataset_new ~ cycle(dataset_new), xlab = "Month", ylab = "Number of
Passengers", main = "Number of passengers for each month during duration")

