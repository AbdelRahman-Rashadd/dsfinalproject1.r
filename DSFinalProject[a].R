# Change the working directory to the folder containing the .csv file
data <- read.csv("D:/Project/grc.csv") # This line is used to extract the excel file from my directory.
data # Calling the database.
sum (duplicated(data)) # Checking out number of duplicates, there are exactly 2.
data_without = unique(data) # Used to clean the data from duplicates, and making a new unique one.
data_without # Calling it. 
sum (duplicated(data_without)) # Assuring there are no duplicates anymore. Output = 0 duplicates.
sum (is.na(data)) # Checking out number of "NA" values, there is none. 
outlier = boxplot(data[, c(2,3,4,6)])$out # Detecting outliers; abnormal values from the file.
outlier # In this specific case and context of grocery sets, it isn't required to remove
# the outliers due to the reason that the value of each outlier is logical, purchasable
# , and not ridiculous. 


# Summary: Three methods were used in order to clean the data. Checking duplicates, checking
# NA values, and illogical outliers. Results only concluded two errors, being 2 duplicates.
# Load data

par(mfrow = c(2,2)) # This command puts all the diagrams on 1 dashboard that is
# 2 x 2 (2 rows and 2 columns)

# The instructions below are used to calculate the sums of cash and credit but
# INDEPENDENTLY, then combining them into a vector for comparison.

sum_cash <- sum(data_without$total[data_without$paymentType == "Cash"])
sum_credit <- sum(data_without$total[data_without$paymentType == "Credit"])


totals <- c(sum_cash, sum_credit) 

# This command is used to calculate the percentages of both payment types on the chart.
percentages <- round((totals / sum(totals)) * 100, 1)



# Pie chart will be used for the comparison.

labels <- paste(percentages, "%")

pie(
  totals,
  labels = labels,
  col = c("pink", "skyblue"),
  main = "Comparison between Cash and Credit Totals")
# Legend is used for the bottom right box to provide info for labels and their colors.
legend("bottomright", legend = c("Cash", "Credit"), fill = c("pink", "skyblue"))

# We're calculating here the total amount using the scatter plot, and the total amount 
# spent by each age, and comparing them, and as before, we labelled the x and y with
#  "age" and "total spending", chose a green color, used "as.numeric" to turn the vector
# we received from the "age_sum" to numeric so we'd be able to represent it on the graph.

age_sum <- tapply(data_without$total, data_without$age, sum)

plot(
  x = as.numeric(names(age_sum)), # Ages
  y = as.numeric(age_sum),       # Total spending
  main = "Comparison of Age vs Total Spending",
  xlab = "Age",
  ylab = "Total Spending",                       
  col = "green"
)

# Here we're calculating the total amount paid of every city mentioned in the 
# database given, and representing them using a barplot in DESCENDING order
# , with the x-axis being called "City", and y-axis "Total".

spendcity_sum <- tapply(data_without$total, data_without$city, sum)
spendcity_sum_desc <- spendcity_sum[order(spendcity_sum, decreasing = T)]

barplot(
  spendcity_sum,
  main = "Each City Total Spending in Descending Order",
  xlab = "City",
  ylab = "Total",
  col = "firebrick",
  las = 2
)

# Here We're representing the frequency and distribution of total spending using
# histogram diagram, with the x-axis labeled "Total".

hist(
  x = data_without$total,
  main = "Distribution of Total Spending",
  xlab = "Total",
  col = "lightsalmon"
)
#necessary library
library(dplyr)

#Aggregate total spending by customer
customer_spending <- aggregate(total ~ customer, data = data_without, FUN = sum)
customer_spending

#Aggregate ages for customer
customer_age <- aggregate(age ~ customer ,data = data_without,FUN = mean)
customer_age

#Merging total spending and ages for customers
customer_result= merge(customer_spending,customer_age,by="customer")

#View result
print(customer_result)

#taking number of groups from user
print(" Enter number of groups(between 2 and 4 : ") 
n_groups =as.integer(readline())

#Checking that number is valid 
if (n_groups==2 ||n_groups==3||n_groups==4)
{
  #Apply k_means on the data 
  kmeans_clustring =kmeans(customer_result$total,centers = n_groups)
  print(kmeans_clustring)
  
  # Add the cluster numbers to the data frame
  customer_data <- data.frame(customer_result,Clusters=kmeans_clustring$cluster)
    
  # Print the resulting table
  print(customer_data)

  }else {
  print("Your number is not valid ")
}

