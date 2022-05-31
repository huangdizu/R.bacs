
setwd("C:/Users/eason/Desktop/清大 BACS/資料/")
customers <- read.table(file = "customers.txt", header = TRUE)
ages <- customers$age

#1. What is the 5th element in the original list of ages?
ages[5]

#2. What is the fifth lowest age?
sorted_ages <- sort(ages)
sorted_ages[5]

#3. Extract the five lowest ages together 
#HINT: to get a sequence of numbers from a list, you can use:
#my_list[c(1,2,3,4,5)]   but can you think of a shorter or clearer way of doing this?
sorted_ages[c(1,2,3,4,5)]

#4.	Get the five highest ages by first sorting them in decreasing order first.
#HINT: find out how to sort in decreasing order by using:  help(sort) or  ?sort
sorted_ages2 <- sort(ages, decreasing = TRUE)
sorted_ages2[c(1,2,3,4,5)]

#5.	What is the average (mean) age?
mean(ages)
sum(ages)/length(ages)

#6.	What is the standard deviation of ages? (guess or google the standard deviation function in R)
sd(ages)

#7.	Make a new variable called age_diff, with the difference between each age and the mean age
age_diff <- ages - mean(ages)
age_diff

#8.	What is the average ? Difference between each age and the mean age?
#HINT: think carefully why someone would want to know this, and what it implies about how to do #7
mean(age_diff)

#9.	Visualize the raw data as we did in class: (a) histogram, (b) density plot, (c) boxplot + stripchart
hist(ages)
plot(density(ages))
boxplot(ages, horizontal = TRUE)
stripchart(ages, method = "stack", add = TRUE)
