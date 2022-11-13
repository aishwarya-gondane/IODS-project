# Aishwarya Gondane. 09/11/2022 
# Assignment 2. Regression and model validation 

# Data wrangling 
######## Question 2 #############
# read the data into memory
data_test <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

summary(data_test)
dim(data_test)
# The data set has 183 rows and 60 colums 

str(data_test)

########### End #################

##### Question 3 #############

# read the data into memory
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
head(lrn14)
colnames(lrn14)
# Access the dplyr library
library(dplyr)

# questions related to deep, surface and strategic learning
lrn14$attitude <- lrn14$Attitude / 10
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
lrn14$deep <- rowMeans(lrn14[, deep_questions])
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
lrn14$surf <- rowMeans(lrn14[, surface_questions])
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
lrn14$stra <- rowMeans(lrn14[, strategic_questions])
learning2014 <- lrn14[, c("gender","Age","attitude", "deep", "stra", "surf", "Points")]
learning2014 <- filter(learning2014, Points > 0 )
dim(learning2014)

## Question 4 ##

setwd("C:/Users/gondanea/OneDrive - University of Helsinki/Courses/IODS-project")

write.csv(learning2014,"Learning_2014.csv", sep= "")
learnin_2014_reRead <- ("Learning_2014.csv")



## Analysis ###
## Question 1 ###
learning2014 <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/learning2014.txt",
                           sep = ",", header = T)

str(learning2014)
dim(learning2014)


## Question 2 ###
# Access the gglot2 library
library(ggplot2)

# initialize plot with data and aesthetic mapping
p1 <- ggplot(learning2014, aes(x = attitude, y = points))

# define the visualization type (points)
p2 <- p1 + geom_point()

# draw the plot
p2

# add a regression line
p3 <- p2 + geom_smooth(method = "lm")

# add a main title
p4 <- p3 + labs(title = "data overview of learning 2014") 
p4


##Question 3 ###
library(GGally)
ggpairs(learning2014, lower = list(combo = wrap("facethist", bins = 20)))
# create a regression model with multiple explanatory variables
my_model2 <- lm(points ~ attitude + stra, data = learning2014)

summary(my_model2)

# changing stra to age 
my_model2 <- lm(points ~ attitude + age, data = learning2014)

summary(my_model2)

#Multiple R-squared statistic indicates the percentage of the variance in 
#the dependent variable that the independent variables explain
#collectively. R-squared measures the strength of the 

#Question 5 #
plot(lm(points ~ attitude + age, data = learning2014))


