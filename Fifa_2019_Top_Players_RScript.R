#Load data via readxl
##Player names make it difficult to import via csv
install.packages("readxl")
library("readxl")

Fifa_Data <- read_excel("C:/Users/brook/OneDrive/Documents/Data Sciences Courses/Intro to Stats/Practice/FIFA Analysis/Raw Data/Fifa_Data.xlsx")


#View Fifa df
View(Fifa_Data)

#Check datatype
str(Fifa_Data)
Fifa_Data <- as.data.frame(Fifa_Data)


#Change Age to Factor and Overall
Fifa_Data$Age <- as.factor(Fifa_Data$Age)
Fifa_Data$Overall <- as.integer(Fifa_Data$Overall)

## Player level Analysis

#Examination of the shape of the distribution
hist(Fifa_Data$Overall, main = "Histogram of Overall Rating", xlab = "Overall Rating", 
     col = blues9)

#Examination of boxplot for identifing median, IQR, and possilbe outliers
boxplot(Fifa_Data$Overall, coef = 1.5, do.conf = TRUE, do.out = TRUE, 
        main = "Boxplot of Overall Rating", 
        ylab = "Overall Rating",col = "lightblue")

#Boxplot stats to determine median and outliers
boxplot.stats(Fifa_Data$Overall, coef = 1.5, do.conf = TRUE, do.out = TRUE)

#5 number summary of the Fifa_Data
fivenum(Fifa_Data$Overall)

#Identifing number of outliers in dataset based on the Q3 + 1.5IQR criteria
boxplot.stats(Fifa_Data$Overall)

#Use the 5 number summary and the Q3 + 1.5IQR criteria to set the upper outlier statistic
Fifa_fivenum <- fivenum(Fifa_Data$Overall)
Fifa_fivenum
Fifa_Q1 <- Fifa_fivenum[2]  
Fifa_Q3 <-  Fifa_fivenum[4]
Fifa_UpperOutlier <- Fifa_Q3 + 1.5 * (Fifa_Q3 - Fifa_Q1)
Fifa_UpperOutlier

#Create a list of the best players based on the upper outlier statistic
Fifa_Data_v2 <- Fifa_Data
Fifa_Data_v2["Top Performer"] <- "No"
Fifa_Data_v2$`Top Performer`[Fifa_Data_v2$Overall > Fifa_UpperOutlier] <- "Yes"
FIFA_Top_Players <- Fifa_Data_v2$Name[Fifa_Data_v2$`Top Performer` == "Yes"]
FIFA_Top_Players
View(FIFA_Top_Players)

#Save Top Players list to csv
write.csv(FIFA_Top_Players, file = X, 
          row.names = FALSE)

