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

#Decriptive statistics
mean(Fifa_Data$Height)
sd(Fifa_Data$Height)
fivenum(Fifa_Data$Height)

#Boxplot for distribution summary and identifying outliers
boxplot(Fifa_Data$Height, coef = 1.5, do.conf = TRUE, do.out = TRUE, 
        main = "Boxplot of Player Height", 
        ylab = "Height in Inches",col = "lightblue")

#Examine shape of distribution using histogram plot and Density model

histPlayerHeight <- hist(Fifa_Data$Height, main = "Histogram of Player Height", 
                         xlab = "Height in Inches", col = blues9) #Create Histogram
histPlayerHeight #View histogram
abline(v = mean(Fifa_Data$Height), col = "red", lwd = 2) #Add line for mean
abline(v = median(Fifa_Data$Height), col = "purple", lwd = 2) #Add line for median
#Create Density model
xfit <- seq(min(Fifa_Data$Height), max(Fifa_Data$Height), length = 40) 
yfit <- dnorm(xfit, mean = mean(Fifa_Data$Height), sd = sd(Fifa_Data$Height)) 
yfit <- yfit * diff(h$mids[1:2]) * length(Fifa_Data$Height) 
#Add legend
lines(xfit, yfit, col = "black", lwd = 2)
legend(x = "topright",
       col = c("red", "purple", "black"),
       c("Mean", "Median", "Density"),  lwd = 5)

#Check to see how well a density curve vits the Fifa Height distribution
##install.packages("car")
library("car")
qqPlot(Fifa_Data$Height, main = "Quantile Plot of Player Height", ylab = "Height in Inches") #Quantile plot using cars library

#Calculation of z scores for popular players and save standardized Height in new column
HeightSD <- sd(Fifa_Data$Height)
HeightMEAN <- mean(Fifa_Data$Height)
Fifa_Data_v2 <- Fifa_Data
Fifa_Data_v2["Standardized Height"] <- ((Fifa_Data_v2$Height - HeightMEAN) / HeightSD)
View(Fifa_Data_v2)

Fifa_Data_v2["Height Percentile"] <- pnorm(Fifa_Data_v2$`Standardized Height`, lower.tail = TRUE)
View(Fifa_Data_v2)

#Create dataframe with height percentiles
FIFA_Player_Height <- Fifa_Data_v2
FIFA_Player_Height
View(FIFA_Player_Height)

#Save player height csv
write.csv(FIFA_Top_Players, file = X, 
          row.names = FALSE)

#Further exploratory analysis
((67- HeightMEAN) / HeightSD) ##Z-Score calculation
dnorm(67, HeightMEAN, HeightSD) ##probability of gettting this exact height
qnorm(.5, HeightMEAN, HeightSD, lower.tail = TRUE) ##input percentile and return height in inches
pnorm(80, HeightMEAN, HeightSD, lower.tail = TRUE) ##input the height in inches and get percentile


