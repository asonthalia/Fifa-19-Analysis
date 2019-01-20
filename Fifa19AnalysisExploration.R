# TASK 1 : Understanding different kinds of players - Clustering players based on their ability and value

rm(list=ls(all=TRUE))
setwd("/Users/akash/Desktop/")

# Reading the CSV file
data = read.csv("data.csv", header = TRUE)
summary(data)
print(colnames(data))
match("Crossing",colnames(data))
extra_Cols_kMeans = data[,55:88]
head(extra_Cols_kMeans)

# Seperating the variables
attr_National_Analysis <- c("Name","Overall","Wage","International.Reputation","Weak.Foot","Potential")
attr_National_Analysis <- append(attr_National_Analysis,colnames(extra_Cols_kMeans))
attr_National_Analysis_Data <- data[,attr_National_Analysis]
attr_National_Analysis_Data <- na.omit(attr_National_Analysis_Data)

# Understanding our data

# Checking for missing values
missingValues <- lapply(attr_National_Analysis_Data, function(x) sum(is.na(x)))
print.listof(missingValues)
rm(missingValues)
# Converting Wage to Integer and removing K factor to produce absolute numbers
attr_National_Analysis_Data$Wage <- as.character(attr_National_Analysis_Data$Wage)
attr_National_Analysis_Data$Wage <- sapply(attr_National_Analysis_Data$Wage, function(x) substring(x,2,nchar(x)-1))
attr_National_Analysis_Data$Wage <- sapply(attr_National_Analysis_Data$Wage, function(x) as.numeric(x)*1000)
str(attr_National_Analysis_Data)

# Separating numeric attributes
attr_Cat = c("Name")
attr_Num = setdiff(colnames(attr_National_Analysis_Data),attr_Cat)
attr_Num_Data = attr_National_Analysis_Data[,attr_Num]
View(attr_Num_Data)
# Becasue I'm planning to do a kMeans Clustering it is essential to standardize the data becasuse kMeans works on the
# distance formula and there is a lot of difference in the Wage value and other values. So it would make sense to scale
# the data
library(vegan)
# attr_Num_Data = decostand(attr_Num_Data,"range",na.rm = TRUE)

kMeans_Player_Categories <- kmeans(na.omit(attr_Num_Data),10)
kMeans_Player_Categories
table(kMeans_Player_Categories$cluster)

# Exploring some of the data points.

# This is the plot for the Overall against the player wage, rising drastically eh? There's some curve to it.
# Defintely something going strong off pitch there. Lot of money pumped in some top players but not all.

plot(attr_Num_Data$Overall,attr_Num_Data$Wage*1.15/1000,xlab="Overall Rating / 100",ylab="Wages in Thousand $",col = kMeans_Player_Categories$cluster, 
     pch = 20,cex.axis=0.8, main = "Analysis of Player Wage vs. Rating")

# This is a plot for player reactions vs overall rating, while people would argue that the data is heteroscadastic, there's a sharp
# Pattern in the heteroscadasticity, Top there I guess, that's Leo Messi and Cristiano Ronaldo. Bottom guy needs to
# Work hard. The variance drastically decreases in the pattern.

plot(attr_Num_Data$Overall,attr_Num_Data$Reactions,xlab="Overall Rating / 100",ylab="Reactions / 100",col = kMeans_Player_Categories$cluster, 
     pch = 20,cex.axis=0.8, main = "Analysis of Player Reactions vs. Rating")

# Players above 65 start getting into two broad groups of having ball control and not; I feel the one's who don't have ball control
# Generally tend to dwell among the defenders central mid def. But to be an attacker, winger and game maker you need to have
# Excellent ball control.
plot(attr_Num_Data$Overall,attr_Num_Data$BallControl,xlab="Overall Rating / 100",ylab="Ball Control / 100",col = kMeans_Player_Categories$cluster, 
     pch = 20,cex.axis=0.8, main = "Analysis of Player Ball Control vs. Rating")

# What's funny is short passing is somewhat similar to ball control, makes me think if going from a defender to a playmaker
# Increases your rating.
plot(attr_Num_Data$Overall,attr_Num_Data$ShortPassing,xlab="Overall / 100",ylab="Short Passing / 100",col = kMeans_Player_Categories$cluster, 
     pch = 20,cex.axis=0.8, main = "Analysis of Short Passing vs. Overall Rating")

# Well, bend it like beckham? The data is bent like a banana kick! A lot of the average players are excellent
# Free kick takers! A good free kick taker will have a good ball curves.
plot(attr_Num_Data$Curve,attr_Num_Data$FKAccuracy,xlab="Free Kick Accuracy / 100",ylab="Ball Curve / 100",col = kMeans_Player_Categories$cluster, 
     pch = 20,cex.axis=0.8, main = "Bend it like Beckham?")

#################*****************************************************************####################

# We'll try predicting a player's rating taking their attributes as inputs
# We would try doing this with Linear Regression

# Taking data from crossing till before release clause covering all the major attributes
pred_Rating_Data = data[,55:88]
add_Col_Heads = c("Weight","Overall")
add_Col_Heads_Data = data[,add_Col_Heads]
pred_Rating_Data = cbind(pred_Rating_Data, add_Col_Heads_Data)
str(pred_Rating_Data)

# Checking for NA values
missingValues <- lapply(pred_Rating_Data, function(x) sum(is.na(x)))
print.listof(missingValues)
pred_Rating_Data = na.omit(pred_Rating_Data)

# Dropped 48 rows, sorry players :( blame Fifa I can't impute fakes here
# Preprossessing data for linear regression

# Converting weight to kgs 
pred_Rating_Data$Weight = sapply(pred_Rating_Data$Weight, function(x) as.numeric(substring(as.character(x),1,nchar(as.character(x))-3))*0.453592)

# Analyse the correlation matrix
cor(pred_Rating_Data)

# Removing penalties, Aggression, Interceptions, Agility, Volleys
rem = c("Penalties", "Aggression", "Interceptions", "Agility", "Volleys","Jumping","FKAccuracy")
rem = setdiff(colnames(pred_Rating_Data),rem)
pred_Rating_Data <- pred_Rating_Data[,rem]

# pred_Rating_Data is train data now
set.seed(1244)
train_RowIDs = sample(1:nrow(pred_Rating_Data),nrow(pred_Rating_Data)*0.6)
pred_Rating_Data_Train = pred_Rating_Data[train_RowIDs,]
pred_Rating_Data_Test = pred_Rating_Data[-train_RowIDs,]
Actual <- pred_Rating_Data_Test$Overall
pred_Rating_Data_Test$Overall = NULL
pred_Rating_Data_Test <- cbind(pred_Rating_Data_Test,Actual)

pred_lm <- lm(
  Overall ~ BallControl + Reactions + HeadingAccuracy + ShortPassing + Composure + Marking + GKDiving + GKHandling + Finishing + Curve + StandingTackle + Positioning
    ,data = pred_Rating_Data_Train,method = "qr")
pred_lm
summary(pred_lm)
par(mfrow=c(1,1))

plot(pred_Rating_Data_Train$Overall,pred_Rating_Data_Train$Weight,pch=20)
(pred_lm$coefficients)

#Overall = pred_Rating_Data_Test$Actual
#pred_Rating_Data_Test$Actual = Overall
Overall_Predicted <- predict(pred_lm,newdata = pred_Rating_Data_Test)
pred_Rating_Data_Check = cbind(pred_Rating_Data_Test,ceiling(Overall_Predicted))
View(pred_Rating_Data_Check)
