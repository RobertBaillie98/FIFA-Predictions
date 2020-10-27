# Loading Data and Libraries
library(ggplot2)
library(ggridges)
library(gridExtra)
library(MASS)
library(tidyverse)
library(dplyr)
library(caret)
library(e1071)
library(kernlab)
library(randomForest)
library(ggthemes)
library(ggcorrplot)
library(psych)

setwd("C:/Users/Bobby/Desktop/Learning/Projects/Complete-Not Quite/Fifa Predictions")

fifa20.full <- read.csv("players_20.csv", header = T,na.strings = c("","NA"))
fifa21.full <- read.csv("reduced.csv", header = T,na.strings = c("","NA"))


head(fifa20.full)
head(fifa21.full)

ncol(fifa20.full)

# Cleaning Data

#We have 18278 entries
colSums(is.na(fifa20.full)) #2036 GK and 16242 Others

#Move GK stats to corresponding columns
#Need to set the NA to empty strings
fifa20.full <- fifa20.full %>% 
  mutate_all(~ replace_na(.x, ""))

fifa20.full$pace <- paste(fifa20.full$pace,fifa20.full$gk_diving)
fifa20.full$shooting <- paste(fifa20.full$shooting,fifa20.full$gk_handling)
fifa20.full$passing <- paste(fifa20.full$passing,fifa20.full$gk_kicking)
fifa20.full$dribbling <- paste(fifa20.full$dribbling,fifa20.full$gk_reflexes)
fifa20.full$defending <- paste(fifa20.full$defending,fifa20.full$gk_speed)
fifa20.full$physic <- paste(fifa20.full$physic,fifa20.full$gk_positioning)


#12 Columns in FIFA 21, 104 in FIFA 20. make new data frames to match columns.
keep <- c("player_positions","pace","shooting","passing","dribbling","defending","physic","club","nationality","overall","short_name")
remove <- c("Ranking")

fifa20.reduced <- fifa20.full[ , (names(fifa20.full) %in% keep)]
fifa21.reduced <- fifa21.full[ , !(names(fifa21.full) %in% remove)]


list <- gsub(",.*","",fifa20.reduced$player_positions)
fifa20.reduced$player_positions <- c(list)

#Change column headers on FIFA 20 data set to match FIFA 21
fifa20.reduced <- fifa20.reduced %>% 
  rename(
    Name = short_name,
    OVR = overall, 
    Nationality = nationality,
    Club = club,
    Position = player_positions,
    PAC = pace, SHO = shooting, PAS = passing, DRI = dribbling, DEF = defending, PHY = physic
  )


#Set each attribute as factor/numeric

fifa20.reduced$OVR <- as.numeric(fifa20.reduced$OVR)
fifa20.reduced$PAC <- as.numeric(fifa20.reduced$PAC)
fifa20.reduced$SHO <- as.numeric(fifa20.reduced$SHO)
fifa20.reduced$PAS <- as.numeric(fifa20.reduced$PAS)
fifa20.reduced$DRI <- as.numeric(fifa20.reduced$DRI)
fifa20.reduced$DEF <- as.numeric(fifa20.reduced$DEF)
fifa20.reduced$PHY <- as.numeric(fifa20.reduced$PHY)

#No RWB or LWB in FIFA 21 set - Set RWB & LWB to RB & LB respectively

fifa20.reduced$Position <- gsub("RWB", "RB", fifa20.reduced$Position)
fifa20.reduced$Position <- gsub("LWB", "LB", fifa20.reduced$Position)





#EDA

#Distribution of players

Dis.20 <- ggplot(fifa20.reduced, aes(Position)) +
  geom_bar(aes(fill=factor(Position)), alpha=0.8)+ 
  theme(legend.position = "none")+
  ggtitle("FIFA 20 Player Distributions") +
  xlab("Count") + ylab("Position")


Dis.21 <- ggplot(fifa21.reduced, aes(Position))+ 
  geom_bar(aes(fill=factor(Position)), alpha=0.8)+ 
  theme(legend.position = "none")+
  ggtitle("FIFA 21 Top 100 \n Player Distributions") +
  xlab("Count") + ylab("Position")


grid.arrange(Dis.20, Dis.21, ncol=2)


# New Columns for GK, Def, Mid, Atk
fifa20.reduced$Area <- ifelse(grepl("GK", fifa20.reduced$Position, ignore.case = T), "GK", 
                  ifelse(grepl("LB", fifa20.reduced$Position, ignore.case = T), "DEF", 
                  ifelse(grepl("RB", fifa20.reduced$Position, ignore.case = T), "DEF", 
                  ifelse(grepl("CB", fifa20.reduced$Position, ignore.case = T), "DEF", 
                  ifelse(grepl("CAM", fifa20.reduced$Position, ignore.case = T), "MID", 
                  ifelse(grepl("CDM", fifa20.reduced$Position, ignore.case = T), "MID", 
                  ifelse(grepl("CM", fifa20.reduced$Position, ignore.case = T), "MID", 
                  ifelse(grepl("LM", fifa20.reduced$Position, ignore.case = T), "MID", 
                  ifelse(grepl("RM", fifa20.reduced$Position, ignore.case = T), "MID", 
                  ifelse(grepl("LW", fifa20.reduced$Position, ignore.case = T), "ATK", 
                  ifelse(grepl("RW", fifa20.reduced$Position, ignore.case = T), "ATK", 
                  ifelse(grepl("ST", fifa20.reduced$Position, ignore.case = T), "ATK", 
                  ifelse(grepl("CF", fifa20.reduced$Position, ignore.case = T), "ATK", "Other")))))))))))))


fifa21.reduced$Area <- ifelse(grepl("GK", fifa21.reduced$Position, ignore.case = T), "GK", 
                       ifelse(grepl("LB", fifa21.reduced$Position, ignore.case = T), "DEF", 
                      ifelse(grepl("RB", fifa21.reduced$Position, ignore.case = T), "DEF", 
                      ifelse(grepl("CB", fifa21.reduced$Position, ignore.case = T), "DEF", 
                      ifelse(grepl("CAM", fifa21.reduced$Position, ignore.case = T), "MID", 
                      ifelse(grepl("CDM", fifa21.reduced$Position, ignore.case = T), "MID", 
                      ifelse(grepl("CM", fifa21.reduced$Position, ignore.case = T), "MID", 
                      ifelse(grepl("LM", fifa21.reduced$Position, ignore.case = T), "MID", 
                      ifelse(grepl("RM", fifa21.reduced$Position, ignore.case = T), "MID", 
                      ifelse(grepl("LW", fifa21.reduced$Position, ignore.case = T), "ATK", 
                      ifelse(grepl("RW", fifa21.reduced$Position, ignore.case = T), "ATK", 
                      ifelse(grepl("ST", fifa21.reduced$Position, ignore.case = T), "ATK", 
                      ifelse(grepl("CF", fifa21.reduced$Position, ignore.case = T), "ATK", "Other")))))))))))))

# Stat Distribution

S.Dis.20 <- ggplot(fifa20.reduced, aes(x = OVR,y=Area, fill = Area)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
ggtitle("FIFA 20 Stat Distribution")

S.Dis.21 <- ggplot(fifa21.reduced, aes(x = OVR,y=Area, fill = Area)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
ggtitle("FIFA 21 Top 100 \n Stat Distribution")

grid.arrange(S.Dis.20, S.Dis.21, ncol=2)

# OVR Average

S.Avg.20 <- ggplot(fifa20.reduced, aes(x=Area, y=OVR, fill=Area)) +
  geom_boxplot()+ 
theme(legend.position = "none") +
  ggtitle("FIFA 20 OVR Distribution")

S.Avg.21 <- ggplot(fifa21.reduced, aes(x=Area, y=OVR, fill=Area)) +
  geom_boxplot()+
  theme(legend.position = "none") +
  ggtitle("FIFA 21 Top 100 \n OVR Distribution")

grid.arrange(S.Avg.20, S.Avg.21, ncol=2)



# Stat Average for each Area

x <- fifa20.reduced[,6:11]
y <- as.factor(fifa20.reduced[,12])

# Boxplot for each attribute on one image
par(mfrow=c(1,6))
for(i in 1:6) {
  boxplot(x[,i], main=names(x)[i])
}

featurePlot(x=x, y=y, plot="box")


# Distribution and Correlation Plot for FIFA 21

pairs.panels(fifa21.reduced[,3:9], 
             method = "pearson", 
             hist.col = "#00AFBB",
             density = TRUE,
             ellipses = TRUE)



#Correlation matrix
corr <- round(cor(fifa20.reduced[,6:11]), 1)

ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle",
           ggtheme=theme_bw)





# Making a model

# Setting variables to be tested 
#Name, Club have no valuable information

fifa20.reduced$Position <- as.numeric(as.factor(fifa20.reduced$Position))
fifa21.reduced$Position <- as.numeric(as.factor(fifa21.reduced$Position))


Equation <- "OVR ~ Position + PAC + SHO + PAS + DRI + DEF + PHY"
Formula <- as.formula(Equation)


# Run algorithms using 2-fold cross validation
control <- trainControl(method="cv", number=2)
metric <- "RMSE"

#Training Models
# GLM
fit.glm <- train(Formula, data=fifa20.reduced, method="glm", metric=metric, trControl=control, na.action=na.exclude)
# kNN
fit.knn <- train(Formula, data=fifa20.reduced, method="knn", metric=metric, trControl=control,na.action=na.exclude)
# SVM
fit.svm <- train(Formula, data=fifa20.reduced, method="svmRadial", metric=metric, trControl=control,na.action=na.exclude)
# Random Forest
fit.rf <- train(Formula, data=fifa20.reduced, method="rf", metric=metric, trControl=control,na.action=na.exclude)

# Summarize accuracy of models
results <- resamples(list(glm=fit.glm, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)


# Compare accuracy of models
dotplot(results)

#Lower value of RMSE is better, hence SVM is our best fitting model.
# summarize Best Model
print(fit.svm)

#Writing to a CSV

predicted <- predict(fit.svm, fifa21.reduced,se=TRUE)

Name <- fifa21.reduced$Name
output.df <- as.data.frame(Name)
output.df$OVR_Predicted <- predicted
output.df$OVR_Actual <- fifa21.reduced$OVR 



write.csv(output.df, file = "predictions.csv",row.names=FALSE)

fifa21.predictions <- read.csv("predictions.csv", header = T,na.strings = c("","NA"))
head(fifa21.predictions)

#Result Conclusion

# Scatter Plot

ggplot(output.df, aes(x=OVR_Predicted, y=OVR_Actual)) + 
  geom_point(aes()) + 
  geom_smooth(method="lm", se=F, color ="red")+ 
  ggtitle("Overall Predicted vs Actual Overall") +
  xlab("Predicted Overall") + ylab("Actual Overall")

cor(output.df$OVR_Actual,output.df$OVR_Predicted) #0.72

# Difference between columns
output.df$Difference <- output.df$OVR_Actual - output.df$OVR_Predicted

ggplot(output.df, aes(x=Difference)) + geom_histogram(aes(y=..density..),color="black",fill="indianred1") + 
  geom_vline(aes(xintercept=median(Difference)),
              color="red", linetype="dashed", size=1) +
  geom_density(alpha=.2, fill="beige") + 
  ggtitle("Density plot of Difference of \n Overall vs Predicted") 

mean(output.df$Difference) #0.971
median(output.df$Difference)#0.72
#Slightly undershooting

table(cut(output.df$Difference,breaks=c(-2,-1,-0.5,0,0.5,1,2,4,8)))/length(output.df$Difference)
