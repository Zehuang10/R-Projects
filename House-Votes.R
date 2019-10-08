# Classification Tree
#1
congress = read.csv("house-votes-84.csv", header = FALSE)

#2
colnames(congress) = c("ClassName", "HandicappedInfants", "WaterProjectCostSharing",
                       "AdoptionOfBudgetResolution", "PhysicianFeeFreeze", "ElSalvadorAid",
                       "ReligiousGroupsInSchool", "AntiSatelliteTestBan", "AidToNicaraguanContras",
                       "MxMissile", "Immigration", "SynfuelsCorporationCutback", "EducationSpending",
                       "SuperfundRightToSue", "Crime", "DutyFreeExports", 
                       "ExportAdministrationActSouthAfrica")

#3 
summary(congress$`ClassName`)

#4
library(dplyr)
set.seed(150)
train.index <- sample(row.names(congress), 0.6*dim(congress)[1])  
valid.index <- setdiff(row.names(congress), train.index)  
train.df <- congress[train.index, ]
valid.df <- congress[valid.index, ]

#5
library(rpart.plot)
library(rpart)
model = rpart(ClassName~., data = train.df, method = "class", minsplit =2, minbucket=4)
rpart.plot(model, type = 4, extra = 101)

#6
library(caret)
pred = predict(model, train.df, type = "class")
confusionMatrix(pred, train.df$ClassName)
pred1 = predict(model, valid.df, type = "class")
confusionMatrix(pred1, valid.df$ClassName)

# Association Rules
#1
install.packages("nutshell")
install.packages("arules")
library(nutshell)
library(arules)
data(audioscrobbler)
class(audioscrobbler)
summary(audioscrobbler)

#2
itemFrequencyPlot(audioscrobbler, support = 0.21)

#3 #RHS
artist = apriori(audioscrobbler, parameter = list(support = 0.09, confidence = 0.25, minlen = 2),
                 appearance = list(default = "lhs", rhs = "The Cure"))
artist_lift = sort(artist, by = "lift")
inspect(head(artist_lift))

#LHS
artist1 = apriori(audioscrobbler, parameter = list(support = 0.08, confidence = 0.25, minlen = 2),
                 appearance = list(default = "rhs", lhs = "The Cure"))
artist_lift1 = sort(artist1, by = "lift")
inspect(head(artist_lift1))

#4 
install.packages("arulesViz")
library(arulesViz)
plot(artist_lift1[1:3], method = "scatterplot")

#5
library(arulesViz)
plot(artist_lift1[1:3], method = "graph", engine = "htmlwidget")

# Clustering 
#1
tripAdvisor = read.csv("tripadvisor_review.csv")

#2
row.names(tripAdvisor) = tripAdvisor[,1]
tripAdvisor = tripAdvisor[,-1]

#3 See PDF for the answer
tripAdvisor.norm = sapply(tripAdvisor, scale)
row.names(tripAdvisor.norm) = row.names(tripAdvisor)

#4
km = (nrow(tripAdvisor.norm)-1)*sum(apply(tripAdvisor.norm,2,var)) 
for (i in 2:15) km[i] = sum(kmeans(tripAdvisor.norm, centers = i)$withinss)
km
plot(1:15, km, type = "b", pch=19, xlab = "Number of Clusters", 
     main = "Elbow Chart", ylab = "Within-cluster sum of squares distances")
km1 <- kmeans(tripAdvisor.norm, 6)
km1$cluster
km1$centers

#5 
km1 <- kmeans(tripAdvisor.norm, 6)
km1$centers
