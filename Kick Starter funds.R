# K-Nearest Neighbors 
#1 Download the file ‘google_play_store.csv’ from our class Blackboard site.
#2 Read this file into your R environment. Show the step that you used to accomplish this.
google = read.csv("google_play_store.csv")

#3a Delete the following columns, as you will not need them: X, Category, Genres,
#Last.Updated, Current.Ver and Android.Ver. Show the step(s) that you used to do this.
google = google[c(-1,-3,-11,-12,-13,-14)]

#3b For any rows with “NA” values in Rating column, delete those rows entirely.
#Show the step(s) that you used to do this
library(tidyr)
google = drop_na(google, Rating)
anyNA(google$Rating)

#3c Remove any rows with a Content.Rating of “Adults only 18+” or “Unrated.”
#Then, use the droplevels() function to be sure that your dataset only contains
#the factors that you are actually using. Show the step(s) that you used to do this
table(google$Content.Rating)
str(google$Content.Rating)
dim(google)
which(google$Content.Rating == "Adults only 18+" | google$Content.Rating == "Unrated")
google1 = google[-c(288, 2905, 5949, 7342),]
dim(google1)
google1$Content.Rating = droplevels(google1$Content.Rating)
table(google1$Content.Rating)
str(google1$Content.Rating)

#3d What type of variable is Size? Use the as.numeric() function to convert this variable into a numeric data type
str(google1)
google1$Size = as.numeric(google1$Size)

#4a See PDF document for this answer
#4b 
round(runif(1, min = 3.5, max = 5),1)
#4c 
round(runif(1, min = 1, max = 6002),0)
#4d 
round(runif(1, min = 200, max = 460),0)
#4e 
table(google1$Type)
8715 / (8715+647)
rbinom(1,1,0.930)

#5 
library("caret")
Type1 = dummyVars("~Type", data = google1)
Content1 = dummyVars("~Content.Rating", data = google1)
Type2 = data.frame(predict(Type1, newdata = google1))
Content2 = data.frame(predict(Content1, newdata = google1))
google2 = cbind(google1, Type2, Content2)
google2 = google2[-c(6,8)]

#6
library("dplyr")
set.seed(150)
google3 = sample_n(google2,9362)
training = slice(google3, 1:5620) #60% of dataset
validation = slice(google3, 5621:9362)

#7 
myapp = data.frame(App = "Mindfulness", Rating = 4.7, Reviews = 5727, Size = 425, Price = 0, Type.Free = 1
                   , Type.Paid = 0, Content.Rating.Everyone = 1, Content.Rating.Everyone.10. = 0,
                   Content.Rating.Mature.17. = 0, Content.Rating.Teen = 0)

#8 
library("caret")
google3$Reviews = as.numeric(google3$Reviews)
google3 = google3[c(1,2,3,4,6,7,8,9,10,11,12,5)]
training = training[c(1,2,3,4,6,7,8,9,10,11,12,5)]
validation = validation[c(1,2,3,4,6,7,8,9,10,11,12,5)]
google.norm = google3
train.norm = training
valid.norm = validation
norm.values = preProcess(training[,2:11], method = c("center", "scale"))
train.norm[,2:11] = predict(norm.values, training[,2:11])
valid.norm[,2:11] = predict(norm.values, validation[,2:11])
google.norm[,2:11] = predict(norm.values, google3[,2:11])
new.norm = predict(norm.values, myapp)

#9 
library("FNN")
nn = knn(train = train.norm[,2:11], test = new.norm[,2:11], cl= train.norm[,12], k = 7)
row.names(train.norm)[attr(nn, "nn.index")]
nn

#10 
sqrt(5620)
accuracy = data.frame(k = seq(1, 75, 1), accuracy = rep(0, 75))
for(i in 1:75) {
  knn.pred <- knn(train.norm[, 2:11], valid.norm[, 2:11], 
                  cl = train.norm[, 12], k = i)
  accuracy[i, 2] <- confusionMatrix(knn.pred, valid.norm[, 12])$overall[1] 
}
accuracy
summary(accuracy)

#11
library("FNN")
nn = knn(train = train.norm[,2:11], test = new.norm[,2:11], cl= train.norm[,12], k = 61)
row.names(train.norm)[attr(nn, "nn.index")]
nn

# Naive Bayes
#1 
kickstarter = read.csv("kickstart_project.csv")

#2 See PDF for the explanation

#3 
summary(kickstarter$goal)
kickstarter$goal = cut(kickstarter$goal, breaks = c(0,2000,5000,15000,100000000),
                       labels = c("Small", "Medium", "High", "Extremely High"))
summary(kickstarter$goal)
#4 
library("dplyr")
set.seed(150)
selected.var = c(4,6,8,10,13,15)
train.index = sample(c(1:dim(kickstarter)[1]), dim(kickstarter)[1]*0.6)
train.df = kickstarter[train.index, selected.var]
valid.df = kickstarter[-train.index, selected.var]

#5
library(e1071)
kickstarter.nb = naiveBayes(state ~ ., data = train.df)
kickstarter.nb

#6 
summary(kickstarter$state)
library(caret)
pred.training = predict(kickstarter.nb, newdata = train.df)
confusionMatrix(pred.training, train.df$state)
pred.validation = predict(kickstarter.nb, newdata = valid.df)
confusionMatrix(pred.validation, valid.df$state)

#7 
library(dplyr) 
kickstarter.state = group_by(kickstarter, state)
kickstarter.state = summarize(kickstarter.state, avg = mean(goal))
library(ggplot2)
ggplot(kickstarter.state, aes(x = state, y= avg, fill = state)) + geom_bar(stat = "identity") + 
         labs(x = "State", y = "Average Goal", title = "State vs. Goal") + theme(axis.text.x = element_text(angle = 30, hjust = 1), 
         plot.title = element_text(hjust = 0.5))

