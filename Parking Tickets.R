#1 Download the file ‘parkingLA2017.csv’ from our class Blackboard site

#2 Read this file into your R environment
Parking= read.csv("parkingLA2017.csv")
#2a What are the dimensions of this dataframe? Show the code that you used to determine this.
dim(Parking)

#3 Filter the dataframe
library(dplyr)
Mazda=filter(Parking, Make=="MAZD")
dim(Mazda)

#4 Dealing with missing values 
#4a Are there any missing values in your dataset?
anyNA(Mazda)
#4b Does the variable RP.State.Plate contain any missing values?
anyNA(Mazda$RP.State.Plate)
#4c Find and display the standard deviation for the variable Fine.amount
sd(Mazda$Fine.amount)
Mazda$Fine.amount[is.na(Mazda$Fine.amount)]=median(Mazda$Fine.amount, na.rm = TRUE)
#4d Replace any blank cells in the Location column with NA
Mazda[Mazda$Location==" "]="NA"
anyNA(Mazda$Location)
#4e remove all of the records that contain “NA” for AGENCY.SHORT.NAME
library(tidyr)
Mazda=drop_na(Mazda,AGENCY.SHORT.NAME)

#5 Date Data Type
#5a
str(Mazda)
Mazda$Issue.Date=as.Date(Mazda$Issue.Date)
#5b
View(Mazda)
Mazda=arrange(Mazda,Issue.Date)
#5c
Juan_Birthday=subset(Mazda,Issue.Date>='2017-10-01' & Issue.Date<='2017-10-31')
View(Juan_Birthday)

#6 Remove 'Ticket Number' column
Mazda=Mazda[-c(2)]

#7 Using the summary() function
summary(Mazda)

#8 Identify the five most common types of violation descriptions in your dataset
summary(Mazda$Violation.Description)
summary(Mazda)

#9 Create a new dataframe that only contains data for the five most common violations
Violation_Mazda=filter(Mazda,Violation.Description %in% c("NO PARK/STREET CLEAN", "METER EXP.", 
"PREFERENTIAL PARKING", "DISPLAY OF TABS", "RED ZONE"))

#10 Using ggplot, create a barplot 
library(ggplot2)
ggplot(Violation_Mazda, aes(x=Violation.Description, fill=Violation.Description))+geom_bar(width = 0.5)+
labs(x="Violations Description",y="Number of Ocurrences",title="Violations for Mazda Cars")+theme(axis.text.x = element_text(angle = 30,hjust = 1),plot.title = element_text(hjust = 0.5))

#11 How did the average size of a fine vary from agency to agency?
Agency_Fine=group_by(Violation_Mazda, AGENCY.SHORT.NAME)
Agency_Fine=summarize(Agency_Fine,avg=mean(Fine.amount))
Agency_Fine=arrange(Agency_Fine,desc(avg))
View(Agency_Fine)
ggplot(Agency_Fine, aes(x=AGENCY.SHORT.NAME, y=avg, fill=AGENCY.SHORT.NAME))+geom_bar(stat = 'identity')+
labs(x="Agencies",y="Average Fine",title="Average Fines per Agency")+ theme(axis.text.x = element_text(angle = 30,hjust = 1),plot.title = element_text(hjust = 0.5))

#12 Using ggplot, create a violin plot
ggplot(Violation_Mazda, aes(x=AGENCY.SHORT.NAME, y=Fine.amount))+geom_violin()+theme(axis.text.x = element_text(angle = 30,hjust = 1))

#13 Using ggplot, create a histogram that shows the frequency of ticket issuances per hour
ggplot(Violation_Mazda, aes(x=Issue.time, fill=Violation.Description))+geom_histogram(bins = 24)+ labs(title="Ticket Issuance per Hour", x="Issue Time")
