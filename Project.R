# Read csv into environment 
visa = read.csv('us_perm_visas.csv')
str(visa)

# Clean NA's from dataset
library(tidyr)
visa = visa[-24]
visa[visa == ""] = NA
visa = drop_na(visa)
str(visa)

# Year salary 
library(dplyr)
visa = subset(visa, wage_offer_unit_of_pay_9089 == 'yr')
visa$wage_offer_unit_of_pay_9089 = droplevels(visa$wage_offer_unit_of_pay_9089)
table(visa$wage_offer_unit_of_pay_9089)
str(visa)

# Filter to Certified and Denied
table(visa$case_status)
visa = subset(visa, case_status == 'Certified' | case_status == 'Denied')
visa$case_status = droplevels(visa$case_status)
table(visa$case_status)

# Convert case_status to Logical 
str(visa$case_status)
table(visa$case_status)
visa$case_status = ifelse(visa$case_status == 'Certified', TRUE, FALSE)

# Create a new data frame with the top 5 class of Admissions visas
levels(visa$class_of_admission)
visa = subset(visa, class_of_admission == 'H-1B' | class_of_admission == 'F-1' | class_of_admission == 'L-1' |
               class_of_admission == 'E-2' | class_of_admission == 'B-2')
visa$class_of_admission = droplevels(visa$class_of_admission)
visa = visa[-c(13,21,15)]
levels(visa$pw_level_9089)

# Visualizations 
# Bar chart to see frequency of visas for class admission
library(ggplot2)
ggplot(visa, aes(x=class_of_admission, fill=class_of_admission)) + geom_bar() + 
  labs(x = 'Class of Admission', title = 'Total Visas Distribution') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5)) 

# See distribution of visas for class admission for top 5 countries
summary(visa$country_of_citizenship)
library(dplyr)
top_5_countries = filter(visa, country_of_citizenship %in% c('INDIA','CHINA','SOUTH KOREA', 
                                                              'CANADA','PHILIPPINES', 'MEXICO'))
droplevels(top_5_countries$country_of_citizenship)
ggplot(top_5_countries, aes(x=class_of_admission,fill=class_of_admission)) + geom_bar() +
  facet_wrap(~country_of_citizenship) +
  labs(x = 'Class of Admission', title = 'Countries with Most Applications') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5))

# Create a US map distribution for class of admission
summary(visa$employer_city)
cities = c("NEW YORK", "HOUSTON", "SAN DIEGO", "CHICAGO","SAN JOSE","REDMOND","SAN FRANCISCO",
           "LOS ANGELES", "DALLAS", "SUNNYVALE")
lat = c(40.7128, 29.7604, 32.7157, 41.8781, 37.3382, 47.6740, 37.7749, 34.0522, 32.7767, 37.3688)
long = c(-74.0060, -95.3698, -117.1611, -87.6298, -121.8863, -122.1215, -122.4194, -118.2437, -96.7970, -122.0363)
values = c(792, 334, 230, 210, 148, 163, 161, 148, 140, 179)
cities_df = data.frame(Cities = as.character(), lat = as.numeric(), long = as.numeric())
cities_df = cbind(cities,lat,long, values)
library('leaflet')
leaflet(cities_df) %>% addProviderTiles('Esri.WorldStreetMap') %>% 
  addCircles(lng = long, lat = lat, popup = cities, color = 'red',fillOpacity = 0.4, opacity = 0.5,
             radius = values*200)

# Distribution of top 10 employers with visa applications
library(dplyr)
top_10_employer_visa = visa[,c('application_type', 'employer_name')] %>% 
  group_by(employer_name) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(10, count)
ggplot(top_10_employer_visa, aes(employer_name, count)) + 
  geom_bar(stat = 'identity', fill='deepskyblue4', alpha=0.8) +
  scale_x_discrete("Employer Name") + scale_y_continuous("Number of Applications") +
  labs(title = 'Top 10 Employers with Applications') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5))

# Distribution of top 10 employers with visa applications for Certified vs. Denied
employers_visa_status = visa[,c('case_status','employer_name')] %>% 
  group_by(employer_name, case_status) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(10,count)
employers_visa_status = merge(employers_visa_status, top_10_employer_visa, by = 'employer_name')
employers_visa_status = mutate(employers_visa_status, percentage = round(employers_visa_status$count.x *100
                                                                         / employers_visa_status$count.y, 0))
ggplot() + geom_bar(aes(x = employer_name, y=percentage, fill=case_status), 
  data = employers_visa_status, stat = 'identity') +
  geom_text(data=employers_visa_status, aes(x = employer_name, y= percentage, label = paste0(percentage, '%')),
  colour = 'black', size=4) + theme(legend.position = 'bottom', legend.title = element_blank()) + coord_flip() +
  labs(title = 'Distribution of Application Status for Top 10 Companies', x='Employer', y='Percentage Distribution') + theme_bw()

# Pie Chart
pie_data = data.frame(Class_Admission = c('B-2','E-2','F-1','H-1B','L-1'), 
                      Percent = c(1.8,1.6,2.8,90.6,3.2))
bp = ggplot(pie_data, aes(x='', y=Percent, fill=Class_Admission)) + geom_bar(width = 1, stat='identity') +
  labs(title = 'Pie Chart for Class of Admission')
pie = bp+ coord_polar('y', start=0)
pie
library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid=element_blank(),
    axis.text = element_blank(),
    plot.title=element_text(size=20, face="bold")
  )
pie +
  theme(axis.text.x=element_blank())+ blank_theme 

# Decision Tree
visa2 = read.csv('us_perm_visas2.csv')

tree_data = visa2[c(3,20,4)]
library(dplyr)
set.seed(150)
train.index <- sample(row.names(tree_data), 0.6*dim(tree_data)[1])  
valid.index <- setdiff(row.names(tree_data), train.index)  
train.df <- tree_data[train.index, ]
valid.df <- tree_data[valid.index, ]
library(rpart.plot)
library(rpart)
model = rpart(case_status~., data = valid.df, method ='anova' , minsplit =2, minbucket=4)
rpart.plot(model, extra=100, type=4)

##
library(dplyr)
library(ggplot2)
visa %>% group_by(country_of_citizenship) %>%
  summarise(CountOfCountry = n()) %>%
  arrange(desc(CountOfCountry)) %>%
  mutate(country_of_citizenship = reorder(country_of_citizenship, CountOfCountry)) %>%
  head(20) %>%
  
  ggplot(aes(x = country_of_citizenship,y = CountOfCountry)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = country_of_citizenship, y = 1, label = paste0("(",CountOfCountry,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Country Name', y = 'Count Of Visa Applications', title = 'Country Name and Visas') +
  coord_flip() + 
  theme_bw()
#####
visa %>% 
  
  group_by(employer_name,class_of_admission) %>%
  summarise(CountOfEmployer = n()) %>%
  arrange(desc(CountOfEmployer)) %>%
  head(20) %>%
  
  ggplot(aes(x = employer_name,y = CountOfEmployer, fill =class_of_admission)) +
  geom_bar(stat='identity',colour="white", fill="blue") +
  labs(x = 'Employer', y = 'Count Of Visa Applications', title = 'Company and Visas with Class of Admission') +
  coord_flip() + 
  theme_bw()
###
bp <-
  visa %>%
  filter(!is.na(wage_offer_from_9089)) %>%
  filter(case_status == "TRUE" ) %>%
  group_by(employer_name) %>%
  summarise(MeanEmployeeSalary = mean(wage_offer_from_9089)) %>%
  arrange(desc(MeanEmployeeSalary)) %>%
  mutate(employer_name = reorder(employer_name, MeanEmployeeSalary)) %>%
  mutate(MeanEmployeeSalary2 = MeanEmployeeSalary)  %>%
  mutate(MeanEmployeeSalary = scales::dollar(MeanEmployeeSalary)) %>%
  head(5) %>%
  
  ggplot(aes(x = employer_name,y = MeanEmployeeSalary2)) +
  geom_bar(stat='identity',colour="white", fill='red') +
  geom_text(aes(x = employer_name, y = 1, label = paste0("(",MeanEmployeeSalary,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'blue',
            fontface = 'bold') +
  labs(x = 'Employer Name', y = 'Mean Salary', title = 'Employee Salary') +
  coord_flip() + 
  theme_bw()
bp
##
visa %>% 
  filter(employer_city == "SAN DIEGO") %>% 
  group_by(employer_name) %>%
  summarise(CountOfEmployerName = n()) %>%
  arrange(desc(CountOfEmployerName)) %>%
  mutate(employer_name = reorder(employer_name, CountOfEmployerName)) %>%
  head(20) %>%
  
  ggplot(aes(x = employer_name,y = CountOfEmployerName)) +
  geom_bar(stat='identity',colour="white", fill ="blue") +
  geom_text(aes(x = employer_name, y = 1, label = paste0("(",CountOfEmployerName,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Employer Name', y = 'Count Of Visa Applications in SAN DIEGO', title = 'Top 20 Employers in SAN DIEGO and Visas') +
  coord_flip() + 
  theme_bw()
###
visa %>% group_by(pw_soc_title) %>%
  filter(class_of_admission=="H-1B"& case_status=="TRUE") %>%
  
  summarise(CountOfEmployer = n()) %>%
  arrange(desc(CountOfEmployer)) %>%
  mutate(employer_name = reorder(pw_soc_title, CountOfEmployer)) %>%
  head(20) %>%
  
  ggplot(aes(x = employer_name,y = CountOfEmployer, fill=employer_name)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = employer_name, y = 1, label = paste0("(",CountOfEmployer,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Job Title', y = 'Count H1B Certified Visa Applications', title = 'Job Title and H1B Approved') +coord_flip() +
  theme_bw()
##
visa %>% group_by(us_economic_sector) %>%
  filter(class_of_admission=="H-1B" & case_status=="TRUE") %>%
  
  summarise(CountOfSector = n()) %>%
  arrange(desc(CountOfSector)) %>%
  mutate(employer_name = reorder(us_economic_sector, CountOfSector)) %>%
  head(5) %>%
  
  ggplot(aes(x = us_economic_sector,y = CountOfSector, fill=us_economic_sector)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = us_economic_sector, y = 1, label = paste0("(",CountOfSector,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Sector', y = 'Count of Certified H1B', title = 'Sector and H1B Approved') +
  # coord_flip() + 
  theme_bw()

##
visa %>% group_by(us_economic_sector) %>%
  filter(class_of_admission=="E-2" & case_status=="TRUE") %>%
  
  summarise(CountOfSector = n()) %>%
  arrange(desc(CountOfSector)) %>%
  mutate(employer_name = reorder(us_economic_sector, CountOfSector)) %>%
  head(10) %>%
  
  ggplot(aes(x = us_economic_sector,y = CountOfSector, fill=us_economic_sector)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = us_economic_sector, y = 1, label = paste0("(",CountOfSector,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Job Title', y = 'Count of Certified E-2', title = 'E-2 Visa and Sector') +
  coord_flip() + 
  theme_bw()
####
summary(visa$class_of_admission)

visa %>% group_by(employer_city,class_of_admission) %>%
  filter(class_of_admission!="H-1B") %>%
  summarise(CountOfCity = n()) %>%
  arrange(desc(CountOfCity)) %>%
  head(20) %>%
  
  ggplot(aes(x = employer_city,y = CountOfCity, fill = class_of_admission)) +
  geom_histogram(stat='identity',colour="white") +
  labs(x = 'City Name', y = 'Count Of Applications', title = 'Top City Name and Visas')+ coord_flip() + 
  theme_bw()
##
np <- visa %>% group_by(class_of_admission) %>%
  
  summarise(CountOfAdmission = n()) %>%
  arrange(desc(CountOfAdmission)) %>%
  mutate(class_of_admission = reorder(class_of_admission, CountOfAdmission)) %>%
  head(20) %>%
  
  ggplot(aes(x = class_of_admission,y = CountOfAdmission, fill=class_of_admission)) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = class_of_admission, y = 1, label = paste0("(",CountOfAdmission,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Class of Admission', y = 'Count Of Visa Applications', title = 'Admission Type and Visas') +
  coord_flip() + 
  theme_bw()
pie <- np + coord_polar(theta = "y")
pie  
##
install.packages("lubridate")
library(lubridate)
visa$decision_year = year(as.Date(visa$decision_date, format = "%m/%d/%Y"))

trendCertifiedVisas = visa %>%
  filter(case_status == TRUE) %>%
  group_by(decision_year) %>%
  tally() 

ggplot(trendCertifiedVisas,aes(x = decision_year,y = n, fill=decision_year)) +
  geom_bar(stat='identity',colour="white") +
  labs(x = 'Year', y = 'Count Certified Visas', title = 'Trend Certified Visas') +
  theme_bw()


visa2 <- year(as.Date(visa$decision_date, format = "%m/%d/%Y"))
View(visa2)
#####
library(plyr)
visa$pw_level_9089 =revalue(visa$pw_level_9089, c('Level I'= "Associate",'Level II'='Junior Level', 'Level III'='Senior Level', 'Level IV'='Manager'))
View(visa)
str(visa$pw_level_9089)

trendCertifiedVisas = visa %>%
  filter(case_status == TRUE) %>%
  group_by(pw_level_9089) %>%
  tally() 

ggplot(trendCertifiedVisas,aes(x = pw_level_9089,y = n, fill=pw_level_9089)) +
  geom_bar(stat='identity',colour="white") +
  labs(x = 'Position', y = 'Count Certified Visas', title = 'Certified Visas Based on Position') +
  theme_bw()
##

library(dplyr)
top_5_level = filter(visa, pw_level_9089 %in% c('Associate','Junior Level','Senior Level', 'Manager'))
droplevels(top_5_level$pw_level_9089)
ggplot(top_5_level, aes(x=class_of_admission,fill=class_of_admission)) + geom_bar() +
  facet_wrap(~pw_level_9089) +
  labs(x = 'Class of Admission', title = 'Positions with Most Applications') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5))

###
library(dplyr)
top_5_City = filter(visa, employer_state %in% c('MA','NY','TX', 'CA', 'NJ'))
droplevels(top_5_City$employer_state)
ggplot(top_5_City, aes(x=case_status,fill=case_status)) + geom_bar() +
  facet_wrap(~employer_state, ncol =5) +
  labs(x = 'Case Status', y= 'count of certified VS rejected', title = 'State with Most Applications') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5))

###
usPermVisas=read.csv('us_perm_visas.csv')

usPermVisas$decision_year = year(ymd(usPermVisas$decision_date))

trendCertifiedVisas = usPermVisas %>%
  filter(case_status == 'Certified' ) %>%
  group_by(decision_year) %>%
  tally() 

ggplot(trendCertifiedVisas,aes(x = decision_year,y = n, fill=decision_year)) +
  geom_bar(stat='identity',colour="white") +
  labs(x = 'Year', y = 'Count of Certified Visas', title = 'Trend Certified Visas') +
  theme_bw()

####
library(caret)
dmy <- dummyVars("~class_of_admission", data = visa, fullRank = TRUE)
dmy1 <- dummyVars("~pw_level_9089", data = visa, fullRank = TRUE)
transform = data.frame(predict(dmy, newdata = visa))
transform1 = data.frame(predict(dmy1, newdata = visa))

new_visa = cbind(visa, transform, transform1)
View(new_visa)
regressiondata = new_visa[c(3, 20, 22:28)]

library(GGally)
ggpairs(regressiondata)

fitall = lm(formula = case_status~wage_offer_from_9089, data = regressiondata)
fitall
summary(fitall)
anova(fitall)
randomwage <- data.frame(runif(10, min=40000, max=200000))
randomwage
library(forecast)
predict(fitall, randomwage)


