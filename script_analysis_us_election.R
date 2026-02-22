#PREPARING THE DATASET FOR ALL  
#read data
data<-read.csv("C:/Users/calli/Downloads/PROJECT/us_election_data.csv", stringsAsFactors=FALSE)
summary(data)

#SIDE NOTES
#data in rates: percentage of the people that is (e.g. unemployed) for every 10000 population
#unemployment_dem: percentage of the people that is unemployed in the state that choose democrat

#NEW SUBSET 
#make new subset for the voteshare 
data$votes_share_dem <- data$votes_dem / (data$votes_dem + data$votes_rep)
data$votes_share_rep <- data$votes_rep / (data$votes_rep + data$votes_dem)

#make new subset for the money raised share 
data$money_raised_share_dem <-data$money_raised_dem/(data$money_raised_dem+data$money_raised_rep)
data$money_raised_share_rep <-data$money_raised_rep/(data$money_raised_dem+data$money_raised_rep)

#make new subset for categorize the democrate and republican 
data$win_dem <- ifelse(data$votes_dem > data$votes_rep, 1, 0)
data$win_rep <- ifelse(data$votes_dem < data$votes_rep, 1, 0)

#make the plot for each 
#DEMOCRATIC  
#money raised 
library(ggplot2)
ggplot(data, aes(x=votes_share_dem,y=money_raised_share_dem))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#population
library(ggplot2)
ggplot(data, aes(x=votes_share_dem,y=population))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

#unemployment
library(ggplot2)
ggplot(data, aes(x=votes_dem,y=economy))+
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#crime rate
library(ggplot2)
ggplot(data, aes(x=votes_share_dem,y=crime_rate))+
  geom_point()
#education rate
library(ggplot2)
ggplot(data, aes(x=votes_share_dem,y=education_rate))+
  geom_point()

#CASE 1: LINEAR REGRESSION 
#WHEN ALL VARIABLES SPLITTED BETWEEN DEM & REP
#PRO: THE INDEPENDENT VARIABLES CAN BE MORE COMPARABLE, GOOD VIF 
#CONS: LOWER R SQUARED 
                                               
#split each independent variable into republican and democrate
#winning table probability 
win_dem_split<-data$win_dem[data$win_dem==1]

#vote share 
votes_share_dem_split<-data$votes_share_dem[data$win_dem==1]
votes_share_rep_split<-data$votes_share_rep[data$win_dem==0]
#money raised 
money_raised_share_dem_split<-data$money_raised_share_dem[data$win_dem==1]
money_raised_share_rep_split<-data$money_raised_share_rep[data$win_dem==0]
#economy i.e. unemployment rate
unemployment_dem<-data$economy_rate [data$win_dem == 1]
unemployment_rep<-data$economy_rate [data$win_dem == 0]
#crime rate
crime_dem<-data$crime_rate [data$win_dem == 1]
crime_rep<-data$crime_rate [data$win_dem == 0]
#education
education_dem<-data$education_rate[data$win_dem == 1]
education_rep<-data$education_rate[data$win_dem == 0]
#population 
population_dem<-data$population[data$win_dem == 1]
population_rep<-data$population[data$win_dem == 0]

#FOR THE DEMOCRATIC
#run mlr dem
regl<-lm(votes_share_dem_split~money_raised_share_dem_split+unemployment_dem+crime_dem+education_dem+population_dem, data=data)
summary(regl)
library(car)
vif(regl)

#correlation matrix dem
corr_matrix_regl<- cor(data.frame(votes_share_dem_split, money_raised_share_dem_split, unemployment_dem, crime_dem, education_dem, population_dem))
print(corr_matrix_regl)

#backwards stepwise dem 
#remove crime
regl.1<-lm(votes_share_dem_split~money_raised_share_dem_split+unemployment_dem+education_dem+population_dem, data=data)
summary(regl.1)
#remove education
regl.2<-lm(votes_share_dem_split~money_raised_share_dem_split+unemployment_dem+population_dem, data=data)
summary(regl.2)
#remove population
regl.3<-lm(votes_share_dem_split~money_raised_share_dem_split+unemployment_dem, data=data)
summary(regl.3)
#remove unemployment 
regl.4<-lm(votes_share_dem_split~money_raised_share_dem_split, data=data)
summary(regl.4)

#FOR THE REPUBLICAN
#run mlr rep
regl2<-lm(votes_share_rep_split~money_raised_share_rep_split+unemployment_rep+crime_rep+education_rep+population_rep, data=data)
summary(regl2)
vif(regl2)
#correlation matrix rep
corr_matrix_regl2<- cor(data.frame(votes_share_rep_split,money_raised_share_rep_split, unemployment_rep, crime_rep, education_rep, population_rep))
print(corr_matrix_regl2)
#backwards stepwise rep
#remove education
regl2.1<-lm(votes_share_rep_split~money_raised_share_rep_split+unemployment_rep+crime_rep+population_rep, data=data)
summary(regl2.1)
#remove unemployment
regl2.2<-lm(votes_share_rep_split~money_raised_share_rep_split+crime_rep+population_rep, data=data)
summary(regl2.2)
#remove crime 
regl2.3<-lm(votes_share_rep_split~money_raised_share_rep_split+population_rep, data=data)
summary(regl2.3)

#notes: 
#we haven't transform the data 
#transform the data might increase R squared and make each variable mode significant
#so far, transform the data into log-lin make it better
#but not help that much 
regl4<-lm(log(votes_share_dem_split)~money_raised_share_dem_split+economy_dem+crime_dem+education_dem+population_dem, data=data)
summary(regl4)

#CASE 2: LINEAR REGRESSION 
#MONEY SPLITTED WHILE THE OTHER VARIABLES NOT SPLITTED BETWEEN DEM & REP 
#PRO: WAY BETTER R^2
#CONS: BUT THAT'S MIGHT BECAUSE MONEY OVERSHADOWED OTHER INDEPENDENT VARIABLES
#CONS: MONEY & POPULATION HAVE WAY LARGE AMOUNT IN TERMS OF NUMBER, VOTES ALSO HAVE LARGE AMOUNT OF NUMBER
#CONS: THIS LIKELY DOMINATE THE RESULTS COMPARE TO THE OTHER IND VARIABLES (RATES OF CRIME ETC) (?)

#FOR THE DEMOCRATIC
#run mlr dem
regl_new<-lm(votes_share_dem~money_raised_share_dem+economy_rate+crime+education+population, data=data)
summary(regl_new)
library(car)
vif(regl_new)
#high vif for crime, education, population

#FOR THE REPUBLICAN
#run mlr rep
regl_new<-lm(votes_share_rep~money_raised_share_rep+economy_rate+crime+education+population, data=data)
summary(regl_new)
library(car)
vif(regl_new)
#high vif for crime, education, population

#CASE 3: LOGISTIC REGRESSION
#FIND THE MOST SIGNIFICANT PREDICTOR IN WINNING 
#CONS: MONEY & POPULATION HAVE WAY LARGE AMOUNT IN TERMS OF NUMBER, VOTES ALSO HAVE LARGE AMOUNT OF NUMBER
#CONS: THIS LIKELY DOMINATE THE RESULTS COMPARE TO THE OTHER IND VARIABLES (RATES OF CRIME ETC) (?)

#DEMOCRATIC 
logit<-glm((win_rep)~money_raised_share_dem+(economy_rate)+(crime_rate)+(education_rate)+(population), data=data, family = "binomial")
summary(logit)
#none of them significant
#check the multicolinearity
library(car)
vif(logit)

#try to make the rate into per capita (but introduce multicollinearity) 
data$crime_per_capita <- data$crime_rate / data$population
data$education_per_capita <- data$education_rate / data$population
data$unemployment_per_capita <- data$economy_rate / data$population

#try to make the model
logit_rep<- glm((win_rep) ~ money_raised_share_rep, 
                              data = data, family = "binomial")
summary(logit_rep)
vif(logit_1)

#logit dem
logit_<- glm((win_rep) ~ money_raised_share_rep,
                data = data, family = "binomial")
summary(logit_rep)
#logit rep
logit_rep<- glm((win_rep) ~ money_raised_share_rep,
                data = data, family = "binomial")
summary(logit_rep)
#logit dem
logit_dem<- glm((win_dem) ~ money_raised_share_dem,
                data = data, family = "binomial")
summary(logit_dem)