# Regression analysis for e-bike project

# Notes on new dataset: 
# The column "EB" is 1 for eastbound, 0 for westbound. 
# Participant 31 is missing data and should be dropped (0 pings fell within geographic areas of interest). 

#Setup----
rm(list = ls()) # Clear work space

library(tidyverse)
library(lme4)
library(sjPlot)
library(psych)

workingdir <- "C:/Users/Clark.Calabrese/Documents/E-Bike_Analysis"
setwd(workingdir) 

#Dataset----
# df_cleaned is the output from the ebike stats ipynb
# ebike_data <- read.csv("data/df_cleaned.csv")
# df <- read.csv('data/df_cleaned_all_cols.csv')

#Updated dataset
# data <- read.csv('data/Pings_Tagged_w_Dir_2023-03-21.csv') #old data, commented out on 6/5/23 and data below added
# data <- read.csv('data/Pings_Tagged_Passing_2023-06-04.csv')
data <- read.csv('data/Pings_Tagged_w_Passing_2023-07-09.csv')
participants <- read.csv('data/participants.csv')

colnames(data) <- c('cts', 
                    'date',
                    'lat', 
                    'lon', 
                    'alt',
                    '2D_spd',
                    'spd',
                    'fix',
                    'prec',
                    'altsys',
                    'participant',
                    'ParticipantID',
                    'timestamp',
                    'geometry',
                    'Blind.Turn',
                    'Constrained.Tunnel',
                    'Narrow',
                    'Slow.Sign.EB',
                    'Slow.Sign.WB',
                    'Trail.Hazards',
                    'Trail.Junction',
                    'Vehicle.Conflict.Point',
                    'Walk.Bike.Sign',
                    'EB',
                    'Uphill',
                    'Downhill',
                    'Slow.Sign',
                    'Passing')

data <- data %>%
  select(-c(cts, alt, `2D_spd`, fix, prec, altsys, participant, timestamp, geometry))

colnames(participants) <- c('ParticipantID',
                            'age',
                            'gender',
                            'race',
                            'bike.type',
                            'class',
                            'exp')

df <- left_join(data, participants)

df <- df %>% filter(df$ParticipantID != 31)

df = df %>%
  mutate(bike.type = as.factor(bike.type))

df$Passing_Num <- df$Passing

df = df %>%
  mutate(Passing = as.factor(Passing))

df$spd.mph <- df$spd * 2.24

df$age.centered <- df$age - mean(df$age)

#Descriptives----

participants_summary <- participants %>%
  filter(ParticipantID != 31) %>%
  select(c(age, gender, bike.type))

table(participants_summary$gender)
table(participants_summary$bike.type)
describe(participants_summary$age)

df_summary <- df %>%
  select(-c(lat, lon, date, spd, ParticipantID, race, class, exp, age.centered, age, gender, bike.type))

summary <- describe(df_summary)

write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(summary)

#Models----

# simplest
m0 <- lm(spd.mph ~ bike.type, data = df)
summary(m0)
summary.aov(m0)

# Simple model, no mixed effects

m1 <- lm(spd.mph ~ bike.type + gender + age.centered, data = df)
summary(m1)
summary.aov(m1)

#type, gender, age all significant; effect of age small

# Add category (includes uphill/downhill)
m2 <- lm(spd.mph ~ bike.type + gender + age.centered + Uphill + Downhill, data = df)
summary(m2)
summary.aov(m2)

#uphill, downhill significant

m2a <- lm(spd.mph ~ bike.type + gender + age.centered + Uphill + Downhill + EB +
            Blind.Turn + Constrained.Tunnel + Narrow + Slow.Sign.EB + Slow.Sign.WB +
            Trail.Hazards + Trail.Junction + Vehicle.Conflict.Point + Walk.Bike.Sign, 
          data = df)
summary(m2a) 
summary.aov(m2a)

#all categories except constrained tunnel significant; effects of blind turn, 
#constrained tunnel, trail hazards, and trail junction less then 1/2 MPH

# Add interactions 
m3 <- lm(spd.mph ~ bike.type + gender + age.centered + 
           bike.type:gender + bike.type:age.centered + gender:age.centered +
           Uphill + Downhill + EB +
           Blind.Turn + Constrained.Tunnel + Narrow + Slow.Sign.EB + Slow.Sign.WB +
           Trail.Hazards + Trail.Junction + Vehicle.Conflict.Point + Walk.Bike.Sign, 
         data = df)
summary(m3)
summary.aov(m3)

#all two way interactions also significant

# # Add interactions 
# m3.1 <- lm(spd.mph ~ bike.type + gender + age + Category +
#            bike.type:gender + bike.type:age + gender:age +
#            bike.type:Category, data = df)
# summary(m3.1)
# summary.aov(m3.1)
# 
# # Add interactions 
# m3.2 <- lm(spd.mph ~ bike.type + gender + age + Category +
#            bike.type:gender + bike.type:age, data = df)
# summary(m3.2)
# summary.aov(m3.2)


# Model comparison----
AIC(m0, m1, m2, m2a, m3)


# Use mixed effect approach
# syntax: (slope | intercept)

m4 <- lmer(spd.mph ~ bike.type + gender + age.centered + (1|ParticipantID), data = df)

# m4 <- lmer(spd.mph ~ bike_num + gender_num + age + (1|Category), data = df)

# we don't want to do this because we care about the effect of bike num,
# but for demonstration we can show that we can have a different slope for bike_num
#m4 <- lmer(spd.mph ~ gender_num + age + (bike_num|Category), data = df)

# look at the effect sizes for the window categories
ranef(m4)
summary(m4)
tab_model(m4)

#age no longer significant after control for individual

#add categories
m5 <- lmer(spd.mph ~ bike.type + gender + age.centered + 
             Uphill + Downhill + EB +
             Blind.Turn + Constrained.Tunnel + Narrow + Slow.Sign.EB + Slow.Sign.WB +
             Trail.Hazards + Trail.Junction + Vehicle.Conflict.Point + Walk.Bike.Sign + 
             (1|ParticipantID), data = df)

ranef(m5)
summary(m5)
tab_model(m5)

m5a <- lmer(spd.mph ~ bike.type + gender + age.centered + 
             Uphill + Downhill + EB +
             Blind.Turn + Constrained.Tunnel + Narrow + Slow.Sign +
             Trail.Hazards + Trail.Junction + Vehicle.Conflict.Point + Walk.Bike.Sign + 
             (1|ParticipantID), data = df)

ranef(m5a)
summary(m5a)
tab_model(m5a)


#add interactions
m6 <- lmer(spd.mph ~ bike.type + gender + age.centered + 
             bike.type:gender + bike.type:age.centered + gender:age.centered +
             Uphill + Downhill + EB +
             Blind.Turn + Constrained.Tunnel + Narrow + Slow.Sign.EB + Slow.Sign.WB +
             Trail.Hazards + Trail.Junction + Vehicle.Conflict.Point + Walk.Bike.Sign + 
             (1|ParticipantID), data = df)

ranef(m6)
summary(m6)
tab_model(m6)

#none of the interactions are significant; might be too few cases after controlling for individual?

#Final Model----
#After discussion with Jonah, removed effects for roadway conditions that don't seem meaningful to model; 
#trail hazards that only show up at a single point on the ride
#This is the model we are using for the presentation
m7 <- lmer(spd.mph ~ bike.type + gender + age.centered + 
             Uphill + Downhill + 
             Blind.Turn + Narrow + Trail.Hazards + Trail.Junction + Vehicle.Conflict.Point + Walk.Bike.Sign + 
             (1|ParticipantID), data = df)

ranef(m7)
summary(m7)
tab_model(m7)



# Model comparison
AIC(m0, m1, m2, m2a, m3, m4, m5, m5a, m6, m7)

#Adding in passing

m8 <- lmer(spd.mph ~ bike.type + gender + age.centered + 
             Uphill + Downhill + 
             Blind.Turn + Narrow + Trail.Hazards + Trail.Junction + Vehicle.Conflict.Point + Walk.Bike.Sign + Passing + 
             (1|ParticipantID), data = df)

ranef(m8)
summary(m8)
tab_model(m8)

AIC(m0, m1, m2, m2a, m3, m4, m5, m5a, m6, m7, m8)
AIC(m7, m8)

#Ebike x passing interaction

m9 <- lmer(spd.mph ~ bike.type + gender + age.centered + 
             Uphill + Downhill + 
             Blind.Turn + Narrow + Trail.Hazards + Trail.Junction + Vehicle.Conflict.Point + Walk.Bike.Sign + Passing + 
             bike.type*Passing +
             (1|ParticipantID), data = df)

ranef(m9)
summary(m9)
tab_model(m9)

AIC(m0, m1, m2, m2a, m3, m4, m5, m5a, m6, m7, m8, m9)
#Model with bike type x passing interaction was best fit, improved on final model without passing

