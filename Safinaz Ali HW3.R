#Safinaz Ali
#B2000 Homework 3 
# 09/15/22

#TEAM: Liam O'Neill & Victoria Karadimas


load("~/Desktop/Statistic & Econmetric/Data/Household_Pulse_data.RData")
View(Household_Pulse_data)
library(descr)
library(plotrix)
library("ggplot2")
library("dplyr")
library(margins)

#basic data 
xtabs(~RECVDVACC + ANYWORK)
xtabs(~RECVDVACC + works_onsite)
xtabs(~RECVDVACC + works_remote)

#Created a subset to narrow down the search to analyze the data between the people who were vaccinated and not vaccinated and their work status
restrict1 <-(Household_Pulse_data$RECVDVACC == "yes got vaxx") & (Household_Pulse_data$ANYWORK == "yes employment in last 7 days") & (Household_Pulse_data$Works_onsite == "worked onsite")
data_new1 <- subset(Household_Pulse_data,restrict1) 
#I found the standard error to get the outcome difference  using a t-test to get the confidence interval to prove the difference is actually there. I used the standard deviation and sample size from the restriction to run it
stderror <- function(x) sd(x)/sqrt(length(x))
sd(summary(data_new1$RECVDVACC))
t.test(summary(data_new1$RECVDVACC),var.equal = TRUE)
#with this summary i got the difference from the subset 
summary(data_new1$RECVDVACC) #SUMMARY: 37%
#This prop table was able to find the marginal probabilities after i was able to add the cross table for the data to see the probabilities of vaccine status irrespective of the outcome their employment
prop.table(summary(data_new1$RECVDVACC), margin = NULL)

# I repeated the subset multiple times to see different correlation between the data 

restrict2 <-(Household_Pulse_data$RECVDVACC == "yes got vaxx") & (Household_Pulse_data$ANYWORK == "yes employment in last 7 days") & (Household_Pulse_data$works_remote == "worked remotely")
data_new2 <- subset(Household_Pulse_data,restrict2) 
t.test(summary(data_new2$RECVDVACC),var.equal = TRUE)
sd(summary(data_new2$RECVDVACC))
summary(data_new2$RECVDVACC) #SUMMARY:26%
prop.table(summary(data_new2$RECVDVACC), margin = NULL)
        
restrict3 <-(Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$ANYWORK == "yes employment in last 7 days") & (Household_Pulse_data$Works_onsite == "worked onsite")
data_new3 <- subset(Household_Pulse_data,restrict3) 
sd(summary(data_new3$RECVDVACC))
t.test(summary(data_new3$RECVDVACC),var.equal = TRUE)
summary(data_new3$RECVDVACC) #SUMMARY 5%
prop.table(summary(data_new3$works_remote), margin = NULL)

restrict4 <-(Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$ANYWORK == "yes employment in last 7 days") & (Household_Pulse_data$works_remote == "worked remotely")
data_new4 <- subset(Household_Pulse_data,restrict4) 
sd(summary(data_new4$RECVDVACC))
t.test(summary(data_new4$RECVDVACC),var.equal = TRUE)
summary(data_new4$RECVDVACC) #SUMMARY 2%
prop.table(summary(data_new4$ANYWORK), margin = NULL)

restrict5 <-(Household_Pulse_data$RECVDVACC == "yes got vaxx") & (Household_Pulse_data$ANYWORK == "no employment in last 7 days") 
data_new5 <- subset(Household_Pulse_data,restrict5) 
sd(summary(data_new5$RECVDVACC))
t.test(summary(data_new5$RECVDVACC),var.equal = TRUE)
summary(data_new5$RECVDVACC) #SUMMARY 36%
prop.table(summary(data_new5$ANYWORK), margin = NULL)

restrict6 <-(Household_Pulse_data$RECVDVACC == "no did not get vaxx") & (Household_Pulse_data$ANYWORK == "no employment in last 7 days") 
data_new6 <- subset(Household_Pulse_data,restrict6) 
sd(summary(data_new6$RECVDVACC))
t.test(summary(data_new6$RECVDVACC),var.equal = TRUE)
summary(data_new6$RECVDVACC) #summary 4%
prop.table(summary(data_new6$Works_onsite), margin = NULL)

#Based on the data above i was able to figure out the standard error of the difference between the people that were getting vaccinated or not and working remotely or onsite
#you can tell that there is a huge difference based on people who got vaccinated or not for their job because the people that got vaccinated and work on site was 37% and worked remote was only 26%. however the 
#unvaccinated people that worked remote was only 5% of the data and the ones who worked in person was only 2%. 
#The surprising realization was that there was more vaccinated people that were unemployed (36%) then people who were unvaccinated and unemployed(4%) Vaccination status did not play a factor in people being employed 
# but instead played a factor on if they were onsite or remote for their job. I am mostly confident in the data because there is proof of peoples vaccination status and employment thats guaranteed. The part that is not as accurate is that there 
#can be more then one factor for them choosing to work remotely or onsite, in this case i assumed it was because of vacciantion status because of the strict laws at peoples job or in nyc. However it can also be because of 
#having to take care of family/kids or commute is difficult during the time.. 

RECVDVACC = sample(c("yes got vax", "no did not get vaxx"), 69114, replace = TRUE)
ANYWORK = sample(c("yes employment in last 7 days", "no employment in last 7 days"),  69114, replace = TRUE)
x = data.frame(RECVDVACC, ANYWORK)
CrossTable(RECVDVACC,ANYWORK)

RECVDVACC = sample(c("yes got vax", "no did not get vaxx"), 69114, replace = TRUE)
works_onsite = sample(c("worked onsite", "no"),  69114, replace = TRUE)
works_remote = sample(c("worked remotely", "no"),  69114, replace = TRUE)
x = data.frame(RECVDVACC, works_onsite, works_remote)
CrossTable(RECVDVACC,works_onsite)
CrossTable(RECVDVACC,works_remote)

#For the cross table i was able to run different samples to get to my realization that there is more people that are vaccinated who work remotely(17361) then onsite(17266) according to the data.
#overall the cross table and the marginal probabilities showed that it was not exclusive because the people that where unvaccinated still were employed and a small percentage worked onsite.
#so the crosstable is exhausted as i have ran all subsets to get all the possibilities to see if vaccination status affected people employment. 

#Some additional factors i will like to look at will be the sectors of their job. Because i know during peak of covid a lot of first responders like teachers, cops, doctors, etc
#who still had their jobs in person which could be something interesting to look at to narrow down the sector of jobs that were remote or onsite.
#Also knowing why there job is remote or onsite will be interesting because it can help us see peoples reasoning on why they got vaccinated in which 
#it can help us conclude more conclusion and understand why more vaccinated people were unemployed and worked remote. 
  
