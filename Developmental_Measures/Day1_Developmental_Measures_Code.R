##############################################################################
##       Plotting TIME variables and Controlling for Brain Size             ##
##       By Megan Herting                                                   ##
##############################################################################

##Clear Environment
rm(list=ls())

##Turn off scientific notation
options(scipen=999)

## This code will let you visualize and fit basic models with age vs. tanner stage
##First, run the MDC_Workshop_Simulated_Data_Tanner script and has "MDC_Workshop_Simulated_data_tanner.csv" into your working path/directory
SIM_DATA_long=read.csv("/Users/megan/Dropbox/FLUX/Workshop/workshop-master/Developmental_Measures/MDC_Workshop_Simulated_data_tanner_sexdiff.csv")

## Load required packages 
packages <- c("dplyr", "ggplot2", "nlme")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, library, character.only = TRUE)

##Part 1.1: Plot cortex variable by 3 "time" measurements

##Exploratory data plots to examine correlations between time variables
# Need to check that time measures are correlated but not too much!
library(GGally)
timevars=ggpairs(select(SIM_DATA_long,period,Age,Tanner))
timevars

##Plot Time & Age
Time_age=ggplot(data=SIM_DATA_long, aes(x=period, y=Age, group=ID))+geom_point(aes(shape=period, color=Sex))+geom_line(aes(group=ID, color=Sex))+xlab("Wave")+ylab("Age")
Time_age

##Plot Tanner & Age
Tanner_age=ggplot(data=SIM_DATA_long, aes(x=Age, y=Tanner, group=ID))+geom_point(aes(shape=period, color=Sex))+geom_line(aes(group=ID, color=Sex))+geom_smooth(method = "loess", aes(group=Sex, color=Sex))+xlab("Age")+ylab("Tanner")
Tanner_age


##Create Time/Wave Plot
GM_time=ggplot(data=SIM_DATA_long, aes(x=period, y=T1.cortex, group=ID))+geom_point(aes(shape=period, color=Sex))+geom_line(aes(group=ID, color=Sex))+xlab("Time Point")+ylab("Cortex Volume")+geom_smooth(method = "loess", aes(group=Sex))

#Print Time Plot
GM_time

#Save Time Plot
ggsave(filename="/Users/megan/Dropbox/FLUX/Workshop/workshop-master/Developmental_Measures/TimebyParticipant.jpg",
       plot=GM_time,
       width=5,
       height=7)


##Create Age Plot
GM_age=ggplot(data=SIM_DATA_long, aes(x=Age, y=T1.cortex, group=ID))+geom_point(aes(shape=period, color=Sex))+geom_line(aes(group=ID, color=Sex))+xlab("Age")+ylab("Cortex Volume")+geom_smooth(method = "loess", aes(group=Sex))

#Print Age Plot
GM_age

#Save Age Plot
ggsave(filename="/Users/megan/Dropbox/FLUX/Workshop/workshop-master/Developmental_Measures/AgebyParticipant.jpg",
       plot=GM_age,
       width=5,
       height=7)

##Create Tanner Plot
GM_tanner=ggplot(data=SIM_DATA_long, aes(x=Tanner, y=T1.cortex, group=ID))+geom_point(aes(shape=period, color=Sex))+geom_line(aes(group=ID, color=Sex))+xlab("Tanner Stage")+ylab("Cortex Volume")+geom_smooth(method = "loess", aes(group=Sex))

#Print Tanner Plot
GM_tanner

#Save Tanner Plot
ggsave(filename="/Users/megan/Dropbox/FLUX/Workshop/workshop-master/Developmental_Measures/TannerbyParticipant.jpg",
       plot=GM_tanner,
       width=5,
       height=7)

##Part 1.2: LME models of cortex by 3 "time" measurements
##Run linear mixed effect model to see how cortex volume changes over time 
GM_time_lme=lme(T1.cortex~time+Sex, random = ~ 1 | ID, data=SIM_DATA_long, method='ML')
summary(GM_time_lme)


##Run linear mixed effect model to see how cortex volume changes over time as a function of age
GM_age_lme=lme(T1.cortex~Age+Sex, random = ~ 1 | ID, data=SIM_DATA_long, method='ML')
summary(GM_age_lme)

##Run linear mixed effect model to see how cortex volume changes over time as a function of puberty
GM_tanner_lme=lme(T1.cortex~Tanner+Sex, random = ~ 1 | ID, data=SIM_DATA_long, method='ML')
summary(GM_tanner_lme)

##Put both Tanner and Age in the model
GM_tanner_age_lme=lme(T1.cortex~Age+Tanner+Sex, random = ~ 1 | ID, data=SIM_DATA_long, method='ML')
summary(GM_tanner_age_lme)

##Compare with and without controlling for the other "developmental" variable
anova(GM_age_lme, GM_tanner_age_lme)
anova(GM_tanner_lme, GM_tanner_age_lme)


##Part 2: Exploring a covariate for head size -- WBV

##Create WBV with age
WBV_age=ggplot(data=SIM_DATA_long, aes(x=Age, y=T1.wbv, group=ID))+geom_point(aes(shape=period, color=Sex))+geom_line(aes(group=ID, color=Sex))+xlab("Age")+ylab("Whole Brain Volume")+geom_smooth(method = "loess", aes(group=Sex))

#Print WBV with age Plot
WBV_age

#Save WBV with age Plot
ggsave(filename="/Users/megan/Dropbox/FLUX/Workshop/workshop-master/Developmental_Measures/WBVbyAgebyParticipant.jpg",
       plot=WBV_age,
       width=5,
       height=7)


##Create relationship between WBV and GM cortex volume
GM_WBV=ggplot(data=SIM_DATA_long, aes(x=T1.cortex, y=T1.wbv, group=ID))+geom_point(aes(shape=period, color=Sex))+geom_line(aes(group=ID, color=Sex))+xlab("Cortex Volume")+ylab("Whole Brain Volume")+geom_smooth(method = "loess", aes(group=Sex))

#Print WBV and GM Plot
GM_WBV

#Save WBV with age Plot
ggsave(filename="/Users/megan/Dropbox/FLUX/Workshop/workshop-master/Developmental_Measures/GM_WBV_byParticipant.jpg",
       plot=GM_WBV,
       width=5,
       height=7)



## Determine how cortex volume and wbv relate over time
GM_WBV=lme(T1.cortex~T1.wbv*Sex, random = ~ 1 | ID, data=SIM_DATA_long, method='ML')
summary(GM_WBV)


##Run linear mixed effect model to see how cortex volume changes over time as a function of age with and without control volumes
GM_age_lme=lme(T1.cortex~Age*Sex, random = ~ 1 | ID, data=SIM_DATA_long, method='ML')
summary(GM_age_lme)

WBV_age_lme=lme(T1.wbv~Age*Sex, random = ~ 1 | ID, data=SIM_DATA_long, method='ML')
summary(WBV_age_lme)

GM_age_wbv_lme=lme(T1.cortex~Age*Sex+T1.wbv, random = ~ 1 | ID, data=SIM_DATA_long, method='ML')
summary(GM_age_wbv_lme)

##Compare models
anova(GM_age_lme,GM_age_wbv_lme)