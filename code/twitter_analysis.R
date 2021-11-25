#######################################################################
#Name of code file: twitter_analysis.R

#Data In: 
#twitter_data

#Data Out: 
#Plots, Regression Tables, and Descriptive Statistics from Twitter Experiment 
#Figures 2-5, A1-A2 
#Tables A2-A21
######################################################################

#Load Packages
library(readr)
library(ggplot2)
library(coefplot)
library(dplyr)
library(psych)
library(MASS)
library(xtable)
library(stargazer)

#Set Working Directory
setwd("..")

#Read in Data
data<-read_csv("data/twitter_data.csv")

#Prepare data for analysis 

#treatment variable
data$treatment_num<-as.factor(data$treatment_num)
#date variable
data$treatment_date<-as.Date(data$treatment_date, format="%m/%d/%y")
data$treatment_date2<-as.character(data$treatment_date)
#median or fewer anti-Shia friends
data_anti_shia_net_low<-subset(data, data$anti_shia_friends_count<=38)
data_anti_shia_net_high<-subset(data, data$anti_shia_friends_count>38)
#subset data by follower counts
data_1000_fol<-subset(data, data$followers_count<=1000)
data_100_fol<-subset(data, data$followers_count<=100)
data_150_fol<-subset(data, data$followers_count<=150)
data_200_fol<-subset(data, data$followers_count<=200)
data_median_fol<-subset(data, data$followers_count<=245)
data_300_fol<-subset(data, data$followers_count<=300)
data_350_fol<-subset(data, data$followers_count<=350)
data_400_fol<-subset(data, data$followers_count<=400)
data_450_fol<-subset(data, data$followers_count<=450)
data_500_fol<-subset(data, data$followers_count<=500)
#suspended accounts
data$suspended<-ifelse(is.na(data$month_post), 1,0)
#proportion_variables
data$prop_pre_tpd<-data$tpd_anti_shia_pre/data$tpd_pre
data$prop_post_tpd<-data$tpd_anti_shia_post/data$tpd_post
data$prop_pre_week<-data$week_anti_shia_pre/data$week_pre
data$prop_post_week<-data$week_anti_shia_post/data$week_post
data$prop_pre_two_weeks<-data$two_weeks_anti_shia_pre/data$two_weeks_pre
data$prop_post_two_weeks<-data$two_weeks_anti_shia_post/data$two_weeks_post
data$prop_pre_month<-data$month_anti_shia_pre/data$month_pre
data$prop_post_month<-data$month_anti_shia_post/data$month_post
data[data==Inf]<-NA

##########
#Figure 2#
##########

#Difference in Means (All Data)
month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data)
summary(day)

pdf("figures/figure_2.pdf", height=7, width=11)
multiplot(day, week, two_weeks, month, coefficients=c("treatment_num1", "treatment_num2", "treatment_num3", "treatment_num4", "treatment_num5"), 
          newNames=c(treatment_num1="Arab ID ", treatment_num2="Religious ID ", treatment_num3="Arab ID (Elite)", 
                     treatment_num4="Religious ID (Elite)", treatment_num5=" No ID "), 
          names=c("     Day", "   Week", "  Two Weeks ", "Month    "), title="", scales="free_x",
          sort="alphabetical", innerCI=1.645, outerCI=1.96, single=FALSE,  zeroType = 0,legend.position="none") +
  scale_color_manual(values=c("red", "blue", "seagreen", "black")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.line = element_line(colour = "black"), text = element_text(size=15))+
  ylab("Treatments") + xlab("Difference in Anti-Shia Tweet Count")+ geom_vline(aes(xintercept = 0), size = .5, linetype = "dashed")
dev.off()  


##########
#Figure 3#
##########

#Followers Hist:
ticks<-seq(from=0, to=10000, by=500)
pdf("figures/figure_3.pdf", height=7, width=11)
hist(data$followers_count, breaks=ticks, xaxt="n", xlab="Number of Followers", main="Distribution of Follower Counts", freq=TRUE, col="magenta")
(axis(1, at=ticks))
dev.off()

##########
#Figure 4#
##########

#Difference in Means (Median or fewer followers)
month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data_median_fol)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data_median_fol)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data_median_fol)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data_median_fol)
summary(day)

pdf("figures/figure_4.pdf", height=7, width=11)
multiplot(day, week, two_weeks, month, coefficients=c("treatment_num1", "treatment_num2", "treatment_num3", "treatment_num4", "treatment_num5"), 
          newNames=c(treatment_num1="Arab ID ", treatment_num2="Religious ID ", treatment_num3="Arab ID (Elite)", 
                     treatment_num4="Religious ID (Elite)", treatment_num5=" No ID "), 
          names=c("     Day", "   Week", "  Two Weeks ", "Month    "), title="", scales="free_x",
          sort="alphabetical", innerCI=1.645, outerCI=1.96, single=FALSE,  zeroType = 0,legend.position="none") +
  scale_color_manual(values=c("red", "blue", "seagreen", "black")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.line = element_line(colour = "black"), text = element_text(size=15))+
  ylab("Treatments") + xlab("Difference in Anti-Shia Tweet Count")+ geom_vline(aes(xintercept = 0), size = .5, linetype = "dashed")
dev.off()  

###########
#Figure 5a#
###########

#Low Anti-Shia Friend Network 
month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_low)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_low)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_low)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_low)
summary(day)


pdf("figures/figure_5a.pdf", height=7, width=11)
multiplot(day, week, two_weeks, month, coefficients=c("treatment_num1", "treatment_num2", "treatment_num3", "treatment_num4", "treatment_num5"), 
          newNames=c(treatment_num1="Arab ID ", treatment_num2="Religious ID ", treatment_num3="Arab ID (Elite)", 
                     treatment_num4="Religious ID (Elite)", treatment_num5=" No ID "), 
          names=c("     Day", "   Week", "  Two Weeks ", "Month    "), title="", scales="free_x",
          sort="alphabetical", innerCI=1.645, outerCI=1.96, single=FALSE,  zeroType = 0,legend.position="none") +
  scale_color_manual(values=c("red", "blue", "seagreen", "black")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.line = element_line(colour = "black"), text = element_text(size=15))+
  ylab("Treatments") + xlab("Difference in Anti-Shia Tweet Count")+ geom_vline(aes(xintercept = 0), size = .5, linetype = "dashed")
dev.off() 

###########
#Figure 5b#
###########

#High Anti-Shia Friend Network 
month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_high)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_high)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_high)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_high)
summary(day)


pdf("figures/figure_5b.pdf", height=7, width=11)
multiplot(day, week, two_weeks, month, coefficients=c("treatment_num1", "treatment_num2", "treatment_num3", "treatment_num4", "treatment_num5"), 
          newNames=c(treatment_num1="Arab ID ", treatment_num2="Religious ID ", treatment_num3="Arab ID (Elite)", 
                     treatment_num4="Religious ID (Elite)", treatment_num5=" No ID "), 
          names=c("     Day", "   Week", "  Two Weeks ", "Month    "), title="", scales="free_x",
          sort="alphabetical", innerCI=1.645, outerCI=1.96, single=FALSE,  zeroType = 0,legend.position="none") +
  scale_color_manual(values=c("red", "blue", "seagreen", "black")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.line = element_line(colour = "black"), text = element_text(size=15))+
  ylab("Treatments") + xlab("Difference in Anti-Shia Tweet Count")+ geom_vline(aes(xintercept = 0), size = .5, linetype = "dashed")
dev.off() 


###########
#Figure A1#
###########

#Difference in Means GGC Countries Only 
month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=filter(data, gcc==TRUE))
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=filter(data, gcc==TRUE))
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=filter(data, gcc==TRUE))
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=filter(data, gcc==TRUE))
summary(day)

pdf("figures/figure_A1.pdf", height=7, width=11)
multiplot(day, week, two_weeks, month, coefficients=c("treatment_num1", "treatment_num2", "treatment_num3", "treatment_num4", "treatment_num5"), 
          newNames=c(treatment_num1="Arab ID ", treatment_num2="Religious ID ", treatment_num3="Arab ID (Elite)", 
                     treatment_num4="Religious ID (Elite)", treatment_num5=" No ID "), 
          names=c("     Day", "   Week", "  Two Weeks ", "Month    "), title="", scales="free_x",
          sort="alphabetical", innerCI=1.645, outerCI=1.96, single=FALSE,  zeroType = 0,legend.position="none") +
  scale_color_manual(values=c("red", "blue", "seagreen", "black")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.line = element_line(colour = "black"), text = element_text(size=15))+
  ylab("Treatments") + xlab("Difference in Anti-Shia Tweet Count")+ geom_vline(aes(xintercept = 0), size = .5, linetype = "dashed")
dev.off()  

###########
#Figure A2#
###########

#Difference in Means Conflict Zones Only (Yemen, Iraq, Syria)

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=filter(data, conflict==TRUE))
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=filter(data, conflict==TRUE))
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=filter(data, conflict==TRUE))
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=filter(data, conflict==TRUE))
summary(day)

pdf("figures/figure_A2.pdf", height=7, width=11)
multiplot(day, week, two_weeks, month, coefficients=c("treatment_num1", "treatment_num2", "treatment_num3", "treatment_num4", "treatment_num5"), 
          newNames=c(treatment_num1="Arab ID ", treatment_num2="Religious ID ", treatment_num3="Arab ID (Elite)", 
                     treatment_num4="Religious ID (Elite)", treatment_num5=" No ID "), 
          names=c("     Day", "   Week", "  Two Weeks ", "Month    "), title="", scales="free_x",
          sort="alphabetical", innerCI=1.645, outerCI=1.96, single=FALSE,  zeroType = 0,legend.position="none") +
  scale_color_manual(values=c("red", "blue", "seagreen", "black")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.line = element_line(colour = "black"), text = element_text(size=15))+
  ylab("Treatments") + xlab("Difference in Anti-Shia Tweet Count")+ geom_vline(aes(xintercept = 0), size = .5, linetype = "dashed")
dev.off()  


##########
#Table A2#
##########

#Descriptive Statistics 

vars<-c("month_anti_shia_pre","month_anti_shia_post", "month_pre", "month_post", "two_weeks_anti_shia_pre", "two_weeks_anti_shia_post", "two_weeks_pre", "two_weeks_post", "week_anti_shia_pre", "week_anti_shia_post", "week_pre", "week_post", 
        "tpd_anti_shia_pre", "tpd_anti_shia_post", "tpd_pre", "tpd_post", "followers_count")
des_stat<-data[vars]
des_stat$month_agg_anti_shia_pre<-des_stat$month_anti_shia_pre +des_stat$two_weeks_anti_shia_pre+ des_stat$week_anti_shia_pre + des_stat$tpd_anti_shia_pre
des_stat$month_agg_anti_shia_post<-des_stat$month_anti_shia_post +des_stat$two_weeks_anti_shia_post+ des_stat$week_anti_shia_post + des_stat$tpd_anti_shia_post
des_stat$month_agg_pre<-des_stat$month_pre +des_stat$two_weeks_pre+ des_stat$week_pre + des_stat$tpd_pre
des_stat$month_agg_post<-des_stat$month_post +des_stat$two_weeks_post+ des_stat$week_post + des_stat$tpd_post
des_stat<-as.data.frame(describe(des_stat))
des_stat<-des_stat[c("n", "mean", "median", "sd", "min", "max")]
rownames(des_stat)<-gsub("month_agg_anti_shia_pre", "Anti-Shia Pre-Treatment Tweet Count (Days 1-30)", rownames(des_stat))
rownames(des_stat)<-gsub("month_agg_anti_shia_post", "Anti-Shia Post-Treatment Tweet Count (Days 1-30)", rownames(des_stat))
rownames(des_stat)<-gsub("month_agg_pre", "Pre-Treatment Total Tweet Count (Days 1-30)", rownames(des_stat))
rownames(des_stat)<-gsub("month_agg_post", "Post-Treatment Total Tweet Count (Days 1-30)", rownames(des_stat))
rownames(des_stat)<-gsub("month_anti_shia_pre", "Anti-Shia Pre-Treatment Tweet Count (Days 15-30)", rownames(des_stat))
rownames(des_stat)<-gsub("month_anti_shia_post", "Anti-Shia Post-Treatment Tweet Count (Days 15-30)", rownames(des_stat))
rownames(des_stat)<-gsub("month_pre", "Pre-Treatment Total Tweet Count (Days 15-30)", rownames(des_stat))
rownames(des_stat)<-gsub("month_post", "Post-Treatment Total Tweet Count (Days 15-30)", rownames(des_stat))
rownames(des_stat)<-gsub("two_weeks_anti_shia_pre", "Anti-Shia Pre-Treatment Tweet Count (Days 8-14)", rownames(des_stat))
rownames(des_stat)<-gsub("two_weeks_anti_shia_post", "Anti-Shia Post-Treatment Tweet Count (Days 8-14)", rownames(des_stat))
rownames(des_stat)<-gsub("two_weeks_pre", "Pre-Treatment Total Tweet Count (Days 8-14)", rownames(des_stat))
rownames(des_stat)<-gsub("two_weeks_post", "Post-Treatment Total Tweet Count (Days 8-14)", rownames(des_stat))
rownames(des_stat)<-gsub("week_anti_shia_pre", "Anti-Shia Pre-Treatment Tweet Count (Days 2-7)", rownames(des_stat))
rownames(des_stat)<-gsub("week_anti_shia_post", "Anti-Shia Post-Treatment Tweet Count (Days 2-7)", rownames(des_stat))
rownames(des_stat)<-gsub("week_pre", "Pre-Treatment Total Tweet Count (Days 2-7)", rownames(des_stat))
rownames(des_stat)<-gsub("week_post", "Post-Treatment Total Tweet Count (Days 2-7)", rownames(des_stat))
rownames(des_stat)<-gsub("tpd_anti_shia_pre", "Anti-Shia Pre-Treatment Tweet Count (Day 1)", rownames(des_stat))
rownames(des_stat)<-gsub("tpd_anti_shia_post", "Anti-Shia Post-Treatment Tweet Count (Day1)", rownames(des_stat))
rownames(des_stat)<-gsub("tpd_pre", "Pre-Treatment Total Tweet Count (Day 1)", rownames(des_stat))
rownames(des_stat)<-gsub("tpd_post", "Post-Treatment Total Tweet Count (Day 1)", rownames(des_stat))
rownames(des_stat)<-gsub("followers_count", "Followers Count", rownames(des_stat))
xtable(des_stat)



##########
#Table A3#
##########

#Difference in Means (All Data)
month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

##########
#Table A4#
##########

#Location Metadata Table
loc_table<-as.data.frame(table(data$loc_qual))
loc_table$Country<-loc_table$Var1
loc_table<-loc_table[c("Country", "Freq")]
loc_table<-loc_table[order(-loc_table$Freq),] 
latex_table<-xtable(loc_table, row.names=F)
print(latex_table)

##########
#Table A5#
##########

#Difference in Means GGC Countries Only 
month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=filter(data, gcc==TRUE))
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=filter(data, gcc==TRUE))
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=filter(data, gcc==TRUE))
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=filter(data, gcc==TRUE))
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

##########
#Table A6#
##########

#Difference in Means Conflict Zones (Yemen, Iraq, Syria)

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=filter(data, conflict==TRUE))
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=filter(data, conflict==TRUE))
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=filter(data, conflict==TRUE))
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=filter(data, conflict==TRUE))
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

##########
#Table A7#
##########

#Run Models without suspended accounts to address attrition 

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=filter(data, suspended==0))
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=filter(data, suspended==0))
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=filter(data, suspended==0))
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=filter(data, suspended==0))
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

##########
#Table A8#
##########

#Run Models with suspended accounts included up untl suspension, zeros afterwards 

data2<-data[c("month_anti_shia_post", "month_anti_shia_pre", "two_weeks_anti_shia_post", "two_weeks_anti_shia_pre", "week_anti_shia_post", "week_anti_shia_pre", "tpd_anti_shia_post", "tpd_anti_shia_pre", "treatment_num")]
data2[is.na(data2)] <- 0

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data2)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data2)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data2)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data2)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)


##########
#Table A9#
##########

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data_median_fol)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data_median_fol)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data_median_fol)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data_median_fol)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

###########
#Table A10#
###########

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data_200_fol)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data_200_fol)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data_200_fol)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data_200_fol)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

###########
#Table A11#
###########

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data_300_fol)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data_300_fol)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data_300_fol)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data_300_fol)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

###########
#Table A12#
###########

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data_350_fol)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data_350_fol)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data_350_fol)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data_350_fol)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

###########
#Table A13#
###########

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data_400_fol)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data_400_fol)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data_400_fol)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data_400_fol)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

###########
#Table A14#
###########

#Low Anti-Shia Friend Network 
month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_low)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_low)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_low)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_low)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)


###########
#Table A15#
###########

#High Anti Shia Friend Network 
month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_high)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_high)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_high)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num, data=data_anti_shia_net_high)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

###########
#Table A16#
###########

#Negative Binomial Models (all data)
day<-glm.nb(tpd_anti_shia_post ~ treatment_num +tpd_anti_shia_pre, data = data, control=glm.control(maxit=50))
summary(day)
week<-glm.nb(week_anti_shia_post ~ treatment_num + week_anti_shia_pre, data = data, control=glm.control(maxit=50))
summary(week)
two_weeks<-glm.nb(two_weeks_anti_shia_post ~ treatment_num + two_weeks_anti_shia_pre, data = data, control=glm.control(maxit=50))
summary(two_weeks)
month<-glm.nb(month_anti_shia_post ~ treatment_num +month_anti_shia_pre, data = data, control=glm.control(maxit=50))
summary(month)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", 
                                                                                  "Arab ID (Elite)","Religious ID (Elite)","No ID",
                                                                                  "Anti-Shia Pre-Treatment Tweet Count (Day)","Anti-Shia Pre-Treatment Tweet Count (Week)",
                                                                                  "Anti-Shia Pre-Treatment Tweet Count (Two Weeks)", "Anti-Shia Pre-Treatment Tweet Count (Month)",
                                                                                  "Anti-Shia Pre-Treatment Tweet Count (Two Months)"))




###########
#Table A17#
###########

#Run models with proportion rather than count of tweets

month<-lm(prop_post_month-prop_pre_month ~ treatment_num, data=data)
summary(month)
two_weeks<-lm(prop_post_two_weeks-prop_pre_two_weeks ~ treatment_num, data=data)
summary(two_weeks)
week<-lm(prop_post_week-prop_pre_week ~ treatment_num, data=data)
summary(week)
day<-lm(prop_post_tpd-prop_pre_tpd ~ treatment_num, data=data)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

###########
#Table A18#
###########

#Run models with date fixed effect

month<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num + treatment_date2, data=data)
summary(month)
two_weeks<-lm(two_weeks_anti_shia_post-two_weeks_anti_shia_pre ~ treatment_num +treatment_date2, data=data)
summary(two_weeks)
week<-lm(week_anti_shia_post-week_anti_shia_pre ~ treatment_num +treatment_date2, data=data)
summary(week)
day<-lm(tpd_anti_shia_post-tpd_anti_shia_pre ~ treatment_num + treatment_date2, data=data)
summary(day)

stargazer(day, week, two_weeks, month, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", "Arab ID (Elite)","Religious ID (Elite)","No ID"), star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"))

###############
#Tables A19-21#
###############

#Each subset leaves out one treatment date 
date1<-subset(data, treatment_date!="2018-01-31")
date2<-subset(data, treatment_date!="2018-02-01")
date3<-subset(data, treatment_date!="2018-02-02")
date4<-subset(data, treatment_date!="2018-02-03")
date5<-subset(data, treatment_date!="2018-02-04")
date6<-subset(data, treatment_date!="2018-02-05")
date7<-subset(data, treatment_date!="2018-02-06")
date8<-subset(data, treatment_date!="2018-02-07")
date9<-subset(data, treatment_date!="2018-02-08")
date10<-subset(data, treatment_date!="2018-02-09")
date11<-subset(data, treatment_date!="2018-02-10")
date12<-subset(data, treatment_date!="2018-02-11")
date13<-subset(data, treatment_date!="2018-02-13")
date14<-subset(data, treatment_date!="2018-02-14")
date15<-subset(data, treatment_date!="2018-02-15")
date16<-subset(data, treatment_date!="2018-02-16")
date17<-subset(data, treatment_date!="2018-02-17")
date18<-subset(data, treatment_date!="2018-02-18")
date19<-subset(data, treatment_date!="2018-02-19")
month1<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date1)
summary(month1)
month2<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date2)
summary(month2)
month3<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date3)
summary(month3)
month4<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date4)
summary(month4)
month5<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date5)
summary(month5)
month6<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date6)
summary(month6)
month7<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date7)
summary(month7)
month8<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date8)
summary(month8)
month9<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date9)
summary(month9)
month10<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date10)
summary(month10)
month11<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date11)
summary(month11)
month12<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date12)
summary(month12)
month13<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date13)
summary(month13)
month14<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date14)
summary(month14)
month15<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date15)
summary(month15)
month16<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date16)
summary(month16)
month17<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date17)
summary(month17)
month18<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date18)
summary(month18)
month19<-lm(month_anti_shia_post-month_anti_shia_pre ~ treatment_num, data=date19)
summary(month19)

#Split output into 3 tables to fit in latex 

stargazer(month1, month2, month3, month4, month5, month6,ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", 
                                                                                  "Arab ID (Elite)","Religious ID (Elite)","No ID"),
          star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"))

stargazer(month7, month8, month9, month10, month11, month12, ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", 
                                                                                                            "Arab ID (Elite)","Religious ID (Elite)","No ID"),
          star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"))

stargazer(month13, month14, month15, month16, month17, month18, month19,ci = F, single.row = F, covariate.labels=c("Arab ID","Religious ID", 
                                                                                                                                    "Arab ID (Elite)","Religious ID (Elite)","No ID"),
          star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"))

