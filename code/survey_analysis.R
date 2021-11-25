#######################################################################
#Name of code file: survey_analysis.R

#Data In: 
#survey_data

#Data Out: 
#Plots and Regression Tables from Survey Experiment
#Figures 6-7
#Tables A22-A24
######################################################################


#Load Packages
library(readr)
library(ggplot2)
library(coefplot)
library(stargazer)
library(dplyr)

#Set Working Directory
#setwd("..")
setwd("~/Dropbox/siegel_badaan_replication/")

#Read in Data
data<-read_csv("data/survey_data.csv")

#Prepare Data for Analysis 

#Covariate Indices (average responses)
data$relig<-(data$relig1+data$relig2+data$relig3+data$relig4+data$relig5+data$relig6+data$relig7+data$relig8+data$relig9+data$relig10+data$relig11+data$relig12)/12
data$sectarian<-(data$sectarian1+data$sectarian2+data$sectarian3+data$sectarian4+data$sectarian5)/5
data$sectsj<-(data$sectsj1+data$sectsj2+data$sectsj3+data$sectsj4+data$sectsj5+data$sectsj6+data$sectsj7+data$sectsj8)/8
data$mcp<-(data$mcp1+data$mcp2+data$mcp3+data$mcp4+data$mcp5+data$mcp6+data$mcp7+data$mcp8+data$mcp9+data$mcp10+data$mcp11)/11

#Sect Variables
data$maronite<-ifelse(data$Sect==1, 1,0)
data$sunni<-ifelse(data$Sect==7, 1,0)
data$shia<-ifelse(data$Sect==8, 1,0)

#Counter Sectarian Tweets
#rating
data$counter_sec1<-(data$Tweet8_1+data$Tweet6_1+data$Tweet4_1+data$Tweet7_1)/4
#person rating
data$counter_sec2<-(data$Tweet8_2+data$Tweet6_2+data$Tweet4_2+data$Tweet7_2)/4
#sharing likelihood
data$counter_sec3<-(data$Tweet8_3+data$Tweet6_3+data$Tweet4_3+data$Tweet7_3)/4
hist(data$counter_sec3)

#Sectarian Tweets 
#rating
data$sec1<-(data$Tweet1_1+data$Tweet2_1+data$Tweet3_1+data$Tweet5_1)/4
#person rating
data$sec2<-(data$Tweet1_2+data$Tweet2_2+data$Tweet3_2+data$Tweet5_2)/4
#sharing likelihood
data$sec3<-(data$Tweet1_3+data$Tweet2_3+data$Tweet3_3+data$Tweet5_3)/4

#Counter Sectarian Ratings - Sectarian Ratings
data$combined1<-data$sec1-data$counter_sec1
data$combined2<-data$sec2-data$counter_sec2
data$combined3<-data$sec3-data$counter_sec3

#Treatment Var
data$treatment<-as.factor(data$Prmt)

##########
#Figure 6#
##########

#OLS Combined Ratings
model1<-lm(combined1~treatment, data=data)
model1_cov<-lm(combined1~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model1)
summary(model1_cov)

model2<-lm(combined1~treatment, data=data)
model2_cov<-lm(combined2~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model2)
summary(model2_cov)

model3<-lm(combined3~treatment, data=data)
model3_cov<-lm(combined3~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model3)
summary(model3_cov)

pdf("figures/figure_6.pdf", height=7, width=11)
multiplot(model1, model2, model3, coefficients=c("treatment2", "treatment3", "treatment4", "treatment5"), 
          newNames=c(treatment2="Religious ID", treatment3="National ID ", treatment4="Religious ID (Elite)", 
                     treatment5="National ID (Elite)"), 
          names=c(" Tweet Rating", " User Rating", "Likely to Share"), title="", 
          sort="alphabetical", innerCI=1.645, outerCI=1.96, single=FALSE, zeroType = 0,legend.position="none") +
  scale_color_manual(values=c("red", "blue", "seagreen")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.line = element_line(colour = "black"), text = element_text(size=16))+
  ylab("Treatments") + xlab("OLS Estimates")+ geom_vline(aes(xintercept = 0), size = .5, linetype = "dashed")
dev.off()   


###########
#Figure 7a#
###########

#OLS Sectarian Ratings
model1<-lm(sec1~treatment, data=data)
model1_cov<-lm(sec1~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model1)
summary(model1_cov)

model2<-lm(sec1~treatment, data=data)
model2_cov<-lm(sec2~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model2)
summary(model2_cov)

model3<-lm(sec3~treatment, data=data)
model3_cov<-lm(sec3~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model3)
summary(model3_cov)

pdf("figures/figure_7a.pdf", height=7, width=11)
multiplot(model1, model2, model3, coefficients=c("treatment2", "treatment3", "treatment4", "treatment5"), 
          newNames=c(treatment2="Religious ID", treatment3="National ID ", treatment4="Religious ID (Elite)", 
                     treatment5="National ID (Elite)"), 
          names=c(" Tweet Rating", " User Rating", "Likely to Share"), title="", 
          sort="alphabetical", innerCI=1.645, outerCI=1.96, single=FALSE, zeroType = 0,legend.position="none") +
  scale_color_manual(values=c("red", "blue", "seagreen")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.line = element_line(colour = "black"), text = element_text(size=16))+
  ylab("Treatments") + xlab("OLS Estimates")+ geom_vline(aes(xintercept = 0), size = .5, linetype = "dashed")
dev.off()   


###########
#Figure 7b#
###########

#OLS Counter Sectarian Ratings
model1<-lm(counter_sec1~treatment, data=data)
model1_cov<-lm(counter_sec1~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model1)
summary(model1_cov)

model2<-lm(counter_sec2~treatment, data=data)
model2_cov<-lm(counter_sec2~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model2)
summary(model2_cov)

model3<-lm(counter_sec3~treatment, data=data)
model3_cov<-lm(counter_sec3~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model3)
summary(model3_cov)



pdf("figures/figure_7b.pdf", height=7, width=11)
multiplot(model1, model2, model3, coefficients=c("treatment2", "treatment3", "treatment4", "treatment5"), 
          newNames=c(treatment2="Religious ID", treatment3="National ID ", treatment4="Religious ID (Elite)", 
                     treatment5="National ID (Elite)"), 
          names=c(" Tweet Rating", " User Rating", "Likely to Share"), title="", 
          sort="alphabetical", innerCI=1.645, outerCI=1.96, single=FALSE, zeroType = 0,legend.position="none") +
  scale_color_manual(values=c("red", "blue", "seagreen")) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none", axis.line = element_line(colour = "black"), text = element_text(size=16))+
  ylab("Treatments") + xlab("OLS Estimates")+ geom_vline(aes(xintercept = 0), size = .5, linetype = "dashed")
dev.off()   


###########
#Table A22#
###########

#OLS Combined Ratings
model1<-lm(combined1~treatment, data=data)
model1_cov<-lm(combined1~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model1)
summary(model1_cov)

model2<-lm(combined1~treatment, data=data)
model2_cov<-lm(combined2~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model2)
summary(model2_cov)

model3<-lm(combined3~treatment, data=data)
model3_cov<-lm(combined3~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model3)
summary(model3_cov)

stargazer(model1, model1_cov, model2, model2_cov, model3, model3_cov, ci = F, single.row = F, covariate.labels=c("Religious ID", "National ID", "Religious ID (Elite)", "National ID (Elite)", "Sectarianism Index", "Social Media Use", "Sectarian System Justification Index", "MCP", "Gender", "Education", "Religiosity", "Internet Use", "Political Interest"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)

###########
#Table A23#
###########

#OLS Sectarian Ratings
model1<-lm(sec1~treatment, data=data)
model1_cov<-lm(sec1~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model1)
summary(model1_cov)

model2<-lm(sec1~treatment, data=data)
model2_cov<-lm(sec2~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model2)
summary(model2_cov)

model3<-lm(sec3~treatment, data=data)
model3_cov<-lm(sec3~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model3)
summary(model3_cov)

stargazer(model1, model1_cov, model2, model2_cov, model3, model3_cov, ci = F, single.row = F, covariate.labels=c("Religious ID", "National ID", "Religious ID (Elite)", "National ID (Elite)", "Sectarianism Index", "Social Media Use", "Sectarian System Justification Index", "MCP", "Gender", "Education", "Religiosity", "Internet Use", "Political Interest"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)


###########
#Table A24#
###########

#OLS Counter Sectarian Ratings
model1<-lm(counter_sec1~treatment, data=data)
model1_cov<-lm(counter_sec1~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model1)
summary(model1_cov)

model2<-lm(counter_sec2~treatment, data=data)
model2_cov<-lm(counter_sec2~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model2)
summary(model2_cov)

model3<-lm(counter_sec3~treatment, data=data)
model3_cov<-lm(counter_sec3~treatment +sectarian+ socmedia+sectsj+mcp+sex+EdLvl+relig+internetuse+polinterest, data=data)
summary(model3)
summary(model3_cov)

stargazer(model1, model1_cov, model2, model2_cov, model3, model3_cov, ci = F, single.row = F, covariate.labels=c("Religious ID", "National ID", "Religious ID (Elite)", "National ID (Elite)", "Sectarianism Index", "Social Media Use", "Sectarian System Justification Index", "MCP", "Gender", "Education", "Religiosity", "Internet Use", "Political Interest"),star.cutoffs = c(.1, .05, .01, .001), star.char = c("dagger", "*", "**", "***"), notes="", notes.append=FALSE)






