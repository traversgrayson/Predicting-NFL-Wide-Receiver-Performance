# Travers Parsons-Grayson

library(readr)
library(dplyr)
library(ggplot2)
library(lattice)
library(GGally)
library(broom)
library(car)

data = read_csv("wrData/finalWrDataset.csv")
dataP = read_csv("wrData/finalRookies.csv")

dataP = cbind(dataP,predict(jointModel,dataP))

# Check to see that LogScore is normally distributed
data %>% ggplot(aes(x = WrScore)) + geom_histogram(binwidth = 175) + ggtitle("WrScore Distribution")
data %>% ggplot(aes(x = LogScore)) + geom_histogram(binwidth = 0.28) + ggtitle("Log WrScore Distribution")

# Baseline
baseModel = lm(LogScore~1,data=data)

# LogScore vs College Yds
yardsModel = lm(LogScore ~ Yds, data = data)

data %>% ggplot(aes(x = Yds, y = LogScore)) + geom_point()

# Look at the residuals
yardsModel %>% ggplot(aes(x= .fitted,y=.resid)) + geom_point()

yardsModel %>% ggplot(aes(x=.fitted)) + geom_histogram(binwidth = .1) 

# LogScore vs College YdsCatch
catchModel = lm(LogScore ~ YdsCatch,data=data)

data %>% ggplot(aes(x = YdsCatch, y = LogScore)) + geom_point()

catchModel %>% ggplot(aes(x= .fitted,y=.resid)) + geom_point()

# Look at the residuals
catchModel %>% ggplot(aes(x= .fitted,y=.resid)) + geom_point()

catchModel %>% ggplot(aes(x=.fitted)) + geom_histogram(binwidth = .002) 

# LogScore vs College TD

tdModel = lm(LogScore ~ TD,data=data)

data %>% ggplot(aes(x = TD, y = LogScore)) + geom_point()

# Look at the residuals
tdModel %>% ggplot(aes(x= .fitted,y=.resid)) + geom_point()

tdModel %>% ggplot(aes(x=.fitted)) + geom_histogram(binwidth = .09) 

# LogScore vs Conf

confModel = lm(LogScore ~ as.factor(Conf),data=data)

data %>% ggplot(aes(x = as.factor(Conf), y = LogScore,color = as.factor(Conf)))  + geom_boxplot() + 
  theme(axis.text.x=element_text(angle = -55, hjust = 0)) + labs(x = "Conference",y = "LogScore",color = "Conference") + ggtitle("LogScore by Conference")


anova(confModel,baseModel)

# LogScore vs Height

heightModel = lm(LogScore ~ Height,data=data)

data %>% ggplot(aes(x = Height, y =LogScore)) + geom_point()

# Look at the residuals
heightModel %>% ggplot(aes(x= .fitted,y=.resid)) + geom_point()

heightModel %>% ggplot(aes(x=.fitted)) + geom_histogram(binwidth = .098) 

# LogScore vs Weight

weightModel = lm(LogScore ~ Wt,data=data)

data %>% ggplot(aes(x = Wt, y = LogScore)) + geom_point()

# Look at the residuals
weightModel %>% ggplot(aes(x= .fitted,y=.resid)) + geom_point()

weightModel %>% ggplot(aes(x=.fitted)) + geom_histogram(binwidth = .085) 

# LogScore vs 40Yd
speedModel = lm(LogScore ~ fourtyYD ,data=data)

data %>% ggplot(aes(x = fourtyYD, y = LogScore)) + geom_point() + geom_smooth(method = 'lm')

# Look at the residuals
speedModel %>% ggplot(aes(x= .fitted,y=.resid)) + geom_point()

speedModel %>% ggplot(aes(x=.fitted)) + geom_histogram(binwidth = .015) 

### LogScore vs Vertical ###
jumpModel = lm(LogScore ~ Vertical ,data=data)

data %>% ggplot(aes(x = Vertical, y = LogScore)) + geom_point()

# Look at the residuals
jumpModel %>% ggplot(aes(x= .fitted,y=.resid)) + geom_point()

jumpModel %>% ggplot(aes(x=.fitted)) + geom_histogram(binwidth = .025) 

# LogScore vs Rec
recModel = lm(LogScore ~ Rec ,data=data)

data %>% ggplot(aes(x = Rec, y = LogScore)) + geom_point()

# Look at the residuals
recModel %>% ggplot(aes(x= .fitted,y=.resid)) + geom_point()

recModel %>% ggplot(aes(x=.fitted)) + geom_histogram(binwidth = .04) 

# Joint
noInteraction = lm(LogScore ~ Yds + Height + Wt,data=data)

# Check for multicollinearity
vif(noInteraction) # < 2.5, so we are good

# Final Model
jointModel = lm(LogScore ~ Yds + Height*Wt,data=data)
summary(jointModel)

# Check for normality in the residuals
jointModel %>% ggplot(aes(x= .fitted,y=.resid)) + geom_point()

jointModel %>% ggplot(aes(x = .resid)) + geom_histogram(binwidth = .265) 
# Create bootstrap confidence intervals for Betas because residuals are slightly left-skewed

library(boot)
library(tidyr)

bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
} 

# Get bootstrap intervals for each coefficient
bootObj = boot(data=data,statistic = bs,R= 5000,formula = jointModel)

intBoot =boot.ci(bootObj, type="bca", index=1)$'bca'[4:5] 
ydBoot = boot.ci(bootObj, type="bca", index=2)$'bca'[4:5] 
heightBoot = boot.ci(bootObj, type="bca", index=3)$'bca'[4:5] 
weightBoot = boot.ci(bootObj, type="bca", index=4)$'bca'[4:5] 
htWtBoot = boot.ci(bootObj, type="bca", index=5)$'bca'[4:5] 

# Create a table with the CIs
boots = t(data.frame(intBoot))
boots = rbind(boots,ydBoot)
boots = rbind(boots,heightBoot)
boots = rbind(boots,weightBoot)
boots = rbind(boots,htWtBoot)
boots = round(boots,2)
boots = data.frame(boots)

bootTable = unite(boots,"95% Confidence Interval",sep=",",remove=TRUE)
bootTable$`95% Confidence Interval` = paste( "(",bootTable$`95% Confidence Interval`,")",sep = "")
bootTable =cbind(bootTable,signif(jointModel$coefficients,2))
bootTable = cbind(bootTable,signif(summary(jointModel)$coefficients[,4],2) )
names(bootTable)[2:3] = c("Beta","p-value")
bootTable = bootTable[c(2,1,3)]

