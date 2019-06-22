
fitTable = data.frame(confint(join))
fitTable =cbind(fitTable,join$coefficients)
fitTable = cbind(fitTable,summary(join)$coefficients[,4] )
fitTable = round(fitTable,2)
fitTable["95% Confidence Interval"]= paste(fitTable[1],fitTable[2],sep=",")
fitTable = unite(fitTable,"95% Confidence Interval",c("2.5% Percentile","97.5% Percentile"),sep=",",remove=TRUE)
fitTable$`95% Confidence Interval` = paste( "(",fitTable$`95% Confidence Interval`,")",sep = "")
fitTable = fitTable[c(2,1,3)]
#library(gridExtra)
#png("test.png", height = 50*nrow(fitTable), width = 200*ncol(fitTable))
#grid.table(fitTable)
#dev.off()

library(xtable)

xtable(fitTable,digits = 4)
library(dplyr)
library(ggplot2)

ggplot(aes(x = .resid),data=join) +geom_histogram(binwidth = .2) 

c = ggplot(aes(y = .resid,x = .fitted),data=join) + geom_point()
plot(c)


# Create table for univariate models
newTable = data.frame(c("Yds",,0.07))
                      