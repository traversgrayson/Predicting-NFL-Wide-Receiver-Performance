library(readr)
library(dplyr)
library(ggplot2)
library(lattice)
library(GGally)
library(broom)
library(car)


# =====================
# Clean College Wr Data
# =====================

# Read in each file of data and put in one dataset
cfbList = list.files(path = "wrData/", pattern = "cfb.*" ,full.names = TRUE)
cfbList = cfbList[2:9]
collegeWrData = read.csv("../wrData/cfb_wr_2010.csv",as.is = TRUE,header = T,skipNul = T)
collegeWrData['Year'] = 2010

for (file in cfbList)
{
year = as.numeric(substr(file,15,18))
curYear = read.csv(file,sep = ',',as.is = TRUE,header = T,skipNul = T)
print(file)
curYear['Year'] = year
if (year == 2018) {
  curYear = curYear[c(-2,-18)]
  
}
names(curYear)[1:5] = c("X","X.1","X.2","X.3","X.4")
collegeWrData = rbind(collegeWrData,curYear)
}

# Chnage column names and replace empty cells with NAs
collegeWrData = collegeWrData[c("X.1","X.2","X.3","X.4","Receiving","Receiving.1",
                                "Receiving.2","Receiving.3","Year")]

collegeWrData = data.frame(lapply(collegeWrData, function(x){
  x[x == ""] <- NA
  return(x)
}))


# Clean players' names
names(collegeWrData) = c("Player","School","Conf","G","Rec","Yds","YdsCatch","TD","Year")
collegeWrData = collegeWrData %>% filter(Player != "Player")
collegeWrData$Player <-gsub("*", "", collegeWrData$Player, fixed=TRUE)
collegeWrData$Player <-  sub("\\\\.*", "", collegeWrData$Player)
collegeWrData$Player <-  gsub("Jr.", "", collegeWrData$Player)
collegeWrData$Player <-  trimws(collegeWrData$Player)

# Keep only each player's senior year
senior = aggregate(Year ~ Player, collegeWrData,max)
seniors = merge(collegeWrData,senior,by = c("Player","Year"))


# ===================
# Clean NFL WR Data
# ===================

nflList = list.files(path = "../wrData/", pattern = "air.*" ,full.names = TRUE)
nflList = nflList[2:8]

nflWrData = read_csv("../wrData/airyards_2011.csv")
nflWrData['Year'] = 2011

for (file in nflList)
{
  currentYear = read_csv(file)
  year = as.numeric(substr(file,17,21))
  currentYear['Year'] = year
  nflWrData = rbind(nflWrData,currentYear)
}

names(nflWrData)[2] = "Player"
nflWrData$Player <-  trimws(nflWrData$Player)

# Keep only player's rookie years (who actually played)
rookieYear = aggregate(Year ~ Player,nflWrData,min)
rookies = merge(nflWrData,rookieYear,by = c("Player","Year"))
rookies = rookies[c("Player","Year","team","rec_yards","td")]

# Seperate players with data and those in who have not finished their rookie year
predict = rookies %>% filter(Year == 2018)
rookies = rookies %>% filter(Year < 2018 & rec_yards > 50)


# Merge college Qb stats with NFL Qb stats
mergedWr = merge(seniors,rookies, by= c("Player"))

predMerged = merge(seniors,predict, by = c("Player"))

#=======================================#

### Cleaning the combine Data ###

#=======================================#

comb = read.csv("../wrData/combine_wr.csv",as.is = TRUE,header = T,skipNul = T)

# normalize players' names and keep only the needed columns
comb$Player <-gsub("*", "", comb$Player, fixed=TRUE)
comb$Player <-  sub("\\\\.*", "", comb$Player)
comb$Player <-  gsub("Jr.", "", comb$Player)
comb$Player <-  trimws(comb$Player)

comb = comb[c("Player","Height","Wt","X40YD","Vertical")]
names(comb) =  c("Player","Height","Wt","40YD","Vertical")

# Convert the height from 'feet and inches' to 'inches'
# and make strings numeric
for (i in 1:length(comb$Height))
{
  split = strsplit(comb$Height[i],"-")
  first = as.numeric(split[[1]][1])
  second = as.numeric(split[[1]][2])
  comb$Height[i] = (first * 12) + second
}

# ===================
# Create Final Rookie Dataset
# ===================

# Comments: Create dataset of current NFL rookies to test predictions on

finalPred = merge(predMerged,comb, by = "Player")
finalPred = within(finalPred, rm(Year.x,School,Year.y,team))
finalPred = na.omit(finalPred)

finalPred["PredScore"] = finalPred[c("rec_yards")] + 60*finalPred["td"]
finalPred['LogScore'] = log(finalPred["PredScore"])
finalPred = finalPred %>% filter(PredScore > 10)
colnames(finalPred)[colnames(finalPred)=="40YD"] <- "fourtyYD"

write.csv(finalPred,file = "clean_data/finalRookies.csv")

# ====================
# Create Final Dataset
# ====================

# Merge combine data and player data  and remove NAs/columns we don't need
finalWr = merge(mergedWr,comb,by="Player")

finalWr = within(finalWr, rm(Year.x,School,Year.y,G,team))
finalWr = na.omit(finalWr)

finalWr["WrScore"] = finalWr[c("rec_yards")] + 60*finalWr["td"]
finalWr['LogScore'] = log(finalWr["WrScore"])

colnames(finalWr)[colnames(finalWr)=="40YD"] <- "fourtyYD"
names(finalWr) = c("Player","Conf","Rec","Yds","YdsCatch","TD","NflYds",
                   "NflTD","Height","Wt","fourtyYD",
                   "Vertical","WrScore","LogScore" )
write.csv(finalWr,file = "clean_data/finalWrDataset.csv")




