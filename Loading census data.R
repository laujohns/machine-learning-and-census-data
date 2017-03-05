setwd("/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/code")

#load necessary packages
library(rgdal)
library(rgeos)
library(tigris)
library(dplyr)

#create block group spatial file in R
blocks_AL <- block_groups('AL',cb=TRUE) #using tigris package loads block groups for AL from American Community Survey
writeOGR(blocks_AL, dsn="/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets/", layer="blocks_AL",driver="ESRI Shapefile") #save block group file
head(blocks_AL@data)#inspect data
names(blocks_AL)

#create variables in age dataset downloaded from American Community Survey (5-year, 2015 data): block-group level for AL
agebysex<-read.csv("/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets/Agebysex.csv")
agebysex[1:10, c("GEO.id2")]
names(agebysex)
data.age <- select(agebysex, GEO.id2, GEO.display.label, 
                   HD01_VD01:HD01_VD49) %>% rename(geography=GEO.display.label,total=HD01_VD01,
                          totalmen=HD01_VD02, totalwomen=HD01_VD26) #create dataset for age
data.age2 <- mutate(data.age, id=as.character(GEO.id2)) #recode GEO.id2 as character variable "id"
head(data.age2) #inspect data
data.age2[1:10,c("id")] #inspect first 10 id's to make sure characters

#create age groups and proportions
#5-19 Men
table(data.age$totalmen==0)
data.age$totalmen5.19<-data.age$HD01_VD03+data.age$HD01_VD04+data.age$HD01_VD05+data.age$HD01_VD06+data.age$HD01_VD07
data.age[1:10,c("HD01_VD03", "HD01_VD04","HD01_VD05","HD01_VD06","HD01_VD07","totalmen5.19")]#sum
data.age$percmen5.19<-data.age$totalmen5.19/data.age$totalmen
summary(data.age$percmen5.19)#6 NA
#20-39Men
data.age$totalmen20.39=data.age$HD01_VD08+data.age$HD01_VD09+data.age$HD01_VD10+data.age$HD01_VD11+data.age$HD01_VD12+data.age$HD01_VD13
data.age$percmen20.39=data.age$totalmen20.39/data.age$totalmen
#40-59 Men
data.age$totalmen40.59=data.age$HD01_VD14+data.age$HD01_VD15+data.age$HD01_VD16+data.age$HD01_VD17
data.age$percmen40.59=data.age$totalmen40.59/data.age$totalmen
#60+Men
data.age$totalmen60.plus=data.age$HD01_VD18+data.age$HD01_VD19+data.age$HD01_VD20+data.age$HD01_VD21+data.age$HD01_VD22+data.age$HD01_VD23+data.age$HD01_VD24+data.age$HD01_VD25
data.age$percmen60.plus=data.age$totalmen60.plus/data.age$totalmen
#5-19 Women
data.age$totalwomen5.19<-data.age$HD01_VD27+data.age$HD01_VD28+data.age$HD01_VD29+data.age$HD01_VD30+data.age$HD01_VD31
data.age$percwom5.19=data.age$totalwomen5.19/data.age$totalwomen
#20-39 Women
data.age$totalwomen20.39<-data.age$HD01_VD32+data.age$HD01_VD33+data.age$HD01_VD34+data.age$HD01_VD35+data.age$HD01_VD36+data.age$HD01_VD37
data.age$percwom20.39<-data.age$totalwomen20.39/data.age$totalwomen
#40-59 Women
data.age$totalwomen40.59<-data.age$HD01_VD38+data.age$HD01_VD39+data.age$HD01_VD40+data.age$HD01_VD41
data.age$percwom40.59<-data.age$totalwomen40.59/data.age$totalwomen
#60+ Women
data.age$totalwomen60.plus<-data.age$HD01_VD42+data.age$HD01_VD43+data.age$HD01_VD44+data.age$HD01_VD45+data.age$HD01_VD46+data.age$HD01_VD47+data.age$HD01_VD48+data.age$HD01_VD49
data.age$percwom60.plus<-data.age$totalwomen60.plus/data.age$totalwomen

data.age[1:10, c("HD01_VD42","HD01_VD43","HD01_VD44","HD01_VD45","HD01_VD46","HD01_VD47","HD01_VD48","HD01_VD49", "totalwomen60.plus", "percwom60.plus")]#check
percent<-data.age$percwom5.19+data.age$percwom20.39+data.age$percwom40.59+data.age$percwom60.plus #make temporary variable to inspect that total age groups for women adds to 1
percent #make sure proportions add up to 1

data.age.merge<-data.age[,c("GEO.id2","totalmen","totalwomen","totalmen5.19","percmen5.19","totalmen20.39","totalmen40.59","totalmen60.plus","totalwomen5.19",
                    "percwom5.19" ,"totalwomen40.59","percwom40.59","totalwomen60.plus", "percwom60.plus","percmen20.39", "percmen40.59","percmen60.plus",
                    "totalwomen20.39","percwom20.39")] #create dataset of age group proportions for each block group in AL that will be merged with other datasets later

#education variable creation
education<-read.csv("/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets/education.csv")
#create proportions for education

#HS/GED
education$totalhsged<-education$HD01_VD17+education$HD01_VD18
education$perchsged<-education$totalhsged/education$HD01_VD01
#Some college
education$totalsomecoll<-education$HD01_VD19+education$HD01_VD20+education$HD01_VD21
education$percsomecoll<-education$totalsomecoll/education$HD01_VD01
#Bachelors
education$totalbach<-education$HD01_VD22
education$percbach<-education$totalbach/education$HD01_VD01
#Graduate
education$totalgrad<-education$HD01_VD23+education$HD01_VD24+education$HD01_VD25
education$percgrad<-education$totalgrad/education$HD01_VD01
#need to add up and substract to get <HS
education$all.percent<-education$perchsged+education$percsomecoll+education$percbach+education$percgrad
#<HS
education$perchs<-1-education$all.percent

data.education.merge<-education[,c("GEO.id2","perchs","perchsged","percsomecoll","percbach","percgrad")] #create dataset to merge later

#health insurance variable creation
health<-read.csv("/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets/healthinsurance.csv")
#groups
health$totins<-health$HD01_VD03+health$HD01_VD19+health$HD01_VD35+health$HD01_VD52+health$HD01_VD10+health$HD01_VD26+
              +health$HD01_VD42+health$HD01_VD58 #those with any health insurance
health$percins<-health$totins/health$HD01_VD01 #percent with health insurance in each block group

health$totnoins<-health$HD01_VD33+health$HD01_VD50+health$HD01_VD66+health$HD01_VD17 #total with no health insurance
health$percnoins<-health$totnoins/health$HD01_VD01 #percent with no haelth insurance
percent<-health$percins+health$percnoins #make sure adds to 1 --> then we know it's coded correctly
percent

data.health.merge<-health[,c("GEO.id2", "percins","percnoins")]#create dataset to merge later
head(data.health.merge)

#race/ethnicity variable creation
hispanic<-read.csv("/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets/hispanic.csv")
#percent hispanic
hispanic$tothisp<-hispanic$HD01_VD03
hispanic$perchip<-hispanic$tothisp/hispanic$HD01_VD01

data.hispanic.merge<-hispanic[,c("GEO.id2","tothisp","perchip")]#create dataset to merge later

#race variable creation
race<-read.csv("/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets/race.csv")
names(race)
#race totals and percents for each block group
race$totwhite<-race$HD01_VD02
race$percwhite<-race$totwhite/race$HD01_VD01
race$black<-race$HD01_VD03
race$percblack<-race$black/race$HD01_VD01
race$asian<-race$HD01_VD05
race$percasian<-race$asian/race$HD01_VD01
race$other<-race$HD01_VD04+race$HD01_VD05+race$HD01_VD06+race$HD01_VD07
race$percother<-race$other/race$HD01_VD01

data.race.merg<-race[,c("GEO.id2","percwhite","percblack","percasian","percother")] #create dataset to merge later

#HH income variable creation
income<-read.csv("/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets/medianincome.csv")
income$income<-income$HD01_VD01
data.merge.income<-income[,c("GEO.id2", "income")]#create dataset to merge

#merge all datasets
merge<-merge(data.age.merge, data.education.merge, by="GEO.id2",all.x=TRUE)
head(merge)
head(data.age.merge)
merge2<-merge(merge, data.health.merge, by="GEO.id2",all.x=TRUE)
head(merge2)
merge3<-merge(merge2, data.hispanic.merge,by="GEO.id2",all.x=TRUE)
head(merge3)
merge4<-merge(merge3,data.race.merg, by="GEO.id2", all.x=TRUE)
head(merge4)
merge5<-merge(merge4, data.merge.income, by="GEO.id2", all.x=TRUE)
head(merge5)

#change geoid to factor
merge5$geoid<-as.factor(merge$GEO.id2)
head(merge5)
#add a zero to match ESRI files
merge5$geoid<-paste0("0",merge5$geoid)#add 0 to beginning of geoid
merge5[1:10,c("geoid")]

#import shapefile
census<-readOGR(dsn="/home/lauren/Dropbox/Data Science/Insight Project/Data/Insight/For machine learning/datasets",layer="merged_census")
names(census)#import merged shapefiles from ESRI
census@data<-census@data[,c("ID","X8021_X", "X8021_I","X8021_A","TOTPOP_CY")] #select necessary variables
summary(census)
names(census)
summary(census@data$X8021_X)

#create expenditures per capital variable
census@data$expenditure=census@data$X8021_X/census@data$TOTPOP_CY
table(census@data$X8021_X==0)#31
table(census@data$expenditure==0)#24
summary(census@data$expenditure)
hist(census@data$expenditure)
summary(census@data)

#create dataframe from shapefile
census2<-data.frame(census)
names(census2)
census2$geoid<-census$ID
census2[1:10,c("geoid","X8021_X")]

#merge ESRI file with ACS merged file from above
final.data<-merge(census2, merge5, by="geoid", all.x=TRUE)
head(final.data)
write.csv(final.data, "final.data.csv") #use this file for machine learning

#check to see if merge worked correctly
final.data[1:100,c("geoid")]
final.data[final.data$geoid=="010570202004",]
data.merge.income[data.merge.income$GEO.id2==10570202004,]
names(data.age.merge)

