#Robin start
#library(devtools)
library("fpc")
library("tidyverse")
library("lubridate")
library("DBI")
library("RMySQL")
#library("psych")
#library("dbplyr")

#install.packages("data.table")

#Goal: See which officer are doing the most stops and find insights

nj <- read.csv("C:/Users/rodriguezr/Documents/group7100/datafile/NJ_cleaned.csv")
nj

# according to the dim this dataset has 3845335 observables and 27 variables 
dim (nj)

# We had to shave off 13 column that were empty or N/A in them. This reduced to 14 usable columns.
nj_clean = nj[,c("officer_id","stop_date","stop_time",
                 "location_raw","county_name","driver_gender",
                 "driver_race","violation_raw","violation",
                 "stop_outcome","out_of_state","vehicle_make",
                 "vehicle_model","vehicle_color")]
str(nj_clean)

#Exploring the data to gain insight. 
head(nj_clean)
tail(nj_clean)
names(nj_clean)

dim(nj_clean) #14 columns same number of obs.  
summary(nj_clean)


# Database query function 
DBQuery = function(sql){
  startTime = Sys.time()
  con = dbConnect(MySQL(),user = 'ruser', password = 'ban7100', db = 'NJStopsDB', host = '149.151.167.88', port=3306)
  # SELECT Statement
  res <- dbSendQuery(con, sql)
  # Fetch Results into variable stops (n = -1 or n = Inf, will fetch all rows)
  mData = dbFetch(res, n = -1)
  # Clear results
  dbClearResult(res)
  # Disconnect from the database
  dbDisconnect(con)
  #Print the time it took to execute the query
  print(paste("SQL Query Completion Time: ",Sys.time() - startTime,"secs"))
  return (mData)
}


# Get Violation Count Per Officer ID
vioByOfficer = DBQuery("SELECT T1.officer_id, T2.violation, COUNT(*) CNT
                       FROM NJ_STOPS_ORIG T1
                       LEFT JOIN NJ_STOPS_VIOLATIONS T2 ON (T2.id = T1.id)
                       WHERE T1.officer_id IS NOT NULL
                       AND T1.violation IS NOT NULL
                       GROUP BY T1.officer_id, T2.violation")
write.csv(vioByOfficer,"vioByOfficer.csv",row.names = F)
# Expending on db query to also include stop outcome and County name per officer
vio.cnByOfficer = DBQuery("SELECT T2.officer_id, T2.county_name, T1.violation, T2.stop_outcome, COUNT(*) CNT
                          FROM NJ_STOPS_VIOLATIONS T1
                          LEFT JOIN NJ_STOPS_ORIG T2 ON (T2.id = T1.id)
                          WHERE T2.officer_id IS NOT NULL
                          AND T2.county_name IS NOT NULL
                          AND T2.stop_outcome IS NOT NULL
                          GROUP BY T2.officer_id, T2.county_name, T1.violation, T2.stop_outcome")
write.csv(vio.cnByOfficer,"vio.cnByOfficer.csv",row.names = F)

####################################################################################
# convert violation column to factor
vioByOfficer$violation <- as.factor(vioByOfficer$violation)
vioByOfficer$officer_id <- as.integer(vioByOfficer$officer_id)

# converting county names and violation
names(vio.cnByofficer)

vio.cnByOfficer$violation <- as.factor(vio.cnByOfficer$violation)
vio.cnByOfficer$county_name <- as.factor(vio.cnByOfficer$county_name)
vio.cnByOfficer$officer_id <- as.integer(vio.cnByOfficer$officer_id)
vio.cnByOfficer$stop_outcome <- as.factor(vio.cnByOfficer$stop_outcome)

str(vio.cnByOfficer)

### median of top performing and low performers officer
top_officers = subset(officer_counts,num_stops >= 5000)
bot_officers = subset(officer_counts,num_stops <  5000)

#Pulling out the vectors new for top half and bottom half of officer based on number of stops
top_officers_vector <- pull(top_officers[,1])

bot_officers_vector <- pull(bot_officers[,1])

# adding the vectors pulled from officer count and adding it to nj_clean data frame
top_nj_off <- nj_clean[nj_clean$officer_id %in% top_officers_vector,]

bot_nj_off <- nj_clean[nj_clean$officer_id %in% bot_officers_vector,]


# created new data frame containing only the county names, violation and violation counts for the top 5000 officers 

top.vioNJofficer <- vio.cnByOfficer[as.integer(vio.cnByOfficer$officer_id) %in% top_officers_vector,]
str(top.vioNJofficer)

### created new data frame containing only the county names, violation and violation counts for low 5000 officers

bot.vioNJofficer <- vio.cnByOfficer[as.integer(vio.cnByOfficer$officer_id) %in% bot_officers_vector,]
#####################################################################################

str(bot.vioNJofficer)
names(bot.vioNJofficer)

names(top.vioNJofficer)

str(top_nj_off)

str(top.vioNJofficer)
names(top.vioNJofficer)

str(bot.vioNJofficer)

# Using table and pie on county_name to see which county has the highest number of stops. 

cn <-nj_clean$county_name
table(cn)
pie(table(cn))

# Drilling down
# Grouping the officer_id column we a count of 2759 officers making stops.  
# we noticed that out of the 2759 officers five of them had over 10,000 stops.
# We are calling them the top earners for now.

officer_counts <- nj_clean %>%
  group_by(officer_id) %>%
  summarise(num_stops=n())

# sorted the column num_stops to see which officers had the 
#highest number of stop and notice we have top five stoppers.

officer_counts <- arrange(officer_counts, desc(num_stops))
head(officer_counts,15)

# starting with officer id 6215 you notice that officer 6215 works in both Burlington and Middlesex county which is very interesting and needs further exploring.  
officer.6215 <- subset(nj_clean, nj_clean$officer_id == 6215)
pie(table(officer.6215$county_name))

# Top five officers with more than 10,000 stops
topFive <- subset(nj_clean,
                     officer_id == 6215 |
                       officer_id ==  6691 |
                       officer_id ==  7240 |
                       officer_id ==  6415 |
                       officer_id ==  6984 
)
head(topFive)



# Top Five officers with over 10,00 Stops and the County They Work In
ggplot(topFive, aes(county_name, factor(officer_id), colour=factor(officer_id))) +coord_flip() +
geom_count() + 
labs(x="County Names", y = "Officer ID", colour = "Officer's ID", 
title= "Top Five Based On Number of Stops and the County They Work In") + 
theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.4))

# Top Five Officer Violation and counties
topFiveByVio <- subset(vio.cnByOfficer,
officer_id == 6215 |
officer_id ==  6691 |
officer_id ==  7240 |
officer_id ==  6415 |
officer_id ==  6984 
)
head(topFiveByVio)

#  "Top Five officers with over over 10,00 Stops and the Type of Violations They are giving based on county"
ggplot(topFiveByVio, aes(violation, factor(officer_id), colour=factor(officer_id))) +
geom_point() + 
labs(x="Violation type", y = "Officer ID", colour = "Officer's ID", 
title= "Top Five Officers with over 10,000 Stops. ", subtitle= "Officer's Counties and Violation Types.") +coord_flip() +facet_wrap(~county_name)
+ theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.4))

str(vio.cnByOfficer)


#Analysis

#######################################
### group by officer_id number of stop#
# officer_counts <- nj_clean %>%      #
#  group_by(officer_id) %>%           #
#  summarise(num_stops=n())           #
######################################

### median of top performing and low performers officer
#top_officers = subset(officer_counts,num_stops >= 5000)
#bot_officers = subset(officer_counts,num_stops <  5000)

#Pulling out the vectors new for top half and bottom half of officer based on number of stops
#top_officers_vector <- pull(top_officers[,1])

#bot_officers_vector <- pull(bot_officers[,1])

# adding the vectors pulled from officer count and adding it to nj_clean data frame
#top_nj_off <- nj_clean[nj_clean$officer_id %in% top_officers_vector,]

#bot_nj_off <- nj_clean[nj_clean$officer_id %in% bot_officers_vector,]


# created new data frame containing only the county names, violation and violation counts for the top 5000 officers 

#top.vioNJofficer <- vio.cnByOfficer[as.integer(vio.cnByOfficer$officer_id) %in% top_officers_vector,]
#str(top.vioNJofficer)

### created new data frame containing only the county names, violation and violation counts for low 5000 officers

#bot.vioNJofficer <- vio.cnByOfficer[as.integer(vio.cnByOfficer$officer_id) %in% bot_officers_vector,]
#str(bot.vioNJofficer)
#names(bot.vioNJofficer)

#names(top.vioNJofficer)

#str(top_nj_off)

#str(top.vioNJofficer)
#names(top.vioNJofficer)

#str(bot.vioNJofficer)

# see which counties the top performers working in . 
#str(top_nj_off)

# top performing officers grouped by county name
top_countyName <- top_nj_off %>%
  group_by(officer_id,county_name) %>%
  summarise(countyName.cnt=n())
#top performing officers base on county count
ggplot(top_countyName, aes(county_name, countyName.cnt, colour=factor(officer_id))) +coord_flip() +
  geom_point() +labs(x="County Names", y = "County Count", colour = "Officer's ID", title= "Top Performing Officer Count") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.4))

# Top Performing Officers Based On Number of Stops and the County They Work In
ggplot(top.vioNJofficer, aes(county_name, CNT, colour=factor(officer_id))) +coord_flip() +
  geom_point() + labs(x="County Names", y = "Violation Count", colour = "Officer's ID", 
       title= "Top Performing Officers Based On Number of Stops and the Counties They Work In") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.4)) 

#  "Top Performing Officers Based On Number of Stops and the Type of Violations They are giving"
ggplot(top.vioNJofficer, aes(violation, factor(officer_id), colour=factor(officer_id))) +coord_flip() +
  geom_point() + labs(x="Violation type", y = "Officer ID", colour = "Officer's ID", 
       title= "Top Performing Officers Based On Number of Stops and the Type of Violations They are giving") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.4))


# plotting the lower performing officers will kills R beware. 
bottom_countyName <- bot_nj_off %>%
  group_by(officer_id,county_name) %>%
  summarise(countyName.cnt=n())

#ggplot(bottom_countyName, aes(county_name, factor(officer_id), colour=factor(officer_id))) +coord_flip() +
# geom_point() + 
#labs(x="County Names on x axis", y = "Officer ID on y axis", colour = "Officer's ID", title= "Bottom Feeder and the Countys They Work in")


#ggplot(bot.vioNJofficer, aes(county_name, factor(officer_id), colour=factor(officer_id))) +coord_flip() +
# geom_point() + 
#  labs(x="Violation type on the x axis", y = "Officer ID on the y axis", colour = "Officer's ID", 
#     title= "Low Performing Officers Based On Number of Stops and the Type of Violations They are giving") + 
# theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.4))

#ggplot(bot.vioNJofficer, aes(violation, factor(officer_id), colour=factor(officer_id))) +coord_flip() +
# geom_point() + 
#  labs(x="Violation type on the x axis", y = "Officer ID on the y axis", colour = "Officer's ID", 
#    title= "Low Performing Officers Based On Number of Stops and the Type of Violations They are giving") + 
# theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.4))


#instead I will plot using the average of per officer id by county as well as violation

#using aggregate bottom officer
ag.bot.id <-aggregate(bot.vioNJofficer$CNT, by=list(bot.vioNJofficer$violation), FUN=mean)
ag.bot.id

#using tapply for mean
summary.bot.id <- data.frame(
  violation=levels(as.factor(bot.vioNJofficer$violation)),
  meanbot.CNT=tapply(bot.vioNJofficer$CNT, bot.vioNJofficer$violation, mean))
summary.bot.id

# average citation given by low officers
ggplot(summary.bot.id, aes(x = factor(violation), y = meanbot.CNT)) + geom_bar(stat = "identity")+ 
  labs(x="Violation Type", y = "Low Performing Officers Average",
       title= "Average violation given by Low Performing Officers")


#using aggregate top officer
ag.top.id <-aggregate(top.vioNJofficer$CNT, by=list(top.vioNJofficer$violation), FUN=mean)
ag.top.id

#using tapply for mean
summary.top.id <- data.frame(
  violation=levels(as.factor(top.vioNJofficer$violation)),
  meantop.CNT=tapply(top.vioNJofficer$CNT, top.vioNJofficer$violation, mean))
summary.top.id

# average citation given by top officers  
ggplot(summary.top.id, aes(x = factor(violation), y = meantop.CNT)) + geom_bar(stat = "identity")+ 
  labs(x="Violation Type", y = "Top Performing Officers Average", 
      title= "Average violation given by Top Performing Officers")

####################+++++++++++++++++++++++++++++++++++++++++++++++++++++++###########

####
# see which top earner and bot feeder are giving summons or warning (stop_outcome)
# From the graph it shows that the top earners are all concentrated in Burlington, MiddleSex and Monmouth
# so why does Burlington, Middlessex, and Monmouth has the most stops? 


# Average of violation per officer.
tapply(vioByOfficer$CNT, vioByOfficer$violation, mean, na.rm=TRUE)

tapply(vio.cnByofficer$CNT, vio.cnByofficer$county_name, mean, na.rm=TRUE)

# simplified pie chart overview of average ticket  
pie(tapply(vioByOfficer$CNT, vioByOfficer$violation, mean, na.rm=TRUE))

# plot of the average volition given per office

xname = names(tapply(vioByOfficer$CNT, vioByOfficer$violation, mean, na.rm=TRUE))
plot(tapply(vioByOfficer$CNT, vioByOfficer$violation, mean, na.rm=TRUE),xaxt="n")
axis(1, at=1:length(xname), labels=xname)

# average violation per county 
xname = names(tapply(vio.cnByOfficer$CNT, vio.cnByOfficer$county_name, mean, na.rm=TRUE))
plot(tapply(vio.cnByOfficer$CNT, vio.cnByOfficer$county_name, mean, na.rm=TRUE),xaxt="n")
axis(1, at=1:length(xname), labels=xname)

# lower performing officers violation average

xname = names(tapply(bot.vioNJofficer$CNT, bot.vioNJofficer$violation, mean, na.rm=TRUE))
plot(tapply(bot.vioNJofficer$CNT, bot.vioNJofficer$violation, mean, na.rm=TRUE),xaxt="n")
axis(1, at=1:length(xname), labels=xname)

# lower performer average violation per county 
xname = names(tapply(bot.vioNJofficer$CNT, bot.vioNJofficer$county_name, mean, na.rm=TRUE))
plot(tapply(bot.vioNJofficer$CNT, bot.vioNJofficer$county_name, mean, na.rm=TRUE),xaxt="n")
axis(1, at=1:length(xname), labels=xname)


# Linear regression and clustering

nj_clean$month <- month(as.Date(nj_clean$stop_date))

officer_counts <- nj_clean %>%
  group_by(officer_id,month) %>%
  summarise(num_stops=n(),
            summons=sum(stop_outcome=="Summons"),
            warnings = sum(stop_outcome=="Warning"))


numstops_summons <- lm(num_stops ~ summons,data = officer_counts)
par(mfrow=c(3,2))
plot(numstops_summons)
summary(numstops_summons)
plot(x = officer_counts$summons, y = officer_counts$num_stops); abline(numstops_summons, col = "red")

###
officer_gender <- nj_clean %>%
  group_by(officer_id,month) %>%
  summarise(num_stops=n(),
            male=sum(driver_gender=="M"),
            female = sum(driver_gender=="F"))


numstops_gender <- lm(male ~ female,data = officer_gender)
par(mfrow=c(3,2))
plot(numstops_gender)
summary(numstops_gender)
plot(x = officer_gender$female, y = officer_gender$male); abline(numstops_summons, col = "red")

offsamp = officer_gender[sample(nrow(officer_gender),300),]

normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

x<- as.data.frame(lapply(offsamp[3:5], normalize))

data_k= kmeans(x,4)
#?plotcluster
plotcluster(x,data_k$cluster)
plotcluster(x[,c(1,2)],data_k$cluster,clnum = 1, method = "mvdc")
plotcluster(x[,c(1,2)],data_k$cluster,clnum = 1, method = "adc")
plotcluster(x[,c(1,2)],data_k$cluster,clnum = 1, method = "nc")
#plotcluster(x[,c(1,2)],data_k$cluster,clnum = 1, method = "anc")
#plotcluster(x[,c(1,2)],data_k$cluster,clnum = 1, method = "anc")
#plotcluster(x[,c(1,2)],data_k$cluster,clnum = 1, method = "bc")
#Robin end








