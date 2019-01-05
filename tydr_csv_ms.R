# PREP -------------------------------------------------------------------

# install.packages("RMariaDB")
# install.packages("RMySQL")

library(RMySQL)
library(DBI)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)



# SQL ---------------------------------------------------------------------
# setwd("/TYDR")
# 
# library(sqldf)
# 
# tydr <- read.csv.sql("2018-12-06.sql", row.names = TRUE)

con <- dbConnect(MySQL(), user='user', password='stat2c!', dbname='dynamicdb_prod', host='130.149.22.39', port=3306)
# 
# res <- dbSendQuery(con, "SELECT * FROM phoneinfo LIMIT 100;")
# 
# res2 <- dbSendQuery(con, "SELECT DATEDIFF(MAX(dynamicdb_prod.user.timestamp),MIN(dynamicdb_prod.user.timestamp)) FROM dynamicdb_prod.user WHERE dynamicdb_prod.user.userId = 000d7cb40f81143e7e32fc697b8c526bbb30d946943ffc7bfa10daf6fa9cd3e4;")
# 
# while(!dbHasCompleted(res)){chunk <- dbFetch(res, n = 5) print(nrow(chunk))}

# for (i in 1:length(allin)) {allin_call1 <- dbSendQuery(con, "SELECT * FROM dynamicdb_prod.permission WHERE dynamicdb_prod.permission.userId = i;")} #duh, check loop syntax

# test <- dbSendQuery(con, "SELECT * FROM dynamicdb_prod.permission WHERE dynamicdb_prod.permission.userId;")

test_table <- dbReadTable (con, "dynamicdb_prod.permission")




# 1st try --------------------------------------------------------------------

setwd("/Users/shanti/Dropbox/+++++hs18/TYDR")

perm <- read.csv("pre_permission_unmelt.csv")

perm2 <- perm[c(-1,-3)]

perm_wide <- reshape(perm2, idvar = "userId", timevar = "id", direction = "wide")

names(perm_wide)[c(3,5,7,9,11)]<-c("APP_USAGE","NOTIFICATION","STORAGE","CALL_LOG","LOCATION")

perm_final <- perm_wide[c(-2,-4,-6,-8,-10)]

perm_final2 <- perm_final[-1]

df <- data.frame(perm_final2$APP_USAGE,perm_final2$NOTIFICATION,perm_final2$STORAGE,perm_final2$CALL_LOG,perm_final2$LOCATION)

perm_final2$APP_USAGE <- as.factor(perm_final2$APP_USAGE)
perm_final2$NOTIFICATION <- as.factor(perm_final2$NOTIFICATION)
perm_final2$STORAGE <- as.factor(perm_final2$STORAGE)
perm_final2$CALL_LOG <- as.factor(perm_final2$CALL_LOG)
perm_final2$LOCATION <- as.factor(perm_final2$LOCATION)

filtest <- filter(perm_final2, perm_final2$APP_USAGE == 1)

sum(perm_final2$APP_USAGE ==1)
sum(perm_final2$NOTIFICATION ==1)
sum(perm_final2$STORAGE ==1)
sum(perm_final2$CALL_LOG ==1)
sum(perm_final2$LOCATION ==1)


# sql2table -------------------------------------------------------------------

permx <- test_table[-2]

perm_wide <- reshape(perm2, idvar = "userId", timevar = "id", direction = "wide")

# counts ------------------------------------------------------------------

count(permx,permx$permissionEnabled)

count_master <- data.frame(c(count(permx,permx$permissionEnabled & permx$permissionName == "APP_USAGE"),
                             count(permx,permx$permissionEnabled & permx$permissionName == "NOTIFICATION"),
                             count(permx,permx$permissionEnabled & permx$permissionName == "STORAGE"),
                             count(permx,permx$permissionEnabled & permx$permissionName == "CALL_LOG"),
                             count(permx,permx$permissionEnabled & permx$permissionName == "LOCATION")))

names(count_master)[c(2,4,6,8,10)]<-c("APP_USAGE","NOTIFICATION","STORAGE","CALL_LOG","LOCATION")

count_masterx <- count_master[c(-1,-3,-5,-7,-9)] 

# plots -------------------------------------------------------------------

g <- ggplot(perm_final2, aes(colnames))
# Number of cars in each class:
g + geom_histogram(aes(y = stat(count)))



# stats -------------------------------------------------------------------

# https://www.statpac.com/statistics-calculator/counts.htm
# https://stats.idre.ucla.edu/other/mult-pkg/whatstat/

# chisquare goodness of fit

chisq.test(count_masterx)








