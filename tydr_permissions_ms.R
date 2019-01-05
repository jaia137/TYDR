# libs -------------------------------------------------------------------

require(tidyr)
require(dplyr)
require(tidyverse)
require(data.table)

# library(RMySQL)
# library(DBI)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(data.table)


# data prep --------------------------------------------------------------------

setwd("/Users/shanti/Documents/GitHub/TYDR") #working directory

# perm <- read.csv("users-min-usage.pkl_clean.pkl_info.pkl.csv") #load minimum file

perm <- read.csv("users-with-permission-info.pkl_clean.pkl_info.pkl.csv") #load maximum file

perm_wide <- reshape(perm, idvar = "userId", timevar = "fieldId", direction = "wide") #reshape to wideformat

names(perm_wide)[c(3,5,7,9,11,13,15,17,19,21)]<-c("STORAGE","LOCATION","CALL_LOG","APP_USAGE","NOTIFICATION", "GENERAL", "GENDER", "AGE", "EDUCATION","BFI-2") #naming

perm_final <- perm_wide[c(-2,-4,-6,-8,-10,-12,-14,-16,-18,-20)] #delete redundant columns

perm_final_dis <- distinct(perm_final) #check double entries...

perm_final$APP_USAGE <- as.factor(perm_final$APP_USAGE) #factorize for easier later analysis
perm_final$NOTIFICATION <- as.factor(perm_final$NOTIFICATION)
perm_final$STORAGE <- as.factor(perm_final$STORAGE)
perm_final$CALL_LOG <- as.factor(perm_final$CALL_LOG)
perm_final$LOCATION <- as.factor(perm_final$LOCATION)

# single sums -------------------------------------------------------------

sum(perm_final$APP_USAGE ==1)
sum(perm_final$NOTIFICATION ==1)
sum(perm_final$STORAGE ==1)
sum(perm_final$CALL_LOG ==1)
sum(perm_final$LOCATION ==1)


# all sums ----------------------------------------------------------------

sum(perm_final$APP_USAGE ==1 & perm_final$NOTIFICATION==1 & perm_final$STORAGE ==1 & perm_final$CALL_LOG ==1 & perm_final$LOCATION==1)


# primary perm sums ------------------------------------------------------

sum(perm_final$APP_USAGE ==1 & perm_final$NOTIFICATION==1)

# secondary perm sums -----------------------------------------------------

sum(perm_final$STORAGE ==1 & perm_final$CALL_LOG ==1 & perm_final$LOCATION==1)


# filtering dataframes (ordered by count) ----------------------------------------------------

some_perm <- filter(perm_final, APP_USAGE ==1 | NOTIFICATION ==1 | STORAGE ==1 | CALL_LOG ==1 | LOCATION ==1)
# 664
no_perm <- filter(perm_final, APP_USAGE ==0 & NOTIFICATION ==0 & STORAGE ==0 & CALL_LOG ==0 & LOCATION ==0)
# 546
prim_perm <- filter(perm_final, perm_final$APP_USAGE ==1 & perm_final$NOTIFICATION==1)
# 506
second_perm <- filter(perm_final, perm_final$STORAGE ==1 & perm_final$CALL_LOG ==1 & perm_final$LOCATION==1)
# 468
all_perm <- filter(perm_final, APP_USAGE ==1 & NOTIFICATION ==1 & STORAGE ==1 & CALL_LOG ==1 & LOCATION ==1)
# 437


appusageonly_perm <- filter(perm_final, APP_USAGE ==1 & NOTIFICATION ==0 & STORAGE ==0 & CALL_LOG ==0 & LOCATION ==0) 
# 33
calllogonly_perm <- filter(perm_final, APP_USAGE ==0 & NOTIFICATION ==0 & STORAGE ==0 & CALL_LOG ==1 & LOCATION ==0)
# 15
locationonly_perm <- filter(perm_final, APP_USAGE ==0 & NOTIFICATION ==0 & STORAGE ==0 & CALL_LOG ==0 & LOCATION ==1)
# 9
notificationonly_perm <- filter(perm_final, APP_USAGE ==0 & NOTIFICATION ==1 & STORAGE ==0 & CALL_LOG ==0 & LOCATION ==0)
# 1
storageonly_perm <- filter(perm_final, APP_USAGE ==0 & NOTIFICATION ==0 & STORAGE ==1 & CALL_LOG ==0 & LOCATION ==0)
# 1


# filtering dataframes testing combinations ----------------------------------------------------

app_call_perm <- filter(perm_final, APP_USAGE ==1 & NOTIFICATION ==0 & STORAGE ==0 & CALL_LOG ==1 & LOCATION ==0) 
app_location_perm <- filter(perm_final, APP_USAGE ==1 & NOTIFICATION ==0 & STORAGE ==0 & CALL_LOG ==0 & LOCATION ==1) 
call_location_perm <- filter(perm_final, APP_USAGE ==0 & NOTIFICATION ==0 & STORAGE ==0 & CALL_LOG ==1 & LOCATION ==1) 

app_allin_perm <- filter(perm_final, APP_USAGE ==1 & NOTIFICATION ==0 & STORAGE ==0 & CALL_LOG ==1 & LOCATION ==1) 
# all pretty rdnm...\close analysis path...

# barplot (could be ggplotted for fanciness....)  -----------------------------------------------------------------------

B <- c(664,546,506,468,437)
barplot(B, main="GENERAL PERMISSIONS", xlab="PERMISSIONS", ylab="COUNT", names.arg=c("some","none","primary","secondary","all"), border="black", density=c(90, 70, 50, 40, 30))

BB <- c(33,15,9,1,1)
barplot(BB, main="SINGLE PERMISSIONS", xlab="PERMISSIONS", ylab="COUNT", names.arg=c("app_usage","call_log","location","notification","storage"), border="black", density=c(90, 70, 50, 40, 30))


####################
####################

# stats -------------------------------------------------------------------
# https://stats.idre.ucla.edu/other/mult-pkg/whatstat/

# 1) permissions all vs. none: gruppenunterschiede in den anderen variabeln? --------------------------

# filter out NAs, build df for contrasts ----------------------------------------------------------

all_perm_dis <- distinct(all_perm)
all_perm_na = drop_na(all_perm)

no_perm_dis <- distinct(no_perm)
no_perm_na = drop_na(no_perm)

all_perm_na$PERM <- rep(1,nrow(all_perm_na)) 

no_perm_na$PERM <- rep(0,nrow(no_perm_na)) 

#merge by row
df1 <- rbind(all_perm_na,no_perm_na)


# statistics --------------------------------------------------------------

chisq.test(table(df1$GENDER,df1$PERM))
# X-squared = 0.6377, df = 1, p-value = 0.4245

t.test(df1$AGE ~ df1$PERM)
# t = -2.699, df = 489.2, p-value = 0.007196
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -5.3037756 -0.8348927
# sample estimates:
#   mean in group 0 mean in group 1 
# 33.22594        36.29528 

wilcox.test(df1$AGE ~ df1$PERM)
# W = 25946, p-value = 0.005297


# hist (df1$AGE)
hist (no_perm_na$AGE)
hist (all_perm_na$AGE)


wilcox.test(df1$EDUCATION ~ df1$PERM)
# W = 25002, p-value = 0.0005349

# hist (df1$EDUCATION)
hist (no_perm_na$EDUCATION)
hist (all_perm_na$EDUCATION)

chisq.test(table(df1$GENERAL,df1$PERM))
# X-squared = 0.45639, df = 1, p-value = 0.4993

sum(no_perm_na$GENERAL==1)
sum(all_perm_na$GENERAL==1)

chisq.test(table(df1$`BFI-2`,df1$PERM))
# X-squared = 7.4197, df = 1, p-value = 0.006451

sum(no_perm_na$`BFI-2`==1)
sum(all_perm_na$`BFI-2`==1)


# 2) permissions primary vs. secondary: gruppenunterschiede in d --------

# filter out NAs, build df for contrast ----------------------------------------------------------

prim_perm_dis <- distinct(prim_perm)
prim_perm_na = drop_na(prim_perm)

second_perm_dis <- distinct(second_perm)
second_perm_na = drop_na(second_perm)

prim_perm_na$PERM <- rep(1,nrow(prim_perm_na)) 

second_perm_na$PERM <- rep(2,nrow(second_perm_na)) 

#merge by row
df2 <- rbind(prim_perm_na, second_perm_na)


# statistics --------------------------------------------------------------

chisq.test(table(df2$GENDER,df2$PERM))
# X-squared = 0.6377, df = 1, p-value = 0.4245

t.test(df2$AGE ~ df2$PERM)
# t = -2.699, df = 489.2, p-value = 0.007196
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -5.3037756 -0.8348927
# sample estimates:
#   mean in group 0 mean in group 1 
# 33.22594        36.29528 

wilcox.test(df2$AGE ~ df2$PERM)
# W = 25946, p-value = 0.005297


hist (df2$AGE)
hist (no_perm_na$AGE)
hist (all_perm_na$AGE)


wilcox.test(df2$EDUCATION ~ df2$PERM)
# W = 25002, p-value = 0.0005349

chisq.test(table(df2$GENERAL,df2$PERM))
# X-squared = 0.45639, df = 1, p-value = 0.4993

chisq.test(table(df2$`BFI-2`,df2$PERM))
# X-squared = 7.4197, df = 1, p-value = 0.006451


# 3) NAs vs non-NAs: gruppenunterschiede in den  permissions und --------


# identical except NOT STOR(514)

sum(perm_final$GENERAL ==1)
sum(perm_final$GENDER ==1, na.rm = TRUE)
sum(perm_final$AGE >1, na.rm = TRUE)
sum(perm_final$EDUCATION >=0, na.rm = TRUE)
sum(perm_final$`BFI-2` ==1)

# 612 general, age, edu?

sum(is.na(perm_final$GENDER))
sum(is.na(perm_final$AGE))
sum(is.na(perm_final$EDUCATION))

# pattern arising: either whole line, just NAs, or strange combos


# plots -------------------------------------------------------------------

g <- ggplot(perm_final, aes(colnames))
g
# Number of cars in each class:
g + geom_histogram(aes(y = stat(count)))
g










