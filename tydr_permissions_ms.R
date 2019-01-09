# libs -------------------------------------------------------------------

require(tidyr)
require(dplyr)
require(tidyverse)
require(data.table)
require(ggplot2)

# data prep --------------------------------------------------------------------

setwd("/Users/shanti/Documents/GitHub/TYDR") #working directory (don't panic, private repo + anonymous data ;) )

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

prim_perm_only <- filter(perm_final, perm_final$APP_USAGE ==1 & perm_final$NOTIFICATION==1 & perm_final$STORAGE ==0 & perm_final$CALL_LOG ==0 & perm_final$LOCATION==0)
# 15 (male only)
second_perm_only <- filter(perm_final, perm_final$APP_USAGE ==0 & perm_final$NOTIFICATION==0 & perm_final$STORAGE ==1 & perm_final$CALL_LOG ==1 & perm_final$LOCATION==1)
# 10 (male only)
#random effect...

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
#random effect...


na_perm <-filter(perm_final, GENERAL==0 & is.na(GENDER) & is.na(AGE) & is.na(EDUCATION) &`BFI-2`==0)

nano_perm <-filter(perm_final, GENERAL==1 & !is.na(GENDER) & !is.na(AGE) & !is.na(EDUCATION) &`BFI-2`==1)


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
#####
# stats -------------------------------------------------------------------
# https://stats.idre.ucla.edu/other/mult-pkg/whatstat/

# 1) permissions all vs. none: gruppenunterschiede in den anderen variabeln? --------------------------

# filter out NAs, build df for contrasts ----------------------------------------------------------

all_perm_na = drop_na(all_perm)

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

#####
# 2) a) permissions primary vs. secondary (all): group differences --------

# filter out NAs, build df for contrast ----------------------------------------------------------

prim_perm_dis <- distinct(prim_perm)
prim_perm_na = drop_na(prim_perm)

second_perm_dis <- distinct(second_perm)
second_perm_na = drop_na(second_perm)

prim_perm_na$PERM <- rep(1,nrow(prim_perm_na)) 
  
second_perm_na$PERM <- rep(2,nrow(second_perm_na)) 

#merge by row
df2 <- rbind(prim_perm_na, second_perm_na)


# statistics (no significant results, still report ?!) --------------------------------------------------------------

chisq.test(table(df2$GENDER,df2$PERM))
# X-squared = 0.31994, df = 1, p-value = 0.5716

t.test(df2$AGE ~ df2$PERM)
# t = -0.33778, df = 562.61, p-value = 0.7357
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.455106  1.734613
# sample estimates:
#   mean in group 1 mean in group 2 
# 35.93537        36.29562 

wilcox.test(df2$AGE ~ df2$PERM)
# W = 39668, p-value = 0.7551

hist (df2$AGE)
hist (prim_perm_na$AGE)
hist (second_perm_na$AGE)

wilcox.test(df2$EDUCATION ~ df2$PERM)
# W = 40628, p-value = 0.8546

chisq.test(table(df2$GENERAL,df2$PERM))
# X-squared = 0.70423, df = 1, p-value = 0.4014

chisq.test(table(df2$`BFI-2`,df2$PERM))
# X-squared = 0.062042, df = 1, p-value = 0.8033

#####
# 2) b) permissions primary vs. secondary (exclusive): group differences --------

# filter out NAs, build df for contrast ----------------------------------------------------------

prim_perm_only_na = drop_na(prim_perm_only)
second_perm_only_na = drop_na(second_perm_only)

prim_perm_only_na$PERM <- rep(1,nrow(prim_perm_only_na)) 
second_perm_only_na$PERM <- rep(2,nrow(second_perm_only_na)) 

#merge by row
df2b <- rbind(prim_perm_only_na, second_perm_only_na)


# statistics (no significant results, still report ?!) --------------------------------------------------------------

chisq.test(table(df2b$GENDER,df2b$PERM))
# X-squared = 0, df = 1, p-value = 1 #lol

t.test(df2b$AGE ~ df2b$PERM)

wilcox.test(df2b$AGE ~ df2b$PERM)

hist (df2b$AGE)
hist (prim_perm_only_na$AGE)
hist (second_perm_only_na$AGE)

wilcox.test(df2b$EDUCATION ~ df2b$PERM)

chisq.test(table(df2b$GENERAL,df2b$PERM))

chisq.test(table(df2b$`BFI-2`,df2b$PERM))

#####
# 3) NAs vs non-NAs: gruppenunterschiede --------

na_perm$NAS <- rep(1,nrow(na_perm)) 
nano_perm$NAS <- rep(2,nrow(nano_perm)) 

#merge by row
df3 <- rbind(na_perm, nano_perm)

#check distinct
df3x <- distinct(df3)
rm(df3x)


# statistics --------------------------------------------------------------

chisq.test(table(df3$APP_USAGE,df3$NAS))
# 
sum(df3$APP_USAGE==1 & df3$NAS ==1)
sum(df3$APP_USAGE==0 & df3$NAS ==1)
sum(df3$APP_USAGE==1 & df3$NAS ==2)
sum(df3$APP_USAGE==0 & df3$NAS ==2)

chisq.test(table(df3$STORAGE,df3$NAS))
# 
sum(df3$STORAGE==1 & df3$NAS ==1)
sum(df3$STORAGE==0 & df3$NAS ==1)
sum(df3$STORAGE==1 & df3$NAS ==2)
sum(df3$STORAGE==0 & df3$NAS ==2)

chisq.test(table(df3$CALL_LOG,df3$NAS))
# 
sum(df3$CALL_LOG==1 & df3$NAS ==1)
sum(df3$CALL_LOG==0 & df3$NAS ==1)
sum(df3$CALL_LOG==1 & df3$NAS ==2)
sum(df3$CALL_LOG==0 & df3$NAS ==2)

chisq.test(table(df3$NOTIFICATION,df3$NAS))
# 
sum(df3$NOTIFICATION==1 & df3$NAS ==1)
sum(df3$NOTIFICATION==0 & df3$NAS ==1)
sum(df3$NOTIFICATION==1 & df3$NAS ==2)
sum(df3$NOTIFICATION==0 & df3$NAS ==2)

chisq.test(table(df3$LOCATION,df3$NAS))
# 
sum(df3$LOCATION==1 & df3$NAS ==1)
sum(df3$LOCATION==0 & df3$NAS ==1)
sum(df3$LOCATION==1 & df3$NAS ==2)
sum(df3$LOCATION==0 & df3$NAS ==2)

# effekt immer getrieben von NA und keine permission (n=300+). leute mit keiner permission tendieren zu NA oder ungekehrt. ueberrascht auch nicht.




chisq.test(table(df3$GENERAL,df3$NAS))
#
sum(df3$GENERAL==1 & df3$NAS ==1)
sum(df3$GENERAL==0 & df3$NAS ==1)
sum(df3$GENERAL==1 & df3$NAS ==2)
sum(df3$GENERAL==0 & df3$NAS ==2)

chisq.test(table(df3$`BFI-2`,df3$NAS))
# 
sum(df3$`BFI-2`==1 & df3$NAS ==1)
sum(df3$`BFI-2`==0 & df3$NAS ==1)
sum(df3$`BFI-2`==1 & df3$NAS ==2)
sum(df3$`BFI-2`==0 & df3$NAS ==2)

#leute die keine permissions geben fuellen zu hundert prozent keinen questionnaire aus. hmm


#########################
######under construction....



# plots (tbd...) -------------------------------------------------------------------












