library(haven)
library(ggplot2)
library(dplyr)
library(corrr)
library(tidyr)
library(gdata)
library(polycor)
age5 <- read.delim("~/MRC DTP/Analysis_2 Local/Age 5/UKDA-2699-tab/tab/f699c.tab")
age5_derived <- read.delim("~/MRC DTP/Analysis_2 Local/Age 5/UKDA-2699-tab/tab/bcs2derived.tab")
age10 <- read.delim("~/MRC DTP/Analysis_2 Local/Age 10/UKDA-3723-tab/tab/sn3723.tab")
age10_derived <- read.delim("~/MRC DTP/Analysis_2 Local/Age 10/UKDA-3723-tab/tab/bcs3derived.tab")
age16 <- read.delim("~/MRC DTP/Analysis_2 Local/Age_16/UKDA-3535-tab/tab/bcs7016x.tab")
age16_derived <- read.delim("~/MRC DTP/Analysis_2 Local/Age_16/UKDA-3535-tab/tab/bcs4derived.tab")
age42 <- read.delim("C:/Users/JohnM/Documents/MRC DTP/Analysis_2 Local/Age_42/UKDA-7473-tab/tab/bcs70_2012_flatfile.tab")
age42_employment <- read.delim("C:/Users/JohnM/Documents/MRC DTP/Analysis_2 Local/Age_42/UKDA-7473-tab/tab/bcs70_2012_employment.tab")
age42_derived <- read.delim("C:/Users/JohnM/Documents/MRC DTP/Analysis_2 Local/Age_42/UKDA-7473-tab/tab/bcs70_2012_derived.tab")
BCS70_main <- read.delim("C:/Users/JohnM/Documents/MRC DTP/Analysis_1 Local/HDrive/8547tab_97520C7FE3257A5723A880BF723E4FB5_V1/UKDA-8547-tab/tab/bcs_age46_main.tab") 
BCSAvg <- read.delim("~/MRC DTP/Analysis_1 Local/HDrive/PA/tab/bcs_age46_activpal_avg.tab")
Covariates1 <- read_dta("C:/Users/JohnM/Downloads/temp_CVD_Stroke.dta")
Covariates2 <- read_dta("C:/Users/JohnM/Downloads/temp_hypertensionmeds.dta")
Covariates3 <- read_dta("C:/Users/JohnM/Downloads/temp_hypertensiondata.dta")
source("C:/Users/JohnM/Documents/MRC DTP/Analysis_2 Local/Age_10_cleaner.R")
source("C:/Users/JohnM/Documents/MRC DTP/Analysis_2 Local/Age_16_cleaner.R")
source("C:/Users/JohnM/Documents/MRC DTP/Analysis_2 Local/Age_42_cleaner.R")
source("C:/Users/JohnM/Documents/MRC DTP/Analysis_2 Local/Age_46_cleaner.R")
source("C:/Users/JohnM/Documents/MRC DTP/Analysis_2 Local/age_5_cleaner.R")
source("C:/Users/JohnM/Documents/MRC DTP/Analysis_2 Local/Cog_Rescaler_Funct.R")
age_5_vars_output <- age_5_cleaner(age5, age5_derived)
age_10_vars_output <- age10_output(age10, age10_derived)
age_10_vars <- as.data.frame(age_10_vars_output[1])
age_10_covars <- as.data.frame(age_10_vars_output[2])
age_16_vars_output <- age16_output(age16, age16_derived)
age_16_vars <- as.data.frame(age_16_vars_output[1])
age_16_covars <- as.data.frame(age_16_vars_output[2])
age_42_vars_output <- age42_output(age42, age42_derived, age42_employment)
age_42_vars <- as.data.frame(age_42_vars_output[1])
age_42_covars <- as.data.frame(age_42_vars_output[2])
age_46_vars <- age46_output(BCS70_main, BCSAvg, Covariates1, Covariates2, Covariates3)
age_46_Activities <- as.data.frame(age_46_vars[1])
age_46_Leisure <- as.data.frame(age_46_vars[2])
age_46_Occupation <- as.data.frame(age_46_vars[3])
age_46_PACogCov <- as.data.frame(age_46_vars[4])
age_46_Transport <- as.data.frame(age_46_vars[5])
age_46_SportCats <- as.data.frame(age_46_vars[6])
rm(age10, age10_derived, age42, age_42_vars_output, age16_derived, age42_derived, age_16_vars_output, age16, BCS70_main, BCSAvg, Covariates1, Covariates2, Covariates3)
##--Merge---------------------------------------------------------------------------------------
merge46 <- merge(age_46_PACogCov, age_46_SportCats, by=c("ID"))
merge46_42 <- merge(merge46, age_42_vars, by=c("ID"), all=T)
merge46_42_16 <- merge(merge46_42, age_16_vars, by=c("ID"), all=T)
merge46_42_16_10 <- merge(merge46_42_16, age_10_vars, by=c("ID"), all=T)
merge46_42_16_10 <- merge(merge46_42_16_10, age_10_covars, by=c("ID"), all=T)
merge46_42_16_10 <- merge(merge46_42_16_10, age_16_covars, by=c("ID"), all=T)
merge46_42_16_10 <- merge(merge46_42_16_10, age_42_covars, by=c("ID"), all=T)
#merge46_16_10 <- merge(merge46_16, age_10_vars, by=c("ID"), all=T)

##--Drop Cog Missing entirely----------------------------------------------------------------------------
df <- merge46_42_16_10[!(is.na(merge46_42_16_10$named) | is.na(merge46_42_16_10$missed) | is.na(merge46_42_16_10$speed) | is.na(merge46_42_16_10$immediate) | is.na(merge46_42_16_10$delayed)) ,]
##--Drop min.3 PA vars-Subsetter_Function-----------------------------------------------------------------------------
CleanMinimumPA <- function(samplesubsetsport, df) {
samplesubsetsport$na_count <- apply(is.na(samplesubsetsport), 1, sum)
samplesubsetsport <- samplesubsetsport[!(samplesubsetsport$na_count>=3),]
samplesubsetsport <- select(samplesubsetsport, select=-c(na_count, select2, select3, select4, select5))
samplesubsetsport <- dplyr::rename(samplesubsetsport, ID=select1)
df <- merge(df, samplesubsetsport, by=c("ID"))
return(df) }
##--Drop min.3 PA vars------------------------------------------------------------------------------
samplesubsetsport <- select(df, select=c(ID, sport_10, Sports_16, Sportstrichotomous42, SportsTrichotomy46))
df <- CleanMinimumPA(samplesubsetsport, df)
#samplesubsetswim <- select(df, select=c(ID, swim_10, Swimming16, SwimTrichotomy, SwimTrichotomy46))
#df <- CleanMinimumPA(samplesubsetswim, df)
#samplesubsetcycle <- select(df, select=c(ID, bike_10, Cycling16, CyclingTrichotomy, CycleTrichotomy46))
#df <- CleanMinimumPA(samplesubsetcycle, df)
#samplesubsetwalk <- select(df, select=c(ID, walk_10, Walking16, WalkTrichotomy, WalkTrichotomy46))
#df <- CleanMinimumPA(samplesubsetwalk, df)
##----RM unused df's----## 
rm(merge46, merge46_42, merge46_42_16, merge46_42_16_10, age10_output, age16_output, age42_output, age46_output, CleanMinimumPA)
df$openskill_46 <- dplyr::recode(df$openskill_46, '-99'=NA_real_, '1'=1, '2'=2, '3'=3, '4'=4)
df$closedskill_46 <- dplyr::recode(df$closedskill_46, '-99'=NA_real_, '1'=1, '2'=2, '3'=3, '4'=4)
##Sport Summary - M
Sport_M <- subset(df, select=c("ID", "sport_10", "Sports_16", "Sportstrichotomous42", "SportsTrichotomy46", "executive_z"))
Sport_M$Early <- NA
Sport_M$Proximal <- NA
for (i in 1:length(Sport_M$ID)) {
  if ((Sport_M$sport_10[i]>=2 & Sport_M$Sports_16[i]>=2)  & (!is.na(Sport_M$sport_10[i]) & !is.na(Sport_M$Sports_16[i])) ) {
    Sport_M$Early[i] <- 1}
  else if (is.na(Sport_M$Sports_16[i]) & is.na(Sport_M$sport_10[i])) {
    Sport_M$Proximal[i] <- NA_real_ }
  else { Sport_M$Early[i] <- 0 
  }
}
for (i in 1:length(Sport_M$ID)) {
  if ((Sport_M$Sportstrichotomous42[i]>=2 | Sport_M$SportsTrichotomy46[i]>=2) & (!is.na(Sport_M$Sportstrichotomous42[i]) & !is.na(Sport_M$SportsTrichotomy46[i])) ) {
    Sport_M$Proximal[i] <- 1} 
  else if (is.na(Sport_M$Sportstrichotomous42[i]) & is.na(Sport_M$SportsTrichotomy46[i])) {
    Sport_M$Proximal[i] <- NA_real_ }
  else { Sport_M$Proximal[i] <- 0 }
}
Sport_M$Count_Sport <- 0
for (i in 1:length(Sport_M$ID)) {
  if ((Sport_M$Early[i]==1 & Sport_M$Proximal[i]==1) & (!is.na(Sport_M$Early[i]) & !is.na(Sport_M$Proximal[i])) ) {
    Sport_M$Count_Sport[i] <- 3} 
  else if ((Sport_M$Proximal[i]==1 & Sport_M$Early[i]==0) & (!is.na(Sport_M$Early[i]) & !is.na(Sport_M$Proximal[i])) ) {
    Sport_M$Count_Sport[i] <- 2}
  else if ((Sport_M$Early[i]==1 & Sport_M$Proximal[i]==0) & (!is.na(Sport_M$Early[i]) & !is.na(Sport_M$Proximal[i])) ) {
    Sport_M$Count_Sport[i] <- 1}
  else if (is.na(Sport_M$Early[i]) & is.na(Sport_M$Proximal[i])) {
    Sport_M$Count_Sport[i] <- NA_real_}
}
Sport_M <- subset(Sport_M, select=c("ID", "Count_Sport"))
df <- merge(Sport_M, df, by=c("ID"), all=T)

df_M <- df[which(df$Sex==1),]
df_F <- df[which(df$Sex==2),]

age10_derived <- read.delim("~/MRC DTP/Analysis_2 Local/Age 10/UKDA-3723-tab/tab/bcs3derived.tab")
dfSEC <- subset(age10_derived, select=c("BD3PSOC", "BCSID"))
dfSEC <- rename(dfSEC, ID='BCSID', SEC_10="BD3PSOC")

dfSEC$SEC_102 <- 99
for (i in 1:length(dfSEC$ID)) {
  if (dfSEC$SEC_10[i]==-1 | is.na(dfSEC$SEC_10[i])) {
    dfSEC$SEC_102[i] <- NA
  }
  else { dfSEC$SEC_102[i] <- dfSEC$SEC_10[i] }
} 


df <- merge(df, dfSEC, by="ID", all.x=TRUE)
df <- df[!duplicated(df$ID),]


##Modelling PA - Summary##

##Sport Summary - M
Sport_M <- subset(df, select=c("ID", "sport_10", "Sports_16", "Sportstrichotomous42", "SportsTrichotomy46", "executive_z"))
Sport_M$Early <- NA
Sport_M$Proximal <- NA
for (i in 1:length(Sport_M$ID)) {
  if ((Sport_M$sport_10[i]>=1 & !is.na(Sport_M$sport_10[i])) | (Sport_M$Sports_16[i]>=1  & !is.na(Sport_M$Sports_16[i])) ) {
    Sport_M$Early[i] <- 1}
  else if (is.na(Sport_M$Sports_16[i]) & is.na(Sport_M$sport_10[i])) {
    Sport_M$Proximal[i] <- NA_real_ }
  else { Sport_M$Early[i] <- 0 
  }
}
for (i in 1:length(Sport_M$ID)) {
  if ((Sport_M$Sportstrichotomous42[i]>=1 & !is.na(Sport_M$Sportstrichotomous42[i])) | (Sport_M$SportsTrichotomy46[i]>=1  & !is.na(Sport_M$SportsTrichotomy46[i])) ) {
    Sport_M$Proximal[i] <- 1} 
  else if (is.na(Sport_M$Sportstrichotomous42[i]) & is.na(Sport_M$SportsTrichotomy46[i])) {
    Sport_M$Proximal[i] <- NA_real_ }
  else { Sport_M$Proximal[i] <- 0 }
}
Sport_M$Count_Sport <- 0
for (i in 1:length(Sport_M$ID)) {
  if ((Sport_M$Early[i]==1 & !is.na(Sport_M$Early[i])) | (Sport_M$Proximal[i]==1  & !is.na(Sport_M$Proximal[i])) ) {
    Sport_M$Count_Sport[i] <- 3} 
  else if ((Sport_M$Proximal[i]==1 & Sport_M$Early[i]==0) & (!is.na(Sport_M$Early[i]) & !is.na(Sport_M$Proximal[i])) ) {
    Sport_M$Count_Sport[i] <- 2}
  else if ((Sport_M$Early[i]==1 & Sport_M$Proximal[i]==0) & (!is.na(Sport_M$Early[i]) & !is.na(Sport_M$Proximal[i])) ) {
    Sport_M$Count_Sport[i] <- 1}
  else if (is.na(Sport_M$Early[i]) & is.na(Sport_M$Proximal[i])) {
    Sport_M$Count_Sport[i] <- NA_real_}
}

Sport_M <- subset(Sport_M, select=c("ID", "Count_Sport"))
names(Sport_M) <- c("ID", "Count_Sport_Med_High") 
df <- merge(Sport_M, df, by="ID", all=T)


#Modelling_EarlyLife <- subset(df, select=c("ID", "Count_Sport", "executive_z", "memory_z", "Sex", "disab"))


Sport_M <- subset(df, select=c("ID", "sport_10", "Sports_16", "Sportstrichotomous42", "SportsTrichotomy46", "executive_z"))
Sport_M$Early <- NA
Sport_M$Proximal <- NA

for (i in 1:length(Sport_M$ID)) {
  if ((Sport_M$sport_10[i]>=2 & !is.na(Sport_M$sport_10[i]) | (Sport_M$Sports_16[i]>=2) & !is.na(Sport_M$Sports_16[i]))) {
    Sport_M$Early[i] <- 1}
  else if (is.na(Sport_M$Sports_16[i]) & is.na(Sport_M$sport_10[i])) {
    Sport_M$Early[i] <- NA_real_ }
  else { Sport_M$Early[i] <- 0 
  }
}
for (i in 1:length(Sport_M$ID)) {
  if ((Sport_M$Sportstrichotomous42[i]>=2 & !is.na(Sport_M$Sportstrichotomous42[i])) | (Sport_M$SportsTrichotomy46[i]>=2) & !is.na(Sport_M$SportsTrichotomy46[i])) {
    Sport_M$Proximal[i] <- 1 } 
  else if (is.na(Sport_M$Sportstrichotomous42[i]) & is.na(Sport_M$SportsTrichotomy46[i])) {
    Sport_M$Proximal[i] <- NA_real_ }
  else { Sport_M$Proximal[i] <- 0 }
}
Sport_M$Count_Sport <- 0
for (i in 1:length(Sport_M$ID)) {
  if ((Sport_M$Early[i]==1 & Sport_M$Proximal[i]==1) & (!is.na(Sport_M$Early[i]) & !is.na(Sport_M$Proximal[i])) ) {
    Sport_M$Count_Sport[i] <- 3} 
  else if ((Sport_M$Proximal[i]==1 & Sport_M$Early[i]==0) & (!is.na(Sport_M$Early[i]) & !is.na(Sport_M$Proximal[i])) ) {
    Sport_M$Count_Sport[i] <- 2}
  else if ((Sport_M$Early[i]==1 & Sport_M$Proximal[i]==0) & (!is.na(Sport_M$Early[i]) & !is.na(Sport_M$Proximal[i])) ) {
    Sport_M$Count_Sport[i] <- 1}
  else if (is.na(Sport_M$Early[i]) | is.na(Sport_M$Proximal[i])) {
    Sport_M$Count_Sport[i] <- NA_real_}
}


#Loop checker#
#Sport_M %>% filter(Count_Sport==3) %>% head()
#Sport_M %>% filter(ID=="B10038B") %>% print()  

Sport_M <- subset(Sport_M, select=c("ID", "Count_Sport"))
names(Sport_M) <- c("ID", "Count_Sport_High") 
df <- merge(Sport_M, df, by=c("ID"), all=T)

##Rescale Cog Vars off raw scores##
df <- cog_rescaler(df)

gmerge <- subset(age_5_vars_output, select=c("bcsid", "g"))
names(gmerge) <- c("ID", "g5")
gmerge <- merge(df, gmerge, by="ID", all.x=T)

gdata::keep(df, gmerge, sure=T)
##-----------End_of_Clean--------------##
df_MI <- subset(gmerge, select=c('ID', 'Sex.x', 'sport_10','RHR', 'Sports_16', 'Sportstrichotomous42', 'SportsTrichotomy46', 'disability_10', 'g5', 'SEC_102', 'SEC.x', 'BMI.x', 'Diabetes.x', 'AlcRisk.x', 'Smoker.x', 'CVDHTN', 'executive_z2', 'composite_z2', 'memory_z2', 'Open_Skill_Tri16','Open_Skill_Tri42', 'Open_Skill_Tri46', 'Closed_Skill_Tri42', 'Closed_Skill_Tri16', 'Closed_Skill_Tri46', #'B10HDL'# , #'B10CHOL'#
                             'B10HBA1C', 'nonHDLratio', 'BP', 'Admissions', 'Count_Sport_High', 'Education'))

#df_MI_Proximal_Distal <- subset(df, select=c('ID', 'Sex.x', 'RHR', 'disability_10', 'g', 'SEC_102', 'SEC.x', 'BMI.x', 'Diabetes.x', 'AlcRisk.x', 'Smoker.x', 'CVDHTN', 'Count_Sport_High', 'executive_z', 'memory_z'))
write.table(df_MI, "c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\df_MI_09_05.tab", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)































