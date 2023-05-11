age42_output <- function(df1, df2, df3) {
##-9.0 Not Stated ; -8.0 Multicode; -1.0 Paper self completion not received; 1.0 Every day; 2.0 4-5 days a week; 3.0 2-3 days a week
#4.0	Once a week; 5.0	2-3 times a month; 6.0 Less than monthly; 7.0 Not in the last 12 months


age42_vars <- as.data.frame(age42$BCSID)
age42_vars <- dplyr::rename(age42_vars, BCSID='age42$BCSID',)
age42_vars$sport <- age42$B9SCQ2H
age42_vars$martialarts_boxing_wrestling <- age42$B9SCQ2I
age42_vars$watersports <- age42$B9SCQ2J
age42_vars$horseriding <- age42$B9SCQ2K
age42_vars$yoga_pilates <- age42$B9SCQ2L
age42_vars$golf <- age42$B9SCQ2M
age42_vars$Skiing <- age42$B9SCQ2N
age42_vars$Othersporting <- age42$B9SCQ2O
age42_vars$Raquet <- age42$B9SCQ2G
age42_vars$Walk <- age42$B9SCQ2F
age42_vars$Jogging <- age42$B9SCQ2E
age42_vars$Dance <- age42$B9SCQ2D
age42_vars$Cycling <- age42$B9SCQ2C
age42_vars$Swimming_Diving <- age42$B9SCQ2B
age42_vars$Fitness_gym_conditioning <- age42$B9SCQ2A

#table(age_42_vars$Fitness_gym_conditioning)
Sports <- subset(age42_vars, select= -c(Walk))
#row_sub = apply(Sports, 1, function(row) all(row !=-1 ))


for (i in 1:length(Sports$BCSID)) {
  for (j in 2:ncol(Sports)) {
    if (Sports[i,j]==1 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 7
    }
    else if (Sports[i,j]==2 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 6
    }
    else if (Sports[i,j]==3 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 5
    }
    else if (Sports[i,j]==4 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 4
    }
    else if (Sports[i,j]==5 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 3
    }
    else if (Sports[i,j]==6 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 2
    }
    else if (Sports[i,j]==7 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 1
    }
    else if ((Sports[i,j]==-1 | Sports[i,j]==-8)  & !is.na(Sports[i,j])) {
      Sports[i,j] <- NA_real_
    }
    else if (Sports[i,j]==-9 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 0
    }
  }}


OpenSkill <- subset(Sports, select= -c(yoga_pilates, Fitness_gym_conditioning, Swimming_Diving, Jogging, Dance, Othersporting 
                                      , golf, Skiing, Cycling, horseriding))

ClosedSkill <- subset(Sports, select= -c(sport, martialarts_boxing_wrestling, watersports, Raquet))



Sportsfreq <- Sports
Sportsfreqlist <- vector("double", 9840)
for (i in 1:length(Sports$BCSID)) {
  for (j in 2:ncol(Sportsfreq)) {
    if (Sportsfreq[i,j]==7 & !is.na(Sportsfreq[i,j])) {
      Sportsfreqlist[i] <- 7
    }
    else if (Sportsfreq[i,j]==6 & !is.na(Sportsfreq[i,j]) & Sportsfreqlist[i]<7) {
      Sportsfreqlist[i] <- 6
    }
    else if (Sportsfreq[i,j]==5 & !is.na(Sportsfreq[i,j]) & Sportsfreqlist[i]<6) {
      Sportsfreqlist[i] <- 5
    }
    else if (Sportsfreq[i,j]==4 & !is.na(Sportsfreq[i,j]) & Sportsfreqlist[i]<5) {
      Sportsfreqlist[i] <- 4
    }
    else if (Sportsfreq[i,j]==3 & !is.na(Sportsfreq[i,j]) & Sportsfreqlist[i]<4) {
      Sportsfreqlist[i] <- 3
    }
    else if (Sportsfreq[i,j]==2 & !is.na(Sportsfreq[i,j]) & Sportsfreqlist[i]<3) {
      Sportsfreqlist[i] <- 2
    }
    else if (Sportsfreq[i,j]==1 & !is.na(Sportsfreq[i,j]) & Sportsfreqlist[i]<2) {
      Sportsfreqlist[i] <- 1
    }
    else if (Sportsfreq[i,j]==0 & !is.na(Sportsfreq[i,j]) & Sportsfreqlist[i]<1) {
      Sportsfreqlist[i] <- 0.1
    }
    else if (is.na(Sportsfreq[i,j]) & (Sportsfreqlist[i]==0 | is.na(Sportsfreqlist[i]))) {
      Sportsfreqlist[i] <- -99
    }
  }}

ClosedSkillFreq <- ClosedSkill
ClosedSkillfreqlist <- vector("double", 9840)
for (i in 1:length(ClosedSkillFreq$BCSID)) {
  for (j in 2:ncol(ClosedSkillFreq)) {
    if (ClosedSkillFreq[i,j]>=4 & !is.na(ClosedSkillFreq[i,j])) {
      ClosedSkillfreqlist[i] <- 4
    }
    else if (ClosedSkillFreq[i,j]==6 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]<7) {
      ClosedSkillfreqlist[i] <- 6
    }
    else if (ClosedSkillFreq[i,j]==5 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]<6) {
      ClosedSkillfreqlist[i] <- 5
    }
    else if (ClosedSkillFreq[i,j]==4 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]<5) {
      ClosedSkillfreqlist[i] <- 4
    }
    else if (ClosedSkillFreq[i,j]==3 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]<4) {
      ClosedSkillfreqlist[i] <- 3
    }
    else if (ClosedSkillFreq[i,j]==2 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]<3) {
      ClosedSkillfreqlist[i] <- 2
    }
    else if (ClosedSkillFreq[i,j]==1 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]<2) {
      ClosedSkillfreqlist[i] <- 1
    }
    else if (ClosedSkillFreq[i,j]==0 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]<1) {
      ClosedSkillfreqlist[i] <- 0.1
    }
    else if (is.na(ClosedSkillFreq[i,j]) & (ClosedSkillfreqlist[i]==0 | is.na(ClosedSkillfreqlist[i]))) {
      ClosedSkillfreqlist[i] <- -99
    }
  }}



OpenSkillFreq <- OpenSkill
OpenSkillfreqlist <- vector("double", 9840)
for (i in 1:length(OpenSkillFreq$BCSID)) {
  for (j in 2:ncol(OpenSkillFreq)) {
    if (OpenSkillFreq[i,j]==7 & !is.na(OpenSkillFreq[i,j])) {
      OpenSkillfreqlist[i] <- 7
    }
    else if (OpenSkillFreq[i,j]==6 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<7) {
      OpenSkillfreqlist[i] <- 6
    }
    else if (OpenSkillFreq[i,j]==5 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<6) {
      OpenSkillfreqlist[i] <- 5
    }
    else if (OpenSkillFreq[i,j]==4 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<5) {
      OpenSkillfreqlist[i] <- 4
    }
    else if (OpenSkillFreq[i,j]==3 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<4) {
      OpenSkillfreqlist[i] <- 3
    }
    else if (OpenSkillFreq[i,j]==2 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<3) {
      OpenSkillfreqlist[i] <- 2
    }
    else if (OpenSkillFreq[i,j]==1 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<2) {
      OpenSkillfreqlist[i] <- 1
    }
    else if (OpenSkillFreq[i,j]==0 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<1) {
      OpenSkillfreqlist[i] <- 0.1
    }
    else if ( is.na(OpenSkillFreq[i,j]) & (OpenSkillfreqlist[i]==0 | is.na(OpenSkillfreqlist[i]))) {
      OpenSkillfreqlist[i] <- -99
    }
  }}

age42_vars$openskill_freq_42 <-OpenSkillfreqlist
age42_vars$closedskill_freq_42 <-ClosedSkillfreqlist

##-9.0 Not Stated ; -8.0 Multicode; -1.0 Paper self completion not received; 1.0 Every day; 2.0 4-5 days a week; 3.0 2-3 days a week
#4.0	Once a week; 5.0	2-3 times a month; 6.0 Less than monthly; 7.0 Not in the last 12 months
##7 -Daily ##6 4-5/week; ##5  2-3/week ##4  1/week ##3  2-3/month ##2  less often ##1 not in last 12m ## 

OpenSkillFreq <- OpenSkill
OpenSkillfreqlist <- vector("double", 9840)
for (i in 1:length(OpenSkillFreq$BCSID)) {
  for (j in 2:ncol(OpenSkillFreq)) {
    if (OpenSkillFreq[i,j]>=4 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]>=3  &  !is.na(OpenSkillfreqlist[i])) {
      OpenSkillfreqlist[i] <- 4
    }
    else if (OpenSkillFreq[i,j]>=4 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<3) {
      OpenSkillfreqlist[i] <- 3
    }
    else if (OpenSkillFreq[i,j]>=2 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]==2) {
      OpenSkillfreqlist[i] <- 2.5
    }
    else if (OpenSkillFreq[i,j]>=2 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<2) {
      OpenSkillfreqlist[i] <- 2
    }
    else if (OpenSkillFreq[i,j]<2 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<2) {
      OpenSkillfreqlist[i] <- 1
    }
    else if ((is.na(OpenSkillFreq[i,j])) & ((OpenSkillfreqlist[i]==0 | is.na(OpenSkillfreqlist[i])))) {
      OpenSkillfreqlist[i] <- -99
    }
  }}


ClosedSkillFreq <- ClosedSkill
ClosedSkillfreqlist <- vector("double", 9840)
for (i in 1:length(ClosedSkillFreq$BCSID)) {
  for (j in 2:ncol(ClosedSkillFreq)) {
    if (ClosedSkillFreq[i,j]>=4 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]>=3  &  !is.na(ClosedSkillfreqlist[i])) {
      ClosedSkillfreqlist[i] <- 4
    }
    else if (ClosedSkillFreq[i,j]>=4 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]<3) {
      ClosedSkillfreqlist[i] <- 3
    }
    else if (ClosedSkillFreq[i,j]>=2 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]==2) {
      ClosedSkillfreqlist[i] <- 2.5
    }
    else if (ClosedSkillFreq[i,j]>=2 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]<2) {
      ClosedSkillfreqlist[i] <- 2
    }
    else if (ClosedSkillFreq[i,j]<2 & !is.na(ClosedSkillFreq[i,j]) & ClosedSkillfreqlist[i]<2) {
      ClosedSkillfreqlist[i] <- 1
    }
    else if ((is.na(ClosedSkillFreq[i,j])) & ((ClosedSkillfreqlist[i]==0 | is.na(ClosedSkillfreqlist[i])))) {
      ClosedSkillfreqlist[i] <- -99
    }
  }}

age42_vars$openskill_42 <-OpenSkillfreqlist
age42_vars$closedskill_42 <-ClosedSkillfreqlist




#0 - none -> <monthly (0.1,1);1 - 2-3/m - 1/week (2,3,); 2 >weekly (7,6,5,4) ?
SportsTrichotomy <- dplyr::recode(Sportsfreqlist, '-99'=NA_real_, '0.1'=0, '1'=0, '2'=1, '3'=1, '4'=2, '5'=2, '6'=2, '7'=2)
Sportsfreqlist <- dplyr::recode(Sportsfreqlist, '0.1'=0)



##Team/Partner sport:
TeamSports <- subset(Sports, select= -c(watersports, horseriding, Jogging, Skiing, Swimming_Diving, Othersporting, yoga_pilates, Fitness_gym_conditioning))
TeamSportsfreq <- TeamSports
TeamSportsfreqlist <- vector("double", 9840)

for (i in 1:length(TeamSports$BCSID)) {
  for (j in 2:ncol(TeamSportsfreq)) {
    if (TeamSportsfreq[i,j]==7 & !is.na(TeamSportsfreq[i,j])) {
      TeamSportsfreqlist[i] <- 7
    }
    else if (TeamSportsfreq[i,j]==6 & !is.na(TeamSportsfreq[i,j]) & TeamSportsfreqlist[i]<7) {
      Sportsfreqlist[i] <- 6
    }
    else if (TeamSportsfreq[i,j]==5 & !is.na(TeamSportsfreq[i,j]) & TeamSportsfreqlist[i]<6) {
      TeamSportsfreqlist[i] <- 5
    }
    else if (TeamSportsfreq[i,j]==4 & !is.na(TeamSportsfreq[i,j]) & TeamSportsfreqlist[i]<5) {
      TeamSportsfreqlist[i] <- 4
    }
    else if (TeamSportsfreq[i,j]==3 & !is.na(TeamSportsfreq[i,j]) & TeamSportsfreqlist[i]<4) {
      TeamSportsfreqlist[i] <- 3
    }
    else if (TeamSportsfreq[i,j]==2 & !is.na(TeamSportsfreq[i,j]) & TeamSportsfreqlist[i]<3) {
      TeamSportsfreqlist[i] <- 2
    }
    else if (TeamSportsfreq[i,j]==1 & !is.na(TeamSportsfreq[i,j]) & TeamSportsfreqlist[i]<2) {
      Sportsfreqlist[i] <- 1
    }
    else if (TeamSportsfreq[i,j]==0 & !is.na(TeamSportsfreq[i,j]) & TeamSportsfreqlist[i]<1) {
      TeamSportsfreqlist[i] <- 0.1
    }
    else if (is.na(TeamSportsfreq[i,j]) & (TeamSportsfreqlist[i]==0 | is.na(TeamSportsfreqlist[i]))) {
      TeamSportsfreqlist[i] <- -99
    }
  }}


##Recode team sport, cycling, swimming, walking 
age42_vars$TeamSportTrichotomy <- dplyr::recode(age42_vars$sport, '-9'=0, '-8'=NA_real_, '-1'=NA_real_, '7'=0, '6'=1, '5'=1, '4'=2, '3'=2, '2'=2, '1'=2)
age42_vars$CyclingTrichotomy <- dplyr::recode(age42_vars$Cycling, '-9'=0, '-8'=NA_real_, '-1'=NA_real_, '7'=0, '6'=1, '5'=1, '4'=2, '3'=2, '2'=2, '1'=2)
age42_vars$SwimTrichotomy <- dplyr::recode(age42_vars$Swimming_Diving, '-9'=0, '-8'=NA_real_, '-1'=NA_real_, '7'=0, '6'=1, '5'=1, '4'=2, '3'=2, '2'=2, '1'=2)
age42_vars$WalkTrichotomy <- dplyr::recode(age42_vars$Walk, '-9'=0, '-8'=NA_real_, '-1'=NA_real_, '7'=0, '6'=1, '5'=1, '4'=2, '3'=2, '2'=2, '1'=2)
TeamSportsTrichotomy <- dplyr::recode(TeamSportsfreqlist, '-99'=NA_real_, '0.1'=0, '1'=0, '2'=1, '3'=1, '4'=2, '5'=2, '6'=2, '7'=2)

##Push into df. NB: -99 is missing.
age42_vars$SportsFreq42 <- Sportsfreqlist
age42_vars$Sportstrichotomous42 <- SportsTrichotomy 
age42_vars$TeamSportstrichotomous42 <- TeamSportsTrichotomy 
age42_vars$TeamSportsFreq42 <- TeamSportsTrichotomy 

age42activities <- subset(age42_vars, select=c(BCSID, Sportstrichotomous42, TeamSportstrichotomous42, SwimTrichotomy, WalkTrichotomy, CyclingTrichotomy))
age42activities$na_count <- apply(is.na(age42activities), 1, sum)
age42activities <- age42activities[!(age42activities$na_count==5),]
age42activities <- subset(age42activities, select=c(BCSID))
age42_vars <- merge(age42_vars, age42activities, by=c("BCSID"))
# hist(Sportsfreqlist)
# table(Sportsfreqlist, useNA="always")
# 
# Sports$SportCats <- 0
# Sports$SportCheck <- rowSums(Sports[2:15])
# 
# Sports$SportCats2 <- rowSums(Sports[2:29])

# 
# age42_vars$SportFreq <- 0
# for (i in 1:length(age42_vars$SportFreq)) {
#   if (age42_vars$sport[i]>0 & !is.na(age42_vars$sport[i]) 
#       | age42_vars$martialarts_boxing_wrestling[i]>0 & !is.na(age42_vars$martialarts_boxing_wrestling[i])
#       | age42_vars$watersports[i]>0 & !is.na(age42_vars$watersports[i])
#       | age42_vars$Dance[i]>0 & !is.na(age42_vars$Dance[i])
#       | age42_vars$horseriding[i]>0 & !is.na(age42_vars$horseriding[i])
#       | age42_vars$yoga_pilates[i]>0 & !is.na(age42_vars$yoga_pilates[i])
#       | age42_vars$golf[i]>0 & !is.na(age42_vars$golf[i])
#       | age42_vars$Skiing[i]>0 & !is.na(age42_vars$Skiing[i])
#       | age42_vars$Othersporting[i]>0 & !is.na(age42_vars$Othersporting[i])
#       | age42_vars$Raquet[i]>0 & !is.na(age42_vars$Raquet[i])
#       | age42_vars$Jogging[i]>0 & !is.na(age42_vars$Jogging[i])
#       | age42_vars$Cycling[i]>0 & !is.na(age42_vars$Cycling[i])
#       | age42_vars$Swimming_Diving[i]>0 & !is.na(age42_vars$Swimming_Diving[i])
#       | age42_vars$Fitness_gym_conditioning[i]>0 & !is.na(age42_vars$Fitness_gym_conditioning[i])) {
#     age42_vars$age42_varsocial[i] <- 2 } 
#   else if (age42_vars$SportCats[i]>0 & !is.na(age42_vars$SportCats[i])) {
#     age42_vars$SportSocial[i] <- 1 }
# }



##Covariates##
age42_covars <- data.frame(0, 1:length(age42$BCSID))
age42_covars$BCSID <- age42$BCSID
#age42_covars$Education <- dplyr::recode(age42_derived$BD9ACHQ1,'0'=1, '1'=1, '2'=1, '3'=1, '4'=2, '5'=2, '6'=3, '7'=3, '8'=3)
##Number of days in a typical week does 30 mins or more of exercise
##Value = -9.0 Refused; -8.0 Don't know;  -1.0	Label = Not applicable
age42_covars$days_exercise <- age42$B9EXERSE
##Whether regularly take part in any exercise activities (binary)
##Value = -9.0	Label = Refused; -8 Don't know; -1.0 Not applicable; 1.0 Yes; 2.0  No     
age42_covars$regular_exercise <- age42$B9EXERDR

age42_covars$Disability <- dplyr::recode(age42_derived$BD9DISLS, '-8'=NA_real_, '1'=1, '2'=2, '0'=0)

age42_covars$Mood <- dplyr::recode(age42_derived$BD9MALG, '-8'=NA_real_, '1'=1, '2'=2)

age42_covars$AlcRisk <- dplyr::recode(age42_derived$BD9AUDG, '-8'=NA_real_, '1'=1, '2'=2, '-1'=0)

age42_covars$Smoker <- dplyr::recode(age42$B9SMOKIG, '-9'=NA_real_, '-8'=NA_real_, '-1'=NA_real_, '1'=0, '2'=1, '3'=2, '4'=3)

#1-yes
#age42_covars$diabetes <- age42$B9KHPB04
#1-yes; 2-No
#age42_covars$insulin <- age42$B9INSULN
#table(age42_covars$insulin)

Diabetes <- ifelse(age42$B9KHPB04==1 | age42$B9INSULN==1, 1, 0)
age42_covars$Diabetes <- Diabetes



age42_covars$BMI <- age42_derived$BD9BMI

for (i in 1:length(age42_covars$BMI)){
  if (age42_covars$BMI[i]<10 & !is.na(age42_covars$BMI[i])) {
    age42_covars$BMI[i] <- NA}

  }

Min <- mean(age42_covars$BMI, na.rm=TRUE) - 3*sd(age42_covars$BMI, na.rm=TRUE)
Max <- mean(age42_covars$BMI, na.rm=TRUE) + 3*sd(age42_covars$BMI, na.rm=TRUE)  
#plot(age42_covars$BMI)
for (i in 1:length(age42_covars$BMI)){
  age42_covars$BMI[i][age42_covars$BMI[i] < Min | age42_covars$BMI[i] > Max] <- NA}



age42_vars$Open_Skill_Tri42 <- dplyr::recode(age42_vars$openskill_42, '1'=0, '2'=1, '2.5'=1, '3'=2, '4'=2)
age42_vars$Closed_Skill_Tri42 <- dplyr::recode(age42_vars$closedskill_42, '1'=0, '2'=1, '2.5'=1, '3'=2, '4'=2)


#-8's
#table(round(age42_covars$BMI, digits=0))


##Freq meeting friends
##-9, Refused; -8, Dknow; -1 N/A; 1 3+/week; 2, 1or2/week; 3, 1/2/month; 4, Every few months; 5, Once or twice/year; 6, <1/year; 7 Never; 8 N/A has No friends.
age42_covars$friends <- age42$B9FREMT


#age42_covars$heart <- age42$B9PKHP18
#age42_covars$stroke <- age42$B9PKHP19

#table(age42_covars$heart)
#table(age42_covars$stroke)

#CVD <- ifelse(age42$B9PKHP18==1 | age42$B9PKHP19==1, 1, 0)



##TV viewing
##-9 Not stated; -8 Multicode; -1 Paper self compl not completed; 1 None; 2 <1h; 3 1h->3h; 4 3h,<5h; 5  >5h
age42_covars$TV_Weekday <- age42$B9SCQ10A
age42_covars$TV_Weekend <- age42$B9SCQ10B
#age42_vars <- merge(age42_vars, age42_covars, by=c("BCSID"))
age42_covars <- rename(age42_covars, ID="BCSID")
age42_vars <- rename(age42_vars, ID="BCSID")


age42_varsSEC <- subset(age42_employment, select=c("BCSID", "B9ASC"))
age42_varsSEC <- age42_varsSEC[age42_varsSEC$B9ASC>-1,]
age42_varsSEC <- age42_varsSEC[!duplicated(age42_varsSEC), ]
names(age42_varsSEC) <- c("ID","SEC42")
age42_vars <- merge(age42_vars, age42_varsSEC, by="ID", all.x=T)



return(list(age42_vars, age42_covars)) }
