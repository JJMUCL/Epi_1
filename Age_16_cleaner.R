age16_output <- function(df1, df2) {
  
age16_vars <- as.data.frame(age16$bcsid)
#1.0- at least once a week, 2.0- At least once month, -1 No questionnaire; 3 No response
# age16_vars$school_baseball <-age16$f20a1
# age16_vars$school_basketball <-age16$f20a2
# age16_vars$school_cricket <-age16$f20a3
# age16_vars$school_football <-age16$f20a4
# age16_vars$school_hockey <-age16$f20a5
# age16_vars$school_netball <-age16$f20a6
# age16_vars$school_rounders <-age16$f20a7
# age16_vars$school_rugby <-age16$f20a8
# age16_vars$school_volleyball <-age16$f20a9
# age16_vars$school_other_team <-age16$f20a10
# age16_vars$school_aerobics <-age16$f20a11
# age16_vars$school_trackfield <-age16$f20a12
# age16_vars$school_badminton <-age16$f20a13
# age16_vars$school_canoeing <-age16$f20a14
# age16_vars$school_crosscountry <-age16$f20a15
# age16_vars$school_cycling <-age16$f20a16
# age16_vars$school_dancing <-age16$f20a17
# age16_vars$school_gymnastics <-age16$f20a18
# age16_vars$school_horseriding <-age16$f20a19
# age16_vars$school_jogging <-age16$f20a20
# age16_vars$school_fitness <-age16$f20a21
# age16_vars$school_iceskating <-age16$f20a23
# age16_vars$school_rowing <-age16$f20a24
# age16_vars$school_sailing <-age16$f20a25
# age16_vars$school_scrambling <-age16$f20a26
# age16_vars$school_skiing <-age16$f20a27
# age16_vars$school_squash <-age16$f20a28
# age16_vars$school_swimming <-age16$f20a29
# age16_vars$school_tabletennis <-age16$f20a30
# age16_vars$school_tennis <-age16$f20a31
# age16_vars$school_walking <-age16$f20a32
# age16_vars$school_waterskiing <-age16$f20a33
# age16_vars$school_weights <-age16$f20a34
# age16_vars$school_windsurf <-age16$f20a35
# age16_vars$school_other_individual <-age16$f20a36
# age16_vars$school_billiards <-age16$f20a37
# age16_vars$school_darts <-age16$f20a38
# age16_vars$school_fishing <-age16$f20a39
# age16_vars$school_pool <-age16$f20a40
# age16_vars$school_shooting <-age16$f20a41
# age16_vars$school_snooker <-age16$f20a42
# age16_vars$school_other <-age16$f20a43

age16_vars$baseball <-age16$f20b1
age16_vars$basketball <-age16$f20b2
age16_vars$cricket <-age16$f20b3
age16_vars$football <-age16$f20b4
age16_vars$hockey <-age16$f20b5
age16_vars$netball <-age16$f20b6
age16_vars$rounders <-age16$f20b7
age16_vars$rugby <-age16$f20b8
age16_vars$volleyball <-age16$f20b9
age16_vars$other_team <-age16$f20b10
age16_vars$aerobics <-age16$f20b11
age16_vars$trackfield <-age16$f20b12
age16_vars$badminton <-age16$f20b13
age16_vars$canoeing <-age16$f20b14
age16_vars$crosscountry <-age16$f20b15
age16_vars$cycling <-age16$f20b16
age16_vars$dancing <-age16$f20b17
age16_vars$gymnastics <-age16$f20b18
age16_vars$horseriding <-age16$f20b19
age16_vars$jogging <-age16$f20b20
age16_vars$fitness <-age16$f20b21
age16_vars$iceskating <-age16$f20b23
age16_vars$rowing <-age16$f20b24
age16_vars$sailing <-age16$f20b25
age16_vars$scrambling <-age16$f20b26
age16_vars$skiing <-age16$f20b27
age16_vars$squash <-age16$f20b28
age16_vars$swimming <-age16$f20b29
age16_vars$tabletennis <-age16$f20b30
age16_vars$tennis <-age16$f20b31
age16_vars$walking <-age16$f20b32
age16_vars$waterskiing <-age16$f20b33
age16_vars$weights <-age16$f20b34
age16_vars$windsurf <-age16$f20b35
age16_vars$other_individual <-age16$f20b36
age16_vars$billiards <-age16$f20b37
age16_vars$darts <-age16$f20b38
age16_vars$fishing <-age16$f20b39
age16_vars$pool <-age16$f20b40
age16_vars$shooting <-age16$f20b41
age16_vars$snooker <-age16$f20b42
age16_vars$other <-age16$f20b43
Sports <- subset(age16_vars, select= -c(walking #school_walking#
))

library(tidyverse)
table(Sports$rugby, useNA = "always")
Sports <- rename(Sports, bcsid = 'age16$bcsid')
for (i in 1:length(Sports$bcsid)) {
  for (j in 2:ncol(Sports)) {
    if (Sports[i,j]==1 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 3
    }
    else if (Sports[i,j]==2 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 2
    }
    else if (Sports[i,j]==3 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 1
    }
    else if (Sports[i,j]==-1 & !is.na(Sports[i,j])) {
      Sports[i,j] <- NA_real_
    }
    else if (is.na(Sports[i,j])) {
      Sports[i,j] <- NA_real_}
  }
}

OpenSkill <- subset(Sports, select= -c(other, snooker, shooting, pool, fishing, darts 
                                           , billiards, other_individual, swimming, skiing, 
                                           aerobics, fitness, trackfield, crosscountry, cycling, dancing, gymnastics, 
                                           iceskating, rowing, scrambling, horseriding, weights, jogging, waterskiing, canoeing))

ClosedSkill <- subset(Sports, select= -c(windsurf, tennis, tabletennis, squash, baseball, basketball, 
                                             cricket, football, netball, volleyball, hockey, rounders, rugby, badminton, sailing, other_team))

Sportsfreq <- Sports
Sportsfreqlist <- vector("double", 11615)

for (i in 1:length(Sports$bcsid)) {
  for (j in 2:ncol(Sportsfreq)) {
    if (Sportsfreq[i,j]==3 & !is.na(Sportsfreq[i,j])) {
      Sportsfreqlist[i] <- 3
    }
    else if (Sportsfreq[i,j]==2 & !is.na(Sportsfreq[i,j]) & Sportsfreqlist[i]<3) {
      Sportsfreqlist[i] <- 2
    }
    else if (Sportsfreq[i,j]==1 & !is.na(Sportsfreq[i,j]) & Sportsfreqlist[i]<2) {
      Sportsfreqlist[i] <- 1
    }
    else if (is.na(Sportsfreq[i,j]) & (Sportsfreqlist[i]==0 | is.na(Sportsfreqlist[i]))) {
      Sportsfreqlist[i] <- -99
    }
  }}
# 
# table(Sportsfreqlist)
# table(Openfreqlist)

#0 - none -> <monthly (0.1,1);1 - 2-3/m - 1/week (2,3,4); 2 >weekly (7,6,5) ?
#SportsTrichotomy <- dplyr::recode(Sportsfreqlist, '-99'=NA_real_, '0.1'=0, '1'=0, '2'=1, '3'=1, '4'=2, '5'=2, '6'=2, '7'=2)
#Sportsfreqlist <- dplyr::recode(Sportsfreqlist, '0.1'=0)


OpenSkillfreq <- OpenSkill
Openfreqlist <- vector("double", 11615)


for (i in 1:length(OpenSkill$bcsid)) {
  for (j in 2:ncol(OpenSkillfreq)) {
    if (OpenSkillfreq[i,j]==3 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]>=3  &  !is.na(Openfreqlist[i])) {
      Openfreqlist[i] <- 4
    }
    else if (OpenSkillfreq[i,j]==3 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]<3) {
      Openfreqlist[i] <- 3
    }
    else if (OpenSkillfreq[i,j]==2 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]==2) {
      Openfreqlist[i] <- 2.5
    }
    else if (OpenSkillfreq[i,j]==2 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]<2) {
      Openfreqlist[i] <- 2
    }
    else if (OpenSkillfreq[i,j]==1 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]<2) {
      Openfreqlist[i] <- 1
    }
    else if (is.na(OpenSkillfreq[i,j]) & (Openfreqlist[i]==0 | is.na(Openfreqlist[i]))) {
      Openfreqlist[i] <- -99
    }
  }}


ClosedSkillfreq <- ClosedSkill
Closedfreqlist <- vector("double", 11615)

##>1 open weekly = 4##
for (i in 1:length(ClosedSkill$bcsid)) {
  for (j in 2:ncol(ClosedSkillfreq)) {
    if (ClosedSkillfreq[i,j]==3 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]>=3 & !is.na(Closedfreqlist[i])) {
      Closedfreqlist[i] <- 4
    }
    else if (ClosedSkillfreq[i,j]==3 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]<3) {
      Closedfreqlist[i] <- 3
    }
    else if (ClosedSkillfreq[i,j]==2 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]==2) {
      Closedfreqlist[i] <- 2.5
    }
    else if (ClosedSkillfreq[i,j]==2 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]<2) {
      Closedfreqlist[i] <- 2
    }
    else if (ClosedSkillfreq[i,j]==1 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]<2) {
      Closedfreqlist[i] <- 1
    }
    else if (is.na(ClosedSkillfreq[i,j]) & (Closedfreqlist[i]==0 | is.na(Closedfreqlist[i]))) {
      Closedfreqlist[i] <- -99
    }
  }}




##Team/Partner sport:
TeamSports <- subset(Sports, select= -c(aerobics, trackfield, badminton, canoeing, crosscountry, cycling, dancing, gymnastics, horseriding, jogging, fitness, iceskating, scrambling, 
                                        skiing, swimming, waterskiing, weights, windsurf, other_individual, billiards, darts, fishing, pool, shooting, snooker, other ##school_aerobics, school_trackfield, school_badminton, school_canoeing, school_crosscountry, school_cycling, school_dancing, gymnastics, school_horseriding, school_jogging, school_fitness, school_iceskating, school_scrambling, 
                                        ##school_skiing, school_swimming, school_waterskiing, school_weights, school_windsurf, school_other_individual, school_billiards, school_darts, school_fishing, school_pool, school_shooting, school_snooker, school_other##
                                        ))
TeamSportsfreq <- TeamSports
TeamSportsfreqlist <- vector("double", 11615)

for (i in 1:length(TeamSportsfreq$bcsid)) {
  for (j in 2:ncol(TeamSportsfreq)) {
    if (TeamSportsfreq[i,j]==3 & !is.na(TeamSportsfreq[i,j])) {
      TeamSportsfreqlist[i] <- 3
    }
    else if (TeamSportsfreq[i,j]==2 & !is.na(TeamSportsfreq[i,j]) & TeamSportsfreqlist[i]<3) {
      TeamSportsfreqlist[i] <- 2
    }
    else if (TeamSportsfreq[i,j]==1 & !is.na(TeamSportsfreq[i,j]) & TeamSportsfreqlist[i]<2) {
      TeamSportsfreqlist[i] <- 1
    }
    else if (is.na(TeamSportsfreq[i,j]) & (TeamSportsfreqlist[i]==0 | is.na(TeamSportsfreqlist[i]))) {
      TeamSportsfreqlist[i] <- -99
    }
  }}


table(TeamSportsfreqlist)

##0.0-No, 1.0-Yes
age16_vars$bike_school <-age16$f9a1
age16_vars$bike_work <-age16$f9a2
age16_vars$bike_friends <-age16$f9a3
age16_vars$bike_enjoyment <-age16$f9a4
age16_vars$bike_race <-age16$f9a6
#age16_vars$school_cycling 
#age16_vars$cycling

age16_vars <- rename(age16_vars, bcsid='age16$bcsid')
Cyclingfreq <- subset(Sports, select=c(bcsid, #school_cycling,#
                                       cycling))
Cyclingfreqlist <- vector("double", 11615)

for (i in 1:length(Cyclingfreq$bcsid)) {
  for (j in 2:ncol(Cyclingfreq)) {
    if (Cyclingfreq[i,j]==3 & !is.na(Cyclingfreq[i,j])) {
      Cyclingfreqlist[i] <- 3
    }
    else if (Cyclingfreq[i,j]==2 & !is.na(Cyclingfreq[i,j]) & Cyclingfreqlist[i]<3) {
      Cyclingfreqlist[i] <- 2
    }
    else if (Cyclingfreq[i,j]==1 & !is.na(Cyclingfreq[i,j]) & Cyclingfreqlist[i]<2) {
      Cyclingfreqlist[i] <- 1
    }
    else if (is.na(Cyclingfreq[i,j]) & (Cyclingfreqlist[i]==0 | is.na(Cyclingfreqlist[i]))) {
      Cyclingfreqlist[i] <- -99
    }
}}


Swimmingfreq <- subset(Sports, select=c(bcsid, #school_swimming#, 
                                        swimming))
Swimmingfreqlist <- vector("double", 11615)

for (i in 1:length(Swimmingfreq$bcsid)) {
  for (j in 2:ncol(Swimmingfreq)) {
    if (Swimmingfreq[i,j]==3 & !is.na(Swimmingfreq[i,j])) {
      Swimmingfreqlist[i] <- 3
    }
    else if (Swimmingfreq[i,j]==2 & !is.na(Swimmingfreq[i,j]) & Swimmingfreqlist[i]<3) {
      Swimmingfreqlist[i] <- 2
    }
    else if (Swimmingfreq[i,j]==1 & !is.na(Swimmingfreq[i,j]) & Swimmingfreqlist[i]<2) {
      Swimmingfreqlist[i] <- 1
    }
    else if (is.na(Swimmingfreq[i,j]) & (Swimmingfreqlist[i]==0 | is.na(Swimmingfreqlist[i]))) {
      Swimmingfreqlist[i] <- -99
    }
  }}

Walkingfreq <- subset(age16_vars, select=c(bcsid, walking #,school_walking#
                                           ))
Walkingfreq$walking <- dplyr::recode(Walkingfreq$walking, '-99'=NA_real_, '-1'=NA_real_, '1'=2, '2'=1, '3'=0)
#Walkingfreq$school_walking <- dplyr::recode(Walkingfreq$school_walking, '-99'=NA_real_, '-1'=NA_real_, '1'=2, '2'=1, '3'=0)
Walkingfreqlist <- vector("double", 11615)

for (i in 1:length(Walkingfreq$bcsid)) {
  for (j in 2:ncol(Walkingfreq)) {
    if (Walkingfreq[i,j]==2 & !is.na(Walkingfreq[i,j])) {
      Walkingfreqlist[i] <- 2
    }
    else if (Walkingfreq[i,j]==1 & !is.na(Walkingfreq[i,j]) & Walkingfreqlist[i]<2) {
      Walkingfreqlist[i] <- 1
    }
    else if (Walkingfreq[i,j]==0 & !is.na(Walkingfreq[i,j]) & Walkingfreqlist[i]<1) {
      Walkingfreqlist[i] <- 0
    }
    else if (is.na(Walkingfreq[i,j]) & (Walkingfreqlist[i]==0 | is.na(Walkingfreqlist[i]))) {
      Walkingfreqlist[i] <- NA_real_
    }
  }}

##Dwelling
##1- big city; 2- town; 3- village; 4- countryside;
age16_covars <- subset(age16_vars, select="bcsid")
age16_covars$dwelling <-age16$c6.13
age16_covars$bcsid <- age16_vars$bcsid
##Television - 1 - rare/never; 2 less than weekly; 3 once a week; 4more than once a week;
age16_covars$SB_TV <- age16$c5j1a

##Blood Pressure
age16_covars$Systolic <- age16$rd5.1
age16_covars$Diastolic <- age16$rd5.2
age16_covars$CVDAbnormal <- age16$rd6e.1

##Smoking
##weekly cigarettes 1- none; 2- <1; 3- 1-4; 4- 5-20; -1 no questionairre
age16_covars$Smoking <- dplyr::recode(age16$c6.18, '-2'=NA_real_, '-1'=NA_real_, '1'=0, '2'=1, '3'=1, '4'=1)


##Annual alcohol 1- every/mostdays; 2- 4-5/week; 3- 2-3/week; 4- 1/week; 5- 1/month; 6- specialocc; 7- never; -2 not stated. 
age16_covars$Yearly_Alcohol <- dplyr::recode(age16$hd1, '1'=2, '2'=2, '3'=2, '4'=2, '5'=1, '6'=1, '7'=0, '-2'=NA_real_, '-1'=NA_real_) 




#age16_covars$alcohol <- age16$f57_tot
#alcohol <- subset(age16, select=c(bcsid, f57_tot))

#age16_covars %>% filter(alcohol>=0) %>% count(.) 
##Covariates##
##Teen has no Disability or handicap coding (1 -True none, 2 no response, ) 
age16_covars$disability_no <- age16$od7.1 
age16_covars$disability_handicap_yes
##Teen has Disability coding (1 -True yes, , 2 no response, ) 
age16_covars$disability_yes <- age16$od7.3
age16_covars$disability_handicap_yes <- age16$od7.4

age16_covars$disability <- NA
for (i in 1:length(age16_covars$bcsid)) {
  if (age16_covars$disability_yes[i]==1 | age16_covars$disability_handicap_yes[i]==1 & (!is.na(age16_covars$disability_yes[i]) & !is.na(age16_covars$disability_handicap_yes[i]))) {
    age16_covars$disability[i] <- 1
  }
  else if (age16_covars$disability_yes[i]>1 | age16_covars$disability_handicap_yes[i]>1 & (!is.na(age16_covars$disability_yes[i]) & !is.na(age16_covars$disability_handicap_yes[i]))) {
    age16_covars$disability[i] <- 0
  }  
}
table(age16_covars$disability, useNA="always")

##BMI
age16_covars$weight <- age16$rd4.1
age16_covars$weight_sreport <- age16$ha1.1
age16_covars$height <- age16$rd2.1
age16_covars$height_sreport <- age16$ha1.2

age16_covars$bmi <- NA
for (i in 1:length(age16_covars$bcsid)) {
  if (age16_covars$weight[i]>10 & age16_covars$height[i]>0.3 & !is.na(age16_covars$height[i]) & !is.na(age16_covars$weight[i])) {
    age16_covars$bmi[i] <- ((age16_covars$weight[i])/(age16_covars$height[i])^2)
  }
  else if (age16_covars$weight[i]>10 & age16_covars$height_sreport[i]>0.3 & !is.na(age16_covars$height_sreport[i]) & !is.na(age16_covars$weight[i]))
{
  age16_covars$bmi[i] <- ((age16_covars$weight[i])/(age16_covars$height_sreport[i])^2)
  }
  else if (age16_covars$weight_sreport[i]>10 & age16_covars$height[i]>0.3 & !is.na(age16_covars$height[i]) & !is.na(age16_covars$weight_sreport[i]))
{
    age16_covars$bmi[i] <- ((age16_covars$weight_sreport[i])/(age16_covars$height[i])^2)
  }
  else if (age16_covars$weight_sreport[i]>10 & age16_covars$height_sreport[i]>0.3 & !is.na(age16_covars$height_sreport[i]) & !is.na(age16_covars$weight_sreport[i]))
{
    age16_covars$bmi[i] <- ((age16_covars$weight_sreport[i])/(age16_covars$height_sreport[i])^2)
  }
}


Min <- mean(age16_covars$bmi, na.rm=TRUE) - 3*sd(age16_covars$bmi, na.rm=TRUE)
Max <- mean(age16_covars$bmi, na.rm=TRUE) + 3*sd(age16_covars$bmi, na.rm=TRUE)  
#plot(age16_covars$bmi)
for (i in 1:length(age16_covars$bmi)){
  age16_covars$bmi[i][age16_covars$bmi[i] < Min | age16_covars$bmi[i] > Max] <- NA}


age16_covars$mood <- age16_derived$BD4MALG

age16_covars$SEC <- dplyr::recode(age16_derived$BD4PSOC, '-3'=0, '-1'=NA_real_, '0'=0, '1'=1, '2'=2, '3'=3, '4'=4, '5'=5, '6'=6)
#table(round(age16_covars$bmi, digits= 0))

##Social engagement
##Friends (1.0- Rarely/never, 2.0-Less than once week, 3- 1/week, 4- >1/week)
age16_covars$social_friends <- age16$c5j43a
##Leisure see activites: c5j1a - c5j47b 
age16_vars$Cycling16 <- Cyclingfreqlist
age16_vars$Swimming16 <- Swimmingfreqlist
age16_vars$Walking16 <- Walkingfreqlist
age16_vars$TeamSports16 <- TeamSportsfreqlist
age16_vars$Sports_16 <- Sportsfreqlist
age16_vars$openskill_16 <- Openfreqlist
age16_vars$closedskill_16 <- Closedfreqlist
age16_vars$Open_Skill_Tri16 <- dplyr::recode(age16_vars$openskill_16, '1'=0, '2'=1, '2.5'=1, '3'=2, '4'=2)
age16_vars$Closed_Skill_Tri16 <- dplyr::recode(age16_vars$closedskill_16, '1'=0, '2'=1, '2.5'=1, '3'=2, '4'=2)



table(age16_vars$Swimming16)

age16_vars$Sports_16 <- dplyr::recode(age16_vars$Sports_16, '1'=0, '2'=1, '3'=2, '-99'=NA_real_)
age16_vars$TeamSports16 <- dplyr::recode(age16_vars$TeamSports16, '1'=0, '2'=1, '3'=2, '-99'=NA_real_)
# age16_vars$Walking16 <- dplyr::recode(age16_vars$Walking16, '1'=0, '2'=1, '3'=2, '-99'=NA_real_)
age16_vars$Cycling16 <- dplyr::recode(age16_vars$Cycling16, '1'=0, '2'=1, '3'=2, '-99'=NA_real_)
age16_vars$Swimming16 <- dplyr::recode(age16_vars$Swimming16, '1'=0, '2'=1, '3'=2, '-99'=NA_real_)

#age16_vars$OpenSkill <- dplyr::recode(age16_vars$openskill, '1'=0, '2'=1, '3'=2, '4'=3, '-99'=NA_real_)
#age16_vars$closedskill <- dplyr::recode(age16_vars$closedskill, '1'=0, '2'=1, '3'=2, '4'=3, '-99'=NA_real_)
age16_activities <- subset(age16_vars, select=c("bcsid", "Sports_16", "Cycling16", "Swimming16", "Walking16"))
age16_activities$na_count <- apply(is.na(age16_activities), 1, sum)
age16_activities <- age16_activities[!(age16_activities$na_count==4),]
age16_activities <- subset(age16_activities, select=c("bcsid"))
age16_vars <- merge(age16_vars, age16_activities, by=c("bcsid"))
age16_vars <- rename(age16_vars, ID="bcsid")
age16_covars <- rename(age16_covars, ID="bcsid")





return(list(age16_vars, age16_covars))}
