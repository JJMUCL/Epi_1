##Load libraries 
##2-19
library(tidyverse)
library(haven)
library(dplyr)
library(compositions)
library(Hotelling)
library(robCompositions)
library(zCompositions)
library(MASS)
library(ggplot2)
library(ggtern)
library(ggplot2)
library(plotly)
library(ggtern)
library(Ternary)
library(car)

##Load Main & PA 20-24
##20-24

age46_output <- function(BCS70_main, BCSAvg, Covariates1, Covariates2, Covariates3) {
age46_vars <- as.data.frame(BCS70_main$BCSID)
##Create Dataframes 
##29-45

PA<-BCSAvg[c('BCSID', 'B10AAMVPAH', 'B10AASTDH','B10AATSTEPH','B10AAWWT','B10AASITH')]
Biomarkers <- BCS70_main[c('BCSID', 'B10HBA1C', 'B10HSCRP', 'B10CHOL' , 'B10HDL', 'B10BPSYSR1', 'B10BPSYSR2', 'B10BPSYSR3')]
recall<-BCS70_main[c('BCSID', 'B10CFWRDS','B10CFLISN','B10CFLISD')]
letters<-BCS70_main[c('BCSID','B10CFCMP','B10CFCOR','B10CFMIS','B10CFTOT','B10CFRC')]
animal<-BCS70_main[c('BCSID', 'B10CFANI')]
Covariates<-BCS70_main[c('BCSID', 'BD10AGEINT', 'B10CMSEX', 'BD10MS', 'BD10HACHQ', 'BD10DISLS', 'BD10MBMI', 'BD10BMI',  'BD10MALG','BD10CNS8', 'B10SMOKIG', 'BD10AUDG')]
Occupational<-BCS70_main[c('BCSID', 'B10Q19', 'B10Q20A', 'B10Q18', 'B10Q20B', 'B10Q21A', 'B10Q21B', 'B10Q21C')]
Transport<-BCS70_main[c('BCSID', 'B10Q22', 'B10Q22MID', 'B10Q23', 'B10Q23MID', 'B10Q24C', 'B10Q24D', 'B10Q11A', 'B10Q11B', 'B10Q11C')]
Leisure<- BCS70_main[c('BCSID', 'B10Q25A', 'B10Q25AH', 'B10Q25AM', 'B10Q25B', 'B10Q25BH', 'B10Q25BM', 'B10Q25C', 'B10Q25CH', 'B10Q25CM', 'B10Q25D', 'B10Q25DH', 'B10Q25DM',
                       'B10Q25E', 'B10Q25EH', 'B10Q25EM', 'B10Q25F', 'B10Q25FH', 'B10Q25FM', 'B10Q25G', 'B10Q25GH', 'B10Q25GM', 'B10Q25H', 'B10Q25HHO', 'B10Q25HMI',
                       'B10Q25J', 'B10Q25JH', 'B10Q25JM', 'B10Q25K', 'B10Q25KH', 'B10Q25KM', 'B10Q25L', 'B10Q25LH', 'B10Q25LM', 'B10Q25M', 'B10Q25MH', 'B10Q25MM',
                       'B10Q25N', 'B10Q25NH', 'B10Q25NM', 'B10Q25O', 'B10Q25OH', 'B10Q25OM', 'B10Q25P', 'B10Q25PH', 'B10Q25PM','B10Q25Q', 'B10Q25QH', 'B10Q25QM',
                       'B10Q25R', 'B10Q25RH', 'B10Q25RM', 'B10Q25S','B10Q25SH','B10Q25SM', 'B10Q25T', 'B10Q25TH', 'B10Q25TM', 'B10Q25U', 'B10Q25UH', 'B10Q25UM',
                       'B10Q25V', 'B10Q25VH', 'B10Q25VM', 'B10Q25W', 'B10Q25WH', 'B10Q25WM', 'B10Q25X', 'B10Q25XH', 'B10Q25XM','B10Q25Y', 'B10Q25YH', 'B10Q25YM',
                       'B10Q25Z','B10Q25ZH','B10Q25ZM', 'B10Q25AA', 'B10Q25AAH', 'B10Q25AAM', 'B10Q25BB', 'B10Q25BBH', 'B10Q25BBM' , 'B10Q25CC', 'B10Q25CCH', 'B10Q25CCM',
                       'B10Q25DD', 'B10Q25DDH', 'B10Q25DDM', 'B10Q25EE', 'B10Q25EEH', 'B10Q25EEM', 'B10Q25FF', 'B10Q25FFH', 'B10Q25FFM', 'B10Q25GG', 'B10Q25GGH', 'B10Q25GGM',
                       'B10Q25HH', 'B10Q25HHH', 'B10Q25HHM', 'B10Q25II', 'B10Q25IIH', 'B10Q25IIM', 'B10Q25I', 'B10Q25IH', 'B10Q25IM' )]

RHR <- BCS70_main[c('BCSID', 'B10BPPLSR3', 'B10BPPLSR2')]

RHR$RHR1 <- ifelse(RHR$B10BPPLSR3==-8 | RHR$B10BPPLSR3==-1 , NA, RHR$B10BPPLSR3)
RHR$RHR2 <- ifelse(RHR$B10BPPLSR2==-8 | RHR$B10BPPLSR2==-1 , NA, RHR$B10BPPLSR2)
RHR$RHR <- ifelse(is.na(RHR$RHR1) & is.na(RHR$RHR2), NA, 0)

for (i in 1:length(RHR$RHR)) {
  if (is.na(RHR$RHR[i])) {
    RHR$RHR[i] <- NA
  }
  else if (is.na(RHR$RHR1[i])){
    RHR$RHR[i] <- RHR$RHR2[i] 
  }
  else if (is.na(RHR$RHR2[i])){
    RHR$RHR[i] <- RHR$RHR1[i]
  }
  else if (!is.na(RHR$RHR[i])){
    RHR$RHR[i] <- mean(RHR$RHR2[i], RHR$RHR1[i])
  }
}



Region <- BCS70_main[c('BCSID', 'BD10GOR')]
CVDStroke<-Covariates1
HTN <- Covariates3[c('BCSID', 'PHV_SBP', 'PHV_diabetesany')]
HTNmeds <- Covariates2

##Rename vars
##52-96

Leisure <- rename(Leisure, ID='BCSID', CSwim='B10Q25A', CSwimH='B10Q25AH', CSwimM='B10Q25AM', Swim='B10Q25B', SwimH='B10Q25BH', SwimM='B10Q25BM'
                  , Mount='B10Q25C', MountH='B10Q25CH', MountM='B10Q25CM', Walk='B10Q25D', WalkH='B10Q25DH', WalkM='B10Q25DM', CCycle='B10Q25E'
                  , CCycleH='B10Q25EH', CCycleM='B10Q25EM', Cycle='B10Q25F', CycleH='B10Q25FH', CycleM='B10Q25FM', Lawn='B10Q25G', LawnH='B10Q25GH', LawnM='B10Q25GM'
                  ,Water='B10Q25H', WaterH='B10Q25HHO', WaterM='B10Q25HMI', Garden='B10Q25J', GardenH='B10Q25JH', GardenM='B10Q25JM', DIY='B10Q25K', DIYH='B10Q25KH', DIYM='B10Q25KM'
                  ,Aero='B10Q25L', AeroH='B10Q25LH', AeroM='B10Q25LM', Aero2='B10Q25M', Aero2H='B10Q25MH', Aero2M='B10Q25MM', Weight='B10Q25N', WeightH='B10Q25NH', WeightM='B10Q25NM'
                  ,Condition='B10Q25O', ConditionH='B10Q25OH', ConditionM='B10Q25OM',Yoga='B10Q25P', YogaH='B10Q25PH', YogaM='B10Q25PM', Dance='B10Q25Q', DanceH='B10Q25QH', DanceM='B10Q25QM'
                  ,CRun='B10Q25R',CRunH='B10Q25RH',CRunM='B10Q25RM', Jog='B10Q25S', JogH='B10Q25SH', JogM='B10Q25SM', Bowl='B10Q25T', BowlH='B10Q25TH', BowlM='B10Q25TM', Tennis='B10Q25U'
                  ,TennisH='B10Q25UH', TennisM='B10Q25UM', Squash='B10Q25V', SquashH='B10Q25VH', SquashM='B10Q25VM', Table='B10Q25W', TableH='B10Q25WH', TableM='B10Q25WM', Golf='B10Q25X'
                  ,GolfH='B10Q25XH', GolfM='B10Q25XM', Football='B10Q25Y', FootballH='B10Q25YH', FootballM='B10Q25YM', Cricket='B10Q25Z', CricketH='B10Q25ZH', CricketM='B10Q25ZM'
                  ,Rowing='B10Q25AA', RowingH='B10Q25AAH', RowingM='B10Q25AAM', Netball='B10Q25BB', NetballH='B10Q25BBH', NetballM='B10Q25BBM', Fish='B10Q25CC', FishH='B10Q25CCH', FishM='B10Q25CCM'
                  ,Horse='B10Q25DD', HorseH='B10Q25DDH', HorseM='B10Q25DDM', Snooker='B10Q25EE', SnookerH='B10Q25EEH', SnookerM='B10Q25EEM', Music='B10Q25FF', MusicH='B10Q25FFH', MusicM='B10Q25FFM'
                  ,Ice='B10Q25GG', IceH='B10Q25GGH', IceM='B10Q25GGM', Sail='B10Q25HH', SailH='B10Q25HHH', SailM='B10Q25HHM', Wrestle='B10Q25II', WrestleH='B10Q25IIH', WrestleM='B10Q25IIM', Dig='B10Q25I', DigH = 'B10Q25IH', DigM = 'B10Q25IM')

Covariates<- rename(Covariates, ID = BCSID, Age = BD10AGEINT, Sex = B10CMSEX, Married = BD10MS, Education = BD10HACHQ, BMI = BD10MBMI, BMIS = BD10BMI,
                    SEC = BD10CNS8, Mood = BD10MALG, Disability = BD10DISLS, Smoker = B10SMOKIG, AlcRisk = BD10AUDG)



Sex <- as.data.frame(subset(Covariates, select=c(ID, Sex)))
  
  CVDStroke <- rename(CVDStroke, ID=BCSID)
  HTN <- rename(HTN, ID=BCSID)
  HTNmeds <- rename(HTNmeds, ID=BCSID)
  Diabetes <- rename(Covariates3, ID=BCSID)
  Region <- rename(Region, ID=BCSID, Region=BD10GOR)
  
  Occupational<- rename(Occupational, ID=BCSID, type=B10Q19, jobcheck=B10Q18, stairs=B10Q20A, ladder=B10Q20B, kneel=B10Q21A, squat=B10Q21B, rise30=B10Q21C)
  
  Transport<- rename(Transport, ID=BCSID, miles=B10Q22, milesmid=B10Q22MID, work_freq=B10Q23, work_freqmid=B10Q23MID, work_bike=B10Q24C, work_walk=B10Q24D, one_mile=B10Q11A, five_mile=B10Q11B, many_mile=B10Q11C)
  
  letters <- rename(letters, correct=B10CFCOR
                    , total = B10CFTOT ,
                    missed = B10CFMIS , 
                    speed = B10CFRC ,
                    ID = BCSID, 
                    completed = B10CFCMP
  )
  
  PA <- rename(PA, 'mvpa'= 'B10AAMVPAH'
               , 'wear'='B10AAWWT' ,
               'sedent'='B10AASITH', 
               'totalactivity'='B10AATSTEPH',
               'stand'='B10AASTDH',
               'ID'='BCSID', 
  )
  
  
  animal <- rename(animal, ID=BCSID
                   , named = B10CFANI )
  
  
  recall <- rename(recall, ID=BCSID
                   , Wordlist = B10CFWRDS 
                   , immediate = B10CFLISN ,
                   delayed = B10CFLISD )
  
  
  
  
  ##SAMPLE SELECTION 
  ##Drop no PA measures)
  ##Drop missing PA cases 106 - 116; Derive sleep 114 - 116
  ##Drop missing occupational PA 120 - 123
  ##Drop missing transport PA 123 - 124
  ##Drop missing covariates 123 - 125
  ##Drop missing Leisure PA 128 - 133
  
  PA <- PA[!(PA$mvpa<=0),]
  PA <- PA[!(PA$wear<=0),]
  PA <- PA[!(PA$totalactivity<=0),]
  PA <- PA[!(PA$sedent<=0),]
  PA <- PA[!(PA$stand<0),]
  
  
  ##Derive LIPA & SLEEP and drop their missingness
  PA$LIPA <- (PA$totalactivity - PA$mvpa + PA$stand)
  PA$SLEEP <- 24 - PA$wear
  PA <- PA[!(PA$LIPA<0),]
  PA <- PA[!(PA$SLEEP<0),]
  PA <- subset(PA, select= -c(stand))
  
  
  ##Drop Missing Occupational PA
  Occupational <- Occupational[(Occupational$type> -2) & (Occupational$jobcheck> -2),] 
  #                              & (Occupational$type> -2) & (Occupational$stairs> -2) & (Occupational$ladder> -2) & (Occupational$kneel> -2) & (Occupational$squat> -2) & (Occupational$rise30> -2),]
  # ##Drop Missing Transport PA
  Transport <- Transport[(Transport$miles> -2) & (Transport$milesmid> -2) & (Transport$work_freq> -2) & (Transport$work_freqmid> -2) & (Transport$work_bike> -2) & (Transport$work_walk> -2) & (Transport$one_mile> -2) & (Transport$five_mile> -2) & (Transport$many_mile> -2),]
  ##Drop Missing Covariates
  Covariates <- Covariates[(Covariates$Age > -2) & (Covariates$Sex > -1) & (Covariates$Married > 0) & (Covariates$Education > -1) & (Covariates$Disability > -1) & (Covariates$SEC > 0) & (Covariates$AlcRisk > -8) 
                            & (Covariates$Smoker > -1) & (Covariates$Disability > -1) & (Covariates$Mood > -1) & (Covariates$SEC<9),]
  ##Drop Missing Leisure PA
  Leisure[Leisure< -1] <- NA
  Activities <- subset(Leisure, select=-c(SwimH, SwimM, CSwimH, CSwimM, MountH, MountM, WalkH, WalkM, CCycleH, CCycleM, CycleH, CycleM, LawnH, LawnM, DigH, DigM, WaterH, WaterM, GardenH, GardenM, DIYH, DIYM, AeroH, AeroM, Aero2H, Aero2M, WeightH, WeightM, ConditionH, ConditionM, YogaH, YogaM, DanceH, DanceM, CRunH, CRunM, 
                                          JogH, JogM, BowlH, BowlM, TennisH, TennisM, SquashH, SquashM, TableH, TableM, GolfH, GolfM, FootballH, FootballM, CricketH, CricketM, RowingH, RowingM, NetballH, NetballM, FishH, FishM, SnookerH, SnookerM, MusicH, MusicM, HorseH, HorseM, IceH, IceM, SailH, SailM, WrestleH, WrestleM))
  
  ##Cleaning Occupation
  
  ##Cleaning covariates 134-191
  for (z in 1:length(Occupational$type)) {
    if (Occupational$jobcheck[z]==2) {
      Occupational$type[z] <- dplyr::recode(Occupational$type[z], '-1'=0)
    }
  }
  Occupational$type <- dplyr::recode(Occupational$type, '0'=0, '1'=1, '2'=2, '3'=3, '4'=4, '-1'=NA_real_)
  
  table(Occupational$type, Occupational$jobcheck)
  
  
  ##Marital status recoded: 1=unmarried, 2= separated divorced widowed, 3=married. 
  ##(1 =8.  2 =1,3,4,6,7.  3 =2, 5,  -8= drop)
  ##Covariates$Married = recode(Covariates$Married, '8=1; 1=2; 3=2; 4=2; 6=2; 7=2; 2=3; 5=3')
  Covariates$Married <- dplyr::recode(Covariates$Married, '8'=1, '1'=2, '3'=2, '4'=2, '6'=2, '7'=2, '2'=3, '5'=3)
  
  ##drop BMI=-8 as insufficient info - line 132
  ##consider impute BMI<0 from self report
  
  for (z in 1:length(Covariates$BMI)) {
    if (Covariates$BMI[z]==-8) {
      Covariates$BMI[z] <- Covariates$BMIS[z]
    }
  }
  
  Covariates <- Covariates[!(Covariates$BMI<0),]
  
  Covariates$BMICats <- 0
  for (z in 1:length(Covariates$BMI)) {
    if (Covariates$BMI[z]<25 & !is.na(Covariates$BMI[z])) {
      Covariates$BMICats[z] <- 0
    }
    else if (Covariates$BMI[z]<=29.99 & !is.na(Covariates$BMI[z])) {
      Covariates$BMICats[z] <- 1
    }
    else if (Covariates$BMI[z]>29.99 & !is.na(Covariates$BMI[z]) ) {
      Covariates$BMICats[z] <- 2
    }
  }
  
  ##SEC Recoded: drop 9 (insufficient info). 4=high professional/managerial, 3=lower managerial and intermediate, 
  ##2= smaller employers/lower supervisory and technical, 1= routine/semi-routine occupations or not in work. 
  ##(1= 6,7,8, 2= 4,5, 3= 2,3, 4= 1,1.1, 1.2,1.3)
  Covariates$SEC <- dplyr::recode(Covariates$SEC, '6'=1, '7'=1, '8'=1, '4'=2, '5'=2, '2'=3, '3'=3, '1'=4, '1.1'=4, '1.2'=4, '1.3'=4)
  
  ##Smoker: drop: -8 not known, -9 refused, -1 not applicable,
  ##(0= 0,-1, 1= 2, 2= 3, 3= 4)
  Covariates$Smoker <- dplyr::recode(Covariates$Smoker, '1'=0, '2'=1, '3'=2, '4'=3)
  
  ##AlcRisk from BD10AUDG, already coded:, 2=higher risk, 1=unproblematic.
  Covariates$AlcRisk <- dplyr::recode(Covariates$AlcRisk, '1'=1, '2'=2, '-1'=0)
  
  ##Education: drop: -8 not enough info, -1 not applicable very high
  ##(0= 0, 1= 1,2,3,  2= 4,5, 3= 4,5, 4= 6,7, 5=8)
  #Covariates$Education <- dplyr::recode(Covariates$Education,'0'=0, '1'=1, '2'=1, '3'=1, '4'=2, '5'=2, '6'=3, '7'=3, '8'=4)
  Covariates$Education <- dplyr::recode(Covariates$Education,'0'=1, '1'=1, '2'=1, '3'=1, '4'=2, '5'=2, '6'=3, '7'=3, '8'=3)
  
  ##Disability: drop: -8 not enough info.
  ##(0= no disability, 1= limited disability, 2= severely hampered)  
  ## None required. 
  Covariates$Disability <- dplyr::recode(Covariates$Disability, '1'=1, '2'=2, '0'=0)
  
  

  ##Mood: drop -8 (not enough info).
  ##(1= low malaise, 2= high malaise)
  
  ##Drop Leisure, sports 35 unanswered 
  Leisure$na_count <- apply(is.na(Leisure), 1, sum)
  Activities$na_count <- apply(is.na(Activities), 1, sum)
  Activities <- Activities[!(Activities$na_count==35),]
  
  
  
  ##Create compositional PA data, merge back ID 
  ##200-203
  # PAREL <- subset(PA, select = -c(totalactivity, wear, ID))
  # PAREL <- as.data.frame(acomp(PAREL))
  # ID = subset(PA, select = -c(totalactivity, wear, mvpa, sedent, LIPA, SLEEP))
  # PAREL <- cbind(ID, PAREL)
  # rm(ID)
  
  
  ##Drop missing Cognition Tasks
  ## 210 - 215
  animal <- animal[!(animal$named<0),]
  letters <- letters[!(letters$missed<0 | letters$speed<0),]
  recall <- recall[!(recall$immediate<0 | recall$delayed<0),]
  
  
  ##Merge PARel & Cognition 
  ## 219 - 221
  #PACog <- merge(PA, letters, by = c("ID"))
  PACog <- merge(letters, animal, by = c("ID"))
  PACogCov <- merge(PACog, recall, by = c("ID"))
  PACogCov <- merge(Sex, PACogCov, by = c("ID"))
  ##Final Complete Sample Derivation
##Merge PACog & Covariates
##223 - 227
#PACogCov <- merge(PACog, Covariates, by = c("ID"))

##Drop intermediate DF's


##Scaling Cognition Tests for Composite Cognition
##236 - 296
##Invert missed score
PACogCov$missed <- -1*(PACogCov$missed)
#Scaling Executive Processing (z-scores, named, missed, speed)
named_z <- scale(PACogCov$named)
named_z <- as.numeric(named_z)
PACogCov$named_z <- named_z

missed_z <- scale(PACogCov$missed)
missed_z <- as.numeric(missed_z)
PACogCov$missed_z <- missed_z

speed_z <- scale(PACogCov$speed)
speed_z <- as.numeric(speed_z)
PACogCov$speed_z <- speed_z

#Scaling Memory (z-scores, memory delayed and immediate)
immediate_z <- scale(PACogCov$immediate)
immediate_z <- as.numeric(immediate_z)
PACogCov$immediate_z <- immediate_z

delayed_z <- scale(PACogCov$delayed)
delayed_z <- as.numeric(delayed_z)
PACogCov$delayed_z <- delayed_z


##Scaling Composite Scores
##Scaling Memory Composite
immediate_z <- as.matrix(immediate_z)
delayed_z <- as.matrix(delayed_z) 
memory_z <- immediate_z + delayed_z

##Scaling Executive Processing Composite
speed_z <- as.matrix(speed_z) 
missed_z <- as.matrix(missed_z)
named_z <- as.matrix(named_z)
executive_z <- (speed_z + missed_z + named_z)
PACogCov$memory_z <- memory_z
PACogCov$executive_z <- executive_z


##Creating Raw composite scores
##Average cognitive z_scores
PACogCov$CompositeCognitiveAVG <- 0
for (z in 1:length(PACogCov$CompositeCognitiveAVG))
{ PACogCov$CompositeCognitiveAVG[z] <- (sum(PACogCov$delayed_z[z], PACogCov$speed_z[z], PACogCov$immediate_z[z], PACogCov$missed_z[z], PACogCov$named_z[z])/5)
}

##Summing Cognitive z_scores 
PACogCov$CompositeCognitiveSum <- 0
for (z in 1:length(PACogCov$CompositeCognitiveSum))
{ PACogCov$CompositeCognitiveSum[z] <- (sum(PACogCov$delayed_z[z], PACogCov$speed_z[z], PACogCov$immediate_z[z], PACogCov$missed_z[z], PACogCov$named_z[z]))
}

#Scale composite cognition average and sum scores
Composite_z <- scale(PACogCov$CompositeCognitiveSum)
PACogCov$Composite_z <- as.numeric(Composite_z)
CompositeCognitiveAVG_Z <- scale(PACogCov$CompositeCognitiveAVG)
PACogCov$Composite_z_AVG <- as.numeric(CompositeCognitiveAVG_Z)



rm(delayed_z, missed_z, named_z, immediate_z, speed_z)
##Produce same results. 



##Finally Quartiling 300-350

##Find quartiles of composite scores
z_quartiles_composite <- quantile(Composite_z)
z_quartiles_compositeAVG <- quantile(CompositeCognitiveAVG_Z)


PACogCov$Composite_quartile <- 0

for (z in 1:length(PACogCov$Composite_quartile))
  if (PACogCov$Composite_z[z]<=z_quartiles_composite[2]) {
    PACogCov$Composite_quartile[z] <- 4
  } else if (PACogCov$Composite_z[z]<=z_quartiles_composite[3]) {
    PACogCov$Composite_quartile[z] <- 3
  } else if (PACogCov$Composite_z[z]<=z_quartiles_composite[4]) {
    PACogCov$Composite_quartile[z] <- 2
  } else  if (PACogCov$Composite_z[z]<=z_quartiles_composite[5]) {
    PACogCov$Composite_quartile[z] <- 1
  }  else {
    stop("Error in Composite loop")
  }

z_quartiles_composite



PACogCov$Composite_AVG_quartile <- 0

for (z in 1:length(PACogCov$Composite_AVG_quartile))
  if (PACogCov$Composite_z_AVG[z]<=z_quartiles_compositeAVG[2]) {
    PACogCov$Composite_AVG_quartile[z] <- 4
  } else if (PACogCov$Composite_z_AVG[z]<=z_quartiles_compositeAVG[3]) {
    PACogCov$Composite_AVG_quartile[z] <- 3
  } else if (PACogCov$Composite_z_AVG[z]<=z_quartiles_compositeAVG[4]) {
    PACogCov$Composite_AVG_quartile[z] <- 2
  } else  if (PACogCov$Composite_z_AVG[z]<=z_quartiles_compositeAVG[5]) {
    PACogCov$Composite_AVG_quartile[z] <- 1
  }  else {
    stop("Error in AVG loop")
  }

##Check Identical Composite Quartiles
for (i in 1:length(PACogCov$Composite_quartile)) {
  if (!identical(PACogCov$Composite_quartile [i], PACogCov$Composite_AVG_quartile [i])) {
    print(paste("Differs in row", i))
    stop()
  }
}



HTN$HTN <- 0 

`%!in%` <- Negate(`%in%`)

for (i in 1:length(HTN$ID)) {
  if ((HTN$PHV_SBP[i]<140 & HTN$ID[i] %!in% HTNmeds$ID) & !is.na(HTN$PHV_SBP[i])) {
    HTN$HTN[i] <- 0}
  else {HTN$HTN[i]<- 1 }
}

HTNMerge <- merge(HTN, PACogCov, by = c("ID"))
PACogCov$HTN <- HTNMerge$HTN 
table(HTNMerge$HTN, useNA = "always")

##CVD##
CVDStroke$B9PKHP19

#map(CVDStroke, table)
CVDStroke$CVD <- 0
for (i in 1:length(CVDStroke$ID)) {
  if (((CVDStroke$B10HRTP01[i]==1 & !is.na(CVDStroke$B10HRTP01[i])) | (CVDStroke$B10HRTP04[i]==1 & !is.na(CVDStroke$B10HRTP04[i])) | (CVDStroke$B9PKHP18[i]==1 & !is.na(CVDStroke$B9PKHP18[i])) | (CVDStroke$B10HRTP02[i]==1 & !is.na(CVDStroke$B10HRTP02[i])) | (CVDStroke$B10KHPB11[i]==1 & !is.na(CVDStroke$B10KHPB11[i])) )) {
    CVDStroke$CVD[i] <- 1 
  }
}
table(CVDStroke$CVD)
##Stroke##
CVDStroke$Stroke <- 0
for (i in 1:length(CVDStroke$ID)) {
  if ((CVDStroke$B10KHPB20[i]==1 & !is.na(CVDStroke$B10KHPB20[i])) |
      (CVDStroke$B9PKHP19[i]==1 & !is.na(CVDStroke$B9PKHP19[i]))) {
    CVDStroke$Stroke[i] <- 1 
  }
}
CVDStrokeMerge <- merge(PACogCov, CVDStroke, by = c("ID"))
PACogCov$CVD <- CVDStrokeMerge$CVD
PACogCov$Stroke <- CVDStrokeMerge$Stroke
table(PACogCov$Stroke, useNA = "always")
PACogCov$CVDHTN <- ifelse(PACogCov$Stroke==1 | PACogCov$CVD==1 | PACogCov$HTN==1, 1, 0)
#PACogCov$CVDHTN <- ifelse(is.na(PACogCov$Stroke) & is.na(PACogCov$CVD) & is.na(PACogCov$HTN), NA, 0)
#table(PACogCov$CVDHTN)
#table(PACogCov$Stroke)
#table(PACogCov$HTN)


##One variable is crossed over (in both) incorrectly ##

##Region##BD10GOR
Region$Region <- dplyr::recode(Region$Region, '8'=1, '9'=1, '13'=1, '7'=1, '4'=2, '5'=2, '6'=2, '1'=3, '2'=3, '3'=3, '14'=3, '11'=4, '10'=5, '12'=6)
RegionMerge <- merge(PACogCov, Region, by = c("ID"))
PACogCov$Region <- RegionMerge$Region

##Diabetes##
DiabetesMerge <- merge(PACogCov, Diabetes, by = c("ID"))
PACogCov$Diabetes <- DiabetesMerge$PHV_diabetesany
##Collapse Sports cols##


Sports <- subset(Activities, select= -c(Lawn, Walk, Garden, Water, DIY, Dig, Music, na_count))
for (i in 1:length(Sports$ID)) {
  for (j in 2:ncol(Sports)) {
    if (Sports[i,j]==1 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 0
    }
    else if (Sports[i,j]==2 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 0.5
    }
    else if (Sports[i,j]==3 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 1
    }
    else if (Sports[i,j]==4 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 2.5
    }
    else if (Sports[i,j]==5 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 4
    }
    else if (Sports[i,j]==6 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 10
    }
    else if (Sports[i,j]==7 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 18
    }
    else if (Sports[i,j]==8 & !is.na(Sports[i,j])) {
      Sports[i,j] <- 24
    }
    else if ((Sports[i,j]==-1) & !is.na(Sports[i,j])) {
      Sports[i,j] <- 0 }
    else if ((Sports[i,j]<-1 ) & !is.na(Sports[i,j])) {
      Sports[i,j] <- NA_real_
    }
  }}

head(Sports)


Sports$SportCats <- 0
Sports$SportCats <- rowSums(Sports[2:29])
Sports$SportCats2 <- rowSums(Sports[2:29])

##Groups Middle Boundary##
for (i in 1:length(Sports$ID)) {
  if (Sports$SportCats[i]==0 & !is.na(Sports$SportCats[i])) {
    Sports$SportCats[i] <-  0
  }
  else if (Sports$SportCats[i]<1 & !is.na(Sports$SportCats[i])) {
    Sports$SportCats[i] <-  1
  }
  else if (Sports$SportCats[i]<4 & !is.na(Sports$SportCats[i])) {
    Sports$SportCats[i] <-  2
  }
  else if (Sports$SportCats[i]<=12 & !is.na(Sports$SportCats[i])) {
    Sports$SportCats[i] <-  3
  }
  else if (Sports$SportCats[i]>12 & !is.na(Sports$SportCats[i])) {
    Sports$SportCats[i] <- 4
  }
}

#0 - none -> <monthly (0.1,1);1 - 2-3/m - 1/week (2,3,4); 2 >weekly (7,6,5) ?
Sports$SportsTrichotomy46 <- dplyr::recode(Sports$SportCats, '0'=0, '1'=1, '2'=1, '3'=2, '4'=2)

Sports$SportSocial <- 0
for (i in 1:length(Sports$SportCats)) {
  if (Sports$Mount[i]>0 & !is.na(Sports$Mount[i]) 
      | Sports$Golf[i]>0 & !is.na(Sports$Golf[i])
      | Sports$Bowl[i]>0 & !is.na(Sports$Bowl[i])
      | Sports$Dance[i]>0 & !is.na(Sports$Dance[i])
      | Sports$Tennis[i]>0 & !is.na(Sports$Tennis[i])
      | Sports$Table[i]>0 & !is.na(Sports$Table[i])
      | Sports$Rowing[i]>0 & !is.na(Sports$Rowing[i])
      | Sports$Squash[i]>0 & !is.na(Sports$Squash[i])
      | Sports$Football[i]>0 & !is.na(Sports$Football[i])
      | Sports$Snooker[i]>0 & !is.na(Sports$Snooker[i])
      | Sports$Netball[i]>0 & !is.na(Sports$Netball[i])
      | Sports$Wrestle[i]>0 & !is.na(Sports$Wrestle[i])
      | Sports$Cricket[i]>0 & !is.na(Sports$Cricket[i])) {
    Sports$SportSocial[i] <- 2 } 
  else if (Sports$SportCats[i]>0 & !is.na(Sports$SportCats[i])) {
    Sports$SportSocial[i] <- 1 }
}

TeamSports <- subset(Sports, select= c(ID, Mount, Golf, Bowl, Dance, Tennis, Table, Rowing, Squash, Football, Snooker, Football, Netball, Wrestle, Cricket))
TeamSports$SportCats <- 0
TeamSports$SportCats <- rowSums(TeamSports[2:16])
TeamSports$SportCats2 <- rowSums(TeamSports[2:16])

##Groups Middle Boundary##
for (i in 1:length(TeamSports$ID)) {
  if (TeamSports$SportCats[i]==0 & !is.na(TeamSports$SportCats[i])) {
    TeamSports$SportCats[i] <-  0
  }
  else if (TeamSports$SportCats[i]<1 & !is.na(TeamSports$SportCats[i])) {
    TeamSports$SportCats[i] <-  1
  }
  else if (TeamSports$SportCats[i]<4 & !is.na(TeamSports$SportCats[i])) {
    TeamSports$SportCats[i] <-  2
  }
  else if (TeamSports$SportCats[i]<=12 & !is.na(TeamSports$SportCats[i])) {
    TeamSports$SportCats[i] <-  3
  }
  else if (TeamSports$SportCats[i]>12 & !is.na(TeamSports$SportCats[i])) {
    TeamSports$SportCats[i] <- 4
  }
}

#0 - none -> <monthly (0.1,1);1 - 2-3/m - 1/week (2,3,4); 2 >weekly (7,6,5) ?
Sports$TeamSportsTrichotomy46 <- dplyr::recode(TeamSports$SportCats, '0'=0, '1'=1, '2'=1, '3'=2, '4'=2)
table(Sports$TeamSportsTrichotomy46)


###Cycle
Cycle46 <- subset(Sports, select=c(ID, Cycle, CCycle))
Cycle46$SportCats <- 0
Cycle46$SportCats <- rowSums(Cycle46[2:3])
Cycle46$SportCats2 <- rowSums(Cycle46[2:3])

##Groups Middle Boundary##
for (i in 1:length(Cycle46$ID)) {
  if (Cycle46$SportCats[i]==0 & !is.na(Cycle46$SportCats[i])) {
    Cycle46$SportCats[i] <-  0
  }
  else if (Cycle46$SportCats[i]<1 & !is.na(Cycle46$SportCats[i])) {
    Cycle46$SportCats[i] <-  1
  }
  else if (Cycle46$SportCats[i]<4 & !is.na(Cycle46$SportCats[i])) {
    Cycle46$SportCats[i] <-  2
  }
  else if (Cycle46$SportCats[i]<=12 & !is.na(Cycle46$SportCats[i])) {
    Cycle46$SportCats[i] <-  3
  }
  else if (Cycle46$SportCats[i]>12 & !is.na(Cycle46$SportCats[i])) {
    Cycle46$SportCats[i] <- 4
  }
}

#0 - none -> <monthly (0.1,1);1 - 2-3/m - 1/week (2,3,4); 2 >weekly (7,6,5) ?
Sports$CycleTrichotomy46 <- dplyr::recode(Cycle46$SportCats, '0'=0, '1'=1, '2'=1, '3'=2, '4'=2)
table(Sports$CycleTrichotomy46)

##Swim cats
Swim46 <- subset(Sports, select=c(ID, Swim, CSwim))
Swim46$SportCats <- 0
Swim46$SportCats <- rowSums(Swim46[2:3])
Swim46$SportCats2 <- rowSums(Swim46[2:3])

##Groups Middle Boundary##
for (i in 1:length(Swim46$ID)) {
  if (Swim46$SportCats[i]==0 & !is.na(Swim46$SportCats[i])) {
    Swim46$SportCats[i] <-  0
  }
  else if (Swim46$SportCats[i]<1 & !is.na(Swim46$SportCats[i])) {
    Swim46$SportCats[i] <-  1
  }
  else if (Swim46$SportCats[i]<4 & !is.na(Swim46$SportCats[i])) {
    Swim46$SportCats[i] <-  2
  }
  else if (Swim46$SportCats[i]<=12 & !is.na(Swim46$SportCats[i])) {
    Swim46$SportCats[i] <-  3
  }
  else if (Swim46$SportCats[i]>12 & !is.na(Swim46$SportCats[i])) {
    Swim46$SportCats[i] <- 4
  }
}

#0 - none -> <monthly (0.1,1);1 - 2-3/m - 1/week (2,3,4); 2 >weekly (7,6,5) ?
Sports$SwimTrichotomy46 <- dplyr::recode(Swim46$SportCats, '0'=0, '1'=1, '2'=1, '3'=2, '4'=2)
table(Sports$SwimTrichotomy46)


Walk46 <- subset(Activities, select= c(ID, Walk))
for (i in 1:length(Walk46$ID)) {
  for (j in 2:ncol(Walk46)) {
    if (Walk46[i,j]==1 & !is.na(Walk46[i,j])) {
      Walk46[i,j] <- 0
    }
    else if (Walk46[i,j]==2 & !is.na(Walk46[i,j])) {
      Walk46[i,j] <- 0.5
    }
    else if (Walk46[i,j]==3 & !is.na(Walk46[i,j])) {
      Walk46[i,j] <- 1
    }
    else if (Walk46[i,j]==4 & !is.na(Walk46[i,j])) {
      Walk46[i,j] <- 2.5
    }
    else if (Walk46[i,j]==5 & !is.na(Walk46[i,j])) {
      Walk46[i,j] <- 4
    }
    else if (Walk46[i,j]==6 & !is.na(Walk46[i,j])) {
      Walk46[i,j] <- 10
    }
    else if (Walk46[i,j]==7 & !is.na(Walk46[i,j])) {
      Walk46[i,j] <- 18
    }
    else if (Walk46[i,j]==8 & !is.na(Walk46[i,j])) {
      Walk46[i,j] <- 24
    }
    else if ((Walk46[i,j]==-8 |  Walk46[i,j]==-1) & !is.na(Walk46[i,j])) {
      Walk46[i,j] <- 0
    }
  }}

Walk46$SportCats <- Walk46$Walk

##Groups Middle Boundary##
for (i in 1:length(Walk46$ID)) {
  if (Walk46$SportCats[i]==0 & !is.na(Walk46$SportCats[i])) {
    Walk46$SportCats[i] <-  0
  }
  else if (Walk46$SportCats[i]<1 & !is.na(Walk46$SportCats[i])) {
    Walk46$SportCats[i] <-  1
  }
  else if (Walk46$SportCats[i]<4 & !is.na(Walk46$SportCats[i])) {
    Walk46$SportCats[i] <-  2
  }
  else if (Walk46$SportCats[i]<=12 & !is.na(Walk46$SportCats[i])) {
    Walk46$SportCats[i] <-  3
  }
  else if (Walk46$SportCats[i]>12 & !is.na(Walk46$SportCats[i])) {
    Walk46$SportCats[i] <- 4
  }
}

#0 - none -> <monthly (0.1,1);1 - 2-3/m - 1/week (2,3,4); 2 >weekly (7,6,5) ?
Sports$WalkTrichotomy46 <- dplyr::recode(Walk46$SportCats, '0'=0, '1'=1, '2'=1, '3'=2, '4'=2)
table(Sports$WalkTrichotomy46)


SportMerge <- merge(TeamSports, Sports, by = c("ID"))
SportsCats46 <- subset(SportMerge, select=c(ID, TeamSportsTrichotomy46, SportsTrichotomy46, CycleTrichotomy46, SwimTrichotomy46, WalkTrichotomy46))
SportMerge <- merge(PACogCov, SportMerge, by = c("ID"))


##
OpenSkill <- subset(Sports, select= c(ID, Tennis, Squash, Football, Cricket, Netball, Sail, Wrestle))


ClosedSkill <- subset(Sports, select= c(ID, CSwim, Swim, Mount, CCycle, Cycle, Aero 
                                         , Aero2, Weight, Condition, Yoga, 
                                         Dance, CRun, Jog, Bowl, Golf, Rowing, Fish, 
                                         Horse, Snooker, Ice))

table(ClosedSkill$CSwim, useNA="always")
table(ClosedSkill$Weight, useNA = "always")

OpenSkillfreq <- OpenSkill
Openfreqlist <- vector("double", 8561)
for (i in 1:length(OpenSkillfreq$ID)) {
  for (j in 2:ncol(OpenSkillfreq)) {
    if (OpenSkillfreq[i,j]>=4 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]>=3  &  !is.na(Openfreqlist[i])) {
      Openfreqlist[i] <- 4
    }
    else if (OpenSkillfreq[i,j]>=4 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]<3) {
      Openfreqlist[i] <- 3
    }
    else if (OpenSkillfreq[i,j]>=1 & OpenSkillfreq[i,j]<4 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]==2) {
      Openfreqlist[i] <- 2.5
    }
    else if (OpenSkillfreq[i,j]>=1 & OpenSkillfreq[i,j]<4 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]<2) {
      Openfreqlist[i] <- 2
    }
    else if (OpenSkillfreq[i,j]<1 & OpenSkillfreq[i,j]>0 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]<2) {
      Openfreqlist[i] <- 1
    }
    else if (OpenSkillfreq[i,j]==0 & !is.na(OpenSkillfreq[i,j]) & Openfreqlist[i]<1) {
        Openfreqlist[i] <- 1
    }
    else if (is.na(OpenSkillfreq[i,j]) & (Openfreqlist[i]==0 | is.na(Openfreqlist[i]))) {
      Openfreqlist[i] <- -99
    }
  }}

ClosedSkillfreq <- ClosedSkill
Closedfreqlist <- vector("double", 8561)
for (i in 1:length(ClosedSkillfreq$ID)) {
  for (j in 2:ncol(ClosedSkillfreq)) {
    if (ClosedSkillfreq[i,j]>=4 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]>=3  &  !is.na(Closedfreqlist[i])) {
      Closedfreqlist[i] <- 4
    }
    else if (ClosedSkillfreq[i,j]>=4 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]<3) {
      Closedfreqlist[i] <- 3
    }
    else if (ClosedSkillfreq[i,j]>=1 & ClosedSkillfreq[i,j]<4 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]==2) {
      Closedfreqlist[i] <- 2.5
    }
    else if (ClosedSkillfreq[i,j]>=1 & ClosedSkillfreq[i,j]<4 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]<2) {
      Closedfreqlist[i] <- 2
    }
    else if (ClosedSkillfreq[i,j]<1 & ClosedSkillfreq[i,j]>0 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]<2) {
      Closedfreqlist[i] <- 1
    }
    else if (is.na(ClosedSkillfreq[i,j]) & (Closedfreqlist[i]==0 | is.na(Closedfreqlist[i]))) {
      Closedfreqlist[i] <- -99 }
     else if (ClosedSkillfreq[i,j]==0 & !is.na(ClosedSkillfreq[i,j]) & Closedfreqlist[i]<1) {
        Closedfreqlist[i] <- 1
      }
    else if (ClosedSkillfreq[i,j]<0 & Closedfreqlist[i]==0) {
      Closedfreqlist[i] <- -99
    }
  }}




SportsCats46$openskill_46 <- Openfreqlist
SportsCats46$closedskill_46 <- Closedfreqlist
SportsCats46$Open_Skill_Tri46 <- dplyr::recode(SportsCats46$openskill_46, '-99'=NA_real_, '1'=0, '2'=1, '2.5'=1, '3'=2, '4'=2)
SportsCats46$Closed_Skill_Tri46 <- dplyr::recode(SportsCats46$closedskill_46, '-99'=NA_real_, '1'=0, '2'=1, '2.5'=1, '3'=2, '4'=2)




##Outlier check##
# outliers <- c()
# for (i in 1:length(SportMerge$ID)) {
#   if (SportMerge$SportCats2[i]>90 & !is.na(SportMerge$SportCats2[i]) ) {
#     outliers <- append(outliers, i)
#   }
# }

Social <- subset(BCS70_main, select= c("BCSID", "B10FAMMT", "B10FREMT"))
Social <- rename(Social, "ID"=BCSID, "Friends"=B10FREMT, "Family"=B10FAMMT)
Social$Family <- dplyr::recode(Social$Family, '8'=60, '7'=10, '6'=10, '5'=20, '4'=30, '3'=40, '2'=50, '1'=60, '-1'=NA_real_, '-8'=NA_real_)
Social$Friends <- dplyr::recode(Social$Friends, '8'=0, '7'=0, '1'=60, '2'=50, '3'=40, '4'=30, '5'=20, '6'=10, '-1'=NA_real_, '-8'=NA_real_)
table(BCS70_main$B10FREMT)

#SocialMerge <- merge(PACogCov, Social, by = c("ID"))
#PACogCov$Family <- SocialMerge$Family
#PACogCov$Friends <- SocialMerge$Friends

#Sport <- subset(SportMerge, select= c("ID","SportCats", "SportSocial"))
#PACogCov <- merge(PACogCov, Sport, by = c("ID"))
#PACogCov <- rename(PACogCov, SportsCatsNoWalk='SportCats', SportsSocial = 'SportSocial')

RHR <- subset(RHR, select=c("BCSID", "RHR"))
names(RHR) <- c("ID", "RHR")

PACogCov <- merge(PACogCov, RHR, by="ID")
#rm(animal, letters, recall, PAREL, PA, BCSAvg, BCS70_main, Covariates, PACog)

#rm(i, z, memory_z, Composite_z, executive_z, CompositeCognitiveAVG_Z)
PACogCov <- merge(PACogCov, Covariates, by="ID", all.x=TRUE)


##Biomarkers:
Biomarkers <- dplyr::rename(Biomarkers, ID='BCSID')
Biomarkers$B10HBA1C[Biomarkers$B10HBA1C==-1 | Biomarkers$B10HBA1C==-8 ] <- NA
Biomarkers$B10CHOL[Biomarkers$B10CHOL==-1 | Biomarkers$B10CHOL==-8] <- NA
Biomarkers$B10HDL[Biomarkers$B10HDL==-1 | Biomarkers$B10HDL==-8] <- NA
Biomarkers$nonHDLratio <- Biomarkers$B10CHOL/Biomarkers$B10HDL
Biomarkers$B10BPSYSR1[Biomarkers$B10BPSYSR1 ==-1 | Biomarkers$B10BPSYSR1==-8] <- NA
Biomarkers$B10BPSYSR2[Biomarkers$B10BPSYSR2 ==-1 | Biomarkers$B10BPSYSR2==-8] <- NA
Biomarkers$B10BPSYSR3[Biomarkers$B10BPSYSR3 ==-1 | Biomarkers$B10BPSYSR3==-8] <- NA
Biomarkers$BP <- NA
for (i in 1:length(Biomarkers$ID)) {
  if ( is.na(Biomarkers$B10BPSYSR1[i]) | is.na(Biomarkers$B10BPSYSR2[i]) | is.na(Biomarkers$B10BPSYSR3[i])) {
    Biomarkers$BP[i] <- NA
  }
  else {
    Biomarkers$BP[i] <- mean(colMeans(Biomarkers[i, 6:8]))
  }
}

PACogCov <- merge(PACogCov, Biomarkers, by='ID', all.x=T)


return(list(Activities,Leisure,Occupational,PACogCov,Transport, SportsCats46))
}


##NB: <=4 changed to<4 ; as most people have high participation. 
