##Age 10

age10_output <- function(df1, df2) {
age10_vars <- as.data.frame(age10$bcsid)
names(age10_vars) <- "ID"
#dfSEC <- subset(age10_derived, select=c("BD3PSOC", "BCSID"))
#dfSEC <- rename(dfSEC, ID='BCSID', SEC="BD3PSOC")
##Exposure##
##Sports coding (1 never/hardly, sometimes 2, often 3, blank -7, )
age10_vars$sport <- as.numeric(age10$m84)
age10_vars$bike <- as.numeric(age10$m87)
age10_vars$walk <- as.numeric(age10$m90)
age10_vars$swim <- as.numeric(age10$m94)
table(age10$m101, useNA="always")

for (i in 1:length(age10_vars$sport)) {
  if (age10_vars$sport[i]>3 & !is.na(age10_vars$sport[i])) {
    age10_vars$sport[i] <- NA_real_}
  else if ((age10_vars$sport[i]==-3 | age10_vars$sport[i]==0 | age10_vars$sport[i]==" " |  age10_vars$sport[i]=="" )  & !is.na(age10_vars$sport[i])) {
  age10_vars$sport[i] <- 1}
  else if (age10_vars$sport[i]==-7 & !is.na(age10_vars$sport[i])) {
    age10_vars$sport[i] <- NA_real_}
  else if (age10_vars$sport[i]==-1 & !is.na(age10_vars$sport[i])) {
    age10_vars$sport[i] <- NA_real_}
  else if (is.na(age10_vars$sport[i])) {
    age10_vars$sport[i] <- NA_real_
  }
}
for (i in 1:length(age10_vars$bike)) {
  if (age10_vars$bike[i]>3 & !is.na(age10_vars$bike[i])) {
    age10_vars$bike[i] <- NA_real_}
  else if ((age10_vars$bike[i]==-3 | age10_vars$bike[i]==0 | age10_vars$bike[i]==" " |  age10_vars$bike[i]=="" )  & !is.na(age10_vars$bike[i])) {
    age10_vars$bike[i] <- 1}
  else if (age10_vars$bike[i]==-7 & !is.na(age10_vars$bike[i])) {
    age10_vars$bike[i] <- NA_real_}
  else if (age10_vars$bike[i]==-1 & !is.na(age10_vars$bike[i])) {
    age10_vars$bike[i] <- NA_real_}
  else if (is.na(age10_vars$bike[i])) {
    age10_vars$bike[i] <- NA_real_
  }
}

for (i in 1:length(age10_vars$swim)) {
  if (age10_vars$swim[i]>3 & !is.na(age10_vars$swim[i])) {
    age10_vars$swim[i] <- NA_real_}
  else if ((age10_vars$swim[i]==-3 | age10_vars$swim[i]==0 | age10_vars$swim[i]==" " |  age10_vars$swim[i]=="" )  & !is.na(age10_vars$swim[i])) {
    age10_vars$swim[i] <- 1}
  else if (age10_vars$swim[i]==-7 & !is.na(age10_vars$swim[i])) {
    age10_vars$swim[i] <- NA_real_}
  else if (age10_vars$swim[i]==-1 & !is.na(age10_vars$swim[i])) {
    age10_vars$swim[i] <- NA_real_}
  else if (is.na(age10_vars$swim[i])) {
    age10_vars$swim[i] <- NA_real_
  }
}



for (i in 1:length(age10_vars$swim)) {
  if ((age10_vars$swim[i]==3 & !is.na(age10_vars$swim[i])) & (age10_vars$sport[i]!=3 | is.na(age10_vars$sport[i]))) {
    age10_vars$sport[i] <- 3}
  else if ((age10_vars$swim[i]==2 & !is.na(age10_vars$swim[i])) & (age10_vars$sport[i]<2 | is.na(age10_vars$sport[i]))) {
    age10_vars$sport[i] <- 2}
  else if ((age10_vars$swim[i]==1 & !is.na(age10_vars$swim[i])) & (age10_vars$sport[i]<1 | is.na(age10_vars$sport[i]))) {
    age10_vars$sport[i] <- 1}
}


for (i in 1:length(age10_vars$bike)) {
  if ((age10_vars$bike[i]==3 & !is.na(age10_vars$bike[i])) & (age10_vars$sport[i]<3| is.na(age10_vars$sport[i]))) {
    age10_vars$sport[i] <- 3}
  else if ((age10_vars$bike[i]==2 & !is.na(age10_vars$bike[i])) & (age10_vars$sport[i]<2 | is.na(age10_vars$sport[i]))) {
    age10_vars$sport[i] <- 2}
  else if ((age10_vars$bike[i]==1 & !is.na(age10_vars$bike[i])) & (age10_vars$sport[i]<1 | is.na(age10_vars$sport[i]))) {
    age10_vars$sport[i] <- 1}
}



for (i in 1:length(age10_vars$walk)) {
  if (age10_vars$walk[i]>3 & !is.na(age10_vars$walk[i])) {
    age10_vars$walk[i] <- NA_real_}
  else if ((age10_vars$walk[i]==-3 | age10_vars$walk[i]==0 | age10_vars$walk[i]==" " |  age10_vars$walk[i]=="" )  & !is.na(age10_vars$walk[i])) {
    age10_vars$walk[i] <- 1}
  else if (age10_vars$walk[i]==-7 & !is.na(age10_vars$walk[i])) {
    age10_vars$walk[i] <- NA_real_}
  else if (age10_vars$walk[i]==-1 & !is.na(age10_vars$walk[i])) {
    age10_vars$walk[i] <- NA_real_}
  else if (is.na(age10_vars$walk[i])) {
    age10_vars$walk[i] <- NA_real_
  }
}


OpenSkillFreq <- subset(age10_vars, select=c("ID", "swim", "bike", "sport"))
OpenSkillFreq <- drop_na(OpenSkillFreq)
OpenSkillfreqlist <- vector("double", 6293)
for (i in 1:length(OpenSkillFreq$swim)) {
  for (j in 2:ncol(OpenSkillFreq)) {
    if (OpenSkillFreq[i,j]==3 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]>=3  &  !is.na(OpenSkillfreqlist[i])) {
      OpenSkillfreqlist[i] <- 4
    }
    else if (OpenSkillFreq[i,j]==3 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<=2) {
      OpenSkillfreqlist[i] <- 3
    }
    else if (OpenSkillFreq[i,j]==2 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]<2) {
      OpenSkillfreqlist[i] <- 2
    }
    else if (OpenSkillFreq[i,j]==2 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]==2) {
      OpenSkillfreqlist[i] <- 2.5
    }
    else if (OpenSkillFreq[i,j]==1 & !is.na(OpenSkillFreq[i,j]) & OpenSkillfreqlist[i]==0)  {
      OpenSkillfreqlist[i] <- 1
    }
  }}
# 
# age10_vars %>% filter(is.na(sport) & is.na(bike) & is.na(swim)) %>% count(.)


OpenSkillFreq$Multi_Sport10 <- OpenSkillfreqlist
OpenSkillFreq <- subset(OpenSkillFreq, select=-c(swim, sport, bike))
age10_vars <- merge(age10_vars, OpenSkillFreq, by=c("ID"), all.x=T)
age10_vars <- age10_vars[-c(1:23), ]


table(age10_vars$Multi_Sport_10, useNA="always")
table(age10_vars$bike, useNA="always")
table(age10_vars$walk, useNA="always")
table(age10_vars$sport, useNA="always")
table(age10_vars$swim, useNA="always")

age10_vars$sport <- dplyr::recode(age10_vars$sport, '1'=0, '2'=1, '3'=2)
age10_vars$bike <- dplyr::recode(age10_vars$bike, '1'=0, '2'=1, '3'=2)
age10_vars$walk <- dplyr::recode(age10_vars$walk, '1'=0, '2'=1, '3'=2)
age10_vars$swim <- dplyr::recode(age10_vars$swim, '1'=0, '2'=1, '3'=2)

table(age10_vars$sport)
age10_vars$sport_10 <- age10_vars$sport
age10_vars$swim_10 <- age10_vars$swim
age10_vars$bike_10 <- age10_vars$bike
age10_vars$walk_10 <- age10_vars$walk








age10_activities <- subset(age10_vars, select=c("ID", "sport_10", "swim_10", "bike_10", "walk_10"))
age10_activities$na_count <- apply(is.na(age10_activities), 1, sum)
age10_activities <- age10_activities[!(age10_activities$na_count==4),]
age10_activities <- subset(age10_activities, select=c("ID"))
age10_vars <- merge(age10_vars, age10_activities, by=c("ID"))




##all LTPA
# for (i in 1:length(age10_vars$walk_10)) {
#   if ((age10_vars$sport_10[i]==3 & !is.na(age10_vars$sport_10[i])) | (age10_vars$swim_10[i]==3 & !is.na(age10_vars$swim_10[i])) | (age10_vars$bike_10[i]==3 & !is.na(age10_vars$bike_10[i])) | (age10_vars$walk_10[i]==3 & !is.na(age10_vars$walk_10[i])) ) {
#     age10_vars$walk[i] <- 3}
#   else if ((age10_vars$sport_10[i]==2 & !is.na(age10_vars$sport_10[i])) | (age10_vars$swim_10[i]==2 & !is.na(age10_vars$swim_10[i])) | (age10_vars$bike_10[i]==2 & !is.na(age10_vars$bike_10[i])) | (age10_vars$walk_10[i]==2 & !is.na(age10_vars$walk_10[i])) ) {
#     age10_vars$walk[i] <- 2}
#   else if ((age10_vars$sport_10[i]==1 & !is.na(age10_vars$sport_10[i])) | (age10_vars$swim_10[i]==1 & !is.na(age10_vars$swim_10[i])) | (age10_vars$bike_10[i]==1 & !is.na(age10_vars$bike_10[i])) | (age10_vars$walk_10[i]==1 & !is.na(age10_vars$walk_10[i])) ) {
#     age10_vars$walk[i] <- 1}
# }




##Age_10_PCA
library('corrr')
library("FactoMineR")


##Digit recall: i3541-i3574   1- Correct; 2- Incorrect; 9- No response;
##Definitions: i3504-i3540  1- Correct; 2- Incorrect; 9- No response;
##Similarities: i3616-i3576  1- correct; 2 - Unacceptable; 9 - No response; 
##Matrices: i3617-i3644   1- Correct; 2- Incorrect; 9- No response;

##---------------------------------------------Digit_Recall

Digit_recall <- subset(age10, select=(i3541:i3574))
Digit_recall <- Digit_recall %>%
  mutate_at(vars(1:length(Digit_recall)),
            function(x) case_when(x < 1 ~ NA_real_,
                                  x == 1 ~ 1,
                                  x == 2 ~ 2, 
                                  x > 2 ~ NA_real_))
NA_all <- rowSums(is.na(Digit_recall))
DigitSum <- vector("double", length(Digit_recall$i3541))
for (i in 1:length(NA_all)) { 
  if (NA_all[i]==34) { DigitSum[i] <- NA_real_ } }

table(DigitSum, useNA="always")

for (i in 1:length(Digit_recall$i3541)){
  for (j in 1:ncol(Digit_recall)) {
    if (Digit_recall[i,j]==1 & !is.na(Digit_recall[i,j])) {
      DigitSum[i] <- DigitSum[i]+1 
    }
  } }  
#Digit_recall$ID <- age10$bcsid
age10$DigitSum <- DigitSum  
table(DigitSum, useNA = "always")

##-------------------------------------Definitions#

Definitions <- subset(age10, select=(i3504:i3540))
Definitions <- Definitions %>%
  mutate_at(vars(1:length(Definitions)),
            function(x) case_when(x < 1 ~ NA_real_,
                                  x == 1 ~ 1,
                                  x == 2 ~ 2, 
                                  x > 2 ~ NA_real_))
NA_all <- rowSums(is.na(Definitions))
table(NA_all)
Definitions_Sum <- vector("double", length(Definitions$i3504))
for (i in 1:length(NA_all)) { 
  if (NA_all[i]==37) { Definitions_Sum[i] <- NA_real_ } }

for (i in 1:length(Definitions$i3504)){
  for (j in 1:ncol(Definitions)) {
    if (Definitions[i,j]==1 & !is.na(Definitions[i,j])) {
      Definitions_Sum[i] <- Definitions_Sum[i]+1 
    }
  } }  
age10$Definitions_Sum <- Definitions_Sum  
table(Definitions_Sum, useNA = "always")
#Definitions$ID <- age10$bcsid


##-------------------------------------Similarities##

Similarities <- subset(age10, select=(i3576:i3616))
Similarities <- Similarities %>%
  mutate_at(vars(1:length(Similarities)),
            function(x) case_when(x < 1 ~ NA_real_,
                                  x == 1 ~ 1,
                                  x == 2 ~ 2, 
                                  x > 2 ~ NA_real_))
NA_all <- rowSums(is.na(Similarities))
table(NA_all)
Similarities_Sum <- vector("double", length(Similarities$i3576))
for (i in 1:length(NA_all)) { 
  if (NA_all[i]==41) { Similarities_Sum[i] <- NA_real_ } }

for (i in 1:length(Similarities$i3576)){
  for (j in 1:ncol(Similarities)) {
    if (Similarities[i,j]==1 & !is.na(Similarities[i,j])) {
      Similarities_Sum[i] <- Similarities_Sum[i]+1 
    }
  } }  
age10$Similarities_Sum <- Similarities_Sum  
table(Similarities_Sum, useNA = "always")
#Similarities$ID <- age10$bcsid

##-------------------------------------Matrices##
#Matrices: i3617-i3644
Matrices <- subset(age10, select=(i3617:i3644))
Matrices <- Matrices %>%
  mutate_at(vars(1:length(Matrices)),
            function(x) case_when(x < 1 ~ NA_real_,
                                  x == 1 ~ 1,
                                  x == 2 ~ 2, 
                                  x > 2 ~ NA_real_))
NA_all <- rowSums(is.na(Matrices))
table(NA_all)
Matrices_Sum <- vector("double", length(Matrices$i3617))
for (i in 1:length(NA_all)) { 
  if (NA_all[i]==28) { Matrices_Sum[i] <- NA_real_ } }

for (i in 1:length(Matrices$i3617)){
  for (j in 1:ncol(Matrices)) {
    if (Matrices[i,j]==1 & !is.na(Matrices[i,j])) {
      Matrices_Sum[i] <- Matrices_Sum[i]+1 
    }
  } }  
age10$Matrices_Sum <- Matrices_Sum  
table(Matrices_Sum, useNA = "always")
#Matrices$ID <- age10$bcsid

##----------------------PCA
df_PCA <- data.frame(age10[1], Matrices_Sum, DigitSum, Similarities_Sum, Definitions_Sum)
df_PCA <- na.omit(df_PCA)
colSums(is.na( df_PCA ))
numerical_data <- df_PCA[,2:5]
numerical_data
data_normalized <- scale(numerical_data)
head(data_normalized)
#sum(data_normalized[1:6916, 1], na.rm=TRUE)
##Calculate Corr Matrix
corr_matrix <- cor(data_normalized)
cor(data_normalized)
library(ggcorrplot)
#install.packages("ggcorrplot")
ggcorrplot(corr_matrix)
data.pca <- prcomp(data_normalized, scale = FALSE)
summary(data.pca)
data.pca$loadings[, 1:2]
library("factoextra")
PCA_out <- get_pca_ind(data.pca)


#install.packages("factoextra")
library("factoextra")

fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")
fviz_cos2(data.pca, choice = "var", axes = 1:2)

df_PCA$PCA1 <- scale(PCA_out$coord[,1])
df_PCA$g <- df_PCA$PCA1*15+100
PCAsubset <- subset(df_PCA, select=c(bcsid, g))
PCAsubset$g <- as.vector(df_PCA$g)
tail(df_PCA)


range(df_PCA$g)
##Sensitivity w/o outliers##


##Covariates##
age10_covars <- subset(age10, select=(bcsid))
##Disability coding (1 Yes, slightly, 2 Yes, severe, 3 No, 4 Not known) 
age10_covars$disability <- age10$b24.1
age10_covars$disability_10 <- dplyr::recode(age10$b24.1, '3'=0, '1'=1, '2'=2, '4'=NA_real_, '9'=NA_real_)
##Neighbourhood description (1 - Yes; -7 - Blank)
age10_covars$dwell_rural <- age10$m304
age10_covars$dwell_village <- age10$m305
age10_covars$dwell_suburban <- age10$m306
age10_covars$dwell_urban <- age10$m307
age10_covars$dwell_innerurban <- age10$m308
age10_covars$dwell_non_residential <- age10$m309
##Other_values: Value = 1.0	 Large seaside resort 2.0 Small rural s resort, 3.0 Caravan site static, 4.0Caravan site transit, 5.0 Slum redevelopment, 6.0	 Married quarters       
age10_covars$dwell_other_type <- age10$m311

##sex 1Male, 2Female
age10_covars$sex <- age10$sex10
# df_PCA <- subset(df_PCA, select=c("bcsid", "PCA1"))
# age10_covars <- PCAsubset$g

library(forcats)
##BMI derivation
##Weight_grams meb19.1; height_mms meb17;
age10_covars$weight <- age10$meb19.1
age10_covars$height <- age10$meb17
#age10_covars$bmi <- 
age10_covars$bmi <- NA
for (i in 1:length(age10_covars$bcsid)) {
  if (age10_covars$weight[i]>100 & age10_covars$height[i]>500 & !is.na(age10_covars$height[i]) & !is.na(age10_covars$weight[i])) {
    age10_covars$bmi[i] <- ((age10_covars$weight[i]/10)/(age10_covars$height[i]/1000)^2)
  }
}

Min <- mean(age10_covars$bmi, na.rm=TRUE) - 3*sd(age10_covars$bmi, na.rm=TRUE)
Max <- mean(age10_covars$bmi, na.rm=TRUE) + 3*sd(age10_covars$bmi, na.rm=TRUE)  
#plot(age10_covars$BMI)
for (i in 1:length(age10_covars$bmi)){
  age10_covars$bmi[i][age10_covars$bmi[i] < Min | age10_covars$bmi[i] > Max] <- NA}


#table(round(age10_covars$bmi, digits= 0))

##Drop or impute if <=3  
# age10_covars$bmi <- age10_covars %>% 

head(age10_covars$weight, n=100L)
#BP
age10_covars$BP_Systolic <- age10$meb20.1
age10_covars$BP_Diastolic <- age10$meb20.2
#CVD Abnormality
age10_covars$ethnic <- age10$a12.1 
age10_covars$CVDAbnormal <- age10$meb21.11

#Negative sedentary - Tv_Viewing /  -7 - blank; 1 - never/hardly; 2 - sometimes; 3- often; 
age10_covars$SB_TV <- age10$m88

#Positive sedentary -
age10_covars$SB_Instrument <- age10$m96
age10_covars$SB_Reading <- age10$m86

#Social and arts Engagement? 
#goes to club -7 - blank; 1 - never/hardly; 2 - sometimes; 3 - often; 
age10_covars$social_club <- age10$m89
age10_covars$social_cinema <- age10$m91
age10_covars$social_museum <- age10$m93
age10_covars$social_library <- age10$m95
age10_covars$smoking <-  dplyr::recode(age10$k053, '-2'=NA_real_, '-1'=NA_real_, '-3'=NA_real_, '1'=1, '2'=1, '3'=1, '3.5'=2, '4'=2, '5'=2, '6'=2, '7'=2, '8'=2, '9'=NA_real_, '20'=NA_real_, '25'=NA_real_, '40'=NA_real_, '84'=NA_real_)
illness <- subset(age10, select=c(bcsid, b24.1, meb4.26:meb4.28, meb4.46:meb4.48, b24.1))
illness$illness <-  vector("double", length(illness$bcsid))
for (i in 1:length(illness$bcsid)) { 
  if ((illness$b24.1[i]==1 | illness$b24.1[i]==2) & !is.na(illness$b24.1[i])) 
  { illness$illness[i] <- 1 } 
  else if ((is.element(1:2, illness$meb4.26[i]) == TRUE) | (is.element(1:2, illness$meb4.27[i]) == TRUE) | (is.element(1:2, illness$meb4.28[i]) == TRUE) | (is.element(1:2, illness$meb4.46[i]) == TRUE) | (is.element(1:2, illness$meb4.48[i]) == TRUE) 
           | (is.element(1:2, illness$b24.1[i]) == TRUE) | (is.element(1:2, illness$meb4.47[i]) == TRUE)) {
    
    { illness$illness[i] <- 1 }
  }
}




age10_covars$hospitalYN <- age10$b16.1
age10_covars$admissions <- age10$b16.2

age10_covars$Admissions <- NA
for (i in 1:length(age10_covars$bcsid)) {
  if (age10_covars$hospitalYN[i] == 2 & !is.na(age10_covars$hospitalYN[i])) {
    age10_covars$Admissions[i] <- 0 }
  else if (age10_covars$hospitalYN[i] == 1 & !is.na(age10_covars$hospitalYN[i]) & age10_covars$admissions[i]>0 ) {
    age10_covars$Admissions[i] <- age10_covars$admissions[i]}
  else { age10_covars$Admissions[i] <- NA }
}


age10_covars$bcsid <- age10$bcsid
age10_covars$illness <- illness$illness
age10_covars <- merge(age10_covars, PCAsubset, by="bcsid", all=TRUE)
age10_covars <- age10_covars[-c(1:23),]

rutterscore <-  dplyr::recode(age10_derived$BD3MRUTG, '-2'=NA_real_, '-1'=NA_real_, '1'=1, '2'=2, '3'=3)
age10_derived$rutterscore <- rutterscore
rutterscore <- subset(age10_derived, select=c(rutterscore, BCSID)) 
names(rutterscore) <- c("rutterscore", "bcsid")
age10_covars <- merge(age10_covars, rutterscore, by="bcsid", all=FALSE)
age10_covars <- age10_covars %>% distinct(bcsid, .keep_all = TRUE)
age10_covars <- rename(age10_covars, ID='bcsid')
#age10_covars <- merge(age10_covars, dfSEC, by="ID", all=TRUE)


# #illness %>% filter(illness==T) %>% head( n = 40L)
return(list(age10_vars, age10_covars)) }
