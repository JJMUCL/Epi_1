library(mice)
library(naniar)
library(tidyverse)
MI_Df <- read.table("c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\df_MI_09_05.tab")
MI_Df <- subset(MI_Df, select= -c(disability_10))
MI_Df <- df_MI[,-1]  

##Specify var class before MICE##
MI_Df$Sex.x <- factor(MI_Df$Sex.x)
MI_Df$sport_10 <- ordered(
  x = MI_Df$sport_10, levels = c(0, 1, 2))
MI_Df$Sports_16 <- ordered(
  x = MI_Df$Sports_16, levels = c(0, 1, 2))
MI_Df$Education <- ordered(
  x = MI_Df$Education, levels = c(1, 2,3))
MI_Df$Sportstrichotomous42 <- ordered(
  x = MI_Df$Sportstrichotomous42, levels = c(0, 1, 2))
MI_Df$SportsTrichotomy46 <- ordered(
  x = MI_Df$SportsTrichotomy46, levels = c(0, 1, 2))
# MI_Df$disability_10 <- ordered(
# x = MI_Df$disability_10, levels = c(0, 1, 2))
MI_Df$SEC_102 <- ordered(
  x = MI_Df$SEC_102, levels = c(1, 2, 3, 4, 5, 6))
MI_Df$SEC_102 <- ordered(
  x = MI_Df$SEC_102, levels = c(1, 2, 3, 4, 5, 6))
MI_Df$SEC.x <- ordered(
  x = MI_Df$SEC.x, levels = c(1, 2, 3, 4))
MI_Df$Diabetes.x <- factor(MI_Df$Diabetes.x)
MI_Df$AlcRisk.x <- ordered(
  x = MI_Df$AlcRisk.x, levels = c(0, 1, 2))
MI_Df$Smoker.x <- ordered(
  x = MI_Df$Smoker.x, levels = c(0, 1, 2, 3))
MI_Df$Open_Skill_Tri16 <- ordered(
  x = MI_Df$Open_Skill_Tri16, levels = c(0, 1, 2))
MI_Df$Open_Skill_Tri42 <- ordered(
  x = MI_Df$Open_Skill_Tri42, levels = c(0, 1, 2))
MI_Df$Open_Skill_Tri46 <- ordered(
  x = MI_Df$Open_Skill_Tri46, levels = c(0, 1, 2))
MI_Df$Closed_Skill_Tri16 <- ordered(
  x = MI_Df$Closed_Skill_Tri16, levels = c(0, 1, 2))
MI_Df$Closed_Skill_Tri42 <- ordered(
  x = MI_Df$Closed_Skill_Tri42, levels = c(0, 1, 2))
MI_Df$Closed_Skill_Tri46 <- ordered(
  x = MI_Df$Closed_Skill_Tri46, levels = c(0, 1, 2))
MI_Df$CVDHTN <- factor(MI_Df$CVDHTN)
MI_Df$RHR <- as.numeric(MI_Df$RHR)
MI_Df$BP <- as.numeric(MI_Df$BP)
MI_Df$g5 <- as.numeric(MI_Df$g5)
MI_Df$CholHDL <- as.numeric(MI_Df$nonHDLratio)
MI_Df$Admissions <- as.numeric(MI_Df$Admissions)
MI_Df$executive_z2 <- as.numeric(MI_Df$executive_z2)
MI_Df$memory_z2 <- as.numeric(MI_Df$memory_z2)
MI_Df$composite_z2 <- as.numeric(MI_Df$composite_z2)
map(MI_Df, class)

##Examine Missingness
stripplot(Imputed_DF)
vis_miss(Imputed_DF)

##MICE
Imputed_DF <- mice(data=MI_Df, m=60, maxit=5, seed=12345)

