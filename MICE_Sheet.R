library(mice)
library(naniar)
library(tidyverse)
MI_Df <- read.table("c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\df_MI_09_05.tab")
MI_Df <- subset(MI_Df, select= -c(disability_10))
MI_Df <- MI_Df[,-1]  
##Specify var class before MICE##
MI_Df$Sex.x <- factor(MI_Df$Sex.x)
MI_Df$sport_10 <- factor(
  x = MI_Df$sport_10, ordered = FALSE)
MI_Df$Sports_16 <- factor(
  x = MI_Df$Sports_16, ordered = FALSE)
class(MI_Df$Sports_16)
MI_Df$Education <- factor(
  x = MI_Df$Education,  ordered = FALSE)
MI_Df$Sportstrichotomous42 <- factor(
  x = MI_Df$Sportstrichotomous42, ordered = FALSE)
MI_Df$SportsTrichotomy46 <- factor(
  x = MI_Df$SportsTrichotomy46, ordered = FALSE)
# MI_Df$disability_10 <- ordered(
# x = MI_Df$disability_10, levels = c(0, 1, 2))
MI_Df$SEC_102 <- factor(
  x = MI_Df$SEC_102, ordered = FALSE)
MI_Df$SEC.x <- factor(
  x = MI_Df$SEC.x, ordered = FALSE)
MI_Df$Diabetes.x <- factor(MI_Df$Diabetes.x)
MI_Df$AlcRisk.x <- ordered(
  x = MI_Df$AlcRisk.x, levels = c(0, 1, 2))
MI_Df$Smoker.x <- ordered(
  x = MI_Df$Smoker.x, levels = c(0, 1, 2, 3))
MI_Df$Open_Skill_Tri16 <- factor(
  x = MI_Df$Open_Skill_Tri16, ordered = FALSE)
MI_Df$Open_Skill_Tri42 <- factor(
  x = MI_Df$Open_Skill_Tri42, ordered = FALSE)
MI_Df$Open_Skill_Tri46 <- factor(
  x = MI_Df$Open_Skill_Tri46, ordered = FALSE)
MI_Df$Closed_Skill_Tri16 <- factor(
  x = MI_Df$Closed_Skill_Tri16, ordered = FALSE)
MI_Df$Closed_Skill_Tri42 <- factor(
  x = MI_Df$Closed_Skill_Tri42, ordered = FALSE)
MI_Df$Closed_Skill_Tri46 <- factor(
  x = MI_Df$Closed_Skill_Tri46, ordered = FALSE)
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
stripplot(MI_Df)
vis_miss(MI_Df)


##MICE
Imputed_DF <- mice(data=MI_Df, m=60, maxit=5, seed=12345)

ID <- MI_Df$ID
temp <- subset(MI_Df, select=c(as.factor(SportsTrichotomy46), as.factor(Sportstrichotomous42), as.factor(Sports_16), as.factor(sport_10), as.factor(Closed_Skill_Tri46), as.factor(Closed_Skill_Tri42), as.factor(Closed_Skill_Tri16), as.factor(Count_Sport_High)))
names(temp) <- c("SportsTrichotomy46M", "Sportstrichotomous42M", "Sports_16M", "sport_10M", "Closed_Skill_Tri46M", "Closed_Skill_Tri42M", "Closed_Skill_Tri16M", "Count_Sport_HighM")
d2 <- complete(Imputed_DF,'long', include = T) # imputed datasets in long format (including the original)
d3 <- cbind(ID, d2)
d3 <- cbind(temp, d3) # as datasets are ordered simply cbind `id`
m2 <- as.mids(d3) # and transform back to mids object
#exploring imputed df requires following rubins rules#
#bmi_mean <- with(m2, lm(BMI.x~1))
#summary(pool(bmi_mean))

##Modelling Additive Model# Executive Function#
source("C:/Users/JohnM/Documents/Epi_1/Table_Builder.R")
reg_proxdist_high_Sex <- with(m2, lm(executive_z2 ~ as.factor(Count_Sport_HighM) + Sex.x))
reg_proxdist_high_childhood <- with(m2, lm(executive_z2 ~ as.factor(Count_Sport_HighM) + Sex.x + g5 + SEC_102 + Admissions + Education))
reg_proxdist_high_adulthealth <- with(m2, lm(executive_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education + nonHDLratio + BMI.x + B10HBA1C + BP))
reg_proxdist_high_adulthealthbehaviour <- with(m2, lm(executive_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education + Smoker.x + AlcRisk.x))
reg_proxdist_high_RHR <- with(m2, lm(executive_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education + RHR))
reg_proxdist_high_full <- with(m2, lm(executive_z2 ~ as.factor(Count_Sport_High) + Sex.x + Smoker.x + AlcRisk.x + nonHDLratio + B10HBA1C + g5 + SEC_102 + Admissions + BP + BMI.x + RHR + Education))
table_sex_adj <-  Mice_output(reg_proxdist_high_Sex)
table_childhood_adj <-  Mice_output(reg_proxdist_high_childhood)
table_adulthood_adj <-  Mice_output(reg_proxdist_high_adulthealth)
table_healthbehav_adj <-  Mice_output(reg_proxdist_high_adulthealthbehaviour)
table_RHR_adj <-  Mice_output(reg_proxdist_high_RHR)
table_full_adj <-  Mice_output(reg_proxdist_high_full)
table_sex
write.csv(table_childhood_adj, "C:/Users/JohnM/Downloads/proximity_tableschildhood_11_05.csv")
write.csv(table_adulthood_adj, "C:/Users/JohnM/Downloads/proximity_tablesadulthood_11_05.csv")
write.csv(table_healthbehav_adj, "C:/Users/JohnM/Downloads/proximity_tableshealthbehav_11_05.csv")
write.csv(table_RHR_adj, "C:/Users/JohnM/Downloads/proximity_tablesRHR_11_05.csv")
write.csv(table_full_adj, "C:/Users/JohnM/Downloads/proximity_tablesFull_11_05.csv")
write.csv(table_sex_adj, "C:/Users/JohnM/Downloads/proximity_tablesSex_11_05.csv")

##Modelling Additive Model# Memory #
reg_proxdist_high_Sex <- with(m2, lm(memory_z2 ~ factor(Count_Sport_HighM, ordered = FALSE) + Sex.x))
reg_proxdist_high_childhood <- with(m2, lm(memory_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education))
reg_proxdist_high_adulthealth <- with(m2, lm(memory_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education + nonHDLratio + BMI.x + B10HBA1C + BP))
reg_proxdist_high_adulthealthbehaviour <- with(m2, lm(memory_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education + Smoker.x + AlcRisk.x))
reg_proxdist_high_RHR <- with(m2, lm(memory_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education + RHR))
reg_proxdist_high_full <- with(m2, lm(memory_z2 ~ as.factor(Count_Sport_High) + Sex.x + Smoker.x + AlcRisk.x + nonHDLratio + B10HBA1C + g5 + SEC_102 + Admissions + BP + BMI.x + RHR + Education))
table_sex_adj <-  Mice_output(reg_proxdist_high_Sex)
table_childhood_adj <-  Mice_output(reg_proxdist_high_childhood)
table_adulthood_adj <-  Mice_output(reg_proxdist_high_adulthealth)
table_healthbehav_adj <-  Mice_output(reg_proxdist_high_adulthealthbehaviour)
table_RHR_adj <-  Mice_output(reg_proxdist_high_RHR)
table_full_adj <-  Mice_output(reg_proxdist_high_full)
table_full_adj
write.csv(table_childhood_adj, "C:/Users/JohnM/Downloads/proximity_tableschildhood_memory_11_05.csv")
write.csv(table_adulthood_adj, "C:/Users/JohnM/Downloads/proximity_tablesadulthood_memory_11_05.csv")
write.csv(table_healthbehav_adj, "C:/Users/JohnM/Downloads/proximity_tableshealthbehav_memory_11_05.csv")
write.csv(table_RHR_adj, "C:/Users/JohnM/Downloads/proximity_tablesRHR_memory_11_05.csv")
write.csv(table_full_adj, "C:/Users/JohnM/Downloads/proximity_tablesFull_memory_11_05.csv")
write.csv(table_sex_adj, "C:/Users/JohnM/Downloads/proximity_tablesSex_11_memory_05.csv")

##Modelling Additive Model# Composite #
reg_proxdist_high_Sex <- with(m2, lm(composite_z2 ~ as.factor(Count_Sport_High) + Sex.x))
reg_proxdist_high_childhood <- with(m2, lm(composite_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education))
reg_proxdist_high_adulthealth <- with(m2, lm(composite_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education + nonHDLratio + BMI.x + B10HBA1C + BP))
reg_proxdist_high_adulthealthbehaviour <- with(m2, lm(composite_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education + Smoker.x + AlcRisk.x))
reg_proxdist_high_RHR <- with(m2, lm(composite_z2 ~ as.factor(Count_Sport_High) + Sex.x + g5 + SEC_102 + Admissions + Education + RHR))
reg_proxdist_high_full <- with(m2, lm(composite_z2 ~ as.factor(Count_Sport_High) + Sex.x + Smoker.x + AlcRisk.x + nonHDLratio + B10HBA1C + g5 + SEC_102 + Admissions + BP + BMI.x + RHR + Education))
table_sex_adj <-  Mice_output(reg_proxdist_high_Sex)
table_childhood_adj <-  Mice_output(reg_proxdist_high_childhood)
table_adulthood_adj <-  Mice_output(reg_proxdist_high_adulthealth)
table_healthbehav_adj <-  Mice_output(reg_proxdist_high_adulthealthbehaviour)
table_RHR_adj <-  Mice_output(reg_proxdist_high_RHR)
table_full_adj <-  Mice_output(reg_proxdist_high_full)
table_full_adj
write.csv(table_childhood_adj, "C:/Users/JohnM/Downloads/proximity_tableschildhood_composite_z2_11_05.csv")
write.csv(table_adulthood_adj, "C:/Users/JohnM/Downloads/proximity_tablesadulthood_composite_z2_11_05.csv")
write.csv(table_healthbehav_adj, "C:/Users/JohnM/Downloads/proximity_tableshealthbehav_composite_z2_11_05.csv")
write.csv(table_RHR_adj, "C:/Users/JohnM/Downloads/proximity_tablesRHR_composite_z2_11_05.csv")
write.csv(table_full_adj, "C:/Users/JohnM/Downloads/proximity_tablesFull_composite_z2_11_05.csv")
write.csv(table_sex_adj, "C:/Users/JohnM/Downloads/proximity_tablesSex_11_composite_z2_05.csv")

##----End of proximal-distal----##


##Sensitive Period
reg_10_Sex <- with(m2, lm(executive_z2 ~ factor(sport_10M) + Sex.x))
reg_10_childhood <- with(m2, lm(executive_z2 ~ sport_10M + Sex.x + g5 + SEC_102 + Admissions + Education))

table_reg_10_Sex <-  Mice_output(reg_10_Sex)
table_reg_10_Sex 


