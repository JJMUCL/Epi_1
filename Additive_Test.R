
test <- MI_Df[!c(is.na(MI_Df$sport_10) | is.na(MI_Df$Sports_16) | is.na(MI_Df$Sportstrichotomous42) | is.na(MI_Df$SportsTrichotomy46)), ]

subsetloop <- subset(test, select=c(sport_10, Sports_16, Sportstrichotomous42, SportsTrichotomy46))
subsetted <- subsetloop %>% 
  rowwise() %>% 
  mutate(count2 = sum(na.omit(c_across(1:4)) == 2), 
         count2 = ifelse(all(is.na(c_across(1:4))), NA, count2))
test$count2 <- as.numeric(subsetted$count2)

summary(lm(composite_z2 ~ count2 + g5 + Admissions + SEC_102 + Education , data=test))


## Post 16

test <- MI_Df[!c(is.na(MI_Df$Sports_16) | is.na(MI_Df$Sportstrichotomous42) | is.na(MI_Df$SportsTrichotomy46)), ]

subsetloop <- subset(test, select=c(Sports_16, Sportstrichotomous42, SportsTrichotomy46))
subsetted <- subsetloop %>% 
  rowwise() %>% 
  mutate(count2 = sum(na.omit(c_across(1:3)) == 2), 
         count2 = ifelse(all(is.na(c_across(1:3))), NA, count2))
test$count2 <- as.numeric(subsetted$count2)

summary(lm(composite_z2 ~ count2 + g5 + Admissions + SEC_102 + Education , data=test))
