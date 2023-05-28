
##Walk data
f <- cbind(Sport_16, Sport_42, Sport_46) ~ 1
data <- data.frame(subset(DFLCMM_28_05, select=c(ID, Sport_16, Sport_42, Sport_46)))
#data$sport_10.x <- dplyr::recode(data$sport_10.x, '2'=2, '1'=1, '0'=1)
data$Sport_16 <- dplyr::recode(data$Sport_16, '4'=5, '3'=4,  '2'=3, '1'=2, '0'=1)
data$Sport_42 <- dplyr::recode(data$Sport_42, '4'=5, '3'=4,  '2'=3, '1'=2, '0'=1)
data$Sport_46 <- dplyr::recode(data$Sport_46, '4'=5, '3'=4, '2'=3, '1'=2, '0'=1)

#data$sport_10.x <- factor(data$sport_10.x, ordered=T, levels = c("1", "2"))
data$Sport_16 <- factor(data$Sport_16, ordered=T, levels = c("1", "2", "3", "4", "5"))
data$Sport_42 <- factor(data$Sport_42, ordered=T, levels = c("1", "2", "3", "4", "5"))
data$Sport_46 <- factor(data$Sport_46, ordered=T, levels = c("1", "2", "3", "4", "5"))


#data$na_count <- apply(is.na(data), 1, sum)
#data <- data[!(data$na_count>=3),]
#data <- data %>% pivot_longer(cols=c('walk_10', 'Walking16', 'WalkTrichotomy', 'WalkTrichotomy46'),
#                              names_to='year',
#                              values_to='Walk')
#ID_model_walk <- data$ID
#table(data$na_count)
#data <- data.frame(subset(data, select=-c(na_count, ID)))

#install.packages("lcmm")
library(lcmm)
library(tidyr)

#sport_10, Sports_16, Sportstrichotomous42, SportsTrichotomy46
data <- data %>% pivot_longer(cols=c('Sport_16', 'Sport_42', 'Sport_46'),
                              names_to='year',
                              values_to='sport')
head(data)
data$year <- dplyr::recode(data$year, Sport_16=0.16, Sport_42=0.42, Sport_46=0.46)
#data$walk <- as.numeric(data$Walk)
head(data)
data$NumID <-  with(data, match(ID, unique(ID)))
#data <- subset(data, select=-c(Walk),)
head(data)
#data %<>% 
# mutate_if(is.factor, funs(as.numeric(as.character(.)))) %>% 
#mutate_all(~if_else(is.infinite(.x) | is.nan(.x), NA, .x)) 


## thresholds link function - specifies as ordinal;  

##Understanding the fixed effect+random effect: https://www.youtube.com/watch?v=scrG1v5fNQ4&t=2485s
#Cubic time parameter; ~ poly(time, degree = 3), #mixture=~year
m1 <-lcmm(sport~year, subject='NumID', ng=1 , data=data , link="thresholds", maxiter=100)
m2 <- lcmm(sport~year, mixture= ~year, subject='NumID', ng=2 , data=data , link="thresholds", maxiter=100, B=random(m1))
m3 <- lcmm(sport~year, mixture= ~year, subject='NumID', ng=3 , data=data , link="thresholds", maxiter=100, B=random(m1))
m4 <- lcmm(sport~year, mixture= ~year, subject='NumID', ng=4 , data=data , link="thresholds", maxiter=100, B=random(m1))
