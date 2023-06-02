DFLCMM_28_05 <- read.csv("~/MRC DTP/Analysis_2 Local/DFLCMM_28_05", sep="")
##Walk data
f <- cbind(Sport_16, Sport_42, Sport_46) ~ 1
data <- data.frame(subset(DFLCMM_28_05, select=c(ID, Sport_16, Sport_42, Sport_46)))
#data$sport_10.x <- dplyr::recode(data$sport_10.x, '2'=2, '1'=1, '0'=1)
data$Sport_16 <- dplyr::recode(data$Sport_16, '4'=5, '3'=4,  '2'=3, '1'=2, '0'=1)
data$Sport_42 <- dplyr::recode(data$Sport_42, '4'=5, '3'=4,  '2'=3, '1'=2, '0'=1)
data$Sport_46 <- dplyr::recode(data$Sport_46, '4'=5, '3'=4, '2'=3, '1'=2, '0'=1)

# #data$sport_10.x <- factor(data$sport_10.x, ordered=T, levels = c("1", "2"))
# data$Sport_16 <- factor(data$Sport_16, levels = c("1", "2", "3", "4", "5"))
# data$Sport_42 <- factor(data$Sport_42, ordered=T, levels = c("1", "2", "3", "4", "5"))
# data$Sport_46 <- factor(data$Sport_46, ordered=T, levels = c("1", "2", "3", "4", "5"))
# 

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
#m5 <- lcmm(sport~year, mixture= ~year, subject='NumID', ng=5 , data=data , link="thresholds", maxiter=100, B=random(m1))
m5 <- gridsearch(rep = 20, maxiter = 60, minit = m1, lcmm(sport~year, mixture= ~year, subject='NumID', ng=5 , data=data , link="thresholds", B=random(m1)))
m6 <- gridsearch(rep = 20, maxiter = 70, minit = m1, lcmm(sport~year, mixture= ~year, subject='NumID', ng=6 , data=data , link="thresholds", B=random(m1)))
m7 <- gridsearch(rep = 20, maxiter = 70, minit = m1, lcmm(sport~year, mixture= ~year, subject='NumID', ng=7 , data=data , link="thresholds", B=random(m1)))



tableBIC <- cbind(m1$BIC, m2$BIC, m3$BIC, m4$BIC, m5$BIC, m6$BIC) 
write.csv(tableBIC, "~/MRC DTP/Analysis_2 Local/gbtm_BIC_Quadr.csv") 
m5$BIC
plot(tableBIC)
plot(x=1:6,  y=c(m1Q$BIC, m2Q$BIC, m3Q$BIC, m4Q$BIC, m5Q$BIC, m6Q$BIC))
install.packages("future")
library("future")
plan(multisession)

m5$pprob %>% filter(class==1) %>% count
m5$pprob %>% filter(class==2) %>% count
m5$pprob %>% filter(class==3) %>% count
m5$pprob %>% filter(class==4) %>% count
m5$pprob %>% filter(class==5) %>% count

resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}

future(m6 <- gridsearch(rep = 20, maxiter = 60, minit = m1, lcmm(sport~year, mixture= ~year, subject='NumID', ng=6 , data=data , link="thresholds", B=random(m1)))
)
code2()


# m1_gmm <-lcmm(sport~year, random=~year, subject='NumID', ng=1 , data=data , link="thresholds", maxiter=100)
# m2_gmm <- lcmm(sport~year, mixture= ~year, random=~year, subject='NumID', ng=2 , data=data , link="thresholds", maxiter=100, B=random(m1))
# m3_gmm <- lcmm(sport~year, mixture= ~year, random=~year, subject='NumID', ng=3 , data=data , link="thresholds", maxiter=100, B=random(m1))
# m4_gmm <- lcmm(sport~year, mixture= ~year, random=~year, subject='NumID', ng=4 , data=data , link="thresholds", maxiter=100, B=random(m1))
# #m5 <- lcmm(sport~year, mixture= ~year, subject='NumID', ng=5 , data=data , link="thresholds", maxiter=100, B=random(m1))
# m5_gmm <- gridsearch(rep = 20, maxiter = 60, minit = m1, lcmm(sport~year, mixture= ~year, random=~year, subject='NumID', ng=5 , data=data , link="thresholds", B=random(m1)))


##Quadratic
m1Q <- lcmm(sport~year + I(year)^2, subject='NumID', ng=1 , data=data , link="thresholds", maxiter=100)
m2Q <- lcmm(sport~year + I(year)^2, mixture= ~year, subject='NumID', ng=2 , data=data , link="thresholds", maxiter=100, B=random(m1Q))
m3Q <- lcmm(sport~year + I(year)^2, mixture= ~year, subject='NumID', ng=3 , data=data , link="thresholds", maxiter=100, B=random(m1Q))
m4Q <- lcmm(sport~year + I(year)^2, mixture= ~year, subject='NumID', ng=4 , data=data , link="thresholds", maxiter=100, B=random(m1Q))
m5Q <- lcmm(sport~year + I(year)^2, mixture= ~year, subject='NumID', ng=5 , data=data , link="thresholds", maxiter=100, B=random(m1Q))
m6Q <- lcmm(sport~year + I(year)^2, mixture= ~year, subject='NumID', ng=6 , data=data , link="thresholds", maxiter=100, B=random(m1Q))
m6Q <- gridsearch(rep = 20, maxiter = 60, minit = m1Q, lcmm(sport~year + I(year)^2, mixture= ~year, subject='NumID', ng=6 , data=data , link="thresholds", B=random(m1Q)))


tableBIC <- cbind(m1Q$BIC, m2Q$BIC, m3Q$BIC, m4Q$BIC, m5Q$BIC, m6Q$BIC) 
tableBIC


library(tidyLPA)
calc_lrt(5748, -19455.13, 14, 4, -19407.68, 17, 5)

m5output <- summarytable(
  m5,
  which = c("G", "loglik", "npm", "BIC", "%class"),
  display = TRUE
)
m4output <- summarytable(
  m4,
  which = c("G", "loglik", "npm", "BIC", "%class"),
  display = TRUE
)

write.csv(cbind(m5output, m4output), "~/MRC DTP/Analysis_2 Local/gmm_m4vsm5.csv")


##Quadratic
m1_randomint <- lcmm(sport~year, subject='NumID', ng=1 , random=~1, data=data , link="thresholds", maxiter=100, nwg=F)
m2_randomint <- lcmm(sport~year, mixture= ~year, subject='NumID', random=~1, ng=2 , data=data , link="thresholds", maxiter=100, nwg=T, B=random(m1_randomint))
m3_randomint <- lcmm(sport~year, mixture= ~year, subject='NumID', random=~1, ng=3 , data=data , link="thresholds", maxiter=100, nwg=T, B=random(m1_randomint))
m4_randomint <- gridsearch(rep = 10, maxiter = 60, minit = m1_randomint, lcmm(sport~year, mixture= ~year, subject='NumID', random=~1, ng=4 , data=data , link="thresholds", nwg=T, B=random(m1_randomint)))
m5_randomint <- lcmm(sport~year, mixture= ~year, subject='NumID', random=~1, ng=5 , data=data , link="thresholds", maxiter=100, nwg=T, B=random(m1_randomint))
m6_randomint <- lcmm(sport~year, mixture= ~year, subject='NumID', random=~1, ng=6 , data=data , link="thresholds", maxiter=100, nwg=T, B=random(m1_randomint))
m6_randomint <- gridsearch(rep = 20, maxiter = 60, minit = m1_randomint, lcmm(sport~year,  mixture= ~year, subject='NumID', random=~1, ng=6 , data=data , link="thresholds", nwg=T, B=random(m1)))



calc_lrt(5748, -19491.64, 10, 2, -19440.34, 14, 3)

tableBIC <- cbind(m1_randomint$BIC, m2_randomint$BIC, m3_randomint$BIC, m4_randomint$BIC) 
tableBIC

m1RIoutput <- summarytable(
  m1_randomint,
  which = c("G", "loglik", "npm", "BIC", "%class"),
  display = TRUE
)
m2RIoutput <- summarytable(
  m2_randomint,
  which = c("G", "loglik", "npm", "BIC", "%class"),
  display = TRUE
)
m3RIoutput <- summarytable(
  m3_randomint,
  which = c("G", "loglik", "npm", "BIC", "%class"),
  display = TRUE
)
m4RIoutput <- summarytable(
  m4_randomint,
  which = c("G", "loglik", "npm", "BIC", "%class"),
  display = TRUE
)
# #m5RIoutput <- summarytable(
#   m5Q,
#   which = c("G", "loglik", "npm", "BIC", "%class"),
#   display = TRUE
# )
write.csv(cbind(m1RIoutput, m2RIoutput, m3RIoutput, m4RIoutput), "~/MRC DTP/Analysis_2 Local/gmm_output_m1_m4_random_int.csv")


summarytable(
  m1,
  ...,
  which = c("G", "loglik", "npm", "BIC", "%class", "entropy"),
  display = TRUE
)

postprob(m3)


plot(x=1:4, y=tableBIC)

tableBIC
