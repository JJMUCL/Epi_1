##Covariate Coding##

##Sex
df$Sex

##Early-life Health 
#age_10 rutter

table(df$rutterscore, useNA="always")
#age_10 disability
table(df$alcohol)
table(age_16_covars$alcohol)
df$disability.x <- dplyr::recode(df$disability.x, '1'=1, '2'=1, '3'=0)
#age_10 smoking - fix
df$smoking <- dplyr::recode(df$smoking, '1'=1, '2'=1, '3'=0)
#age_16 alcohol
df$alcohol <- dplyr::recode(df$alcohol, c(('-1' '0','1','2','3','4','5','6','7','8','9','10','11', '12', '13','14')=1, ('16'  ,'17',   '18',   '19',  '20',   '21'  , '22'  , '23'  , '24'  , '25'  , '26'  , '27'  , '29')=2))
#-1 & 0; <14; >14 ; 
df$alcohol
#age_10 BMI
#age_10 illness



##Early-life SEC
#age_10 g10
#age_10 region
#age_16 SEC


##----------------------------------Midlife------------##
##Mid-life Health
#age_42 malaise
#age_42 BMI
#age_42 alcohol
#age_42 smoking
#age_42 diabetes
#age_42 CVD
#age_42 malaise

##Mid-life SEC
#age_42 SEC
#age_42 Region
#age_42 Education


