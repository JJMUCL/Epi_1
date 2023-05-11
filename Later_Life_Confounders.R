
#Early life confounders#

table(df$SportsTrichotomy46)

EarlylifeCovars10 <- subset(df, select=c(ID, Sex, illness, rutterscore, g, bmi.x))
EarlylifeCovars16 <- subset(df, select=c(ID, SEC, Smoking, Yearly_Alcohol))
EarlylifeCovars <- merge(EarlylifeCovars10, EarlylifeCovars16, by=c("ID"), all=TRUE)

LaterlifeCovars42 <- subset(df, select=c(ID, Sex, illness, rutterscore, g, bmi.x))
LaterlifeCovars46 <- subset(df, select=c(ID, SEC, Smoking, Yearly_Alcohol))
LaterlifeCovars <- merge(EarlylifeCovars10, EarlylifeCovars16, by=c("ID"), all=TRUE)

plot(age_42_covars$bmi)
