##Table_builder
Model_output <- function(model) {  
  Conf <- confint(model)
  m25 <-  Conf[ , 1]
  m95 <-  Conf[ , 2]
  return(cbind(model$coefficients, m25, m95, summary(model)$coefficients[,4])) }
##

Mice_output <- function(rawmodel) {
Conf <- summary(pool(rawmodel), conf.int = TRUE,conf.level = 0.95)[c(1, 2, 7, 8, 6)]
Conf 
return(Conf)
}

#grid <- subset(df, select=c("VE2,"RPE"))

#model_ve2 <- lm(RPE ~ VE2, data=grid)