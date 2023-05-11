##LM Pool Funct##
LMPooled <- function(impdf, outcome, covariates) {
  LM <- with(impdf, lm(outcome ~ covariates))
  return(LM)
} 