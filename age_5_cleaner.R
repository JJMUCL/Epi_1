##Age 5 cleaner#

age_5_cleaner <- function(age5, age5_derived) {

PCA <- subset(age5, select= c('f100', 'f118', 'f119', 'f120', 'f121', 'f122')) 

##-3, 4, 5; in f120; -5 in f121; -5 in 122; 
Profile_test <- PCA$f118 #[!PCA$f118<0]
EPVT <- age5_derived$BD2READ
EPVTstandard <- age5$f120
Copy_Designs <- as.vector(PCA$f122)
HFD <- as.vector(PCA$f121)
Schonell_Read <- subset(age5, select=c('f100', 'f099'))
#Schonell_Read <- age5$f100[!(age5$f100== -2 | age5$f099==-3)] 

# summary(as.numeric(age5$f113))
# table(age5$f118)
# table(age5$f117)
# table(age5$f099)
##Cannot read#f099##
##FINAL SAMPLE: sum(table(age5$f100[!(age5$f100== -2| age5$f099==-3)]))##
Schonell_Read_out <- Schonell_Read$f100
for (i in 1:length(Schonell_Read$f100)) {
  if (Schonell_Read[i,1]==-2 | Schonell_Read[i,2]==-3) {
    Schonell_Read_out[i] <- NA
  }
  else if (Schonell_Read[i,1]==-3 & !is.na(Schonell_Read[i,1])) {
    Schonell_Read_out[i] <- 0 
  }
}
sum(table(HFD))
for (i in 1:length(Profile_test)) {
  if (Profile_test[i]<0 & !is.na(Profile_test[i])) {
    Profile_test[i] <- NA
  }
}
for (i in 1:length(EPVTstandard)) {
  if ((EPVTstandard[i]==-5 | EPVTstandard[i]==-4 | EPVTstandard[i]==-3) & !is.na(EPVTstandard[i])) {
    EPVTstandard[i] <- NA 
  }
}
for (i in 1:length(Copy_Designs)) {
  if (Copy_Designs[i]==-5 & !is.na(Copy_Designs[i])) {
    Copy_Designs[i] <- NA
  }
}
for (i in 1:length(HFD)) {
  if (HFD[i]==-5 & !is.na(HFD[i])) {
    HFD[i] <- NA
  }
}

##put into df..
df_PCA <- data.frame(age5[1], Profile_test, Copy_Designs, Schonell_Read_out, HFD, EPVTstandard)
##rm NA.. 
df_PCA <- na.omit(df_PCA)
colSums(is.na( df_PCA ))
##rm any non numeric vars.
numerical_data <- df_PCA[,2:6]
numerical_data
##normalize
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

summary(data.pca)


#install.packages("factoextra")
library("factoextra")
# 
# fviz_eig(data.pca, addlabels = TRUE)
# fviz_pca_var(data.pca, col.var = "black")
# fviz_cos2(data.pca, choice = "var", axes = 1:2)

df_PCA$PCA1 <- scale(PCA_out$coord[,1])
df_PCA$g <- df_PCA$PCA1*15+100
(df_PCA$g)
PCAsubset <- subset(df_PCA, select=c(bcsid, g))
PCAsubset$g <- as.vector(df_PCA$g)
tail(df_PCA)

return(df_PCA)
}

# 
# 
# 
# 
# 
# 
# 
# # f118[f118<0] <- NA
# # f119[f119<0] <- NA
# # f120[f120==-5 | f120==-4 | f120==-3] <- NA
# # f121[f121==-5] <- NA
# # f122[f122==-5] <- NA
# 
# ##Digit recall: i3541-i3574   1- Correct; 2- Incorrect; 9- No response;
# ##Definitions: i3504-i3540  1- Correct; 2- Incorrect; 9- No response;
# ##Similarities: i3616-i3576  1- correct; 2 - Unacceptable; 9 - No response; 
# ##Matrices: i3617-i3644   1- Correct; 2- Incorrect; 9- No response;
# 
# ##---------------------------------------------Digit_Recall
# 
# Digit_recall <- subset(age10, select=(i3541:i3574))
# Digit_recall <- Digit_recall %>%
#   mutate_at(vars(1:length(Digit_recall)),
#             function(x) case_when(x < 1 ~ NA_real_,
#                                   x == 1 ~ 1,
#                                   x == 2 ~ 2, 
#                                   x > 2 ~ NA_real_))
# NA_all <- rowSums(is.na(Digit_recall))
# DigitSum <- vector("double", length(Digit_recall$i3541))
# for (i in 1:length(NA_all)) { 
#   if (NA_all[i]==34) { DigitSum[i] <- NA_real_ } }
# 
# table(DigitSum, useNA="always")
# 
# for (i in 1:length(Digit_recall$i3541)){
#   for (j in 1:ncol(Digit_recall)) {
#     if (Digit_recall[i,j]==1 & !is.na(Digit_recall[i,j])) {
#       DigitSum[i] <- DigitSum[i]+1 
#     }
#   } }  
# #Digit_recall$ID <- age10$bcsid
# age10$DigitSum <- DigitSum  
# table(DigitSum, useNA = "always")
# 
# ##-------------------------------------Definitions#
# 
# Definitions <- subset(age10, select=(i3504:i3540))
# Definitions <- Definitions %>%
#   mutate_at(vars(1:length(Definitions)),
#             function(x) case_when(x < 1 ~ NA_real_,
#                                   x == 1 ~ 1,
#                                   x == 2 ~ 2, 
#                                   x > 2 ~ NA_real_))
# NA_all <- rowSums(is.na(Definitions))
# table(NA_all)
# Definitions_Sum <- vector("double", length(Definitions$i3504))
# for (i in 1:length(NA_all)) { 
#   if (NA_all[i]==37) { Definitions_Sum[i] <- NA_real_ } }
# 
# for (i in 1:length(Definitions$i3504)){
#   for (j in 1:ncol(Definitions)) {
#     if (Definitions[i,j]==1 & !is.na(Definitions[i,j])) {
#       Definitions_Sum[i] <- Definitions_Sum[i]+1 
#     }
#   } }  
# age10$Definitions_Sum <- Definitions_Sum  
# table(Definitions_Sum, useNA = "always")
# #Definitions$ID <- age10$bcsid
# 
# 
# ##-------------------------------------Similarities##
# 
# Similarities <- subset(age10, select=(i3576:i3616))
# Similarities <- Similarities %>%
#   mutate_at(vars(1:length(Similarities)),
#             function(x) case_when(x < 1 ~ NA_real_,
#                                   x == 1 ~ 1,
#                                   x == 2 ~ 2, 
#                                   x > 2 ~ NA_real_))
# NA_all <- rowSums(is.na(Similarities))
# table(NA_all)
# Similarities_Sum <- vector("double", length(Similarities$i3576))
# for (i in 1:length(NA_all)) { 
#   if (NA_all[i]==41) { Similarities_Sum[i] <- NA_real_ } }
# 
# for (i in 1:length(Similarities$i3576)){
#   for (j in 1:ncol(Similarities)) {
#     if (Similarities[i,j]==1 & !is.na(Similarities[i,j])) {
#       Similarities_Sum[i] <- Similarities_Sum[i]+1 
#     }
#   } }  
# age10$Similarities_Sum <- Similarities_Sum  
# table(Similarities_Sum, useNA = "always")
# #Similarities$ID <- age10$bcsid
# 
# ##-------------------------------------Matrices##
# #Matrices: i3617-i3644
# Matrices <- subset(age10, select=(i3617:i3644))
# Matrices <- Matrices %>%
#   mutate_at(vars(1:length(Matrices)),
#             function(x) case_when(x < 1 ~ NA_real_,
#                                   x == 1 ~ 1,
#                                   x == 2 ~ 2, 
#                                   x > 2 ~ NA_real_))
# NA_all <- rowSums(is.na(Matrices))
# table(NA_all)
# Matrices_Sum <- vector("double", length(Matrices$i3617))
# for (i in 1:length(NA_all)) { 
#   if (NA_all[i]==28) { Matrices_Sum[i] <- NA_real_ } }
# 
# for (i in 1:length(Matrices$i3617)){
#   for (j in 1:ncol(Matrices)) {
#     if (Matrices[i,j]==1 & !is.na(Matrices[i,j])) {
#       Matrices_Sum[i] <- Matrices_Sum[i]+1 
#     }
#   } }  
# age10$Matrices_Sum <- Matrices_Sum  
# table(Matrices_Sum, useNA = "always")
# #Matrices$ID <- age10$bcsid
# 
# ##----------------------PCA
# df_PCA <- data.frame(age10[1], Matrices_Sum, DigitSum, Similarities_Sum, Definitions_Sum)
# df_PCA <- na.omit(df_PCA)
# colSums(is.na( df_PCA ))
# numerical_data <- df_PCA[,2:5]
# numerical_data
# data_normalized <- scale(numerical_data)
# head(data_normalized)
# #sum(data_normalized[1:6916, 1], na.rm=TRUE)
# ##Calculate Corr Matrix
# corr_matrix <- cor(data_normalized)
# cor(data_normalized)
# library(ggcorrplot)
# #install.packages("ggcorrplot")
# ggcorrplot(corr_matrix)
# data.pca <- prcomp(data_normalized, scale = FALSE)
# summary(data.pca)
# data.pca$loadings[, 1:2]
# library("factoextra")
# PCA_out <- get_pca_ind(data.pca)
# 
# 
# #install.packages("factoextra")
# library("factoextra")
# 
# fviz_eig(data.pca, addlabels = TRUE)
# fviz_pca_var(data.pca, col.var = "black")
# fviz_cos2(data.pca, choice = "var", axes = 1:2)
# 
# df_PCA$PCA1 <- scale(PCA_out$coord[,1])
# df_PCA$g <- df_PCA$PCA1*15+100
# PCAsubset <- subset(df_PCA, select=c(bcsid, g))
# PCAsubset$g <- as.vector(df_PCA$g)
# tail(df_PCA)
# 
# 
# range(df_PCA$g)