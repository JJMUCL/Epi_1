##Polytomous LCA

ID <- subset(df, select=c("ID")) 
age16LCA <- merge(age_16_vars, ID, by="ID", all.x=F) 
age16LCA <- age16LCA[,-c(44:57)]
for (i in 1:length(age16LCA$ID)) {
  for (j in 2:43) {
    if (age16LCA[i,j]==1 & !is.na(age16LCA[i,j])) {
      age16LCA[i,j] <- 3
    }
    else if (age16LCA[i,j]==2 & !is.na(age16LCA[i,j])) {
      age16LCA[i,j] <- 2
    }
    else if (age16LCA[i,j]==3 & !is.na(age16LCA[i,j])) {
      age16LCA[i,j] <- 1
    }
    else if (age16LCA[i,j]==-1 & !is.na(age16LCA[i,j])) {
      age16LCA[i,j] <- NA_real_
    }
    else if (is.na(age16LCA[i,j])) {
      age16LCA[i,j] <- NA_real_}
  }
}
age16LCA1 <- age16LCA[,-1]
f1 <- colnames(age16LCA)

f <- cbind(baseball,basketball,cricket,football,hockey,netball,
           rounders,rugby,volleyball,other_team,aerobics,trackfield
           ,badminton,canoeing,crosscountry,cycling,dancing,gymnastics
           ,horseriding,jogging,fitness,iceskating,rowing,sailing,
           scrambling,skiing,squash,swimming,tabletennis,tennis,
           walking,waterskiing,weights,windsurf,other_individual
           ,billiards,darts,fishing,pool,shooting,snooker,other) ~ 1



data <- age16LCA1
#data <- data.frame(subset(data, select=-c(ID, na_count)))
#dev.off()
#install.packages("poLCA")
library(poLCA)
LCA_output_1class <- poLCA(f, data, nclass = 1, maxiter = 50000, graphs = TRUE, na.rm = FALSE, 
                           nrep =  10, verbose = TRUE)

LCA_output_2class <- poLCA(f, data, nclass = 2, maxiter = 50000, graphs = TRUE, na.rm = FALSE, 
                           nrep =  10, verbose = TRUE)

LCA_output_3class <- poLCA(f, data, nclass = 3, maxiter = 50000, graphs = TRUE, na.rm = FALSE, 
                           nrep =  10, verbose = TRUE)

LCA_output_4class <- poLCA(f, data, nclass = 4, maxiter = 50000, graphs = TRUE, na.rm = FALSE, 
                           nrep =  10, verbose = TRUE)

LCA_output_5class <- poLCA(f, data, nclass = 5, maxiter = 50000, graphs = FALSE, na.rm = FALSE, 
                           nrep =  10, verbose = FALSE)

LCA_output_6class <- poLCA(f, data, nclass = 6, maxiter = 50000, graphs = FALSE, na.rm = FALSE, 
                           nrep =  10, verbose = FALSE)

LCA_output_7class <- poLCA(f, data, nclass = 7, maxiter = 50000, graphs = FALSE, na.rm = FALSE, 
                           nrep =  10, verbose = FALSE)

LCA_output_8class <- poLCA(f, data, nclass = 8, maxiter = 50000, graphs = FALSE, na.rm = FALSE, 
                           nrep =  10, verbose = FALSE)

tableBIC <- cbind(c(LCA_output_1class$bic, LCA_output_2class$bic, LCA_output_3class$bic, LCA_output_4class$bic, LCA_output_5class$bic, LCA_output_6class$bic, LCA_output_7class$bic, LCA_output_8class$bic))
LCA_output_5class
write.csv(tableBIC, "c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\BIC_16.csv")

plot(tableBIC)


table_probs

table_probs_c1 <- as.matrix(LCA_output_5class$probs[[1]][1,]) 
table_probs_c2 <- as.matrix(LCA_output_5class$probs[[1]][2,]) 
table_probs_c3 <- as.matrix(LCA_output_5class$probs[[1]][3,]) 
table_probs_c4 <- as.matrix(LCA_output_5class$probs[[1]][4,]) 
table_probs_c5 <- as.matrix(LCA_output_5class$probs[[1]][5,]) 
#rm(table_probs_c1, table_probs_c2, table_probs_c3, table_probs_c4, table_probs_c5)
for (i in 2:length(LCA_output_5class$probs)) {
table_probs_c1 <- cbind(table_probs_c1, LCA_output_5class$probs[[i]][1,])
}
for (i in 2:length(LCA_output_5class$probs)) {
  table_probs_c2 <- cbind(table_probs_c2, LCA_output_5class$probs[[i]][2,])
}
for (i in 2:length(LCA_output_5class$probs)) {
  table_probs_c3 <- cbind(table_probs_c3, LCA_output_5class$probs[[i]][3,])
}
for (i in 2:length(LCA_output_5class$probs)) {
  table_probs_c4 <- cbind(table_probs_c4, LCA_output_5class$probs[[i]][4,])
}
for (i in 2:length(LCA_output_5class$probs)) {
  table_probs_c5 <- cbind(table_probs_c5, LCA_output_5class$probs[[i]][5,])
}


df <- read_csv(t("c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\LCA_output_16.csv"))
df <- t(df)
df <- as.data.frame(df[-1,])
colnames(df) <- c("", "", "", "Type")

classcleaner <- function(table_probs_c5) {
c5<- as.data.frame(t(unlist(table_probs_c5)))
c5
c5$Type <- df$Type
c5$X <- df$X
c5$Pr.tot <- ifelse(c5$'Pr(1)'>0.50, 0, 1)
return(c5)}

c1 <- classcleaner(table_probs_c1)
c2 <- classcleaner(table_probs_c2)
c3 <- classcleaner(table_probs_c3)
c4 <- classcleaner(table_probs_c4)
c5 <- classcleaner(table_probs_c5)

Class1Summary <- c1 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))
Class2Summary <- c2 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))
Class3Summary <- c3 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))
Class4Summary <- c4 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))
Class5Summary <- c5 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))

Class1Summary$typecount <- ifelse(Class1Summary$typecount>=1, 1,0)
Class2Summary$typecount <- ifelse(Class2Summary$typecount>=1, 1,0)
Class3Summary$typecount <- ifelse(Class3Summary$typecount>=1, 1,0)
Class4Summary$typecount <- ifelse(Class4Summary$typecount>=1, 1,0)
Class5Summary$typecount <- ifelse(Class5Summary$typecount>=1, 1,0)
#Class6Summary$typecount <- ifelse(Class6Summary$typecount>=1, 1,0)





radarpl <- merge(Class1Summary, Class2Summary, by="Type")
radarpl <- merge(radarpl, Class3Summary, by="Type")
names(radarpl) <- c("Type", "Class1", "Class2", "Class3")
radarpl
radarpl <- merge(radarpl, Class4Summary, by="Type")
radarpl <- merge(radarpl, Class5Summary, by="Type")

write_csv(radarpl, "c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\LCA_Summary_16.csv")
write_csv(as.data.frame(c(c1,c2,c3,c4,c5)), "c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\LCA_raw_16.csv")


names(radarpl) <- c("Type", "Class1", "Class2", "Class3", "Class4", "Class5")
radarpl
dat_t = as.data.frame(x = t(radarpl[,-1]), stringsAsFactors = FALSE, header=TRUE)
colnames(dat_t) <- c("game", "other", "partner", "solo", "team", "walking", "water")
dat_t
max_min <- data.frame(game = c(1, 0), other = c(1, 0),
  partner = c(1, 0), solo = c(1, 0), team=c(1,0), walking = c(1, 0),
  water = c(1, 0)
##True Maxes
# max_min <- data.frame(game = c(4, 0), other = c(1, 0),
#                       partner = c(5, 0), solo = c(18, 0), team=c(10,0), walking = c(1, 0),
#                       water = c(3, 0)
)
#c5 %>% filter(Type=='walking') %>% count()

rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, dat_t)
dat_t

Class1_data <- df[c("Max", "Min", "Class1"), ]
#radarchart(Class1_data)




create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


#increase colors + titles to 3 
colors <- c("#00AFBB", "#E7B800", "#FC4E07","#E7B800", "#FC4E07")
titles <- c("Class1", "Class2", "Class3", "Class4", "Class5")

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,5))

# Create the radar chart
for(i in c(3,2,5,4,1)) {
  create_beautiful_radarchart(
    data = df[c(1, 2, i+2), ], caxislabels = c("0", "", "", "", ">1 Monthly"),
    color = colors[i], title = titles[i]
  )
}

