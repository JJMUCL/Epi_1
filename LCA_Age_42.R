
age42_LCA <- age_42_vars[,1:16]
age42_LCA
age42merge <- df[!is.na(df$Sportstrichotomous42),]
ID <- subset(age42merge, select=c("ID", "Sportstrichotomous42")) 
age42LCA <- merge(age42_LCA, ID, by="ID", all.x=T) 
age42LCA<-age42LCA[,-17]

for (i in 1:length(age42LCA$ID)) {
  for (j in 2:ncol(age42LCA)) {
    if (age42LCA[i,j]==1 & !is.na(age42LCA[i,j])) {
      age42LCA[i,j] <- 8
    }
    else if (age42LCA[i,j]==2 & !is.na(age42LCA[i,j])) {
      age42LCA[i,j] <- 7
    }
    else if (age42LCA[i,j]==3 & !is.na(age42LCA[i,j])) {
      age42LCA[i,j] <- 6
    }
    else if (age42LCA[i,j]==4 & !is.na(age42LCA[i,j])) {
      age42LCA[i,j] <- 5
    }
    else if (age42LCA[i,j]==5 & !is.na(age42LCA[i,j])) {
      age42LCA[i,j] <- 4
    }
    else if (age42LCA[i,j]==6 & !is.na(age42LCA[i,j])) {
      age42LCA[i,j] <- 3
    }
    else if (age42LCA[i,j]==7 & !is.na(age42LCA[i,j])) {
      age42LCA[i,j] <- 2
    }
    else if ((age42LCA[i,j]==-1 | age42LCA[i,j]==-8)  & !is.na(age42LCA[i,j])) {
      age42LCA[i,j] <- NA
    }
    else if (age42LCA[i,j]==-9 & !is.na(age42LCA[i,j])) {
      age42LCA[i,j] <- 1
    }
  }}

colnames(age42_LCA)

table(data)
map(data, table)


f <- cbind(sport, martialarts_boxing_wrestling, watersports                 
           ,horseriding, yoga_pilates, golf,  Skiing, Othersporting,
           Raquet, Walk, Jogging, Dance, Cycling, Swimming_Diving, Fitness_gym_conditioning) ~ 1

data <- age42LCA[,-1] 

str(data)


#data <- data.frame(subset(data, select=-c(ID, na_count)))
#dev.off()
#install.packages("poLCA")
library(poLCA)



LCA_output_1class <- poLCA(f, data, nclass = 1, maxiter = 50000, graphs = TRUE, na.rm = FALSE, 
                           nrep =  10, verbose = TRUE)

LCA_output_2class <- poLCA(f, data, nclass = 2, maxiter = 50000, graphs = TRUE, na.rm = FALSE, 
                           nrep =  10, verbose = TRUE)

LCA_output_3class <- poLCA(f, data, nclass = 3, maxiter = 50000, graphs = FALSE, na.rm = FALSE, 
                           nrep =  10, verbose = FALSE)

LCA_output_4class <- poLCA(f, data, nclass = 4, maxiter = 50000, graphs = FALSE, na.rm = FALSE, 
                           nrep =  10, verbose = FALSE)

LCA_output_5class <- poLCA(f, data, nclass = 5, maxiter = 50000, graphs = FALSE, na.rm = FALSE, 
                           nrep =  10, verbose = FALSE)

LCA_output_6class <- poLCA(f, data, nclass = 6, maxiter = 50000, graphs = FALSE, na.rm = FALSE, 
                           nrep =  10, verbose = FALSE)

LCA_output_7class <- poLCA(f, data, nclass = 7, maxiter = 50000, graphs = FALSE, na.rm = FALSE, 
                           nrep =  10, verbose = FALSE)

LCA_output_8class <- poLCA(f, data, nclass = 8, maxiter = 50000, graphs = FALSE, na.rm = FALSE, 
                           nrep =  10, verbose = FALSE)


LCA_output_6class
tableBIC <- cbind(c(LCA_output_1class$bic, LCA_output_2class$bic, LCA_output_3class$bic, LCA_output_4class$bic, LCA_output_5class$bic, LCA_output_6class$bic, LCA_output_7class$bic, LCA_output_8class$bic))
plot(tableBIC)
tableBIC
write.csv(tableBIC, "c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\BIC_42.csv")
#LCA_output_6class2 <- as.matrix(LCA_output_6class)
#write.csv(LCA_output_6class2, "c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\LCA_6_42.csv")

par()


##Plot a 5-class##

table_probs_c1 <- as.matrix(LCA_output_6class$probs[[1]][1,]) 
table_probs_c2 <- as.matrix(LCA_output_6class$probs[[1]][2,]) 
table_probs_c3 <- as.matrix(LCA_output_6class$probs[[1]][3,]) 
table_probs_c4 <- as.matrix(LCA_output_6class$probs[[1]][4,]) 
table_probs_c5 <- as.matrix(LCA_output_6class$probs[[1]][5,]) 
table_probs_c6 <- as.matrix(LCA_output_6class$probs[[1]][6,]) 
LCA_output_6class$probs[[1]][2,]

#rm(table_probs_c1, table_probs_c2, table_probs_c3, table_probs_c4, table_probs_c5,table_probs_c6)


for (i in 2:length(LCA_output_6class$probs)) {
  table_probs_c1 <- cbind(table_probs_c1, LCA_output_6class$probs[[i]][1,])
}
for (i in 2:length(LCA_output_6class$probs)) {
  table_probs_c2 <- cbind(table_probs_c2, LCA_output_6class$probs[[i]][2,])
}
for (i in 2:length(LCA_output_6class$probs)) {
  table_probs_c3 <- cbind(table_probs_c3, LCA_output_6class$probs[[i]][3,])
}
for (i in 2:length(LCA_output_6class$probs)) {
  table_probs_c4 <- cbind(table_probs_c4, LCA_output_6class$probs[[i]][4,])
}
for (i in 2:length(LCA_output_6class$probs)) {
  table_probs_c5 <- cbind(table_probs_c5, LCA_output_6class$probs[[i]][5,])
}
for (i in 2:length(LCA_output_6class$probs)) {
  table_probs_c6 <- cbind(table_probs_c6, LCA_output_6class$probs[[i]][6,])
}
table_probs_c5

#write.csv(table_probs_c6, "c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\LCA_output_42.csv")
df <- read.csv("c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\LCA_output_42.csv")

#set level
classcleaner <- function(table_probs_c5) {
  c5<- as.data.frame(t(unlist(table_probs_c5)))
  c5
  df$Type
  c5$Type <- df$Type
  c5$X <- df$Var
  c5$Pr.tot <- ifelse(c5$'Pr(1)'+c5$'Pr(2)'+c5$'Pr(3)'>0.50, 0, 1)
  return(c5)}
table_probs_c1

c1 <- classcleaner(table_probs_c1)
c2 <- classcleaner(table_probs_c2)
c3 <- classcleaner(table_probs_c3)
c4 <- classcleaner(table_probs_c4)
c5 <- classcleaner(table_probs_c5)
c6 <- classcleaner(table_probs_c6)

Class1Summary <- c1 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))
Class2Summary <- c2 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))
Class3Summary <- c3 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))
Class4Summary <- c4 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))
Class5Summary <- c5 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))
Class6Summary <- c6 %>% group_by(Type) %>% summarise(typecount = sum(Pr.tot))

Class1Summary$typecount <- ifelse(Class1Summary$typecount>=1, 1,0)
Class2Summary$typecount <- ifelse(Class2Summary$typecount>=1, 1,0)
Class3Summary$typecount <- ifelse(Class3Summary$typecount>=1, 1,0)
Class4Summary$typecount <- ifelse(Class4Summary$typecount>=1, 1,0)
Class5Summary$typecount <- ifelse(Class5Summary$typecount>=1, 1,0)
Class6Summary$typecount <- ifelse(Class6Summary$typecount>=1, 1,0)



radarpl <- merge(Class1Summary, Class2Summary, by="Type")
radarpl <- merge(radarpl, Class3Summary, by="Type")
names(radarpl) <- c("Type", "Class1", "Class2", "Class3")
radarpl <- merge(radarpl, Class4Summary, by="Type")
radarpl <- merge(radarpl, Class5Summary, by="Type")
radarpl <- merge(radarpl, Class6Summary, by="Type")
names(radarpl) <- c("Type", "Class1", "Class2", "Class3", "Class4", "Class5", "Class6")
write_csv(radarpl, "c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\LCA_Summary_42.csv")
write_csv(as.data.frame(c(c1,c2,c3,c4,c5,c6)), "c:\\Users\\JohnM\\Documents\\MRC DTP\\Analysis_2 Local\\LCA_raw_42_2.csv")


radarpl
dat_t = as.data.frame(x = t(radarpl[,-1]), stringsAsFactors = FALSE, header=TRUE)
colnames(dat_t) <- c("Game", "Other", "Partner", "Solo", "Team", "Walking", "Watersports")
dat_t
max_min <- data.frame(Game = c(1, 0), Other = c(1, 0),
                      Partner = c(1, 0), Solo = c(1, 0), Team=c(1,0), Walking = c(1, 0),
                      Watersports = c(1, 0)
)

#c5 %>% filter(Type=='walking') %>% count()

rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, dat_t)
df

#Class1_data <- df[c("Max", "Min", "Class1"), ]
#library(fmsb)
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
colors <- c("#00AFBB", "#E7B800", "#FC4E07","#00AFBB", "#E7B800", "#FC4E07")
titles <- c("Class1", "Class2", "Class3", "Class4", "Class5", "Class6")

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(1,3))

# Create the radar chart
for(i in 1:6){
  create_beautiful_radarchart(
    data = df[c(1, 2, i+2), ], caxislabels = c("None", "", "", "", "Monrthly"),
    color = colors[i], title = titles[i]
  )
}

# 
# 
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())
