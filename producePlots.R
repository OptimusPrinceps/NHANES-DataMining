GE <- function(v1,v2) {
  #returns TRUE whenever v1 >= v2, excluding NA
  same <- (v1 >= v2)  #|  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}
compareNA <- function(v1,v2) {
  #returns TRUE whenever elements are the same, excluding NA
  same <- (v1 == v2)  #|  (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}
data <- read.csv("data09_14.csv")
for(i in 1:dim(data)[2]){
  if(is.factor(data[,i]))
    data[,i] <- as.integer(data[,i])
  
  r = range(data[!is.na(data[,i]),i])[2]
  #refactors 7.. and 9.. as NA
  if(!is.infinite(r))
    if((!r%%7 || !r%%9 ) && (!r%%1 || !r%%11 || !r%%111 || !r%%1111 || !r%%11111)){
      if(!r%%9)
        r = r*7/9
      data[GE(data[,i],r),i]=NA
    }
  
  if(typeof(data[,i])=="double")
    data[,i] <- as.integer(data[,i])
  #convert to factors
  if(length(levels(as.factor(data[,i])))<10 && typeof(data[,i])!="logical"){
    data[compareNA(data[,i],0),i]=NA
    data[,i] <- as.factor(data[,i])
  }
  if(typeof(data[,i])=="logical")
    data[,i] <- as.factor(data[,i])
  
}

dir.create("./plots")

#Age
#par(mfrow=c(1,2))
ageHistTotal <- hist(data$RIDAGEYR, freq = FALSE, main = "Age of total population", xlab = "Age")

pc=1;write.table(data$RIDAGEYR, file=paste("tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)
ageHistAF <- hist(data$RIDAGEYR[data$AF==TRUE], freq = TRUE, main = "Age of AF patients", xlab = "Age")

pc=2;write.table(data$RIDAGEYR[data$AF==TRUE], file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)

par(mfrow=c(1,1));barplot(ageHistAF$counts / ageHistTotal$counts, names.arg=c(as.character(ageHistAF$breaks[1:(length(ageHistAF$breaks)-2)]), "75+"), main  = "Prevalence of AF by Age group", xlab = "Age")
pc=11;write.table(ageHistAF$counts / ageHistTotal$counts, file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)

pie(ageHistAF$counts, c(as.character(ageHistAF$breaks[1:(length(ageHistAF$breaks)-2)]), "75+"),main = "Ages of patients with AF")

par(mfrow=c(1,2))
genderHistMale <- hist(data$RIDAGEYR[((data$AF==TRUE) + (data$RIAGENDR==1))==2], freq = TRUE, main = "Age of AF patients", xlab = "Age",ylim=c(0,100))
genderHistFemale <- hist(data$RIDAGEYR[((data$AF==TRUE) + (data$RIAGENDR==2))==2], freq = TRUE, main = "Age of AF patients", xlab = "Age", ylim = c(0,100))
pc=3;write.table(data$RIDAGEYR[((data$AF==TRUE) + (data$RIAGENDR==1))==2], file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)
pc=4;write.table(data$RIDAGEYR[((data$AF==TRUE) + (data$RIAGENDR==2))==2], file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)


Male <- hist(data[data$RIAGENDR==1,"RIDAGEYR"], freq = FALSE, main = "Age of total population", xlab = "Age")
Female <- hist(data[data$RIAGENDR==2,"RIDAGEYR"], freq = FALSE, main = "Age of total population", xlab = "Age")
pc=20;write.table(genderHistMale$counts / Male$counts, file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)
pc=21;write.table(genderHistFemale$counts / Female$counts, file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)


par(mfrow=c(1,1))
barplot(genderHistMale$density / genderHistFemale$density, names.arg=c(as.character(genderHistFemale$breaks[1:(length(genderHistFemale$breaks)-2)]), "75+"), main  = "Ratio of AF incidences of Males / Females (does this title make sense??)", xlab = "Age")
#BMI
par(mfrow=c(1,2))
hist(data$BMXBMI, freq = FALSE, main = "BMI of total population", xlab = "BMI", xlim=c(min(data$BMXBMI, na.rm = TRUE),max(data$BMXBMI, na.rm = TRUE)))
hist(data$BMXBMI[data$AF==TRUE], freq = FALSE, main = "BMI of AF patients", xlab = "BMI", xlim=c(min(data$BMXBMI, na.rm = TRUE),max(data$BMXBMI, na.rm = TRUE)))

par(mfrow=c(1,2))
obeseHistNoAF <- hist(data$RIDAGEYR[((GE(data$BMXBMI,30))+(data$AF==FALSE))==2], freq = FALSE, main = "Age of Obese patients without AF", xlab = "Age")
obeseHistAF <- hist(data$RIDAGEYR[((GE(data$BMXBMI,30))+(data$AF==TRUE))==2], freq = FALSE, main = "Age of Obese patients with AF", xlab = "Age")

pc=5;write.table(data$RIDAGEYR[((GE(data$BMXBMI,30))+(data$AF==FALSE))==2], file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)
pc=6;write.table(data$RIDAGEYR[((GE(data$BMXBMI,30))+(data$AF==TRUE))==2], file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)

par(mfrow=c(1,1)); barplot(obeseHistAF$counts / obeseHistNoAF$counts, names.arg=c(as.character(obeseHistNoAF$breaks[1:(length(obeseHistAF$breaks)-2)]), "75+"), main  = "Ratio of AF incidence compared to total population by Age group (does this title make sense??)", xlab = "Age")
pc=14;write.table(obeseHistAF$counts / obeseHistNoAF$counts, file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)

par(mfrow=c(1,2))
diabHistNoAF <- hist(data$RIDAGEYR[((data$DIABETES==TRUE)+(data$AF==FALSE))==2], freq = FALSE, main = "Age of Diabetic patients without AF", xlab = "Age", ylim = c(0,0.07))
diabHistAF <- hist(data$RIDAGEYR[((data$DIABETES==TRUE)+(data$AF==TRUE))==2], freq = FALSE, main = "Age of Diabetic patients with AF", xlab = "Age", xlim = c(20,80))
par(mfrow=c(1,1)); barplot(diabHistAF$density / diabHistNoAF$density, names.arg=c(as.character(diabHistAF$breaks[1:(length(diabHistAF$breaks)-2)]), "75+"), main  = "Ratio of AF incidence compared to total population by Age group (does this title make sense??)", xlab = "Age")
pc=12;write.table(data$RIDAGEYR[((data$DIABETES==TRUE)+(data$AF==FALSE))==2], file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)
pc=13;write.table(data$RIDAGEYR[((data$DIABETES==TRUE)+(data$AF==TRUE))==2], file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)

par(mfrow=c(1,2))
BPHistNoAF <- hist(data$RIDAGEYR[((compareNA(data$BPQ020,1))+(data$AF==FALSE))==2], freq = FALSE, main = "Age of high BP patients without AF", xlab = "Age", ylim = c(0,0.08))
BPHistAF <- hist(data$RIDAGEYR[((compareNA(data$BPQ020,1))+(data$AF==TRUE))==2], freq = FALSE, main = "Age of high BP patients with AF", xlab = "Age", ylim = c(0,0.08))
pc=9;write.table(data$RIDAGEYR[((compareNA(data$BPQ020,1))+(data$AF==FALSE))==2], file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)
pc=10;write.table(data$RIDAGEYR[((compareNA(data$BPQ020,1))+(data$AF==TRUE))==2], file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)

par(mfrow=c(1,1)); barplot(BPHistAF$counts / BPHistNoAF$counts, names.arg=c(as.character(BPHistAF$breaks[1:(length(BPHistAF$breaks)-2)]), "75+"), main  = "Ratio of AF incidence compared to total population by Age group (does this title make sense??)", xlab = "Age")

par(mfrow=c(1,2))
CHDHistNoAF <- hist(data$RIDAGEYR[((compareNA(data$MCQ160C,1))+(data$AF==FALSE))==2], freq = FALSE, main = "Age of Coronary Heart Disease patients without AF", xlab = "Age", ylim = c(0,0.1))
CHDHistAF <- hist(data$RIDAGEYR[((compareNA(data$MCQ160C,1))+(data$AF==TRUE))==2], freq = FALSE, main = "Age of Coronary Heart Disease patients with AF", xlab = "Age", ylim = c(0,0.1), xlim = c(20,80))
par(mfrow=c(1,1)); barplot(CHDHistAF$density / CHDHistNoAF$density, names.arg=c(as.character(CHDHistAF$breaks[1:(length(CHDHistAF$breaks)-2)]), "75+"), main  = "Ratio of AF incidence compared to total population by Age group (does this title make sense??)", xlab = "Age")

par(mfrow=c(1,2))
HAHistNoAF <- hist(data$RIDAGEYR[((compareNA(data$MCQ160E,1))+(data$AF==FALSE))==2], freq = FALSE, main = "Age of Heart Attack patients without AF", xlab = "Age", ylim = c(0,0.1))
HAHistAF <- hist(data$RIDAGEYR[((compareNA(data$MCQ160E,1))+(data$AF==TRUE))==2], freq = FALSE, main = "Age of Heart Attack patients with AF", xlab = "Age", ylim = c(0,0.1), xlim = c(20,80))
par(mfrow=c(1,1)); barplot(HAHistAF$density / HAHistNoAF$density, names.arg=c(as.character(HAHistAF$breaks[1:(length(HAHistAF$breaks)-2)]), "75+"), main  = "Ratio of AF incidence compared to total population by Age group (does this title make sense??)", xlab = "Age")


dAF <- data[data$AF==TRUE,]
dNAF <- data[data$AF==FALSE,]
#dAF <- data[((data$AF==TRUE)+(GE(data$RIDAGEYR,60)))==2,]
#dNAF <- data[((data$AF==FALSE)+(GE(data$RIDAGEYR,60)))==2,]
par(mfrow=c(1,2))
hist((  (GE(dAF$BMXBMI,30)) + (dAF$DIABETES==TRUE) +(compareNA(dAF$BPQ020,1)) + (compareNA(dAF$MCQ160C,1)) + (compareNA(dAF$MCQ160E,1))  ),freq=FALSE, main="Disease distribution in AF patients", ylim = c(0,0.9))
hist((  (GE(dNAF$BMXBMI,30)) + (dNAF$DIABETES==TRUE) +(compareNA(dNAF$BPQ020,1)) + (compareNA(dNAF$MCQ160C,1)) + (compareNA(dNAF$MCQ160E,1))  ),freq=FALSE, main="Disease distribution in non AF patients", ylim = c(0,0.9))

pc=7;write.table((  (GE(dAF$BMXBMI,30)) + (dAF$DIABETES==TRUE) +(compareNA(dAF$BPQ020,1)) + (compareNA(dAF$MCQ160C,1)) + (compareNA(dAF$MCQ160E,1))  ), file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)
pc=8;write.table((  (GE(dNAF$BMXBMI,30)) + (dNAF$DIABETES==TRUE) +(compareNA(dNAF$BPQ020,1)) + (compareNA(dNAF$MCQ160C,1)) + (compareNA(dNAF$MCQ160E,1))  ), file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)

#using my risk factors
hist((  (GE(dAF$BMXBMI,30)) + (compareNA(dAF$MCQ160M,1)) +(compareNA(dAF$BPQ020,1)) + (compareNA(dAF$MCQ160B,1)) + (GE(dAF$RIDAGEYR,65))  ),freq=FALSE, main="Disease distribution in AF patients", ylim = c(0,0.9))
hist((  (GE(dNAF$BMXBMI,30)) + (compareNA(dNAF$MCQ160M,1)) +(compareNA(dNAF$BPQ020,1)) + (compareNA(dNAF$MCQ160B,1)) + (compareNA(dNAF$RIDAGEYR,65))  ),freq=FALSE, main="Disease distribution in non AF patients", ylim = c(0,0.9))
pc=15;write.table((  (GE(dAF$BMXBMI,30)) + (compareNA(dAF$MCQ160M,1)) +(compareNA(dAF$BPQ020,1)) + (compareNA(dAF$MCQ160B,1)) + (GE(dAF$RIDAGEYR,65))  ), file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)
pc=16;write.table((  (GE(dNAF$BMXBMI,30)) + (compareNA(dNAF$MCQ160M,1)) +(compareNA(dNAF$BPQ020,1)) + (compareNA(dNAF$MCQ160B,1)) + (compareNA(dNAF$RIDAGEYR,65))  ), file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)

#only people aboe 65
dAF <- data[((data$AF==TRUE)+GE(data$RIDAGEYR,65))==2,]
dNAF <- data[((data$AF==FALSE)+GE(data$RIDAGEYR,65))==2,]
par(mfrow=c(1,2))
hist((  (GE(dAF$BMXBMI,30)) + (dAF$DIABETES==TRUE) +(compareNA(dAF$BPQ020,1)) + (compareNA(dAF$MCQ160C,1)) + (compareNA(dAF$MCQ160E,1))  ),freq=FALSE, main="Disease distribution in AF patients", ylim = c(0,0.9))
hist((  (GE(dNAF$BMXBMI,30)) + (dNAF$DIABETES==TRUE) +(compareNA(dNAF$BPQ020,1)) + (compareNA(dNAF$MCQ160C,1)) + (compareNA(dNAF$MCQ160E,1))  ),freq=FALSE, main="Disease distribution in non AF patients", ylim = c(0,0.9))

pc=31;write.table((  (GE(dAF$BMXBMI,30)) + (dAF$DIABETES==TRUE) +(compareNA(dAF$BPQ020,1)) + (compareNA(dAF$MCQ160C,1)) + (compareNA(dAF$MCQ160E,1))  ), file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)
pc=32;write.table((  (GE(dNAF$BMXBMI,30)) + (dNAF$DIABETES==TRUE) +(compareNA(dNAF$BPQ020,1)) + (compareNA(dNAF$MCQ160C,1)) + (compareNA(dNAF$MCQ160E,1))  ), file=paste("./plots/tab",as.character(pc),".csv",sep=''),sep=",", row.names = FALSE, col.names=FALSE)



