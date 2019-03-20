### Preamble ---------------
rm(list=ls())
library(survey);  library(mice); library(mitools);

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

### Begin Relative Risk Analysis ------------

### Preprocess ----------------

#read in data
data <- read.csv("data09_14.csv")

#pre-process data: recoding missing values and converting columns to factors
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

#define risk factors
riskFactors <- c("AF","ALCOHOL","DIABETES","RIDAGEYR","SDMVPSU","SDMVSTRA","WTINT2YR","WTMEC2YR","BMXBMI","LBXLYPCT","LBXPLTSI","MCQ080","MCQ160E","MCQ160D","MCQ160M","MCQ160C","MCQ160B","MCQ160F","MCQ160G","MCQ160K","MCQ220","MCQ160L","RIAGENDR","BPQ020","RIDRETH1", "BMXWAIST","LBDHDD","LBDSTRSI", "BPQ050A", "LBXGLU", "BPXSY","BPXDI")
riskVars <- c("ALCOHOL2","DIABETES","OLD","OBESE","MCQ080","MCQ160E","MCQ160D","MCQ160M","MCQ160C","MCQ160B","MCQ160F","MCQ160G","MCQ160K","MCQ220","MCQ160L","RIAGENDR","BPQ020","RIDRETH1")

#define new variables
data$ALCOHOL2 <- GE(data$ALCOHOL,1)
data$OLD <- GE(data$RIDAGEYR,65)
data$OBESE <- GE(data$BMXBMI,30)
data$EURO <- compareNA(data$RIDRETH1, 3)

#subset data
data <- data[,c(riskVars,"AF","SDMVPSU","SDMVSTRA","WTINT2YR","WTMEC2YR")]

#reweight data
data$WEIGHTS <- data$WTMEC2YR/3

#specify survey design
des <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WEIGHTS ,data=data, nest=TRUE) 

#loop for calculating relative risk
RRisk <- data.frame()
for (i in 1:(length(riskVars)-1)){
  #build regression formula
  form <- "AF~"
  for(j in 1:length(riskVars))
    if(riskVars[i]!=riskVars[j])
      form <- paste(form, riskVars[j], "+", sep="")
    form <- substring(form,1,nchar(form)-1)
    
    #fit model
    model<-svyglm(form, design=des,family=quasibinomial)
    
    #calculate model statistics
    means<-svypredmeans(model, as.formula(paste("~",riskVars[i],sep="")))
    if(is.element("FALSE",names(means)))
      cont <- svycontrast(means,quote(`TRUE`/`FALSE`))
    else if(is.element("1",names(means)))
      cont <- svycontrast(means,quote(`1`/`2`))
    
    #calculate relative risk confidence interval
    RRisk[riskVars[i],"Lower"] <- confint(cont, level=0.95)[1]
    RRisk[riskVars[i],"Upper"] <- confint(cont, level=0.95)[2]
}


#additional calculation for ethnicity
model<-svyglm(AF~ALCOHOL2+DIABETES+OLD+OBESE+MCQ080+MCQ160E+MCQ160D+MCQ160M+MCQ160C+MCQ160B+MCQ160F+MCQ160G+MCQ160K+MCQ220+MCQ160L+RIAGENDR+BPQ020, design=des,family=quasibinomial)
means<-svypredmeans(model, ~RIDRETH1)

cont <- svycontrast(means,quote(`3`/`1`));RRisk[paste("RIDRETH1",as.character(1)),"Lower"] <- confint(cont, level=0.95)[1];RRisk[paste("RIDRETH1",as.character(1)),"Upper"] <- confint(cont, level=0.95)[2]
cont <- svycontrast(means,quote(`3`/`2`));RRisk[paste("RIDRETH1",as.character(2)),"Lower"] <- confint(cont, level=0.95)[1];RRisk[paste("RIDRETH1",as.character(2)),"Upper"] <- confint(cont, level=0.95)[2]
cont <- svycontrast(means,quote(`3`/`4`));RRisk[paste("RIDRETH1",as.character(4)),"Lower"] <- confint(cont, level=0.95)[1];RRisk[paste("RIDRETH1",as.character(4)),"Upper"] <- confint(cont, level=0.95)[2]
cont <- svycontrast(means,quote(`3`/`5`));RRisk[paste("RIDRETH1",as.character(5)),"Lower"] <- confint(cont, level=0.95)[1];RRisk[paste("RIDRETH1",as.character(5)),"Upper"] <- confint(cont, level=0.95)[2]




### Begin Regression Analysis ---------------

data <- read.csv("data09_14.csv") #read in data
data$EURO <- compareNA(data$RIDRETH1, 3) #define new variable

#define important risk factors
riskFactors <- c("AF","ALCOHOL","DIABETES","RIDAGEYR","SDMVPSU","SDMVSTRA","WTINT2YR","WTMEC2YR","BMXBMI","LBXLYPCT","LBXPLTSI","MCQ080","MCQ160E","MCQ160D","MCQ160M","MCQ160C","MCQ160B","MCQ160F","MCQ160G","MCQ160K","MCQ220","MCQ160L","RIAGENDR","BPQ020","RIDRETH1", "BMXWAIST","LBDHDD","LBDSTRSI", "BPQ050A", "LBXGLU", "BPXSY","BPXDI", "EURO")

#pre-process data: recoding missing values and converting columns to factors
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

#subset dataframe
data2 <- data[,riskFactors]

#adjust weights
data$WTMEC2YR <- data$WTMEC2YR/3

### Impute missing values -----------------
#Impute missing values with chosen parameters
nm=10; data3 <- mice(data2, m=nm, maxit=10)

#collate data into datalist
datalist <- lapply(1:nm, complete, x=data3)
data4 <- imputationList(datalist)

### Regression ---------------

#specify survey design
surveyDesign <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR ,data=data4, nest=TRUE) 

#specify regression formula
form <- "AF ~ ALCOHOL+DIABETES+RIDAGEYR+BMXBMI+LBXLYPCT+LBXPLTSI+MCQ080+MCQ160E+MCQ160D+MCQ160M+MCQ160C+MCQ160B+MCQ160F+MCQ160G+MCQ160K+MCQ220+MCQ160L+RIAGENDR+BPQ020+RIDRETH1+EURO"

#fit model
sFullModel <- with(surveyDesign, svyglm(formula = form, family=quasibinomial))

#pool models from imputed results
comModel <- pool(as.mira(sFullModel))

### Results -----------------
f = format(summary(comModel)[,5],scientific=F)
for(i in 1:length(rownames(RRisk)))
  for(j in 1:length(names(f)))
    if(rownames(RRisk)[i] == names(f)[j] || rownames(RRisk)[i] == substr(names(f)[j], 1,(nchar(names(f[j]))-1) ))
      RRisk[i,"Reg Pval"] = as.numeric(f[j])

RRisk[,"Reg sig"]=NULL    
for(i in 1:dim(RRisk)[1])
  if(GE(0.1,RRisk[i,"Reg Pval"]))
    RRisk[i,"Reg sig"]="*"

f


