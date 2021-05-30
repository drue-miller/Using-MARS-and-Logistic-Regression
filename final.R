library(readxl)
FINALDATA <- read_excel("Predictive Analytics/FINALDATA.xlsx")
summary(FINALDATA)
View(FINALDATA)
str(FINALDATA)

#--------Correlation Matrix to pick variables
library(ggcorrplot)
ggcorrplot(cor(FINALDATA))
cor(FINALDATA)

#--------Split Data into Test and Train 
library(caTools)
set.seed(103)
sample<-sample.int(n=nrow(FINALDATA), size=floor(.6*nrow(FINALDATA)), replace=F)
train<-FINALDATA[sample, ]
test<-FINALDATA[-sample, ]

#---------Linear Model
FINALDATA$CreditCut=cut(FINALDATA$Credit.Limit,breaks=c(-Inf, 250, 300, 350, 400, 600, 800, Inf),right=FALSE)

table(FINALDATA$CreditCut,FINALDATA$Bad)

Ctab=as.matrix(table(FINALDATA$CreditCut,FINALDATA$Bad))

#copy and paste this table into Excel and calculate WOE and IV
#...or write the code to calculate IV and WOE
#note the text import wizard will help
Ctab

## CL cut
##################################
## WOE calculation
Ccount0=sum(Ctab[,1])
Ccount1=sum(Ctab[,2])

C1overnot1=Ccount1/Ccount0
CWOE=rep(NA, nrow(Ctab))
for(i in 1:length(CWOE)){
  CWOE[i]=log((Ctab[i,2]/Ctab[i,1]))-log(C1overnot1)
}
CWOE

## IV calculation
CIVbin=rep(NA,(nrow(Ctab)))
for(i in 1:length(CIVbin)){
  CIVbin[i]=(Ctab[i,2]/sum(Ctab[,2])-Ctab[i,1]/sum(Ctab[,1]))*CWOE[i]
}
CIVbin
CIV=sum(CIVbin)
CIV


## Creating Tables
##########################################
## Creating Table for Credit Limit
Ctab1=as.data.frame.matrix(Ctab)
Ctab1$WOE=CWOE
Ctab1$Inf_Val=CIVbin
Ctab1
Ctab1[8,]=c("--","--","--",CIV)
rownames(Ctab1)[8]="Information Value"
Ctab1
## model 1 reports
library(ROCR)
FINALDATA$CL_WOE=NA
## make model 3 here

asd=function(k){
  if(k<250){return(1)}
  if(k>=250 && k<300){return(2)}
  if(k>=300 && k<350){return(3)}
  if(k>=350 && k<400){return(4)}
  if(k>=400 && k<600){return(5)}
  if(k>=600 && k<800){return(6)}
  if(k>=800){return(7)}
  
}

for(i in 1:nrow(FINALDATA)){
  FINALDATA$CL_WOE[i]=CWOE[asd(FINALDATA$Credit.Limit[i])]
}

logmodel1=glm(Bad~Months.On.Book + Days.Deliq+CreditCut,family=binomial,data=train)
summary(logmodel1)

logmodel2=glm(Bad~Months.On.Book + Days.Deliq+CreditCut,family=binomial,data=test)
summary(logmodel2)

logmodel3=glm(Bad~Months.On.Book + Days.Deliq+CL_WOE,data=train,family = binomial)
summary(logmodel3)

logmodel4=glm(Bad~Months.On.Book + Days.Deliq + CL_WOE,data=test,family = binomial)
summary(logmodel4)

pred1=prediction(logmodel1$fitted.values,train$Bad)
perf1=performance(pred1,"tpr","fpr")

pred3=prediction(logmodel3$fitted.values,train$Bad)
perf3=performance(pred3,"tpr","fpr")

pred4=prediction(logmodel4$fitted.values,test$Bad)
perf4=performance(pred4,"tpr","fpr")

#--------ROC 
plot(perf3,col=1, lty=1, lwd=2, main = "ROC Curve for Logistic Model")
lines(unlist(perf4@y.values)~unlist(perf4@x.values),col=4, lwd=2)
abline(h=c(0:10)/10,v=c(0:10)/10,lty=3, col="gray")
legend("topleft",c("Model 1 (Training)","Model 2 (Test)"),
       col=c(1,4),lwd=2,bty = "n")


p <- predict(logmodel3, newdata=train, type="response")
pr <- prediction(p, train$Bad)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve Logistic Model Training", col="blue", lwd=3)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.738044
lines(x = c(0,1),y=c(0,1), lwd=3)

p <- predict(logmodel3, newdata=test, type="response")
pr <- prediction(p, test$Bad)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve Logistic Model Test", col="blue", lwd=3)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.7504056
lines(x = c(0,1),y=c(0,1), lwd=3)
CWOE
summary(logmodel3)
summary(logmodel4)
#------------KS statistic
sum(train$Bad)
#Training data
train$scoredProb<-predict(logmodel3, newdata=train, type="response")
train2<-train[order(-train$scoredProb),]
train2$ID <- seq.int(nrow(train2))
train2$depth<-train2$ID/5995
train2$Cum_0=cumsum(train2$Bad==0)
train2$CumProp0=train2$Cum_0/sum(train2$Bad==0)
train2$Cum_1=cumsum(train2$Bad)
train2$CumProp1=train2$Cum_1/2250
plot(train2$depth, train2$CumProp1, lwd=0.1, main="KS Curve for Training Data (5,995 Obs, 2,250 Bads)", xlab="Depth", ylab="Cumulative Proportion of 1", col="blue")
lines(train2$depth, train2$CumProp0, lwd=8, col="orange")
legend('bottomright', legend=c("TPR", "FPR"),
       col=c("blue", "orange"), lty=1)
#Test data
sum(test$Bad)
test$scoredProb<-predict(logmodel3, newdata=test, type="response")
test2<-test[order(-test$scoredProb),]
test2$ID <- seq.int(nrow(test2))
test2$depth<-test2$ID/3997
test2$Cum_0=cumsum(test2$Bad==0)
test2$CumProp0=test2$Cum_0/sum(test2$Bad==0)
test2$Cum_1=cumsum(test2$Bad)
test2$CumProp1=test2$Cum_1/1477
plot(test2$depth, test2$CumProp1, lwd=0.1, main="KS Curve for Test Data (3,997 Obs, 1,477 Bads)", xlab="Depth", ylab="Cumulative Proportion of 1", col="blue")
lines(train2$depth, train2$CumProp0, lwd=8, col="orange")
legend('bottomright', legend=c("TPR", "FPR"),
       col=c("blue", "orange"), lty=1)

#-------Lift and Gains-------
#LIFT AND GAINS FOR TRAINING
baserate=mean(train2$Bad)
total_No_of_defaults=sum(train2$Bad)
group=10 # you can change this group as required
Depth=seq(1: group)/group
NObs=rep(NA, group)->Cum_NObs->Mean_Response->
  Cum_Mean_Response->Lift->Cum_Lift->
  Pos_Capture_Rate->Cum_Pos_Capture_Rate
for(i in 1:length(NObs)){
  # Define start point
  if(i>1){
    start=Depth[i-1]
  }else{start=0}
  # Define end point
  end=Depth[i]
  startVal=quantile(train2$scoredProb, start)
  endVal=quantile(train2$scoredProb, end)
  if(i>1){
    startIndex=which(train2$scoredProb>startVal)
  }else{
    startIndex=which(train2$scoredProb>=0)
  }
  endIndex=which(train2$scoredProb<=endVal)
  Index=intersect(startIndex,endIndex)
  # Number of Observation
  # and cumulative number of observations
  NObs[i]=length(Index)
  Cum_NObs[i]=length(endIndex) # cumsum could be used later
}
## Work with other measures
for(i in 1:group){
  if(i==1){
    start=1
  }else{
    start=Cum_NObs[i-1]+1
  }
  end=Cum_NObs[i]
  # Mean response and cumulative
  Mean_Response[i]=mean(train2$Bad[start:end])
  Cum_Mean_Response[i]=mean(train2$Bad[1:end])
  # Lift and cumulative lift
  Lift[i]=Mean_Response[i]/baserate
  Cum_Lift[i]=Cum_Mean_Response[i]/baserate
  # Capture Rate and Cumulative Capture Rate
  Pos_Capture_Rate[i]=sum(train2$Bad[start:end])/total_No_of_defaults
  Cum_Pos_Capture_Rate[i]=sum(train2$Bad[1:end])/total_No_of_defaults
}
round3=function(x){round(x,3)}
Mean_Response=round3(Mean_Response)
Cum_Mean_Response=round3(Cum_Mean_Response)
Pos_Capture_Rate=round3(Pos_Capture_Rate)
Cum_Pos_Capture_Rate=round3(Cum_Pos_Capture_Rate)
Lift=round3(Lift)
Cum_Lift=round3(Cum_Lift)
# Make the gains table
gains_table=as.data.frame(cbind (Depth=100*Depth, NObs, Cum_NObs,
                                 Mean_Response,  Cum_Mean_Response,
                                 Pos_Capture_Rate, Cum_Pos_Capture_Rate,
                                 Lift, Cum_Lift) )




print(gains_table)
# Plottings
# Gains Plot
plot(Cum_Pos_Capture_Rate~Depth,data=gains_table,
     type="o", lwd=2, col="orange", pch=16,
     main="Cumulative Gains Plot for Training",xlim=c(0,100),
     xlab="Depth of Population (%)")
grid()

# LIFT PLOT
plot(Cum_Lift~Depth,data=gains_table,
     type="o", lwd=2, col="forestgreen", pch=16,ylab="",
     main="Lift Plot for Training",xlim=c(0,100),ylim=c(0,3),
     xlab="Depth of Population (%)")
lines(gains_table$Lift~gains_table$Depth, type="o", col="gold4",lwd=2, pch=15)
grid()
abline(h=1, lwd=2)
legend("topright", c("Lift", "Cumulative Lift"),lwd=2, pch=c(15,16),
       col=c("gold4", "forestgreen"), bty="n")

#LIFT AND GAINS FOR TEST
baserate=mean(test2$Bad)
total_No_of_defaults=sum(test2$Bad)
group=10 # you can change this group as required
Depth=seq(1: group)/group
NObs=rep(NA, group)->Cum_NObs->Mean_Response->
  Cum_Mean_Response->Lift->Cum_Lift->
  Pos_Capture_Rate->Cum_Pos_Capture_Rate
for(i in 1:length(NObs)){
  # Define start point
  if(i>1){
    start=Depth[i-1]
  }else{start=0}
  # Define end point
  end=Depth[i]
  startVal=quantile(test2$scoredProb, start)
  endVal=quantile(test2$scoredProb, end)
  if(i>1){
    startIndex=which(test2$scoredProb>startVal)
  }else{
    startIndex=which(test2$scoredProb>=0)
  }
  endIndex=which(test2$scoredProb<=endVal)
  Index=intersect(startIndex,endIndex)
  # Number of Observation
  # and cumulative number of observations
  NObs[i]=length(Index)
  Cum_NObs[i]=length(endIndex) # cumsum could be used later
}
## Work with other measures
for(i in 1:group){
  if(i==1){
    start=1
  }else{
    start=Cum_NObs[i-1]+1
  }
  end=Cum_NObs[i]
  # Mean response and cumulative
  Mean_Response[i]=mean(test2$Bad[start:end])
  Cum_Mean_Response[i]=mean(test2$Bad[1:end])
  # Lift and cumulative lift
  Lift[i]=Mean_Response[i]/baserate
  Cum_Lift[i]=Cum_Mean_Response[i]/baserate
  # Capture Rate and Cumulative Capture Rate
  Pos_Capture_Rate[i]=sum(test2$Bad[start:end])/total_No_of_defaults
  Cum_Pos_Capture_Rate[i]=sum(test2$Bad[1:end])/total_No_of_defaults
}
round3=function(x){round(x,3)}
Mean_Response=round3(Mean_Response)
Cum_Mean_Response=round3(Cum_Mean_Response)
Pos_Capture_Rate=round3(Pos_Capture_Rate)
Cum_Pos_Capture_Rate=round3(Cum_Pos_Capture_Rate)
Lift=round3(Lift)
Cum_Lift=round3(Cum_Lift)
# Make the gains table
gains_table=as.data.frame(cbind (Depth=100*Depth, NObs, Cum_NObs,
                                 Mean_Response,  Cum_Mean_Response,
                                 Pos_Capture_Rate, Cum_Pos_Capture_Rate,
                                 Lift, Cum_Lift) )




print(gains_table)
# Plottings
# Gains Plot
plot(Cum_Pos_Capture_Rate~Depth,data=gains_table,
     type="o", lwd=2, col="orange", pch=16,
     main="Cumulative Gains Plot for Test",xlim=c(0,100),
     xlab="Depth of Population (%)")
grid()

# LIFT PLOT
plot(Cum_Lift~Depth,data=gains_table,
     type="o", lwd=2, col="forestgreen", pch=16,ylab="",
     main="Lift Plot for Test",xlim=c(0,100),ylim=c(0,3),
     xlab="Depth of Population (%)")
lines(gains_table$Lift~gains_table$Depth, type="o", col="gold4",lwd=2, pch=15)
grid()
abline(h=1, lwd=2)
legend("topright", c("Lift", "Cumulative Lift"),lwd=2, pch=c(15,16),
       col=c("gold4", "forestgreen"), bty="n")

#------------------MARS MODEL
library(earth)
library(mda)
library(faraway)
library(lattice)
earth1=earth(train$Net.Behavior.Fees.Billed.During.Cycle, train$Ending.Balance)
summary(earth1)#this now gives meaningful output in this library.  notice the gcv.
plot(train$Net.Behavior.Fees.Billed.During.Cycle, train$Ending.Balance, main= "Net Behavior Fees vs. Ending Balance", xlab = "Net Behavior Fees Billed During Cycle", ylab = "Ending Balance")
abline(v=earth1$cuts)
summary(earth1)
head(earth1$bx)#this gives the head of the model matrix frame
head(train)#gives the head of the data
earth1$coef#gives the coefficients
#notice the arithmetic
head(earth1$fitted)
head(predict(earth1))

earth1$fitted[2]#the second observation of the fitted values

mypredict1=earth1$bx[1,]*earth1$coef
sum(mypredict1)

earth2=earth(Bad ~ Months.On.Book + Days.Deliq +
               Credit.Limit, data=train,
             glm=list(family=binomial),degree=1)

summary(earth2)
assign=runif(length(train[,1]))
group=assign*NA #creates a blank vector the length of assign
group=ifelse(assign>.6,1,2)
table(group)

predictions=predict(earth2,type='response',newdata=test)

##TRAINING DATA
#ROC
library(ROCR)
p <- predict(earth2, newdata=train, type="response")
pr <- prediction(p, train$Bad)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve MARS Model Training", col="blue", lwd=3)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.7384368
lines(x = c(0,1),y=c(0,1), lwd=3)

#KS
train$scoredProb<-predict(earth2, newdata=train, type="response")
train2<-train[order(-train$scoredProb),]
train2$ID <- seq.int(nrow(train2))
train2$depth<-train2$ID/5995
train2$Cum_0=cumsum(train2$Bad==0)
train2$CumProp0=train2$Cum_0/sum(train2$Bad==0)
train2$Cum_1=cumsum(train2$Bad)
train2$CumProp1=train2$Cum_1/2250
plot(train2$depth, train2$CumProp1, lwd=0.1, main="KS Curve for Training Data (5,995 Obs, 2,250 Bads)", xlab="Depth", ylab="Cumulative Proportion of 1", col="blue")
lines(train2$depth, train2$CumProp0, lwd=8, col="orange")
legend('bottomright', legend=c("TPR", "FPR"),
       col=c("blue", "orange"), lty=1)


#-------Lift and Gains-------
#LIFT AND GAINS FOR TRAINING
baserate=mean(train2$Bad)
total_No_of_defaults=sum(train2$Bad)
group=10 # you can change this group as required
Depth=seq(1: group)/group
NObs=rep(NA, group)->Cum_NObs->Mean_Response->
  Cum_Mean_Response->Lift->Cum_Lift->
  Pos_Capture_Rate->Cum_Pos_Capture_Rate
for(i in 1:length(NObs)){
  # Define start point
  if(i>1){
    start=Depth[i-1]
  }else{start=0}
  # Define end point
  end=Depth[i]
  startVal=quantile(train2$scoredProb, start)
  endVal=quantile(train2$scoredProb, end)
  if(i>1){
    startIndex=which(train2$scoredProb>startVal)
  }else{
    startIndex=which(train2$scoredProb>=0)
  }
  endIndex=which(train2$scoredProb<=endVal)
  Index=intersect(startIndex,endIndex)
  # Number of Observation
  # and cumulative number of observations
  NObs[i]=length(Index)
  Cum_NObs[i]=length(endIndex) # cumsum could be used later
}
## Work with other measures
for(i in 1:group){
  if(i==1){
    start=1
  }else{
    start=Cum_NObs[i-1]+1
  }
  end=Cum_NObs[i]
  # Mean response and cumulative
  Mean_Response[i]=mean(train2$Bad[start:end])
  Cum_Mean_Response[i]=mean(train2$Bad[1:end])
  # Lift and cumulative lift
  Lift[i]=Mean_Response[i]/baserate
  Cum_Lift[i]=Cum_Mean_Response[i]/baserate
  # Capture Rate and Cumulative Capture Rate
  Pos_Capture_Rate[i]=sum(train2$Bad[start:end])/total_No_of_defaults
  Cum_Pos_Capture_Rate[i]=sum(train2$Bad[1:end])/total_No_of_defaults
}
round3=function(x){round(x,3)}
Mean_Response=round3(Mean_Response)
Cum_Mean_Response=round3(Cum_Mean_Response)
Pos_Capture_Rate=round3(Pos_Capture_Rate)
Cum_Pos_Capture_Rate=round3(Cum_Pos_Capture_Rate)
Lift=round3(Lift)
Cum_Lift=round3(Cum_Lift)
# Make the gains table
gains_table=as.data.frame(cbind (Depth=100*Depth, NObs, Cum_NObs,
                                 Mean_Response,  Cum_Mean_Response,
                                 Pos_Capture_Rate, Cum_Pos_Capture_Rate,
                                 Lift, Cum_Lift) )




print(gains_table)
# Plottings
# Gains Plot
plot(Cum_Pos_Capture_Rate~Depth,data=gains_table,
     type="o", lwd=2, col="orange", pch=16,
     main="Cumulative Gains Plot",xlim=c(0,100),
     xlab="Depth of Population (%)")
grid()

# LIFT PLOT
plot(Cum_Lift~Depth,data=gains_table,
     type="o", lwd=2, col="forestgreen", pch=16,ylab="",
     main="Lift Plot for MARS Model (train)",xlim=c(0,100),ylim=c(0,3),
     xlab="Depth of Population (%)")
lines(gains_table$Lift~gains_table$Depth, type="o", col="gold4",lwd=2, pch=15)
grid()
abline(h=1, lwd=2)
legend("topright", c("Lift", "Cumulative Lift"),lwd=2, pch=c(15,16),
       col=c("gold4", "forestgreen"), bty="n")





###TEST DATA
#ROC
library(ROCR)
p <- predict(earth2, newdata=test, type="response")
pr <- prediction(p, test$Bad)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve MARS Model (test)", col="blue", lwd=3)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.7529
lines(x = c(0,1),y=c(0,1), lwd=3)

#KS
test$scoredProb<-predict(earth2, newdata=test, type="response")
test2<-test[order(-test$scoredProb),]
test2$ID <- seq.int(nrow(test2))
test2$depth<-test2$ID/3997
test2$Cum_0=cumsum(test2$Bad==0)
test2$CumProp0=test2$Cum_0/sum(test2$Bad==0)
test2$Cum_1=cumsum(test2$Bad)
test2$CumProp1=test2$Cum_1/1477
plot(test2$depth, test2$CumProp1, lwd=0.1, main="KS Curve for MARS (Test: 3,997 Obs, 1,477 Bads)", xlab="Depth", ylab="Cumulative Proportion of 1", col="blue")
lines(test2$depth, test2$CumProp0, lwd=8, col="orange")
legend('bottomright', legend=c("TPR", "FPR"),
       col=c("blue", "orange"), lty=1)


#-------Lift and Gains-------
#LIFT AND GAINS FOR TRAINING
baserate=mean(test2$Bad)
total_No_of_defaults=sum(test2$Bad)
group=10 # you can change this group as required
Depth=seq(1: group)/group
NObs=rep(NA, group)->Cum_NObs->Mean_Response->
  Cum_Mean_Response->Lift->Cum_Lift->
  Pos_Capture_Rate->Cum_Pos_Capture_Rate
for(i in 1:length(NObs)){
  # Define start point
  if(i>1){
    start=Depth[i-1]
  }else{start=0}
  # Define end point
  end=Depth[i]
  startVal=quantile(test2$scoredProb, start)
  endVal=quantile(test2$scoredProb, end)
  if(i>1){
    startIndex=which(test2$scoredProb>startVal)
  }else{
    startIndex=which(test2$scoredProb>=0)
  }
  endIndex=which(test2$scoredProb<=endVal)
  Index=intersect(startIndex,endIndex)
  # Number of Observation
  # and cumulative number of observations
  NObs[i]=length(Index)
  Cum_NObs[i]=length(endIndex) # cumsum could be used later
}
## Work with other measures
for(i in 1:group){
  if(i==1){
    start=1
  }else{
    start=Cum_NObs[i-1]+1
  }
  end=Cum_NObs[i]
  # Mean response and cumulative
  Mean_Response[i]=mean(test2$Bad[start:end])
  Cum_Mean_Response[i]=mean(test2$Bad[1:end])
  # Lift and cumulative lift
  Lift[i]=Mean_Response[i]/baserate
  Cum_Lift[i]=Cum_Mean_Response[i]/baserate
  # Capture Rate and Cumulative Capture Rate
  Pos_Capture_Rate[i]=sum(test2$Bad[start:end])/total_No_of_defaults
  Cum_Pos_Capture_Rate[i]=sum(test2$Bad[1:end])/total_No_of_defaults
}
round3=function(x){round(x,3)}
Mean_Response=round3(Mean_Response)
Cum_Mean_Response=round3(Cum_Mean_Response)
Pos_Capture_Rate=round3(Pos_Capture_Rate)
Cum_Pos_Capture_Rate=round3(Cum_Pos_Capture_Rate)
Lift=round3(Lift)
Cum_Lift=round3(Cum_Lift)
# Make the gains table
gains_table=as.data.frame(cbind (Depth=100*Depth, NObs, Cum_NObs,
                                 Mean_Response,  Cum_Mean_Response,
                                 Pos_Capture_Rate, Cum_Pos_Capture_Rate,
                                 Lift, Cum_Lift) )




print(gains_table)
# Plottings
# Gains Plot
plot(Cum_Pos_Capture_Rate~Depth,data=gains_table,
     type="o", lwd=2, col="orange", pch=16,
     main="Cumulative Gains Plot for MARS",xlim=c(0,100),
     xlab="Depth of Population (%)")
grid()

# LIFT PLOT
plot(Cum_Lift~Depth,data=gains_table,
     type="o", lwd=2, col="forestgreen", pch=16,ylab="",
     main="Lift Plot for MARS (test)",xlim=c(0,100),ylim=c(0,3),
     xlab="Depth of Population (%)")
lines(gains_table$Lift~gains_table$Depth, type="o", col="gold4",lwd=2, pch=15)
grid()
abline(h=1, lwd=2)
legend("topright", c("Lift", "Cumulative Lift"),lwd=2, pch=c(15,16),
       col=c("gold4", "forestgreen"), bty="n")
library(caTools)
set.seed(103)
sample<-sample.int(n=nrow(MidtermData2), size=floor(.6*nrow(MidtermData2)), replace=F)
train4<-MidtermData2[sample, ]
test4<-MidtermData2[-sample, ]

logmodel5=glm(Bad~`Good Customer Score` + `Behavior Score`,data=train4,family = binomial)
summary(logmodel5)


#--------ROC 
p <- predict(logmodel5, newdata=train4, type="response")
pr <- prediction(p, train4$Bad)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve GCS&BS Model Training", col="blue", lwd=3)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.7600922
lines(x = c(0,1),y=c(0,1), lwd=3)

#------------KS statistic
sum(train4$Bad)
#Training data
train4$scoredProb<-predict(logmodel5, newdata=train4, type="response")
train2<-train4[order(-train4$scoredProb),]
train2$ID <- seq.int(nrow(train2))
train2$depth<-train2$ID/5995
train2$Cum_0=cumsum(train2$Bad==0)
train2$CumProp0=train2$Cum_0/sum(train2$Bad==0)
train2$Cum_1=cumsum(train2$Bad)
train2$CumProp1=train2$Cum_1/2250
plot(train2$depth, train2$CumProp1, lwd=0.1, main="KS Curve for Training Data (5,995 Obs, 2,250 Bads)", xlab="Depth", ylab="Cumulative Proportion of 1", col="blue")
lines(train2$depth, train2$CumProp0, lwd=8, col="orange")
legend('bottomright', legend=c("TPR", "FPR"),
       col=c("blue", "orange"), lty=1)

#-------Lift and Gains-------
#LIFT AND GAINS FOR TRAINING
baserate=mean(train4$Bad)
total_No_of_defaults=sum(train4$Bad)
group=10 # you can change this group as required
Depth=seq(1: group)/group
NObs=rep(NA, group)->Cum_NObs->Mean_Response->
  Cum_Mean_Response->Lift->Cum_Lift->
  Pos_Capture_Rate->Cum_Pos_Capture_Rate
for(i in 1:length(NObs)){
  # Define start point
  if(i>1){
    start=Depth[i-1]
  }else{start=0}
  # Define end point
  end=Depth[i]
  startVal=quantile(train4$scoredProb, start)
  endVal=quantile(train4$scoredProb, end)
  if(i>1){
    startIndex=which(train4$scoredProb>startVal)
  }else{
    startIndex=which(train4$scoredProb>=0)
  }
  endIndex=which(train4$scoredProb<=endVal)
  Index=intersect(startIndex,endIndex)
  # Number of Observation
  # and cumulative number of observations
  NObs[i]=length(Index)
  Cum_NObs[i]=length(endIndex) # cumsum could be used later
}
## Work with other measures
for(i in 1:group){
  if(i==1){
    start=1
  }else{
    start=Cum_NObs[i-1]+1
  }
  end=Cum_NObs[i]
  # Mean response and cumulative
  Mean_Response[i]=mean(train2$Bad[start:end])
  Cum_Mean_Response[i]=mean(train2$Bad[1:end])
  # Lift and cumulative lift
  Lift[i]=Mean_Response[i]/baserate
  Cum_Lift[i]=Cum_Mean_Response[i]/baserate
  # Capture Rate and Cumulative Capture Rate
  Pos_Capture_Rate[i]=sum(train2$Bad[start:end])/total_No_of_defaults
  Cum_Pos_Capture_Rate[i]=sum(train2$Bad[1:end])/total_No_of_defaults
}
round3=function(x){round(x,3)}
Mean_Response=round3(Mean_Response)
Cum_Mean_Response=round3(Cum_Mean_Response)
Pos_Capture_Rate=round3(Pos_Capture_Rate)
Cum_Pos_Capture_Rate=round3(Cum_Pos_Capture_Rate)
Lift=round3(Lift)
Cum_Lift=round3(Cum_Lift)
# Make the gains table
gains_table=as.data.frame(cbind (Depth=100*Depth, NObs, Cum_NObs,
                                 Mean_Response,  Cum_Mean_Response,
                                 Pos_Capture_Rate, Cum_Pos_Capture_Rate,
                                 Lift, Cum_Lift) )




print(gains_table)
# Plottings
# Gains Plot
plot(Cum_Pos_Capture_Rate~Depth,data=gains_table,
     type="o", lwd=2, col="orange", pch=16,
     main="Cumulative Gains Plot for Training",xlim=c(0,100),
     xlab="Depth of Population (%)")
grid()

# LIFT PLOT
plot(Cum_Lift~Depth,data=gains_table,
     type="o", lwd=2, col="forestgreen", pch=16,ylab="",
     main="Lift Plot for Training",xlim=c(0,100),ylim=c(0,3),
     xlab="Depth of Population (%)")
lines(gains_table$Lift~gains_table$Depth, type="o", col="gold4",lwd=2, pch=15)
grid()
abline(h=1, lwd=2)
legend("topright", c("Lift", "Cumulative Lift"),lwd=2, pch=c(15,16),
       col=c("gold4", "forestgreen"), bty="n")

#------Test Data
p <- predict(logmodel5, newdata=test4, type="response")
pr <- prediction(p, test4$Bad)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve for GCS&BS", col="blue", lwd=3)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc #0.7803337
lines(x = c(0,1),y=c(0,1), lwd=3)

#Test data
sum(test$Bad)
test4$scoredProb<-predict(logmodel5, newdata=test4, type="response")
test2<-test4[order(-test4$scoredProb),]
test2$ID <- seq.int(nrow(test2))
test2$depth<-test2$ID/3997
test2$Cum_0=cumsum(test2$Bad==0)
test2$CumProp0=test2$Cum_0/sum(test2$Bad==0)
test2$Cum_1=cumsum(test2$Bad)
test2$CumProp1=test2$Cum_1/1477
plot(test2$depth, test2$CumProp1, lwd=0.1, main="KS Curve for GCS&BS", xlab="Depth", ylab="Cumulative Proportion of 1", col="blue")
lines(test2$depth, test2$CumProp0, lwd=8, col="orange")
legend('bottomright', legend=c("TPR", "FPR"),
       col=c("blue", "orange"), lty=1)
summary(test4$scoredProb)
test4$scoredProb[is.na(test4$scoredProb)]<-"O"
test4$scoredProb <- as.numeric(test4$scoredProb)
#LIFT AND GAINS FOR TEST
baserate=mean(test2$Bad)
total_No_of_defaults=sum(test2$Bad)
group=10 # you can change this group as required
Depth=seq(1: group)/group
NObs=rep(NA, group)->Cum_NObs->Mean_Response->
  Cum_Mean_Response->Lift->Cum_Lift->
  Pos_Capture_Rate->Cum_Pos_Capture_Rate
for(i in 1:length(NObs)){
  # Define start point
  if(i>1){
    start=Depth[i-1]
  }else{start=0}
  # Define end point
  end=Depth[i]
  startVal=quantile(test4$scoredProb, start)
  endVal=quantile(test4$scoredProb, end)
  if(i>1){
    startIndex=which(test4$scoredProb>startVal)
  }else{
    startIndex=which(test4$scoredProb>=0)
  }
  endIndex=which(test4$scoredProb<=endVal)
  Index=intersect(startIndex,endIndex)
  # Number of Observation
  # and cumulative number of observations
  NObs[i]=length(Index)
  Cum_NObs[i]=length(endIndex) # cumsum could be used later
}
## Work with other measures
for(i in 1:group){
  if(i==1){
    start=1
  }else{
    start=Cum_NObs[i-1]+1
  }
  end=Cum_NObs[i]
  # Mean response and cumulative
  Mean_Response[i]=mean(test2$Bad[start:end])
  Cum_Mean_Response[i]=mean(test2$Bad[1:end])
  # Lift and cumulative lift
  Lift[i]=Mean_Response[i]/baserate
  Cum_Lift[i]=Cum_Mean_Response[i]/baserate
  # Capture Rate and Cumulative Capture Rate
  Pos_Capture_Rate[i]=sum(test2$Bad[start:end])/total_No_of_defaults
  Cum_Pos_Capture_Rate[i]=sum(test2$Bad[1:end])/total_No_of_defaults
}
round3=function(x){round(x,3)}
Mean_Response=round3(Mean_Response)
Cum_Mean_Response=round3(Cum_Mean_Response)
Pos_Capture_Rate=round3(Pos_Capture_Rate)
Cum_Pos_Capture_Rate=round3(Cum_Pos_Capture_Rate)
Lift=round3(Lift)
Cum_Lift=round3(Cum_Lift)
# Make the gains table
gains_table=as.data.frame(cbind (Depth=100*Depth, NObs, Cum_NObs,
                                 Mean_Response,  Cum_Mean_Response,
                                 Pos_Capture_Rate, Cum_Pos_Capture_Rate,
                                 Lift, Cum_Lift) )




print(gains_table)
# Plottings
# Gains Plot
plot(Cum_Pos_Capture_Rate~Depth,data=gains_table,
     type="o", lwd=2, col="orange", pch=16,
     main="Cumulative Gains Plot for Test",xlim=c(0,100),
     xlab="Depth of Population (%)")
grid()

# LIFT PLOT
plot(Cum_Lift~Depth,data=gains_table,
     type="o", lwd=2, col="forestgreen", pch=16,ylab="",
     main="Lift Plot for Test",xlim=c(0,100),ylim=c(0,3),
     xlab="Depth of Population (%)")
lines(gains_table$Lift~gains_table$Depth, type="o", col="gold4",lwd=2, pch=15)
grid()
abline(h=1, lwd=2)
legend("topright", c("Lift", "Cumulative Lift"),lwd=2, pch=c(15,16),
       col=c("gold4", "forestgreen"), bty="n")



