# CHURN PROJECT BZAN 554
# Dey, Tapajit
# Gray, Erin
# Kepler, Kieran
# Richters, Ana

# CLEAR MEMORY
rm(list=ls())

# FUNCTION TO CREATE BASETABLE
create_BT <- function(t1, t2, t3, t4){  

# INSTALL PACKAGES
if (!require("plyr")) install.packages('plyr'); library('plyr')

# SET DATE FORMAT
f <- "%d/%m/%Y";setClass("fDate");setAs(from="character",to="fDate",def=function(from) as.Date(from,format=f))

# LOADS DATA
customers<-read.csv("http://ballings.co/hidden/aCRM/data/chapter6/customers.txt",sep=";",header=TRUE,colClasses=c("character","factor","fDate","factor","factor","character"))
formula<-read.csv("http://ballings.co/hidden/aCRM/data/chapter6/formula.txt",sep=";",header=TRUE,colClasses=c("character","factor","factor","numeric"))
subscriptions<-read.csv("http://ballings.co/hidden/aCRM/data/chapter6/subscriptions.txt",sep=";",header=TRUE,colClasses=c("character","character","factor","factor","fDate","fDate","integer","integer","fDate","factor","factor","fDate","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
delivery<-read.csv("http://ballings.co/hidden/aCRM/data/chapter6/delivery.txt",sep=";",header=TRUE,colClasses=c("character","character","factor","factor","factor","fDate","fDate"))
complaints<-read.csv("http://ballings.co/hidden/aCRM/data/chapter6/complaints.txt",sep=";",header=TRUE,colClasses=c("character","character","factor","fDate","factor","factor","factor"))
credit<-read.csv("http://ballings.co/hidden/aCRM/data/chapter6/credit.txt",sep=";",header=TRUE,colClasses=c("character","character","factor","fDate","factor","numeric","integer"))

# CALC: NUMB COMPLAINTS BY CUSTOMER
complaints <- complaints[which(complaints$ComplaintDate>=t1 & complaints$ComplaintDate<=t2),]
c1<-ddply(complaints,~CustomerID,summarize,num.complaints=length(ComplaintID))

# CALC: NUM.SUBSCRIPTIONS, SUM.NBRNEWSPAPERS, SUM.TOTALDISCOUNT, SUM.TOTALPRICE, SUM.TOTALCREDIT, MAX.RENEWAL
c2 <- subscriptions
c2 <- c2[which(c2$EndDate<=t3),]
c2 <- c2[which(complete.cases(c2)),]
c2<-ddply(c2,~CustomerID,summarize,
          num.subscriptions=length(unique(ProductID)),
          sum.newspapers=sum(NbrNewspapers),
          sum.totaldiscount=sum(TotalDiscount),
          sum.totalprice=sum(TotalPrice),
          sum.credit=sum(TotalCredit),
          num.products=length(unique(ProductID)))

# CALC: COUNT CREDITS
credit <- credit[which(credit$ProcessingDate>=t1 & credit$ProcessingDate<=t2),]
c4 <- subscriptions
c4 <- c4[which(c4$EndDate<=t3),]
c4 <- c4[which(complete.cases(c4)),]
c4$in.credit <- ifelse(c4$SubscriptionID%in%credit$SubscriptionID,1,0) # if subid in credit subid then 1, else 0
c4<-ddply(c4,~CustomerID,summarize,num.credit=sum(in.credit)) # sum num credits by customer id

# CALC: TIME AS CUSTOMER
subscriptions <- subscriptions[which(subscriptions$EndDate>=t3),]
c6<-ddply(subscriptions,~CustomerID,summarize,max.end=max(EndDate),max.start=max(StartDate),min.end=min(EndDate),min.start=min(StartDate))
c6$days.cust<-as.integer(c6$max.end)-as.integer(c6$min.start)

## MERGE TO GET BASETABLE
base<-merge(customers[,c(1,2,3,4)],c1,by="CustomerID",all.x=TRUE)
base$num.complaints[is.na(base$num.complaints)] <- 0
base<-merge(base,c2,by="CustomerID")
base<-merge(base,c4,by="CustomerID",all.x=TRUE)
base$did.credit <- ifelse(base$num.credit==0,0,1) # if cust ever had a credit 1, else 0
base<-merge(base,c6,by="CustomerID",all.x=TRUE)
# change data types
base$num.credit<-as.integer(base$num.credit)
base$did.credit<-as.integer(base$did.credit)
# deal with NAs
base <- base[which(!is.na(base$num.subscriptions)),]
base <- base[complete.cases(base),]

# RETURN BASETABLE
return (base)
}

# FUNCTION TO BUILD MODE
model.build <- function(start_ind, end_ind, start_dep, end_dep,  evaluate = T){
  
  # TIMELINE
  t1 <- as.Date(start_ind)
  t4 <- as.Date(end_dep)
  t3 <- as.Date(start_dep)
  t2 <- as.Date(end_ind)

# CALL BASETABLE FUNCTION
base <- create_BT(t1, t2, t3, t4)
length_ind <- t2 - t1
length_op <- t3 - t2
length_dep <- t4 - t3

## COMPUTE DV (1 if churn, else 2)
base$DV = as.factor(ifelse(base$max.start > t4,2,1))
base$DV = as.factor(ifelse(base$max.end > t4,base$DV,1))

# INSTALL PACKAGES
if (!require("randomForest")) {
  install.packages('randomForest',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('randomForest')
}

if(evaluate){
if (!require("AUC")) {
  install.packages('AUC',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('AUC')
}
if (!require("lift")) {
  install.packages('lift',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('lift')
}}

# RANDOMIZE INDICATORS
ind <- 1:nrow(base)
indTRAIN <- sample(ind,round(0.5*length(ind)))
indTEST <- ind[-indTRAIN]

# ISOLATE DV
DV <-base$DV
  
# DELETE COLUMNS
base$DV <- NULL
base$max.end <- NULL
base$max.start <- NULL
base$min.end <- NULL
base$days.cust <- NULL

# RUN MODEL
rFmodel <- randomForest(x=(base[indTRAIN,]),
                        y=DV[indTRAIN],  
                        ntree=1000)

# RUN IF EVALUATE=TRUE
if (evaluate){
predrF <- predict(rFmodel,base[indTEST,],type="prob")[,2]
cat("AUC of the model:", AUC::auc(roc(predrF,DV[indTEST])))
cat("\nTop Decile Lift:", TopDecileLift(predrF,DV[indTEST]))
varImpPlot(rFmodel)
}

# RETURN MODEL, LENGTH OF INDEPENDENT, OPERATIONAL, DEPENDENT
return (list(rFmodel, length_ind, length_op, length_dep))
}

# FUNCTION TO DEPLOY MODEL
model.predict <- function(object, dumpDate){
  
  # TIMELINE
  t2 <- as.Date(dumpDate)
  t1 <- t2 - object[[2]]
  t3 <- t2 + object[[3]]
  t4 <- t3 + object[[4]]
  
  # SAVE MODEL
  rFmodel <- object[[1]]
  
  # CREATE BASETABLE
  predbase <- create_BT(t1, t2, t3, t4)
  predbase$max.end <- NULL
  predbase$max.start <- NULL
  predbase$min.end <- NULL
  predbase$days.cust <- NULL
  
  # RUN PREDICTIONS
  predrF <- predict(rFmodel, predbase, type="prob")[,2]
  
  # STORE OUTPUT (CUSTOMERID AND SCORE)
  ans <- data.frame("Customer_Id" = predbase$CustomerID, "Score" = predrF)
  ans <- ans[order(ans$Score, decreasing=TRUE),]
  
  # RETURN ANSWER
  ans  
}

# CALL MODEL.BUILD
Cmodel <- model.build("2006-01-02","2009-02-23","2009-02-24","2010-02-24",evaluate = TRUE)

# CALL MODEL.PREDICT
pred <- model.predict(object=Cmodel, dumpDate="2010-01-01")


