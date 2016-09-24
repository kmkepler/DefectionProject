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

# DATES
t1<-min(subscriptions$StartDate)
t4<-max(subscriptions$StartDate) - 365
t3<-t4-365 # dependent period of 1 yr
t2<-t3-1 # operational period of 1 to 3 days

# CALC: NUMB COMPLAINTS BY CUSTOMER
complaints <- complaints[which(complaints$ComplaintDate>=t1 & complaints$ComplaintDate<=t2),]
c1<-ddply(complaints,~CustomerID,summarize,num.complaints=length(ComplaintID))

# CALC: NUM.SUBSCRIPTIONS, SUM.NBRNEWSPAPERS, SUM.TOTALDISCOUNT, SUM.TOTALPRICE, SUM.TOTALCREDIT, MAX.RENEWAL
subscriptions <- subscriptions[which(subscriptions$EndDate>=t3),]
c2<-ddply(subscriptions,~CustomerID,summarize,
          num.subscriptions=length(SubscriptionID),
          sum.newspapers=sum(NbrNewspapers),
          sum.totaldiscount=sum(TotalDiscount),
          sum.totalprice=sum(TotalPrice),
          sum.credit=sum(TotalCredit),
          num.products=length(unique(ProductID)))

# CALC: COUNT RENEWALS
# subscriptions$renewed <- ifelse(is.na(subscriptions$RenewalDate),0,1) # if renew 1, else 0
# c3<-ddply(subscriptions,~CustomerID,summarize,num.renew=sum(renewed)) # sum number renewals by customer

# CALC: COUNT CREDITS
credit <- credit[which(credit$ProcessingDate>=t1 & credit$ProcessingDate<=t2),]
subscriptions$in.credit <- ifelse(subscriptions$SubscriptionID%in%credit$SubscriptionID,1,0) # if subid in credit subid then 1, else 0
c4<-ddply(subscriptions,~CustomerID,summarize,num.credit=sum(in.credit)) # sum num credits by customer id

# CALC: NUMB PRODUCTS
# c5<-ddply(subscriptions,~CustomerID,summarize,prod.id=unique(ProductID)) # unique productID by customerID (n=1607)
# c5<-ddply(c5,~CustomerID,summarize,num.products=length(prod.id)) # count unique productID by customerID

# CALC: TIME AS CUSTOMER
c6<-ddply(subscriptions,~CustomerID,summarize,max.end=max(EndDate),max.start=max(StartDate),min.end=min(EndDate),min.start=min(StartDate))
c6$days.cust<-as.integer(c6$max.end)-as.integer(c6$min.start)

## MERGE TO GET BASETABLE
base<-merge(customers[,c(1,2,3,4)],c1,by="CustomerID",all.x=TRUE)
base<-merge(base,c2,by="CustomerID",all.x=TRUE)
#base<-merge(base,c3,by="CustomerID",all.x=TRUE)
#base$did.renew <- ifelse(base$num.renew==0,0,1) # ever renewed 1, else 0
base<-merge(base,c4,by="CustomerID",all.x=TRUE)
base$did.credit <- ifelse(base$num.credit==0,0,1) # if cust ever had a credit 1, else 0
# base<-merge(base,c5,by="CustomerID",all.x=TRUE)
base<-merge(base,c6,by="CustomerID",all.x=TRUE)
# change data types
# base$num.renew<-as.integer(base$num.renew)
# base$did.renew<-as.integer(base$did.renew)
base$num.credit<-as.integer(base$num.credit)
base$did.credit<-as.integer(base$did.credit)
# deal with NAs
# max.renewal <- base$max.renewal
# base$max.renewal <- NULL

#removing 0 subscriptions
base <- base[which(!is.na(base$num.subscriptions)),]
base[is.na(base) == TRUE] <- 0
# base$max.renewal <- max.renewal

## COMPUTE DV
# CALC: DV 1 if churn, else 2
base$DV = as.factor(ifelse(base$max.start > t4,2,1))
base$DV = as.factor(ifelse(base$max.end > t4,DV,1))

#load the package randomForest 
if (!require("randomForest")) {
  install.packages('randomForest',
                   repos="https://cran.rstudio.com/", 
                   quiet=TRUE) 
  require('randomForest')
}

#randomize order of indicators
allind <- sample(x=1:nrow(base),size=nrow(base))
#split in three parts 
trainind <- allind[1:round(length(allind)/2)]
testind <- allind[round(length(allind)*(1/2)+1):length(allind)]

BasetableTRAIN <- base[trainind,]
BasetableTEST <- base[testind,]

yTRAIN <- BasetableTRAIN$DV
BasetableTRAIN$DV <- NULL

yTEST <- BasetableTEST$DV
BasetableTEST$DV <- NULL


rFmodel <- randomForest(x=(BasetableTRAIN),
                        y=yTRAIN,  
                        ntree=350)

predrF <- predict(rFmodel,BasetableTEST,type="prob")[,2]
#assess final performance
AUC::auc(roc(predrF,yTEST))

