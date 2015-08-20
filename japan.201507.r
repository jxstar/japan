library(quantmod)
library(leaps)
library(Hmisc) #describe
library(psych) #describe
library(GPArotation)
library(pastecs) #stat.desc
library(corrgram) # for corralation analysis
library(gvlma)
library(car)
library(relaimpo)

library(xlsx)
library(RSQLite)
library(RMySQL)

library(ggplot2) #add for ggplot
library(reshape2)
library(dplyr)


##deal with CSV file
##1. delete the "," in number
##2. change the date to 
par(mfrow=c(1,1))
mycolor=rainbow(20)

#============================================================
## Global constant
#============================================================
target.time=as.POSIXlt("2015-07-01")
target.mon=target.time$mon+1
target.year=target.time$year+1900
(gcol=paste("m", 1:target.mon, sep=""))
(tm=paste("m",target.mon,sep=""))
(um=paste("m",target.mon-1,sep=""))
gmethod=c("Direct LR","1st Order Diff", "Y2Y Diff12 LR","Y2Y Diff12 1st Diff",
          "M2M 1st Order Diff LR","1st Order Diff Mean","M2M 2nd Order Diff","Baidu Index LR")
gstatus=c("Perfect", "Good", "Conservative","Normal", "Poor", "Singular")
gadopted=c("Y", "N")

resplot=function (usedata,predict.result){
  usedata[which(usedata$year==target.year),tm]=predict.result
  msjp=melt(usedata[,c("year",tm,um)],id="year")
  p <- ggplot(msjp, aes(year, value))
  p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
    labs(title=paste(gpicname,"Result",sep=" : "),x="year", y="Tourist Number")
  print(p1)
}


#=================================================================================================
#1# read the data
#=================================================================================================

japan=read.xlsx("japan.tourists.number.xlsx",1)
japan$arrival1 = c(NA, japan$arrival[1:(nrow(japan)-1)]);
japan$arrival12 = c(rep(NA, 12), japan$arrival[1:(nrow(japan)-12)]);
#define the one stage differentcial
japan$diff=japan$arrival-japan$arrival1
japan$diff12=japan$arrival-japan$arrival12
japan$diff2nd=japan$diff-c(NA, japan$diff[1:(nrow(japan)-1)])
japan$mm=japan$arrival/japan$arrival1
japan$yy=japan$arrival/japan$arrival12
japan$year=as.integer(format(as.Date(japan$date),"%Y"))
japan$month=as.integer(format(as.Date(japan$date),"%m"))
jp=japan 
jp=filter(jp,date<=as.Date(target.time))


#=================================================================================================
#2# analysis the datap <- ggplot(jp, aes(month, arrival))
#=================================================================================================
#2.1 overall prosect---------------------------------------------------------

p <- ggplot(jp, aes(month, arrival))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6) + 
  geom_point(aes(colour = factor(year)))+labs(title="JP History Overview Groupby Year")
print(p1)

p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)+
  labs(title="JP History Tourist Number Overview")
print(p2)


#=================================================================================================
#3 Y2Y Diff12 M6 to Predict M7
#combination
#=================================================================================================

#===========================================================
# Check if we can use the Y2Y method to predict

p<- ggplot(filter(jp,year %in% c(2008:target.year),month %in% c(5:target.mon)), aes(year, arrival))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="JP History Tourist Number Groupby Month")
print(p1)

p<- ggplot(filter(jp,year %in% c(2008:target.year),month %in% c(5:target.mon)), aes(year, diff12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="JP Y2Y Diff Groupby Month")
print(p1)

p<- ggplot(filter(jp,year %in% c(2008:target.year),month %in% c(5:target.mon)), aes(year, diff12/arrival12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="JP Y2Y Diff/arrival12 Groupby Month")
print(p1)


#===========================================================
# prepare the data ready to use
#2003 year is quite special
cmbjp=select(filter(jp,year %in% c(2008:target.year),month %in% c(1:target.mon)),year,month,arrival)
meltjp=melt(cmbjp,id=c("year","month"))

#cast the data as year id and produce m1a2,m34
dmjp=dcast(meltjp,year~month+variable)
names(dmjp)=c("year",gcol)
for (i in 2:(target.mon-1)){
  if (i<=3) {
    dmjp$m1a2=(dmjp$m1+dmjp$m2)/2
    next
  }
  dmjp[,paste("m", (i-1), i, sep="")]=apply(dmjp[, gcol[c((i-1), i)]], 1, mean, na.rm=T)
}

#calculate the Y2Y percentage
dmpjp=dmjp
dmpjp[2:nrow(dmpjp),2:ncol(dmpjp)]=dmjp[2:nrow(dmjp),2:ncol(dmjp)]-dmjp[1:nrow(dmjp)-1,2:ncol(dmjp)]
dmpjp[1,2:ncol(dmpjp)]=NA
dmpjp

#===========================================================
#the method we select
#[*]3.1 direct LR : M6-->M7
#[*]3.2 1st order diff: M2015.7-M2014.7=M2015.6-M2014.6
#[ ]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7)
#[ ]3.4 Diff12 1st order diff(2nd order diff):Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2015.6)


#-----------------------------------------------------------
#[*]3.1 direct LR : M6-->M7: m6 is the best: 0.9928283   M56:0.9975592
(hcor.LR=cor(dmjp[,which(names(dmjp)==gcol[target.mon])],dmjp[,-c(1,which(names(dmjp)==tm))],use="na.or.complete"))
#[ ]3.3 Diff12 LR: Diff12(M56)-->Diff12(M7): 0.9949555
(hcor.Diff=cor(dmpjp[-nrow(dmpjp),which(names(dmpjp)==gcol[target.mon])],dmpjp[-nrow(dmpjp),-1],use="na.or.complete"))
(hcor.LR-hcor.Diff)
knitr::kable(as.data.frame(hcor.LR),caption="Y2Y M7 Coefficient Correlation")
knitr::kable(as.data.frame(hcor.Diff),caption="Y2Y Diff12（M7）Coefficient Correlation")
#vcor no year is quite correlated
(vcor=cor(t(dmpjp[-which(names(dmpjp)==gcol[target.mon])]),t(dmpjp[,-which(names(dmpjp)==gcol[target.mon])])))


#===========================================================
#[*]3.1 direct LR : M6-->M7
#[ ]3.2 1st order diff: M2015.7-M2014.7=M2015.6-M2014.6
#[ ]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7)
#[ ]3.4 Diff12 1st order diff(2nd order diff):Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2015.6)
methodology=gmethod[1]
(usedata=dmjp)
(tm)
(um="m56")
(useformula=as.formula(paste(tm, "~", um, sep="")))
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))

ufit=lm(useformula,data=na.omit(usedata))
#ufit=lm(m7~I(m56^2)+m56,data=na.omit(usedata))
(smfit=summary(ufit))
(predict.result=predict(ufit,newdata=usedata[which(usedata$year==target.year),]))
usedata[which(usedata$year==target.year),tm]=predict(ufit,newdata=usedata[which(usedata$year==target.year),])
usedata$prediction=predict(ufit,newdata=usedata)
usedata$error=(usedata$prediction-usedata[,tm])/usedata[,tm]
usedata

msjp=melt(usedata[,c(tm,um,"prediction")],id=um)
p <- ggplot(msjp, aes(msjp[,um], value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x=um, y=tm)
print(p1)

msjp=melt(usedata[,c("year",tm,um,"prediction")],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x="year", y="Tourist Number")
print(p1)

(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ":LR ERROR",sep=" "), x="YEAR", y="ERROR"))

er=range((usedata[,tm]-usedata$prediction),na.rm = T)
benchmark=data.frame(date=as.Date(target.time), id="1.1",methdology=methodology,target=tm, dependent=um,
                     floor=predict.result+er[1], cap=predict.result+er[2],bestguess=predict.result,
                     correlation=max(hcor.LR,na.rm = T), adjR2=smfit$adj.r.squared,variance=NA,maxError=max(abs(usedata$error),na.rm=T),
                     status="Singular", adopted="Y",stringsAsFactors = F)
(benchmark)
#knitr::kable(benchmark,caption="月度直接线性回归预测")



#===========================================================
#[ ]3.1 direct LR : M6-->M7
#[*]3.2 1st order diff: M2015.7-M2014.7=M2015.6-M2014.6
#[ ]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7)
#[ ]3.4 Diff12 1st order diff(2nd order diff):Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2015.6)

(methodology=gmethod[2])
(usedata=dmjp)
(tm)
(um="m6")
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))

#sjp.data=(dmjp[,-1]-dmjp[,which(names(dmpjp)==tm)])/dmjp[,which(names(dmpjp)==tm)]
sjp.data=(dmjp[,-1]-dmjp[,which(names(dmjp)==tm)])/dmjp[,which(names(dmjp)==tm)]

sigma=apply(sjp.data,2,function (x) {return(var(x,na.rm=T))})
(sigma=sigma[order(sigma)])
(p <- ggplot(data=melt(as.data.frame(t(sigma)),id=tm),aes(x=factor(variable),weight=value)) + 
  geom_bar(position='dodge',fill="grey")+labs(title=paste(gpicname," : (Mx-M7) variance",sep=" ")))
(um="m6")

# m6: M6 Diff 12
#mean(m7)(without2015)-mean(m6)(without2015)=m7(2015)-m6(2015)
#m7(2015)=mean(m7)(without2015)-mean(m6)(without2015)+m6(2015)
#predict.result=mean(usedata[-which(usedata$year==target.year),tm]-
#                    usedata[-which(usedata$year==target.year),um])+usedata[which(usedata$year==target.year),um]
#m7(2015)-m7(2014)=m6(2015)-m6(2014)
#m7(2015)=m7(2014)-m6(2014)+m6(2015)
(predict.result=usedata[which(usedata$year==(target.year-1)),tm]-
  usedata[which(usedata$year==(target.year-1)),um]+usedata[which(usedata$year==target.year),um])

prange=range(usedata[-which(usedata$year==(target.year)),tm]-
               usedata[-which(usedata$year==(target.year)),um]+usedata[which(usedata$year==target.year),um],na.rm = T)

usedata$prediction=c(NA,usedata[-which(usedata$year==(target.year)),tm])-
  c(NA,usedata[-which(usedata$year==(target.year)),um])+usedata[,um]
usedata$error=(usedata$prediction-usedata[,tm])/usedata[,tm]
(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ": ERROR",sep=" "), x="YEAR", y="ERROR"))


benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="1.2",methdology=methodology,target=tm, dependent=um,
             floor=prange[1], cap=prange[2], bestguess=predict.result,
             correlation=NA, adjR2=NA,variance=min(sigma[-1],na.rm = T),maxError=max(abs(usedata$error),na.rm=T),
             status="Perfect", adopted="Y",stringsAsFactors = F)
(benchmark)
#knitr::kable(benchmark,caption="月度一阶差分")
resplot(dmjp,predict.result)



#===========================================================
#[ ]3.1 direct LR : M6-->M7
#[ ]3.2 1st order diff: M2015.7-M2014.7=M2015.6-M2014.6
#[*]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7)
#[ ]3.4 Diff12 1st order diff(2nd order diff):Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2015.6)

(methodology=gmethod[3])
(usedata=dmpjp)
(tm)
(um="m56")
(useformula=as.formula(paste(tm, "~", um, sep="")))
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))
(hcor.Diff=cor(dmpjp[,which(names(dmpjp)==gcol[target.mon])],dmpjp[,-c(1, which(names(dmpjp)==tm))],use="na.or.complete"))
#select M56

ufit=lm(useformula,data=na.omit(usedata))
(smfit=summary(ufit))
(predict.result=predict(ufit,newdata=usedata[which(usedata$year==target.year),])+dmjp[which(usedata$year==(target.year-1)),tm])
usedata[which(usedata$year==target.year),tm]=predict(ufit,newdata=usedata[which(usedata$year==target.year),])
usedata$prediction=predict(ufit,newdata=usedata)
usedata$error=(usedata$prediction-usedata[,tm])/dmjp[,tm]
#usedata

msjp=melt(usedata[,c(tm,um,"prediction")],id=um)
p <- ggplot(msjp, aes(msjp[,um], value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x=um, y=tm)
print(p1)

msjp=melt(usedata[,c("year",tm,um,"prediction")],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x="year", y="Y2Y Diff12")
print(p1)

(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ":LR ERROR",sep=" "), x="YEAR", y="ERROR"))

er=range((usedata[,tm]-usedata$prediction),na.rm = T)
benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="2.1",methdology=methodology, target=paste(tm,"(2015-2014)",sep = ""), dependent=um,
             floor=predict.result+er[1],cap=predict.result+er[2], bestguess=predict.result,
             correlation=max(hcor.Diff,na.rm = T), adjR2=smfit$adj.r.squared,variance=NA,maxError=max(abs(usedata$error),na.rm=T),
             status="Singular", adopted="Y",stringsAsFactors = F)
(benchmark)
knitr::kable(benchmark,caption="Y2Y Diff12 线性回归预测")
resplot(dmjp,predict.result)




#===========================================================
#[ ]3.1 direct LR : M6-->M7
#[ ]3.2 1st order diff: M2015.7-M2014.7=M2015.6-M2014.6
#[ ]3.3 Diff12 LR: Diff12(M6)-->Diff12(M7)
#[*]3.4 Diff12 1st order diff(2nd order diff):Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2015.6)
#Diff12(2015.7)-Diff12(2014.7)=Diff12(2015.6)-Diff12(2014.6)
(methodology=gmethod[4])
(usedata=dmpjp)
(tm)
(um="m56")
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))

sjp.data=(dmpjp[,-1]-dmpjp[,which(names(dmpjp)==tm)])/dmpjp[,which(names(dmpjp)==tm)]
sigma=apply(sjp.data,2,function (x) {return(var(x,na.rm=T))})
(sigma=sigma[order(sigma)])
(p <- ggplot(data=melt(as.data.frame(t(sigma)),id=tm),aes(x=factor(variable),weight=value)) + 
  geom_bar(position='dodge',fill="grey")+labs(title=paste(gpicname," : (Diff12 Mx- Diff12 M7) variance",sep=" ")))

# m6: M6 Diff 12
#mean(m7)(without2015)-mean(m6)(without2015)=m7(2015)-m6(2015)
#m7(2015)=mean(m7)(without2015)-mean(m6)(without2015)+m6(2015)
#predict.result=mean(usedata[-which(usedata$year==target.year),tm]-
#                    usedata[-which(usedata$year==target.year),um])+usedata[which(usedata$year==target.year),um]
usedata
predict.result=usedata[which(usedata$year==(target.year-1)),tm]-usedata[which(usedata$year==(target.year-1)),um]+
  usedata[which(usedata$year==target.year),um]+dmjp[which(usedata$year==(target.year-1)),tm]

prange=range(usedata[-which(usedata$year==(target.year)),tm]-usedata[-which(usedata$year==(target.year)),um]+
               usedata[which(usedata$year==target.year),um],na.rm=T)+dmjp[which(usedata$year==(target.year-1)),tm]

#Diff12(2015.7)=Diff12(2014.7)-Diff12(2014.6)+Diff12(2015.6)

usedata$prediction=c(NA,usedata[-which(usedata$year==(target.year)),tm])-
  c(NA,usedata[-which(usedata$year==(target.year)),um])+usedata[,um]
usedata$error=(usedata$prediction-usedata[,tm])/dmjp[,tm]
(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ": ERROR",sep=" "), x="YEAR", y="ERROR"))

benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="2.2",methdology=methodology, target=paste(tm,"(2015-2014)",sep = ""), dependent=um,
             floor=prange[1], cap=prange[2], bestguess=predict.result,
             correlation=NA, adjR2=NA,variance=min(sigma[-1],na.rm = T),maxError=max(abs(usedata$error),na.rm=T),
             status="Normal", adopted="Y",stringsAsFactors = F)
(benchmark)

knitr::kable(benchmark,caption="基于Y2Y增长绝对数值Diff12一阶差分")

usedata[which(usedata$year==target.year),tm]=usedata[which(usedata$year==(target.year-1)),tm]-usedata[which(usedata$year==(target.year-1)),um]+
  usedata[which(usedata$year==target.year),um]
msjp=melt(usedata[,c("year",tm,um)],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x="year", y="Y2Y Diff12")
print(p1)

resplot(dmjp,predict.result)














#=================================================================================================
#4 M2M Diff1 M6 to Predict M7
#combination
#=================================================================================================

#===========================================================
# Overview 
p<- ggplot(filter(jp,year %in% c(2012:target.year),month %in% c(4:target.mon)), aes(month, arrival))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="jp Number Groupby year")
print(p1)

p<- ggplot(filter(jp,year %in% c(2008:target.year),month %in% c(4:target.mon)), aes(month, diff))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="jp Diff1 Groupby Year")
print(p1)

p<- ggplot(filter(jp,year %in% c(2008:target.year),month %in% c(4:target.mon)), aes(year, diff))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="jp Diff1 Groupby Month")
print(p1)

#===========================================================
# the method we select:  Diff1
#[ ]4.1 Diff1 LR : Mx-->D7
#[ ]4.2 1st order diff: m2015.7-M2014.7=M2015.6-M2014.6
#[*]4.3 2nd order diff: D2015.7-D2014.7=D2015.6-D2014.6


#===========================================================
# Prepare the data
diff1=select(filter(jp,year %in% c(2008:target.year),month %in% c(1:target.mon)),year,month,diff)
diff1=melt(diff1,id=c("year","month"))
diff1=dcast(diff1,year~month+variable)
names(diff1)=c("year",gcol)




#===========================================================
#[*]4.1 Diff1 LR : Mx-->D7
#[ ]4.2 1st order diff: m2015.7-M2014.7=M2015.6-M2014.6
#[ ]4.3 2nd order diff: D2015.7-D2014.7=D2015.6-D2014.6
# First Order with Linear Regression : M7=approximated diff1 + M6
# approximated diff1(M7-M6)=LR(M6)

methodology=gmethod[5]
(tm)
(um="m5")
(useformula=as.formula(paste(tm, "~", um, sep="")))
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))

#(hcor=cor(diff1[,which(names(diff1)==tm)],
#          cbind(diff1[,-c(1,which(names(diff1)==tm))],dmjp[,which(names(dmjp)==gcol[target.mon-1])]),use="na.or.complete"))
(hcor.diff=cor(diff1[,which(names(diff1)==tm)],diff1[,-c(1,which(names(diff1)==tm))],use="na.or.complete"))
(hcor.dmjp=cor(diff1[,which(names(diff1)==tm)],dmjp[,-c(1,which(names(dmjp)==tm))],use="na.or.complete"))

#The cor is bad betweent diffs and the tourist numbers
knitr::kable(as.data.frame(hcor.diff),caption="基于M2M月度一阶差分的线性回归预测")
knitr::kable(as.data.frame(hcor.dmjp),caption="基于M2M月度一阶差分的线性回归预测")

# m6 tourist number, m7(m7-m6)
usedata=as.data.frame(cbind(diff1[,c("year",tm)], dmjp[,um]))
names(usedata)=c("year",tm, um)

ufit=lm(useformula,data=na.exclude(usedata))
(smfit=summary(ufit))
(predict.result=predict(ufit,newdata=usedata[which(usedata$year==target.year),])+usedata[which(usedata$year==(target.year)),um])
usedata[which(usedata$year==target.year),tm]=predict(ufit,newdata=usedata[which(usedata$year==target.year),])
usedata$prediction=predict(ufit,newdata=usedata)
usedata$error=(usedata$prediction-usedata[,tm])/dmjp[,tm]
#usedata

msjp=melt(usedata[,c(tm,um,"prediction")],id=um)
p <- ggplot(msjp, aes(msjp[,um], value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x=um, y=tm)
print(p1)

msjp=melt(usedata[,c("year",tm,um,"prediction")],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x="year", y="M2M Diff1")
print(p1)

msjp=melt(usedata[,c("year",tm,"prediction")],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title=gpicname,x="year", y="M2M Diff1")
print(p1)

(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ":LR ERROR",sep=" "), x="YEAR", y="ERROR"))

er=range((usedata[,tm]-usedata$prediction),na.rm = T)
benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="3.1",methdology=methodology, target=paste(tm,um,sep = "-"), dependent=um,
             floor=predict.result+er[1],cap=predict.result+er[2], bestguess=predict.result,
             correlation=max(hcor.dmjp,na.rm = T), adjR2=smfit$adj.r.squared,variance=NA,maxError=max(abs(usedata$error),na.rm=T),
             status="Normal", adopted="Y",stringsAsFactors = F)
(benchmark)
knitr::kable(benchmark,caption="基于M2M月度一阶差分的线性回归预测")
resplot(dmjp,predict.result)





#===========================================================
#[ ]4.1 Diff1 LR : Mx-->D7
#[*]4.2 1st order diff: m2015.7-M2014.7=M2015.6-M2014.6
#[ ]4.3 2nd order diff: D2015.7-D2014.7=D2015.6-D2014.6
# First Order without Linear Regression : M7(2015)-M6(2015)=M7(2014)-M6(2014)
#mean(m7)(without2015)-mean(m6)(without2015)=m7(2015)-m6(2015)
#m7(2015)=mean(m7)(without2015)-mean(m6)(without2015)+m6(2015)
(methodology=gmethod[6])
(usedata=diff1)
(tm)
(um="m6")
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))

#mean(m7)(without2015)-mean(m6)(without2015)=m7(2015)-m6(2015)
#m7(2015)=mean(m7)(without2015)-mean(m6)(without2015)+m6(2015)
predict.result=mean(usedata[-which(usedata$year==target.year),tm],na.rm=T)+
  dmjp[which(dmjp$year==target.year),um]
prange=range(usedata[,tm]-mean(usedata[-which(usedata$year==target.year),tm],na.rm=T),na.rm=T)+predict.result

usedata$prediction=mean(usedata[-which(usedata$year==target.year),tm],na.rm=T)
usedata$error=(usedata$prediction-usedata[,tm])/dmjp[,tm]
(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ": ERROR",sep=" "), x="YEAR", y="ERROR"))

benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="3.2",methdology=methodology,target=paste(tm,um,sep = "-"), dependent="mean",
             floor=prange[1], cap=prange[2], bestguess=predict.result,
             correlation=NA, adjR2=NA,variance=NA,maxError=max(abs(usedata$error),na.rm=T),
             status="Conservative", adopted="Y",stringsAsFactors = F)
(benchmark)
knitr::kable(benchmark,caption="基于M2M月度一阶差分的保守预测")
resplot(dmjp,predict.result)








#===========================================================
#[ ]4.1 Diff1 LR : Mx-->D7
#[ ]4.2 1st order diff: m2015.7-M2014.7=M2015.6-M2014.6
#[*]4.3 2nd order diff: D2015.7-D2014.7=D2015.6-D2014.6
#=================================================================================
# Second Order without Linear Regression : Diff M7(2015) - Diff M6(2015)=Diff M7(2014) - Diff M6(2014)
# D7(2015)=D6(2015)+D7(2014)-D6(2014)

diff2nd=select(filter(jp,year %in% c(2008:target.year),month %in% c(1:target.mon)),year,month,diff2nd)
diff2nd=melt(diff2nd,id=c("year","month"))
diff2nd=dcast(diff2nd,year~month+variable)
names(diff2nd)=c("year",gcol)

(methodology=gmethod[7])
(tm)
(um="m6")
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", um, "-->", tm, sep=" "))
(usedata=diff2nd)

#mean(m7)(without2015)-mean(m6)(without2015)=m7(2015)-m6(2015)
#D7(2015)=mean(D7)(without2015)-mean(D6)(without2015)+D6(2015)

if (T) {
  predict.result=usedata[which(usedata$year==(target.year-1)),tm]+
    diff1[which(dmjp$year==target.year),um]+dmjp[which(dmjp$year==target.year),um]
  prange=range(usedata[,tm]-c(NA,usedata[-which(usedata$year==target.year),tm]),na.rm=T)+predict.result
  usedata$prediction=c(NA,usedata[-which(usedata$year==target.year),tm])
} else {
  predict.result=mean(usedata[-which(usedata$year==target.year),tm],na.rm=T)+
    diff1[which(dmjp$year==target.year),um]+dmjp[which(dmjp$year==target.year),um]
  prange=range(usedata[,tm]-mean(usedata[-which(usedata$year==target.year),tm],na.rm=T),na.rm=T)+predict.result
  usedata$prediction=mean(usedata[-which(usedata$year==target.year),tm],na.rm=T)
}
usedata$error=(usedata$prediction-usedata[,tm])/dmjp[,tm]
(p <- ggplot(data=usedata,aes(x=factor(year),weight=error,fill=factor(year))) + 
  geom_bar(position='dodge')+labs(title=paste(gpicname, ": ERROR",sep=" "), x="YEAR", y="ERROR"))

benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="3.3",methdology=methodology,target=paste("delta","(",paste(tm,um,sep = "-"),")",sep=""), dependent=paste(tm,um,sep = "-"),
             floor=prange[1], cap=prange[2], bestguess=predict.result,
             correlation=NA, adjR2=NA,variance=NA,maxError=max(abs(usedata$error),na.rm=T),
             status="Singular", adopted="Y",stringsAsFactors = F)
(benchmark)
knitr::kable(benchmark,caption="基于M2M月度二阶差分预测")
resplot(dmjp,predict.result)











#=================================================================================================
#6 predict with linear regression method
(methodology=gmethod[8])
(gpicname=paste("[",which(gmethod==methodology),"]",methodology, ":", "Baidu", "-->", tm, sep=" "))


#5.1 read the data of baidu index----------------------------------------
hotword=read.xlsx("japan-hotword.xlsx",1)
hotword=hotword[order(hotword$date),]
hwname=c("date","qz","ly","y","tq","jd","ryfy","hl","lyqz","jg","dt","lygl","yh","zyx","gwgl","gw","lyjd",
         "ctjc","djdt","csly","cslygl","dj","bhd","db","jingdu","mgw","fg","zhahuang","nailiang","hengbin","fss","chongsheng","xianggen","segu")
names(hotword)=hwname

#turn off the data to numeric
for (i in 2:ncol(hotword)){
  for (j in 1:nrow(hotword)){
    if (hotword[j,i]<(mean(hotword[,i])/100)){
      cat(i,j,hotword[j,i],mean(hotword[,i]),hotword[j-1,i],"\n")
      hotword[j,i]=hotword[j-1,i]
    }
  }
  #cat(mean(index[,i]))
}

hotword$sp=rowSums(hotword[,which(names(hotword)=="ctjc"):which(names(hotword)=="segu")])
hotword$sumall=rowSums(hotword[,c("lyqz","ly","jd","jg","hl","dt","lygl","yh","zyx","gwgl","gw","sp")])

fhw=hotword[,-(which(names(hotword)=="ctjc"):which(names(hotword)=="segu"))]
names(fhw)=c("date","qz","ly","y","tq","jd","ryfy","hl","lyqz","jg","dt","lygl","yh","zyx","gwgl","gw","lyjd","sp","sumall")
fhw=transform(fhw,month=format(date,"%Y-%m"))

fhw.melt=melt(fhw,id=c("date","month"))
fhw.gb=group_by(fhw.melt,month,variable)
fhw.month=summarize(fhw.gb,date=first(date),value=sum(value))
fhw.month=dcast(fhw.month,date+month~variable)


begindate=as.Date("2013-01-01")
enddate=as.Date(target.time)
lag=0

fhw.month=filter(fhw.month,date %in% c(begindate:enddate))
date=fhw.month$date

#5.2 hwm----------------------------------------------------------------hwm

hwm=select(fhw.month,date,month,jd,jg,gwgl,ly,lyqz,lygl,zyx,sp,sumall)
hwm.lag=select(fhw.month,jd,jg,gwgl,ly,lyqz,lygl,zyx,sp,sumall)
hwm.lag[2:(nrow(fhw.month)),]=hwm.lag[1:(nrow(fhw.month)-1),]
hwm.lag[1,]=NA
names(hwm.lag)=c("jd1","jg1","gwgl1","ly1","lyqz1","lygl1","zyx1","sp1","sumall1")
#hwm=transform(hwm,ly1=c(NA,fhw.month[1:nrow(fhw.month)-1,"ly"]),lyqz1=c(NA,fhw.month[1:nrow(fhw.month)-1,"lyqz"]),
#              lygl1=c(NA,fhw.month[1:nrow(fhw.month)-1,"lygl"]),zyx1=c(NA,fhw.month[1:nrow(fhw.month)-1,"zyx"]))
hwm=cbind(hwm,hwm.lag)

hwm=transform(hwm,
              arrival=jp[which(jp$date==first(date)):which(jp$date==last(date)),"arrival"],
              arrival1=jp[which(jp$date==first(date)):which(jp$date==last(date)),"arrival1"],
              arrival12=jp[which(jp$date==first(date)):which(jp$date==last(date)),"arrival12"],
              korea=jp[which(jp$date==first(date)):which(jp$date==last(date)),"korea"],
              holiday=jp[which(jp$date==first(date)):which(jp$date==last(date)),"holiday"],
              nbmonth=jp[which(jp$date==first(date)):which(jp$date==last(date)),"nbmonth"],
              newyear=jp[which(jp$date==first(date)):which(jp$date==last(date)),"newyear"])
hwm$mars=0
#201506-201507
mars.mon=c(as.Date("20150601",format="%Y%m%d"),as.Date("20150701",format="%Y%m%d"))
hwm[which(hwm$date %in% mars.mon),"mars"]=1

              

#5.3 hwm.t--------------------------------------------------------------hwm.t
#For Linear regression, we should determine some really important assumption 
#1st: the time time range, 2014 tillnow,or 2015 tillnow, or the specific month
#2nd: the variables, e.g. lygl1 zyx1  jg newyear

predict.no=1
usefuldate=hwm[which(hwm$date==as.Date("2014-01-01")):which(hwm$date==as.Date("2015-07-01")),"date"]
hwm.p=hwm[which(hwm$date==as.Date("2015-07-01")):which(hwm$date==as.Date("2015-07-01")),]

# choose the best time interval  for  linear regression
location=c(which(names(hwm)=="jd"):which(names(hwm)=="korea"))
time.all=c(which(hwm$date==as.Date("2014-01-01")):which(hwm$date==as.Date("2015-06-01")))
time.2014=c(which(hwm$date==as.Date("2014-01-01")):which(hwm$date==as.Date("2014-06-01")))
time.2015=c(which(hwm$date==as.Date("2015-01-01")):which(hwm$date==as.Date("2015-06-01")))
time.interval=c(time.2014,time.2015)
time.range=time.interval
cor.interval=as.data.frame(cor(hwm[time.interval,"arrival"],hwm[time.interval,location],use="na.or.complete"))
cor.interval[,order(-cor.interval)]
cor.interval=as.data.frame(cor(hwm[time.all,"arrival"],hwm[time.all,location],use="na.or.complete"))
cor.interval[,order(-cor.interval)]


#interval: for 201506 we use 2014 2015 1-5month
#---------------------------------------------------------------------------
#************determine the interval ****************************************
#---------------------------------------------------------------------------
time.range=time.all
#time.range=time.all
cat("The Date We Use in this algorithm\n", as.character(hwm[time.range,"date"]),"\n")
hwm.t=hwm[time.range,]


cat("The correlation of the hotwords we may select:\n")
hwm.cor=as.data.frame(cor(hwm.t$arrival,hwm.t[,location]),use="na.or.complete")
hwm.cor[order(-hwm.cor)]
names(hwm.t[,location])[order(-hwm.cor)]
corrgram(hwm.t[,location],order=T,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)


#5.4 choose the best variable for linear regression
#test=select(hwm,date,arrival,zyx1,lyqz1,lygl1,gwgl,arrival12,jg,jd1,sp,sumall,korea)
#test=select(hwm[c(time.range,max(time.range+1)),],date,arrival,zyx1,lyqz1,lygl1,gwgl,arrival12,jg,jd1,sp,sumall,korea)
names(hwm.t[,location])[order(-hwm.cor)]

test=select(hwm[c(time.range,max(time.range+1)),],date,arrival,
            lygl1,sp1,zyx1,jg1,sumall1,lyqz1,gwgl1,lygl,arrival12,ly1,jd1,jg,korea)
test=hwm[,c("date",names(hwm.t[,location])[order(-hwm.cor)])]
mean.arrival=mean(test$arrival,na.rm=T)
test[,3:ncol(test)]=apply(test[,3:ncol(test)],2,function (x){
  return(x*mean.arrival/mean(x,na.rm=T))
})

for (i in (1:(ncol(test)-2))){
  #test1=select(test,date,arrival,zyx1)
  test1=test[,c(1,2,i+2)]
  mtest=melt(test1,id="date")
  p <- ggplot(mtest, aes(date, value))
  p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
    labs(title=paste("Tourist No Vs ",names(test1)[ncol(test1)]))
  print(p1)
}



if (F){
  test.cor=data.frame()
  for (lag in 0:60){
    #lag=3
    fhw.lag=fhw
    fhw.lag[(lag+1):nrow(fhw.lag),2:(ncol(fhw.lag)-1)]=fhw.lag[1:(nrow(fhw.lag)-lag),2:(ncol(fhw.lag)-1)]
    fhw.lag[1:lag,2:(ncol(fhw.lag)-1)]=NA
    #head(fhw.lag)
    
    lag.melt=melt(fhw.lag,id=c("date","month"))
    lag.gb=group_by(lag.melt,month,variable)
    lag.month=summarize(lag.gb,date=first(date),value=sum(value))
    lag.month=dcast(lag.month,date+month~variable)
    lag.month=lag.month[which(lag.month$date==as.Date("2014-06-01")):which(lag.month$date==as.Date("2015-05-01")),]
    
    
    location=c(which(names(lag.month)=="qz"):which(names(lag.month)=="sumall"))
    lag.cor=cor(hwm.t[which(hwm.t$date==as.Date("2014-06-01")):which(hwm.t$date==as.Date("2015-05-01")),"arrival"],
                lag.month[,location])
    test.cor=rbind(test.cor,lag.cor)
    cat("\nlag is",lag,"max cor is", max(as.data.frame(lag.cor)),"result is\n")
    print(as.data.frame(lag.cor)[order(-lag.cor)])
  } 
  result.cor=rbind(apply(test.cor,2,max),apply(test.cor,2,which.max))
  result.cor[2,]=result.cor[2,]-1
  result.cor
}


#5.4 pick up the best variables -----------------------------------------------------------
#leaps=regsubsets(arrival~ly+tq+jd+lyqz+jg+lvgl+yh+zyx+gwgl+sp+arrival1+arrival12+
#                   holiday+nbmonth+newyear+ctriphotel+ctripreview,data=hwm.t,nbest=3)

names(hwm.t[,location])[order(-hwm.cor)]
leaps=regsubsets(arrival~lygl1+sp1+zyx1+jg1+sumall1+lyqz1+jd1+jg+newyear,data=hwm.t,nbest=4)
leaps=regsubsets(arrival~lygl1+zyx1+jg1+jd1+jg+newyear,data=hwm.t,nbest=4)
leaps=regsubsets(arrival~gwgl+jd1+jg+newyear+mars,data=hwm.t,nbest=4)
plot(leaps,scale="adjr2",main="Variables Selection")


#5.5 fit--------------------------------------------------------------fit
if(F){

  hwfit=lm(arrival~jg+ly+gwgl+newyear,hwm.t)#35.9
  hwfit=lm(arrival~jg+ly+arrival12+newyear,hwm.t)#35.6
  hwfit=lm(arrival~jg+newyear,hwm.t)#35.5
  hwfit=glm(log(arrival)~I(log(jgnd12))+I(log(lynd12))+I(log(m12)),family=gaussian,hwm.t)#12
  hwfit=lm(log(arrival)~log(jg)+log(ly)+newyear,hwm.t)#12
  hwfit=lm(arrival~jg+ly+newyear,hwm.t)#35.5# April
  hwfit=lm(arrival~zyx1+jg+newyear,hwm.t)#32.1 for May
  hwfit=lm(arrival~lygl1+jg+newyear,hwm.t)#38.8 for 201506
  hwfit=lm(arrival~lygl1+newyear,hwm.t)# 40.1 for 201506
}
#------------------------------------------------------------------------------
#******************determine the varaible and make linear regression***********
#------------------------------------------------------------------------------
# multilienar:lygl1 sumall1 sp1 lyqz1
(um="gwgl")


hwfit=lm(arrival~gwgl+newyear+mars,hwm.t)# 40.1

cat("The predic result:",predict(hwfit,newdata=hwm.p),"\n")
summary(hwfit)
sqrt(vif(hwfit))

mse=sum(((fitted(hwfit)-hwm.t$arrival)/hwm.t$arrival)^2)^(1/2)
cat("lag=",lag," mse=",mse,"\n")
error=(fitted(hwfit)-hwm.t$arrival)/hwm.t$arrival
hwm.t$predict=fitted(hwfit)
hwm.t$error=error
(p <- ggplot(data=hwm.t,aes(x=factor(month),weight=error,fill=factor(month))) + 
  geom_bar(position='dodge')+labs(title="Linear Regression Error"))


gvmodel=gvlma(hwfit)
summary(gvmodel)
#multicollinearily
sqrt(vif(hwfit))



#3.2 predict-----------------------------------------------------------predict
hwm.p$arrival=predict(hwfit,newdata=hwm.p)
hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwm.p
hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwm.p
cat("the predicted final value: ",as.character(hwm.p$date), hwm.p$arrival,"\n")

msjp=hwm
msjp$prediction=predict(hwfit,newdata=msjp)
msjp=melt(msjp[,c("date","arrival","prediction")],id="date")
p <- ggplot(msjp, aes(date, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title="LR Prediction Vs Official")
print(p1)

#3.3 evalation--------------------------------------------------------evaluation
predict.result=predict(hwfit,newdata=hwm.p)
prange=range( (fitted(hwfit)-hwm.t$arrival),na.rm=T)+predict.result
benchmark[which(gmethod==methodology),]=
  data.frame(date=as.Date(target.time),id="4",methdology=methodology,target=tm, dependent="gwgl+newyear+mars",
             floor=prange[1], cap=prange[2], bestguess=predict.result,
             correlation=hwm.cor[,um], adjR2=smfit$adj.r.squared,variance=NA,maxError=max(abs(hwm.t$error),na.rm=T),
             status="Normal", adopted="Y",stringsAsFactors = F)

knitr::kable(benchmark,caption="基于百度指数的线性回归预测")




#======================================================================================
# Final Result Analysis
#======================================================================================


(final.result=median(benchmark[1:8,"bestguess"],na.rm=T))
#(final.result=mean(benchmark[1:8,"bestguess"],na.rm=T))
n=length(gmethod)+1
benchmark[n,]=NA
benchmark[n,"date"]=as.Date(target.time)
benchmark[n,"bestguess"]=final.result
benchmark[n,"methdology"]="Median"
(benchmark)

knitr::kable(benchmark,caption=paste("最终预测结果汇总表", as.Date(target.time),sep=""))

jp[which(jp$date==as.Date(target.time)),"arrival"]=actural.result
jp[which(jp$date==as.Date(target.time)),"arrival"]=final.result
jp[which(jp$date==as.Date(target.time)),]=transform(jp[which(jp$date==as.Date(target.time)),],
                                                    diff=arrival-arrival1,diff12=arrival-arrival12,
                                                    mm=arrival/arrival1,yy=arrival/arrival12)
tjp=tail(select(jp,date,thismon=arrival,lastmon=arrival1,lastyear=arrival12,M2M=diff,Y2Y=diff12,mm,yy,korea),13)
names(tjp)=c("date","Tourists","lastMonth","lastYear", "M2M", "Y2Y","mmratio","yyratio","korea")
knitr::kable(tjp,caption=paste(as.Date(target.time),"Result Analysis"))

#---------------------------------------overview--------------------------------------------
p <- ggplot(jp, aes(month, arrival))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6) + 
  geom_point(aes(colour = factor(year)))+labs(title="JP History Overview Groupby Year")
print(p1)
p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)+
  labs(title="JP History Tourist Number Overview")
print(p2)

#---------------------------------------Y2Y group by month--------------------------------------------
p<- ggplot(filter(jp,year %in% c(2002:target.year),month %in% c(4:target.mon)), aes(year, arrival))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="JP History Tourist Number Groupby Month")
print(p1)

p<- ggplot(filter(jp,year %in% c(2002:target.year),month %in% c(4:target.mon)), aes(year, diff12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="JP Y2Y Diff Groupby Month")
print(p1)



#---------------------------------------M2M group by year--------------------------------------------
p <- ggplot(filter(jp,year %in% c(2008:target.year),month %in% c(4:target.mon)), aes(month, arrival))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="JP Tourist Number Groupby Year")
print(p1)

p <- ggplot(filter(jp,year %in% c(2008:target.year),month %in% c(4:target.mon)), aes(month, diff))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="JP M2M Groupby Year")
print(p1)


#










###################################################################################
# 7 Getting data in to MySQL
###################################################################################

if (F){

  
  conn <- dbConnect(MySQL(), dbname = "thcresult", 
                    username="root", password="123456",host="101.200.189.155",port=3306)
  #dbGetQuery(conn,"set names utf8")  #for macos
  dbGetQuery(conn,"set names gbk") # for win
  
  benchmark$prediction=618658.8
  benchmark$acturalreslult=576900
  benchmark$updatetime=as.numeric(Sys.time())
  benchmark$unixdate=as.numeric(as.POSIXct(benchmark$date))
  benchmark
  #rownames(benchmark)
  
  dbListTables(conn)
  #dbRemoveTable(conn,"japanresult")
  
  if (!dbExistsTable(conn, "japanresult"))  dbWriteTable(conn, "japanresult", benchmark)
  if (dbExistsTable(conn, "japanresult"))  dbWriteTable(conn, "japanresult", benchmark,append=T)
  
  
  
  a=dbGetQuery(conn,"select * from japanresult")
  class(a$unixdate) = c('POSIXt','POSIXct')
  class(a$updatetime) = c('POSIXt','POSIXct')
  a
  str(a)
  #dbGetQuery(conn,"delete from japanresult where date='2015-07-01'")
  
  dbDisconnect(conn)
  

}