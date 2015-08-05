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


## Global constant
target.time=as.POSIXlt("2015-07-01")
target.mon=target.time$mon+1
target.year=target.time$year+1900


#=================================================================================================
#1# read the data

#1.1 official JP arrival data and the currency-----------------------------jp

japan=read.xlsx("japan.tourists.number.xlsx",1)
japan$arrival1 = c(NA, japan$arrival[1:(nrow(japan)-1)]);
japan$arrival12 = c(rep(NA, 12), japan$arrival[1:(nrow(japan)-12)]);
#define the one stage differentcial
japan$diff=japan$arrival-japan$arrival1
japan$diff12=japan$arrival-japan$arrival12
japan$mm=japan$arrival/japan$arrival1
japan$yy=japan$arrival/japan$arrival12
japan$year=as.integer(format(as.Date(japan$date),"%Y"))
japan$month=as.integer(format(as.Date(japan$date),"%m"))
jp=japan 


#=================================================================================================
#2# analysis the datap <- ggplot(jp, aes(month, arrival))

#2.1 overall prosect---------------------------------------------------------
p <- ggplot(jp, aes(month, arrival))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6) + 
  geom_point(aes(colour = factor(year)))+labs(title="JP History Overview Groupby Year")
print(p1)

p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)+
  labs(title="JP History Tourist Number Overview")
print(p2)


#=================================================================================================
#3 predict with the Y2Y Trend
#combination

# Check if we can use the Y2Y method to predict----------------------------
p<- ggplot(filter(jp,year %in% c(2002:target.year),month %in% c(1:target.mon)), aes(year, arrival))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="JP History Tourist Number Groupby Month")
print(p1)

p<- ggplot(filter(jp,year %in% c(2002:target.year),month %in% c(1:target.mon)), aes(year, diff12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="JP Y2Y Diff Groupby Month")
print(p1)

p<- ggplot(filter(jp,year %in% c(2002:target.year),month %in% c(1:target.mon)), aes(year, diff12/arrival12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="JP Y2Y Diff/arrival12 Groupby Month")
print(p1)


p<- ggplot(jp, aes(year, diff12))
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="MC History Y2Y Diff12 Groupby Month")
print(p1)



#2003 year is quite special
cmbjp=select(filter(jp,year %in% c(2004:target.year),month %in% c(1:target.mon)),year,month,arrival)
meltjp=melt(cmbjp,id=c("year","month"))

#cast the data as year id and produce m1a2,m34
dmjp=dcast(meltjp,year~month+variable)
names(dmjp)=c("year",paste("m",c(1:target.mon),sep=""))

dmjp$m1a2=(dmjp$m1+dmjp$m2)/2
dmjp$m34=(dmjp$m3+dmjp$m4)/2
dmjp$m45=(dmjp$m5+dmjp$m4)/2
dmjp$m56=(dmjp$m5+dmjp$m6)/2

target.smon=paste("m",target.mon,sep="")

#calculate the Y2Y percentage
dmpjp=dmjp
dmpjp[2:nrow(dmpjp),2:ncol(dmpjp)]=dmjp[2:nrow(dmjp),2:ncol(dmjp)]-dmjp[1:nrow(dmjp)-1,2:ncol(dmjp)]
dmpjp[1,2:ncol(dmpjp)]=0
dmpjp
#apply(dmpjp[,c(6,7)],1,order)

#m56 is the best, m6 is also OK
hcor=cor(dmpjp[-nrow(dmpjp),which(names(dmpjp)==target.smon)],dmpjp[-nrow(dmpjp),-1]) 
#vcor no year is quite correlated
vcor=cor(t(dmpjp[-which(names(dmpjp)==target.smon)]),t(dmpjp[,-which(names(dmpjp)==target.smon)]))

# predict the year 2 year diff
hcor=cor(dmpjp[-nrow(dmpjp),which(names(dmpjp)==target.smon)],dmpjp[-nrow(dmpjp),-1]) 
knitr::kable(as.data.frame(hcor),caption="Y2Y Diff12（M7）Coefficient Correlation")

#dmpjp.t=dmpjp[-which(dmpjp$year==2015 | dmpjp$year==2008),]
dmpjp.t=filter(dmpjp,year<2015 & year>2008)
dmpjp.p=dmpjp[which(dmpjp$year==2015),]

diff12.fit=lm(m7~m56,dmpjp.t)
#diff12.fit=lm(m7~m6,dmpjp.t)
summary(diff12.fit)
predict(diff12.fit,newdata=dmpjp.p)
dmpjp[which(dmpjp$year==2015),target.smon]=predict(diff12.fit,newdata=dmpjp.p)
dmpjp


plot(dmpjp$m56,dmpjp$m7,type="p",pch=1,ylab="Y2Y DIFF M7",xlab="Y2Y DIFF m56",col=mycolor[1],ylim=c(0,500000),main=("Y2Y Diff M7 VS M56 to Predict M7"))
lines(dmpjp$m56,predict(diff12.fit,newdata=dmpjp),type="p",pch=3,col=mycolor[8])
legend("topleft",legend=c("real M7 Y2Y Diff", "predict M7 Y2Y Diff"),lty=c(1,1,1,1),col=mycolor[c(1,8)])


p=ggplot(dmpjp,aes(x=year,y=m7))+geom_line()
plot(dmpjp$year,dmpjp$m7,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(-100000,600000),main=("Y2Y Diff12 M56 to Predict M7"))
lines(dmpjp$year,predict(diff12.fit,newdata=dmpjp),type="b",lty=1,xlab="date",col=mycolor[4])
lines(dmpjp$year,dmpjp$m56,type="b",lty=1,xlab="date",col=mycolor[8])
legend("topleft",legend=c("real m7 diff12", "predict m7 diff12","real m56 diff12"),lty=c(1,1,1,1),col=mycolor[c(1,4,8)])

#dmjp[which(dmjp$year==2015),"m6"]=dmpjp[which(dmpjp$year==2015),"m6"]+dmjp[which(dmjp$year==2014),"m6"]
#predict.result=dmjp[which(dmjp$year==2015),"m6"]
predict.result=dmpjp[which(dmpjp$year==2015),target.smon]+dmjp[which(dmjp$year==2014),"m56"]

range(dmpjp.t$m7-fitted(diff12.fit))
barplot((predict(diff12.fit,newdata=dmpjp)-dmpjp$m7)/dmjp$m7,names.arg=dmjp$year,ylab="ERROR")
benchmark=data.frame(date=as.Date(target.time),methdology="Y2Y Diff LR with M56",
                     floor=predict.result+range(dmpjp.t$m6-fitted(diff12.fit))[1],
                     cap=predict.result+range(dmpjp.t$m6-fitted(diff12.fit))[2],
                     bestguess=predict.result,stringsAsFactors = F)

knitr::kable(benchmark,caption="基于Y2Y增长绝对数值的线性回归预测")



#=================================================================================================
#4 Y2Y Diff Mean with 1st order different
#-------------------------------------------------------------------------------------------
sjp.data=(dmpjp[,-1]-dmpjp[,which(names(dmpjp)==target.smon)])/dmpjp[,which(names(dmpjp)==target.smon)]
sjp.data[1,]=0
#select the best KPI with lowest volatility whichh is sigma--------------------------
sigma=apply(sjp.data,2,function (x) {
  return(var(x,na.rm=T))
})

#here we select m6 with the lowest volatility
sigma=sigma[order(sigma)]
barplot(sigma,main="1st Diff12 variance with some variables")

sjp.data=sjp.data[,names(sigma)]
sjp=cbind(year=dmjp$year,sjp.data)

dmpjp[which(dmpjp$year==2015),target.smon]=NA
msjp=melt(dmpjp[,c("year","m7","m6","m56")],id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title="JP Diff12 Groupby Month")
print(p1)

ddmpjp=as.data.frame(cbind(year=dmpjp$year, diff=dmpjp[,target.smon]-dmpjp$m6))

# m6: M6 Diff 12
#mean(m6)(without2015)-mean(m45)(without2015)=m6(2015)-m45(2015)
#m6(2015)=mean(m6)(without2015)-mean(m45)(without2015)+m45(2015)
#m6diff12=mean(dmpjp[-c(1,nrow(dmpjp)),"m6"]-dmpjp[-c(1,nrow(dmpjp)),"m45"])+dmpjp[nrow(dmpjp),"m45"]

#mxdiff12=ddmpjp[which(ddmpjp$year==2014),"diff"]+dmpjp[which(dmpjp$year==2015),"m6"]
mxdiff12=ddmpjp[which(ddmpjp$year==2014),"diff"]+dmpjp[which(dmpjp$year==2015),"m6"]

predict.result=mxdiff12+dmjp[which(dmjp$year==2014),target.smon]
prange=range(dmpjp[-c(1,nrow(dmpjp)),target.smon]-dmpjp[-c(1,nrow(dmpjp)),"m6"])+dmpjp[nrow(dmpjp),"m6"]+dmjp[which(dmjp$year==2014),target.smon]
benchmark[2,]=data.frame(date=as.Date(target.time),methdology="Y2Y Diff 1st Order M6",
                         floor=prange[1],
                         cap=prange[2],
                         bestguess=predict.result,stringsAsFactors = F)

knitr::kable(benchmark,caption="基于Y2Y增长绝对数值的一阶差分预测")

dmjp[which(dmjp$year==2015),target.smon]=predict.result
dmpjp[which(dmjp$year==2015),target.smon]=mxdiff12
#melt and paint
mdmjp=select(dmjp,year,m7,m6,m56)
mdmjp=melt(mdmjp,id="year")
mdmpjp=select(dmpjp,year,m7,m6,m56)
mdmpjp=melt(mdmpjp,id="year")

p <- ggplot(mdmjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title="JP History Tourist Number Prediction Groupby Month")+
  geom_text(aes(x=2015,y=max(value,na.rm=T)),hjust=1,label="Select The Largest Y2Y Increase Ever")
print(p1)

p <- ggplot(mdmpjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))+
  labs(title="JP History Tourist Y2Y Growth Amount Prediction Groupby Month")+
  geom_text(aes(x=2015,y=max(value,na.rm=T)),hjust=1,label="Select The Largest Y2Y Increase Ever")
print(p1)








#=================================================================================================
#5 predict with the month data 1st order difference

p <- ggplot(filter(jp,year %in% c(2002:target.year),month %in% c(4:target.mon)), aes(date, diff))
#p2=p+geom_line()+labs(title="JP Tourist Number 1st order difference")
#print(p2)  
#Now let's see the diff by month 
#from this pic we can see: month 1/5/6 the y2y month diff is stable, and can use the differential methond
p1 <- p + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))+
  labs(title="JP 1st order difference Groupby Month")
print(p1)



#=================================================================================
# First Order with Linear Regression : M7=approximated diff1 + M6
# approximated diff1(M7-M6)=LR(M6)
p <- ggplot(filter(jp,year %in% c(2008:target.year),month %in% c(4:target.mon)), aes(month, arrival))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="JP Tourist Number Groupby Year")
print(p1)


vjp=dcast(meltjp,year~month+variable)
names(vjp)=c("year",paste("m",c(1:target.mon),sep=""))
vjp$m56=(vjp$m6+vjp$m5)/2

vjp=filter(vjp,year %in% c(2008:target.year))
vjp=select(vjp,year,m7,m6,m56)
vjp=transform(vjp, diff6=m7-m6, diff56=m7-m56)
vjp
cor.diff=cor(vjp$diff6,vjp[,2:ncol(vjp)],use="na.or.complete")
cor.diff
cor.diff=cor(vjp$diff56,vjp[,2:ncol(vjp)],use="na.or.complete")
cor.diff
#m56 is the best

knitr::kable(as.data.frame(cor.diff),caption="基于M2M月度一阶差分的线性回归预测")



#diff.fit=lm(diff6~m6,na.exclude(vjp))
diff.fit=lm(diff56~m56,na.exclude(vjp))
summary(diff.fit)
#predict(diff.fit,newdata=vjp[which(vjp$year==2015),])+vjp[which(vjp$year==2015),"m56"]
approxdiff=predict(diff.fit,newdata=vjp[which(vjp$year==2015),])
predict.result=approxdiff+vjp[which(vjp$year==2015),"m56"]

plot(vjp$m56,vjp$diff56,type="p",pch=1,ylab="DIFF=m7-m56",xlab="m56",col=mycolor[1],ylim=c(0,400000),main=("M2M Diff1 Vs M56 to Predict M7"))
#lines(vjp$m6,vjp$diff6,type="p",pch=2,col=mycolor[3])
lines(vjp$m56,predict(diff.fit,newdata=vjp),type="p",pch=3,col=mycolor[8])
legend("topleft",legend=c("real M7-M56", "predict M7-M56"),lty=c(1,1,1,1),col=mycolor[c(1,8)])

plot(vjp$year,vjp$diff56,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(0,350000),main=("M2M Diff M7-M56 to Predict M7"))
lines(vjp$year,predict(diff.fit,newdata=vjp),type="b",lty=1,xlab="date",col=mycolor[8])
legend("topleft",legend=c("real M7-M56 diff", "predict M7-M56 diff"),lty=c(1,1,1,1),col=mycolor[c(1,8)])

prange=range(vjp$diff56-predict(diff.fit,newdata=vjp),na.rm = T)+predict.result
barplot((predict(diff.fit,newdata=vjp)-vjp$diff56)/vjp$diff56,names.arg=vjp$year,ylab="ERROR")

benchmark[3,]=data.frame(date=as.Date(target.time),methdology="M2M 1st Order Diff LR with M56",
                         floor=prange[1],
                         cap=prange[2],
                         bestguess=predict.result,stringsAsFactors = F)


knitr::kable(benchmark,caption="基于M2M月度一阶差分的线性回归预测")


#=================================================================================
# First Order without Linear Regression : M7(2015)-M6(2015)=M7(2014)-M6(2014)
# M7(2015)=M6(2015)+M7(2014)-M6(2014)
approxdiff=vjp[which(vjp$year==2014),"diff6"]
predict.result=approxdiff+vjp[which(vjp$year==2015),"m6"]

benchmark[4,]=data.frame(date=as.Date(target.time),methdology="M2M 1st Order Diff Staionary M6",
                         floor=NA,
                         cap=NA,
                         bestguess=predict.result,stringsAsFactors = F)

(benchmark)
knitr::kable(benchmark[4,],caption="基于M2M月度一阶差分的保守预测")

temp=filter(jp,year %in% c(2008:target.year),month %in% c(4:target.mon))
temp[which(temp$date==as.Date("2015-07-01")),"arrival"]=predict.result
p <- ggplot(temp, aes(month, arrival))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="JP Tourist Number Groupby Year")
print(p1)





#=================================================================================
# Second Order without Linear Regression : Diff M7(2015) - Diff M6(2015)=Diff M7(2014) - Diff M6(2014)
# M7(2015)=M6(2015)+M7(2014)-M6(2014)

# Second Order
p <- ggplot(filter(jp,year %in% c(2008:target.year),month %in% c(4:target.mon)), aes(month, diff))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="JP 2st order difference Groupby Year")
print(p1)

diff1=select(filter(jp,year %in% c(2004:target.year),month %in% c(1:target.mon)),year,month,diff)
diff1=melt(diff1,id=c("year","month"))
diff1=dcast(diff1,year~month+variable)
names(diff1)=c("year",paste("m",c(1:target.mon),sep=""))

approxdiff=diff1[which(diff1$year==2015),"m6"]+diff1[which(diff1$year==2014),"m7"]-diff1[which(diff1$year==2014),"m6"]
predict.result=approxdiff+vjp[which(vjp$year==2015),"m6"]

benchmark[5,]=data.frame(date=as.Date(target.time),methdology="M2M 2nd Order Diff with M6",
                         floor=NA,
                         cap=NA,
                         bestguess=predict.result,stringsAsFactors = F)

(benchmark)

knitr::kable(benchmark[5,],caption="基于M2M月度二阶差分预测")

temp=filter(jp,year %in% c(2008:target.year),month %in% c(4:target.mon))
temp[which(temp$date==as.Date("2015-07-01")),"diff"]=approxdiff
p <- ggplot(temp, aes(month, diff))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+
  labs(title="JP 2st order difference Groupby Year")
print(p1)








#=================================================================================================
#6 predict with linear regression method

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


hwfit=lm(arrival~gwgl+newyear+mars,hwm.t)# 40.1

cat("The predic result:",predict(hwfit,newdata=hwm.p),"\n")
summary(hwfit)
sqrt(vif(hwfit))

mse=sum(((fitted(hwfit)-hwm.t$arrival)/hwm.t$arrival)^2)^(1/2)
cat("lag=",lag," mse=",mse,"\n")
error=(fitted(hwfit)-hwm.t$arrival)/hwm.t$arrival
hwm.t$predict=fitted(hwfit)
hwm.t$error=error
barplot(error,names.arg=hwm.t$date,ylab="ERROR")


gvmodel=gvlma(hwfit)
summary(gvmodel)
#multicollinearily
sqrt(vif(hwfit))



#3.2 predict-----------------------------------------------------------predict
hwm.p$arrival=predict(hwfit,newdata=hwm.p)
hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwm.p
hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwm.p
cat("the predicted final value: ",as.character(hwm.p$date), hwm.p$arrival,"\n")

plot(hwm$date,hwm$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(0,400000),main=("Prediction Vs Official"))
legend("topleft",legend=c("official", "predict"),lty=c(1,1,1,1),col=mycolor[c(1,3,5,7)])
lines(hwm[c(time.range,max(time.range+1)),"date"],c(fitted(hwfit),hwm.p$arrival),type="p",pch=2,col=mycolor[3])
#lines(hwm$date,hwm$jg*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])

#3.3 evalation--------------------------------------------------------evaluation

predict.result=predict(hwfit,newdata=hwm.p)
hwm.t[,c("date","arrival","predict","error")]
hwm[,c("date","arrival")]

benchmark[6,]=data.frame(date=as.Date(target.time),methdology="LR with gwgl mars",
                         floor=NA,
                         cap=NA,
                         bestguess=predict.result,stringsAsFactors = F)

benchmark[7,]=data.frame(date=as.Date(target.time),methdology="Method Mean",
                         floor=NA,
                         cap=NA,
                         bestguess=mean(benchmark[1:6,"bestguess"]),stringsAsFactors = F)

benchmark

knitr::kable(benchmark[6,],caption="基于百度指数的线性回归预测")

knitr::kable(benchmark,caption="最终预测结果汇总表")










###################################################################################
# 7 Getting data in to MySQL
###################################################################################

conn <- dbConnect(MySQL(), dbname = "thcresult", 
                  username="root", password="123456",host="101.200.189.155",port=3306)
#dbGetQuery(conn,"set names utf8")  #for macos
dbGetQuery(conn,"set names gbk") # for win

benchmark$prediction=benchmark[nrow(benchmark),"bestguess"]
benchmark$acturalreslult=0
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


