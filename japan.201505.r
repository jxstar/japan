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




#=================================================================================================
#1# read the data

#1.1 official JP arrival data and the currency-----------------------------jp
if (F){
  japan=read.csv("japan.csv",header=T,na.strings="",stringsAsFactor=F)
  names(japan)=c("date","bi","arrival","csi")
  japan$date=as.Date(japan$date,"%m/%d/%y")
  japan$bi=as.numeric(japan$bi)
  japan$arrival=as.numeric(japan$arrival)
}

japan=read.xlsx("japan.tourists.number.xlsx",1)
japan$arrival1 = c(NA, japan$arrival[1:(nrow(japan)-1)]);
japan$arrival12 = c(rep(NA, 12), japan$arrival[1:(nrow(japan)-12)]);
japan$mm=japan$arrival/japan$arrival1
japan$yy=japan$arrival/japan$arrival12
japan$year=as.integer(format(as.Date(japan$date),"%Y"))
jp=japan 


#=================================================================================================
#2# analysis the datap <- ggplot(jp, aes(month, arrival))

#2.1 overall prosect---------------------------------------------------------
p <- ggplot(jp, aes(month, arrival))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))
print(p1)

p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)
print(p2)

p3 <- ggplot(filter(jp,year %in% c(2002:2015),month %in% c(1:5)), aes(year, arrival))
p4 <- p3 + geom_line(aes(colour = factor(month)), alpha = 0.6)+geom_point(aes(colour = factor(month)))
print(p4)





#=================================================================================================
#3 predict with the Y2Y Trend
#combination

#2003 year is quite special
cmbjp=select(filter(jp,year %in% c(2004:2015),month %in% c(1:5)),year,month,arrival)
meltjp=melt(cmbjp,id=c("year","month"))

#cast the data as year id and produce m1a2,m34
dmjp=dcast(meltjp,year~month+variable)
names(dmjp)=c("year","m1","m2","m3","m4","m5")
dmjp$m1a2=(dmjp$m1+dmjp$m2)/2
dmjp$m34=(dmjp$m3+dmjp$m4)/2

#calculate the Y2Y percentage
dmpjp=dmjp
dmpjp[2:nrow(dmpjp),2:ncol(dmpjp)]=dmjp[2:nrow(dmjp),2:ncol(dmjp)]/dmjp[1:nrow(dmjp)-1,2:ncol(dmjp)]
dmpjp[1,2:ncol(dmpjp)]=1

# select the best Y2Y percentage
diffdmpjp=(dmpjp[,-c(which(names(dmpjp)=="year"),which(names(dmpjp)=="m5"))]-
           data.frame(dmpjp$m5,dmpjp$m5,dmpjp$m5,dmpjp$m5,dmpjp$m5,dmpjp$m5))/
          data.frame(dmpjp$m5,dmpjp$m5,dmpjp$m5,dmpjp$m5,dmpjp$m5,dmpjp$m5)

best.trend.diff=which.min((colSums(diffdmpjp^2,na.rm=T)/(nrow(diffdmpjp-1)))^(1/2))
mean.trend.diff=colMeans(abs(diffdmpjp[-2,]),na.rm=T)

#predict the M5 Y2Y trend;----------------------------------------------------------------
#--date--method--key factor--result
predict.y2y=dmpjp[which(dmpjp$year==2015),"m4"]
#(pm4-pm5)/pm5=a===> pm5=pm4/(1+-a)
#mean reverting:2013- 2014- so we predict 2015+ pm4>pm5, recent 3year, a=0.06
predict.y2y=predict.y2y/(1+0.06)
predict.tourist.y2y=predict.y2y*dmjp[which(dmpjp$year==2014),"m5"]

dmjp[which(dmjp$year==2015),"m5"]=predict.tourist.y2y
dmpjp[which(dmpjp$year==2015),"m5"]=predict.y2y

benchmark=data.frame(date=as.Date("2015-05-01"),methdology="Y2YTrend",
                     interval.low=dmpjp[which(dmpjp$year==2015),"m4"]/(1+min(mean.trend.diff))*dmjp[which(dmpjp$year==2014),"m5"],
                     interval.high=dmpjp[which(dmpjp$year==2015),"m4"]/(1-min(mean.trend.diff))*dmjp[which(dmpjp$year==2014),"m5"],
                     bestguess=predict.tourist.y2y)
         

#melt and paint
mdmjp=select(dmjp,year,m3,m4,m5,m1a2,m34)
mdmjp=melt(mdmjp,id="year")
mdmpjp=select(dmpjp,year,m3,m4,m5,m1a2,m34)
mdmpjp=melt(mdmpjp,id="year")

p <- ggplot(mdmjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))
print(p1)

p <- ggplot(mdmpjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))
print(p1)




#=================================================================================================
#4 predict with the month data share Volatility
#cast the data as year id and produce m1a2,m34-------------------
current=2015
recent=current-1

vjp=dcast(meltjp,year~month+variable)
names(vjp)=c("year","m1","m2","m3","m4","m5")
vjp$m1a2=(vjp$m1+vjp$m2)
vjp$m34=(vjp$m3+vjp$m4)
vjp$m1234=vjp$m1+vjp$m2+vjp$m3+vjp$m4
vjp$m123=vjp$m1+vjp$m2+vjp$m3
vjp$m124=vjp$m1+vjp$m2+vjp$m4


#define all of the KPI share:sjp
sjp.data=select(vjp,m1234,m34,m1a2,m123,m124,m3,m4)
sjp.data=vjp$m5/sjp.data
#not perform very well
if (T) {
  sjp.data$m45dm1a2=(vjp$m5+vjp$m4)/vjp$m1a2
  sjp.data$m45dm3=(vjp$m5+vjp$m4)/vjp$m3
  sjp.data$m45dm123=(vjp$m5+vjp$m4)/vjp$m123
  sjp.data$m345dm1a2=(vjp$m5+vjp$m34)/vjp$m1a2
}

#select the best KPI with lowest volatility whichh is sigma--------------------------
sigma=apply(sjp.data,2,function (x) {
  #x=x/mean(x,na.rm=T)
  return(var(x,na.rm=T))
})
#sigma=apply(sjp,2,var,na.rm=T)
sigma=sigma[order(sigma)]
barplot(sigma[-length(sigma)])

sjp.data=sjp.data[,names(sigma)]
sjp=cbind(year=vjp$year,sjp.data)

msjp=melt(sjp,id="year")
p <- ggplot(msjp, aes(year, value))
p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))
print(p1)


#make the prediction with the lowest volatility factor--------------------------
#median of all years
medianshare=apply(sjp.data,2,median,na.rm=T);
medianshare=as.data.frame(t(medianshare))

#2014
recentshare=filter(sjp,year==recent);
recentshare=recentshare[-(which(names(recentshare)=="year"))]
#recentshare=as.vector(as.matrix(recentshare))

currentyear=vjp[which(vjp$year==current),]
names(medianshare)

predict.sv.median=data.frame(
  m34=medianshare$m34*currentyear$m34,
  m3=medianshare$m3*currentyear$m3,
  m45dm3=medianshare$m45dm3*currentyear$m3-currentyear$m4,
  m4=medianshare$m4*currentyear$m4,
  m1234=medianshare$m1234*currentyear$m1234,
  m45dm123=medianshare$m45dm123*currentyear$m123-currentyear$m4,
  m123=medianshare$m123*currentyear$m123,
  m124=medianshare$m124*currentyear$m124,
  m345dm1a2=medianshare$m345dm1a2*currentyear$m1a2-currentyear$m3-currentyear$m4,
  m45dm1a2=medianshare$m45dm1a2*currentyear$m1a2-currentyear$m4,
  m1a2=medianshare$m1a2*currentyear$m1a2  )
predict.sv.median
medianshare


#recent is better
predict.sv.recent=data.frame(
  m34=recentshare$m34*currentyear$m34,
  m3=recentshare$m3*currentyear$m3,
  m45dm3=recentshare$m45dm3*currentyear$m3-currentyear$m4,
  m4=recentshare$m4*currentyear$m4,
  m1234=recentshare$m1234*currentyear$m1234,
  m45dm123=recentshare$m45dm123*currentyear$m123-currentyear$m4,
  m123=recentshare$m123*currentyear$m123,
  m124=recentshare$m124*currentyear$m124,
  m345dm1a2=recentshare$m345dm1a2*currentyear$m1a2-currentyear$m3-currentyear$m4,
  m45dm1a2=recentshare$m45dm1a2*currentyear$m1a2-currentyear$m4,
  m1a2=recentshare$m1a2*currentyear$m1a2  )
predict.sv.recent=predict.sv.recent[,names(recentshare)]
predict.sv.recent;rowMeans(predict.sv.recent);median(as.matrix(predict.sv.recent));range(predict.sv.recent)



benchmark=rbind(benchmark,
                data.frame(date=as.Date("2015-05-01"),methdology="ShareVolatility-recent",
                           interval.low=range(predict.sv.recent)[1],
                           interval.high=range(predict.sv.recent)[2],
                           bestguess=median(as.matrix(predict.sv.recent))))

benchmark


#=================================================================================================
#5 predict with linear regression method

#1.4 read the data of baidu index----------------------------------------
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
enddate=as.Date("2015-05-01")
lag=0

fhw.month=filter(fhw.month,date %in% c(begindate:enddate))
date=fhw.month$date

#2.2 hwm----------------------------------------------------------------hwm

hwm=select(fhw.month,date,month,jd,jg,gwgl,ly,lyqz,lygl,zyx,sp,sumall)
hwm=transform(hwm,ly1=c(NA,fhw.month[1:nrow(fhw.month)-1,"ly"]),lyqz1=c(NA,fhw.month[1:nrow(fhw.month)-1,"lyqz"]),
              lygl1=c(NA,fhw.month[1:nrow(fhw.month)-1,"lygl"]),zyx1=c(NA,fhw.month[1:nrow(fhw.month)-1,"zyx"]))
hwm=transform(hwm,
              arrival=jp[which(jp$date==first(date)):which(jp$date==last(date)),"arrival"],
              arrival1=jp[which(jp$date==first(date)):which(jp$date==last(date)),"arrival1"],
              arrival12=jp[which(jp$date==first(date)):which(jp$date==last(date)),"arrival12"],
              holiday=jp[which(jp$date==first(date)):which(jp$date==last(date)),"holiday"],
              nbmonth=jp[which(jp$date==first(date)):which(jp$date==last(date)),"nbmonth"],
              newyear=jp[which(jp$date==first(date)):which(jp$date==last(date)),"newyear"])
              

#2.3 hwm.p--------------------------------------------------------------hwm.p
predict.no=1
usefuldate=hwm[which(hwm$date==as.Date("2014-01-01")):which(hwm$date==as.Date("2015-05-01")),"date"]
cat("The Date We Use in this algorithm\n", as.character(usefuldate),"\n")
hwm.t=hwm[which(hwm$date==as.Date("2014-01-01")):which(hwm$date==as.Date("2015-04-01")),]
hwm.p=hwm[which(hwm$date==as.Date("2015-05-01")):which(hwm$date==as.Date("2015-05-01")),]

cat("The correlation of the hotwords we may select:\n")
hwm.cor=cor(hwm.t$arrival,hwm.t[,3:16])
names(hwm.t[,3:16])[order(-hwm.cor)]

corrgram(hwm.t[,3:16],order=T,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)

test=select(hwm,date,arrival,zyx1,lyqz1,lygl1,gwgl,arrival12,jg,jd,sp,sumall)

test=transform(test,zyx1=zyx1*mean(arrival,na.rm=T)/mean(zyx1,na.rm=T),
               lyqz1=lyqz1*mean(arrival,na.rm=T)/mean(lyqz1,na.rm=T),
               lygl1=lygl1*mean(arrival,na.rm=T)/mean(lygl1,na.rm=T),
               gwgl=gwgl*mean(arrival,na.rm=T)/mean(gwgl),
               jg=jg*mean(arrival,na.rm=T)/mean(jg),
               jd=jd*mean(arrival,na.rm=T)/mean(jd))

for (i in (1:(ncol(test)-2))){
  test1=select(test,date,arrival,zyx1)
  test1=test[,c(1,2,i+2)]
  mtest=melt(test1,id="date")
  p <- ggplot(mtest, aes(date, value))
  p1 <- p + geom_line(aes(colour = factor(variable)), alpha = 0.6)+geom_point(aes(colour = factor(variable)))
  print(p1)
  
}

#----------------------------------------------------------------------------------------------
#3# fit and predict

#3.1 pick up the best variables 
#leaps=regsubsets(arrival~ly+tq+jd+lyqz+jg+lvgl+yh+zyx+gwgl+sp+arrival1+arrival12+
#                   holiday+nbmonth+newyear+ctriphotel+ctripreview,data=hwm.t,nbest=3)

names(hwm.t[,3:16])[order(-hwm.cor)]
leaps=regsubsets(arrival~zyx1+lyqz1+lygl1+gwgl+lygl+arrival12+jg+ly1+jd+arrival1+zyx+lyqz+ly+
                   holiday+nbmonth+newyear,data=hwm.t,nbest=4)
leaps=regsubsets(arrival~zyx1+lyqz1+lygl1+gwgl+arrival12+jg+ly1+jd+holiday+nbmonth+newyear,data=hwm.t,nbest=5)
plot(leaps,scale="adjr2")

#3.1 fit--------------------------------------------------------------fit
if(F){

  hwfit=lm(arrival~jg+ly+gwgl+newyear,hwm.t)#35.9
  hwfit=lm(arrival~jg+ly+arrival12+newyear,hwm.t)#35.6
  hwfit=lm(arrival~jg+newyear,hwm.t)#35.5
  hwfit=glm(log(arrival)~I(log(jgnd12))+I(log(lynd12))+I(log(m12)),family=gaussian,hwm.t)#12
  hwfit=lm(log(arrival)~log(jg)+log(ly)+newyear,hwm.t)#12
  hwfit=lm(arrival~jg+ly+newyear,hwm.t)#35.5# April
}

hwfit=lm(arrival~zyx1+jg+newyear,hwm.t)#32.1 for May

cat("The hotwords we use to predict: jiage lvyou newyear\n")
cat("The predic result:",predict(hwfit,newdata=hwm.p),"\n")


summary(hwfit)
gvmodel=gvlma(hwfit)
summary(gvmodel)
#multicollinearily
sqrt(vif(hwfit))



#3.2 predict-----------------------------------------------------------predict
hwm.p$arrival=predict(hwfit,newdata=hwm.p)
hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwm.p
hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwm.p
cat("the predicted final value: ",as.character(hwm.p$date), hwm.p$arrival,"\n")

plot(hwm$date,hwm$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(0,400000))
legend("topleft",legend=c("official", "predict","jiage","lvyou lagged 1m"),lty=c(1,1,1,1),col=mycolor[c(1,3,5,7)])
lines(usefuldate,c(fitted(hwfit),hwm.p$arrival),type="b",pty=2,col=mycolor[3])
#lines(usefuldate,c(fitted(percentfit)*(585400),hwm.p$nd12*585400),type="b",pty=2,col=mycolor[9])
lines(hwm$date,hwm$jg*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])

#3.3 evalation--------------------------------------------------------evaluation
mse=sum(((fitted(hwfit)-hwm.t$arrival)/hwm.t$arrival)^2)^(1/2)
cat("lag=",lag," mse=",mse,"\n")
error=(fitted(hwfit)-hwm.t$arrival)/hwm.t$arrival
hwm.t$predict=fitted(hwfit)
hwm.t$error=error
barplot(error,names.arg=hwm.t$date,ylab="ERROR")

hwm.t[,c("date","arrival","predict","error")]
hwm[,c("date","arrival")]

benchmark=rbind(benchmark,
                data.frame(date=as.Date("2015-05-01"),methdology="Linear Regression",
                           interval.low=290000,
                           interval.high=350000,
                           bestguess=hwm.p$arrival))

benchmark








jp.p=jp
jp.p[which(jp.p$date==hwm.p$date),"arrival"]=hwm.p$arrival
jp.p=jp.p[40:162,]
plot(jp.p$date,jp.p$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(50000,400000))
legend("topleft",legend=c("japan tourist number"),lty=c(1),col=mycolor[c(1)])

jp.p[,1:2]

save(file="japan.Rdata",japan,jp,hotword,hwm,currency)
load("japan.Rdata")