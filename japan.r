library(quantmod)
library(xlsx)
library(ggplot2) #add for ggplot
library(leaps)
library(Hmisc) #describe
library(psych) #describe
library(GPArotation)
library(pastecs) #stat.desc
library(corrgram) # for corralation analysis
library(gvlma)
library(car)
library(relaimpo)

##deal with CSV file
##1. delete the "," in number
##2. change the date to 
par(mfrow=c(1,1),mar=c(4,4,2,2))
mycolor=rainbow(20)
#----------------------------------------------------------------------------------------------
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
jp=japan 
#head(jp,20)


str(japan)
p <- ggplot(jp, aes(month, arrival))
p1 <- p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))
print(p1)

p2=p + geom_line(aes(colour = factor(year)), alpha = 0.6)+geom_point(aes(colour = factor(year)))+facet_wrap(~year)
print(p2)



#1.2 read the currency----------------------------------------------------currency
currency=read.xlsx("currency.xlsx",1)
#head(currency)
currency=currency[order(currency$DATE),]

#1.3 read the data of ctrip mafengwo quner--------------------------------
ctripreview=read.xlsx("japan-hotword.xlsx",2);ctripreview=ctripreview[order(ctripreview$date),]
ctripvisa=read.xlsx("japan-hotword.xlsx",3);ctripvisa=ctripvisa[order(ctripvisa$date),]
ctriphotel=read.xlsx("japan-hotword.xlsx",4);ctriphotel=ctriphotel[order(ctriphotel$date),]
mfwguide=read.xlsx("japan-hotword.xlsx",5);mfwguide=mfwguide[order(mfwguide$date),]
visa=read.xlsx("japan-hotword.xlsx",6);visa=visa[order(visa$date),]


#1.4 read the data of baidu index----------------------------------------
hotword=read.xlsx("japan-hotword.xlsx",1)
hwname=c("date","qz","ly","y","tq","jd","ryfy","hl","lyqz","jg","dt","lvgl","yh","zyx","gwgl","gw","lyjd",
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

hwname=c("date","qz","ly","y","tq","jd","ryfy","hl","lyqz","jg","dt","lvgl","yh","zyx","gwgl","gw","lyjd")
hotword$sumplace=rowSums(hotword[,which(names(hotword)=="ctjc"):which(names(hotword)=="segu")])
hotword$si=rowSums(hotword[,c("jg","yh")])
fhw=hotword[,-(which(names(hotword)=="ctjc"):which(names(hotword)=="segu"))]
names(fhw)=c("date","qz","ly","y","tq","jd","ryfy","hl","lyqz","jg","dt","lvgl","yh","zyx","gwgl","gw","lyjd","sp","si")

if (F){
  par(mfrow=c(1,1),mar=c(4,4,0,0))
  mycolor=rainbow(10)
  #worst: hl,y,ryfy,dt,qz,gw,lyjd,sp(qz lvqz same): or  tq lvgl zyx 
  #better: tq lvgw zyx
  #best jg jd yh gwgl ly lyqz sp
  
  plot(fhw[1:408,"date"],fhw[1:408,"jg"],type="p",cex=0.3,lty=1,xlab="date",col=mycolor[1],ylim=c(0,3000))
  lines(fhw[1:408,"date"],fhw[1:408,"si"],type="p",cex=0.3,lty=1,xlab="date",col=mycolor[2],ylim=c(0,3000))
  lines(fhw[1:408,"date"],fhw[1:408,"jd"],type="p",cex=0.3,lty=1,xlab="date",col=mycolor[2],ylim=c(0,3000))
  lines(fhw[1:408,"date"],fhw[1:408,"yh"],type="p",cex=0.3,lty=1,xlab="date",col=mycolor[3],ylim=c(0,3000))
  lines(fhw[1:408,"date"],fhw[1:408,"gwgl"],type="p",cex=0.3,lty=1,xlab="date",col=mycolor[4],ylim=c(0,3000))
  lines(fhw[1:408,"date"],fhw[1:408,"ly"]-mean(fhw[1:408,"jg"]),type="p",cex=0.3,lty=1,xlab="date",col=mycolor[5],ylim=c(0,3000))
  lines(fhw[1:408,"date"],fhw[1:408,"lyqz"],type="p",cex=0.3,lty=1,xlab="date",col=mycolor[6],ylim=c(0,3000))
  
  #lines(fhw[1:408,"date"],fhw[1:408,"tq"],type="p",cex=0.3,lty=1,xlab="date",col=mycolor[7],ylim=c(0,3000))
  #lines(fhw[1:408,"date"],fhw[1:408,"lvgl"],type="p",cex=0.3,lty=1,xlab="date",col=mycolor[8],ylim=c(0,3000))
  #lines(fhw[1:408,"date"],fhw[1:408,"zyx"],type="p",cex=0.3,lty=1,xlab="date",col=mycolor[9],ylim=c(0,3000))
  lines(fhw[1:408,"date"],fhw[1:408,"sp"]/10,type="p",cex=0.3,lty=1,xlab="date",col=mycolor[10],ylim=c(0,3000))
  legend("topleft",legend=c("jg", "jd","yh","gwgl","ly","lyqz","tq","lvgl","zyx","sp"),lty=c(1,1),col=mycolor[c(1,2,3,4,6,7,9)])  
}


#1.4 read the baidu sogou 360 index----------------------------------------
Avg2sum=function(xxindex,jp){
  names(xxindex)=c("date","jd","jg","yh","gwgl","ly","lyqz","tq","lygl","zyx");
  xxindex=xxindex[order(xxindex$date),]
  for (i in 1:nrow(xxindex)){
    dpm=jp[which(jp$date==as.Date(xxindex[i,"date"])),"dpm"]
    xxindex[i,2:ncol(xxindex)]=xxindex[i,2:ncol(xxindex)]*dpm
  }
  return(xxindex)
}
bindex=read.xlsx("japan-index.xlsx",1);
bindex=Avg2sum(bindex,jp)
qindex=read.xlsx("japan-index.xlsx",2);
qindex=Avg2sum(qindex,jp)
sindex=read.xlsx("japan-index.xlsx",3);
sindex=Avg2sum(sindex,jp)


#----------------------------------------------------------------------------------------------
#2# prepare the taining and prediction data to hwm

#2.1 date----------------------------------------------------------------date
date1=c(as.Date("2013-01-01"),as.Date("2013-02-01"),as.Date("2013-03-01"),
        as.Date("2015-03-01"),as.Date("2013-05-01"),as.Date("2013-06-01"),
        as.Date("2013-07-01"),as.Date("2013-08-01"),as.Date("2013-09-01"),
        as.Date("2013-10-01"),as.Date("2013-11-01"),as.Date("2013-12-01"),
        as.Date("2014-01-01"),as.Date("2014-02-01"),as.Date("2014-03-01"),
        as.Date("2014-04-01"),as.Date("2014-05-01"),as.Date("2014-06-01"),
        as.Date("2014-07-01"),as.Date("2014-08-01"),as.Date("2014-09-01"),
        as.Date("2014-10-01"),as.Date("2014-11-01"),as.Date("2014-12-01"),
        as.Date("2015-01-01"),as.Date("2015-02-01"),as.Date("2015-03-01"),
        as.Date("2015-04-01"))


#2.2 hwm----------------------------------------------------------------hwm
lag=0

if (F){
  hwm=fhw[1,]
  hwm$jpy=0
  begindate=as.Date("2014-04-01")
  enddate=as.Date("2015-04-01")
  date=date1[which(date1==as.Date("2014-04-01")):which(date1==as.Date("2015-04-01"))]
  for (i in 1:length(date)){
    
    #read the jp table
    dpm=jp[which(jp$date==as.Date(date[i])),"dpm"]
    
    #read the fhw table
    location=which(fhw$date==as.Date(date[i]))
    begin=max(1,location-lag)
    end=min(nrow(fhw),begin+dpm-1)
    
    hwm[i,"date"]=date[i]
    hwm[i,2:ncol(fhw)]=colSums(fhw[begin:end,2:ncol(fhw)])
    cat(begin,end,as.Date(date[i]),"\n")
    
    #read the currency table
    month=substr(as.character(date[i]),1,7)
    jpy=currency[grep(month,as.character(currency$DATE))[1],"JPY"]
    jpy=1/jpy*100
    hwm[i,"jpy"]=jpy
  }
}else{
  hwm.m=data.frame()
  begindate=as.Date("2013-01-01")
  enddate=as.Date("2015-04-01")
  
  date=date1[which(date1==begindate):which(date1==enddate)]
  hwm.b=bindex[which(bindex$date==begindate):which(bindex$date==enddate),]
  hwm.q=qindex[which(qindex$date==begindate):which(qindex$date==enddate),]
  hwm.s=sindex[which(sindex$date==begindate):which(sindex$date==enddate),]
  hwm.m=merge(hwm.q,hwm.s,by.x="date",by.y="date",suffixes=c(".q",".s"))
  hwm.m=merge(hwm.b,hwm.m,by.x="date",by.y="date")  
  hwm.m$arrival=jp[which(jp$date==begindate):(which(jp$date==enddate)),"arrival"]
  
  w=rbind(c(10,50,100),c(10,50,100),c(10,50,100),c(7,20,50),c(10,50,100),c(2,5,10),c(10,50,100),c(5,25,100),c(5,15,50),c(10,50,100))
  for (i in 2:ncol(hwm.b)){
    plot(hwm.m$date,hwm.m$arrival,type="b",cex=0.5,lty=1,xlab="date",col=mycolor[1],ylim=c(0,400000),main=names(hwm.b)[i])
    lines(hwm.b$date,hwm.b[,i]*w[i,1],type="b",cex=0.5,lty=1,xlab="date",col=mycolor[3],ylim=c(0,400000))
    legend("topleft",legend=c("official", paste("baidu ",names(hwm.b)[i]),paste("qihu ",names(hwm.b)[i]),paste("sogou ",names(hwm.b)[i])),lty=c(1,1,1,1),col=mycolor[c(1,3,5,7)])
    lines(hwm.q$date,hwm.q[,i]*w[i,2],type="b",cex=0.5,lty=1,xlab="date",col=mycolor[5],ylim=c(0,100000))
    lines(hwm.s$date,hwm.s[,i]*w[i,3],type="b",cex=0.5,lty=1,xlab="date",col=mycolor[7],ylim=c(0,100000))
#    png(filename=paste(names(hwm.b)[i],".png"))  
  }
  
  #jd.b,jg.b,yh.b,gwgl.b,gwgl.q,ly.b-1,ly.s,lyqz.b-1,lygl.b-1,zyx.b-1
  hwm=data.frame(date=hwm.m$date,jd=hwm.m$jd,jg=hwm.m$jg,gwgl=hwm.m$gwgl,
                 gwgl.q=hwm.m$gwgl.q,ly=c(NA,hwm.m[1:nrow(hwm.m)-1,"ly"]),
                 lyqz=c(NA,hwm.m[1:nrow(hwm.m)-1,"lyqz"]),lygl=c(NA,hwm.m[1:nrow(hwm.m)-1,"lygl"]),
                 zyx=c(NA,hwm.m[1:nrow(hwm.m)-1,"zyx"]))

}

hwm$arrival=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"arrival"]
hwm$arrival1=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"arrival1"]
hwm$arrival12=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"arrival12"]
hwm$holiday=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"holiday"]
hwm$nbmonth=as.factor(jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"nbmonth"])
hwm$newyear=as.factor(jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"newyear"])
hwm$ctripreview=ctripreview[which(ctripreview$date==date[1]):(which(ctripreview$date==date[1])+length(date)-1),"tot"]
hwm$ctriphotel=ctriphotel[which(ctriphotel$date==date[1]):(which(ctriphotel$date==date[1])+length(date)-1),"comment"]

# add some percent value
hwm$mm=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"mm"]
hwm$yy=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"yy"]
hwm$nd12=NA;hwm$jgnd12=NA;hwm$lynd12=NA;hwm$m12=NA;hwm$m1234=NA;hwm$nd1234=NA
strdate=as.character(hwm$date)
hwm[grep("2013",strdate),"m12"]=(hwm[which(hwm$date==as.Date("2013-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2013-02-01")),"arrival"])
hwm[grep("2014",strdate),"m12"]=(hwm[which(hwm$date==as.Date("2014-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2014-02-01")),"arrival"])
hwm[grep("2015",strdate),"m12"]=(hwm[which(hwm$date==as.Date("2015-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2015-02-01")),"arrival"])
hwm[grep("2013",strdate),"m1234"]=(hwm[which(hwm$date==as.Date("2013-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2013-02-01")),"arrival"]+hwm[which(hwm$date==as.Date("2013-03-01")),"arrival"]+hwm[which(hwm$date==as.Date("2013-04-01")),"arrival"])
hwm[grep("2014",strdate),"m1234"]=(hwm[which(hwm$date==as.Date("2014-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2014-02-01")),"arrival"]+hwm[which(hwm$date==as.Date("2014-03-01")),"arrival"]+hwm[which(hwm$date==as.Date("2014-04-01")),"arrival"])
hwm[grep("2015",strdate),"m1234"]=(hwm[which(hwm$date==as.Date("2015-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2015-02-01")),"arrival"]+hwm[which(hwm$date==as.Date("2015-03-01")),"arrival"]+hwm[which(hwm$date==as.Date("2015-04-01")),"arrival"])
hwm$nd12=hwm$arrival/hwm$m12
hwm$nd1234=hwm$arrival/hwm$m1234
hwm[grep("2013",strdate),"jgnd12"]=hwm[grep("2013",strdate),"jg"]/(hwm[which(hwm$date==as.Date("2013-01-01")),"jg"]+hwm[which(hwm$date==as.Date("2013-02-01")),"jg"])
hwm[grep("2014",strdate),"jgnd12"]=hwm[grep("2014",strdate),"jg"]/(hwm[which(hwm$date==as.Date("2014-01-01")),"jg"]+hwm[which(hwm$date==as.Date("2014-02-01")),"jg"])
hwm[grep("2015",strdate),"jgnd12"]=hwm[grep("2015",strdate),"jg"]/(hwm[which(hwm$date==as.Date("2015-01-01")),"jg"]+hwm[which(hwm$date==as.Date("2015-02-01")),"jg"])
hwm[grep("2013",strdate),"lynd12"]=hwm[grep("2013",strdate),"ly"]/(hwm[which(hwm$date==as.Date("2013-03-01")),"ly"]+hwm[which(hwm$date==as.Date("2013-02-01")),"ly"])
hwm[grep("2014",strdate),"lynd12"]=hwm[grep("2014",strdate),"ly"]/(hwm[which(hwm$date==as.Date("2014-03-01")),"ly"]+hwm[which(hwm$date==as.Date("2014-02-01")),"ly"])
hwm[grep("2015",strdate),"lynd12"]=hwm[grep("2015",strdate),"ly"]/(hwm[which(hwm$date==as.Date("2015-03-01")),"ly"]+hwm[which(hwm$date==as.Date("2015-02-01")),"ly"])


#2.3 hwm.p--------------------------------------------------------------hwm.p
predict.no=1
#stand for predict
hwm.p=hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]
#stand for training
hwm.t=hwm[1:nrow(hwm)-predict.no,]

usefuldate=hwm[which(hwm$date==as.Date("2014-01-01")):which(hwm$date==as.Date("2015-04-01")),"date"]
cat("The Date We Use in this algorithm\n", as.character(usefuldate),"\n")
hwm.t=hwm[which(hwm$date==as.Date("2014-01-01")):which(hwm$date==as.Date("2015-03-01")),]
hwm.p=hwm[which(hwm$date==as.Date("2015-04-01")):which(hwm$date==as.Date("2015-04-01")),]

cat("The correlation of the hotwords we may select:\n")
cor(hwm.t$arrival,hwm.t[,2:10])
corrgram(hwm.t,order=T,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)


#----------------------------------------------------------------------------------------------
#3# fit and predict

#3.1 pick up the best variables 
#leaps=regsubsets(arrival~ly+tq+jd+lyqz+jg+lvgl+yh+zyx+gwgl+sp+arrival1+arrival12+
#                   holiday+nbmonth+newyear+ctriphotel+ctripreview,data=hwm.t,nbest=3)
leaps=regsubsets(arrival~jd+jg+gwgl.q+gwgl+ly+lyqz+lygl+zyx+arrival1+arrival12+
                   holiday+nbmonth+newyear+ctriphotel+ctripreview,data=hwm.t,nbest=4)

plot(leaps,scale="adjr2")

#3.1 fit--------------------------------------------------------------fit
if(F){
  hwfit=lm(arrival~jg+yh+arrival12+jd+gwgl,hwm.t)#9898
  hwfit=glm(arrival~jg+yh+jd+arrival12+gwgl+newyear,family=gaussian,hwm.t)#
  hwfit=glm(arrival~jg+newyear,family=gaussian,hwm.t)# same wiht lm
    
  hwfit=lm(arrival~jg+jd+yh+newyear,hwm.t)#
  hwfit=lm(arrival~jg+jd+ctriphotel,hwm.t)#22.4
  hwfit=lm(arrival~jg+ctriphotel+newyear,hwm.t)#34.4
  
  hwfit=lm(arrival~jg+newyear,hwm.t)#35.7 best for now
  hwfit=lm(arrival~jg+gwgl.q+ly+newyear,hwm.t)#36.1
  hwfit=lm(arrival~jg+gwgl+ly+newyear,hwm.t)#35.9
  hwfit=lm(arrival~jg+ly+newyear+holiday,hwm.t)#36
  hwfit=lm(arrival~jg+ly+gwgl+newyear,hwm.t)#35.9
  hwfit=lm(arrival~jg+ly+arrival12+newyear,hwm.t)#35.6
  hwfit=lm(arrival~jg+newyear,hwm.t)#35.5
  hwfit=glm(log(arrival)~I(log(jgnd12))+I(log(lynd12))+I(log(m12)),family=gaussian,hwm.t)#12
  hwfit=lm(log(arrival)~log(jg)+log(ly)+newyear,hwm.t)#12
}

hwfit=lm(arrival~jg+ly+newyear,hwm.t)#35.5
hwfit=lm(arrival~jg+ly+newyear,hwm.t)#35.5
cat("The hotwords we use to predict: jiage lvyou newyear\n")
cat("The predic result:",predict(hwfit,newdata=hwm.p),"\n")
summary(hwfit)

percentfit=lm(nd12~jgnd12+lynd12,hwm.t)
predict(percentfit,newdata=hwm.p)*585400
summary(percentfit)

#3.2 predict-----------------------------------------------------------predict
hwm.p$arrival=predict(hwfit,newdata=hwm.p)
hwm.p$mm=hwm.p$arrival/hwm.p$arrival1;hwm.p$yy=hwm.p$arrival/hwm.p$arrival12
hwm.p$nd12=predict(percentfit,newdata=hwm.p)
hwm.p
hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwm.p
hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwm.p
cat("the predicted final value: ",as.character(hwm.p$date), hwm.p$arrival,"\n")

#percent 
plot(hwm$date,hwm$nd12,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(0,2))
legend("topleft",legend=c("arrival nd12", "fitted","jgnd12","lynd12"),lty=c(1,1,1),col=mycolor[c(1,3,5)])
lines(usefuldate,c(fitted(percentfit),hwm.p$nd12),type="b",pty=2,col=mycolor[3])
#lines(hwm$date,hwm$jgnd12,type="b",lty=1,xlab="date",col=mycolor[5],ylim=c(0,3))
#lines(hwm$date,hwm$lynd12,type="b",lty=1,xlab="date",col=mycolor[7],ylim=c(0,3))


plot(hwm$date,hwm$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(0,400000))
legend("topleft",legend=c("official", "predict","jiage","lvyou lagged 1m"),lty=c(1,1,1,1),col=mycolor[c(1,3,5,7)])
lines(usefuldate,c(fitted(hwfit),hwm.p$arrival),type="b",pty=2,col=mycolor[3])
#lines(usefuldate,c(fitted(percentfit)*(585400),hwm.p$nd12*585400),type="b",pty=2,col=mycolor[9])
lines(hwm$date,hwm$jg*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])
lines(hwm$date,hwm$ly*2,type="b",cex=1,lty=1,xlab="date",col=mycolor[7])

if (F){
  lines(hwm$date,hwm$gwgl*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])
  lines(hwm$date,hwm$ctriphotel*30,type="b",cex=1,lty=1,xlab="date",col=mycolor[7])
  lines(hwm$date,hwm$jg*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[4])
  #lines(hwm$date,hwm$yh*10,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])
  #lines(hwm$date,hwm$jd*20,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])
  lines(hwm$date,hwm$gwgl*20,type="b",cex=1,lty=1,xlab="date",col=mycolor[5])
  lines(hwm$date,hwm$sp/3,type="b",cex=1,lty=1,xlab="date",col=mycolor[6])
  
  legend("topleft",legend=c("official", "ctripreview","ctripvisa","ctriphotel","mfwguide","visa"),lty=c(1,1,1),col=mycolor[c(1,11,12,13,14,15)])
  lines(ctripreview$date,ctripreview$tot*40,type="b",cex=1,lty=1,xlab="date",col=mycolor[11])
  lines(ctripvisa$date,ctripvisa$cmtid*2000,type="b",cex=1,lty=1,xlab="date",col=mycolor[12])
  lines(ctriphotel$date,ctriphotel$comment*100,type="b",cex=1,lty=1,xlab="date",col=mycolor[13])
  lines(mfwguide$date,mfwguide$review*4000,type="b",cex=1,lty=1,xlab="date",col=mycolor[14])
  lines(visa$date,visa$user*10000,type="b",cex=1,lty=1,xlab="date",col=mycolor[15])
}

plot(hwm$date,hwm$mm,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(0,3))
legend("topleft",legend=c("arrival mm", "arrival yy","jgmm"),lty=c(1,1,1),col=mycolor[c(1,3,5)])
lines(hwm$date,hwm$yy,type="b",lty=1,xlab="date",col=mycolor[3],ylim=c(0,2))
jg=data.frame(date=hwm$date,jg=hwm$jg,jg1=c(NA, hwm$jg[1:(nrow(hwm)-1)]))
jg$mm=jg$jg/jg$jg1
lines(jg$date,jg$mm,type="b",lty=1,xlab="date",col=mycolor[5],ylim=c(0,2))


d20140102=hwm[which(hwm$date==as.Date("2014-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2014-02-01")),"arrival"]
d20150102=hwm[which(hwm$date==as.Date("2015-01-01")),"arrival"]+hwm[which(hwm$date==as.Date("2015-02-01")),"arrival"]
d201403=hwm[which(hwm$date==as.Date("2014-03-01")),"arrival"]
d201503=hwm[which(hwm$date==as.Date("2015-03-01")),"arrival"]
d201404=hwm[which(hwm$date==as.Date("2014-04-01")),"arrival"]
d201504=hwm[which(hwm$date==as.Date("2015-04-01")),"arrival"]
resmm=data.frame(year=c("2014","2015","delta"),m3dm1am2=c(d201403/d20140102,d201503/d20150102,d201503/(d20150102)-d201403/d20140102),
                 m4dm1am2=c(d201404/d20140102,d201504/d20150102,d201504/d20150102-d201404/d20140102))
resyy=data.frame(year=c("20150102","201503","201504"),delta=c(d20150102/d20140102-1,d201503/d201403-1,d201504/d201404-1))
resmm
resyy


#3.3 evalation--------------------------------------------------------evaluation
mse=sum(((fitted(hwfit)-hwm.t$arrival)/hwm.t$arrival)^2)^(1/2)
cat("lag=",lag," mse=",mse,"\n")
error=(fitted(hwfit)-hwm.t$arrival)/hwm.t$arrival
hwm.t$predict=fitted(hwfit)
hwm.t$error=error
barplot(error,names.arg=hwm.t$date,ylab="ERROR")


jp.p=jp
jp.p[which(jp.p$date==hwm.p$date),"arrival"]=hwm.p$arrival
jp.p=jp.p[40:162,]

plot(jp.p$date,jp.p$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(50000,400000))
legend("topleft",legend=c("japan tourist number"),lty=c(1),col=mycolor[c(1)])

jp.p[,1:2]
hwm.t[,c("date","arrival","predict","error")]
hwm[,c("date","arrival")]



gvmodel=gvlma(hwfit)
summary(gvmodel)

#multicollinearily
sqrt(vif(hwfit))


cor(hwm.t$arrival,hwm.t[,2:ncol(hwm)])
corrgram(hwm.t,order=T,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)

par(mfrow=c(1,1),mar=c(4,4,0,0))

leaps=regsubsets(arrival~qz+jg+jd+arrival1+arrival12+lyqz+yh+gw+sp,data=hwm.t,nbest=5)
leaps=regsubsets(arrival~jg+yh+arrival12+jd+gwgl,data=hwm.t,nbest=2)
leaps=regsubsets(arrival~qz+ly+y+tq+jd+hl+lyqz+jg+dt+lvgl+yh+zyx+gwgl+gw+lyjd+sp+arrival1+arrival12+jpy,data=hwm.t,nbest=3)

leaps=regsubsets(arrival~qz+ly+y+tq+jd+ryfy+hl+lyqz+jg+dt+lvgl+yh+zyx+gwgl+gw+lyjd+sp+arrival1+arrival12+holiday+nbmonth,data=hwm.t,nbest=4)
leaps=regsubsets(arrival~ly+tq+jd+lyqz+jg+lvgl+yh+zyx+gwgl+sp+arrival1+arrival12+holiday+nbmonth+newyear,data=hwm.t,nbest=4)
plot(leaps,scale="adjr2")

save(file="japan.Rdata",japan,jp,hotword,hwm,currency)
load("japan.Rdata")
