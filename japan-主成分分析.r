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

######################################################################
#1# read the arrival data

##read the japan travelling price hotword index
if (F){
  japan=read.csv("japan.csv",header=T,na.strings="",stringsAsFactor=F)
  names(japan)=c("date","bi","arrival","csi")
  japan$date=as.Date(japan$date,"%m/%d/%y")
  head(japan)
  
  japan$bi=as.numeric(japan$bi)
  japan$arrival=as.numeric(japan$arrival)
  str(japan)
  
}
japan=read.xlsx("japan.tourists.number.xlsx",1)
head(japan)

##### Define Predictors - Time Lags;
japan$arrival1 = c(NA, japan$arrival[1:(nrow(japan)-1)]);
japan$arrival12 = c(rep(NA, 12), japan$arrival[1:(nrow(japan)-12)]);
#japan$bi1 = c(NA, japan$bi[1:(nrow(japan)-1)]);
#jp=japan[which(!(is.na(japan$bi)|is.na(japan$arrival)|is.na(japan$arrival1)|is.na(japan$arrival12))),]
#jp=jp[,-which(names(jp)=="csi")]
#jp=japan[which(!(is.na(japan$bi)|is.na(japan$arrival)|is.na(japan$arrival1)|is.na(japan$arrival12))),]
jp=japan
head(jp)

# read the currency
currency=read.xlsx("currency.xlsx",1)
head(currency)
currency=currency[order(currency$DATE),]
lines(currency$DATE,1/currency$JPY*1000000)




  ######################################################################
  #2# read the 3 focused hotword data
  #fhw=read.xlsx("fucused.3hotwords.2015.03.xlsx",1);
  hotword=read.csv("japan-hotword.csv",header=T,na.strings="",stringsAsFactor=F)
  hotword$date=as.Date(hotword$date,"%m/%d/%y")
  hotword$sumplace=rowSums(hotword[,which(names(hotword)=="东京"):ncol(hotword)])
  head(hotword)
  plot(hotword$date,hotword$sumplace)


# main factor analysis
tail(hotword)
mfa=hotword[,-which(names(hotword)=="日本翻译官")]
#fa.parallel(mfa[,-1],fa="PC",n.iter=300,show.legend=F,main="Scree plot with parallel analysis")
rc=principal(mfa[,-1],nfactor=3,rotate="varimax")
w123=rc$weight
pc1=apply(mfa[,-1],1,function(x){sum(x*w123[,1])})
pc2=apply(mfa[,-1],1,function(x){sum(x*w123[,2])})
pc3=apply(mfa[,-1],1,function(x){sum(x*w123[,3])})
hotword$pc1=pc1
hotword$pc2=pc2
hotword$pc3=pc3
head(hotword)



  #fhw=hotword[,c(1:6,8:17,ncol(hotword))]
  fhw=hotword[,c(1:6,8:17,ncol(hotword)-3,ncol(hotword)-2,ncol(hotword)-1,ncol(hotword))]
  names(fhw)=c("date","qz","ly","y","tq","jd","hl","lyqz","jg","dt","lvgl","yh","zyx","gwgl","gw","lyjd","sp","pc1","pc2","pc3")
  head(fhw)
  
  
  lag=0
  hwm=fhw[1,]
  hwm$jpy=0
  

  date=c(as.Date("2014-04-01"),as.Date("2014-05-01"),as.Date("2014-06-01"),
         as.Date("2014-07-01"),as.Date("2014-08-01"),as.Date("2014-09-01"),
         as.Date("2014-10-01"),as.Date("2014-11-01"),as.Date("2014-12-01"),
         as.Date("2015-01-01"),as.Date("2015-02-01"))
  date=c(as.Date("2014-04-01"),as.Date("2014-05-01"),as.Date("2014-06-01"),
         as.Date("2014-07-01"),as.Date("2014-08-01"),as.Date("2014-09-01"),
         as.Date("2014-10-01"),as.Date("2014-11-01"),as.Date("2014-12-01"),
         as.Date("2015-01-01"),as.Date("2015-02-01"),as.Date("2015-03-01"))

  for (i in 1:length(date)){
    
    #read the jp table
    dpm=jp[which(jp$date==as.Date(date[i])),"dpm"]
    #dpm=30
    
    #read the fhw table
    location=which(fhw$date==as.Date(date[i]))
    begin=max(1,location-lag)
    end=min(nrow(fhw),begin+dpm-1)
    
    hwm[i,"date"]=date[i]
    hwm[i,2:ncol(fhw)]=colSums(fhw[begin:end,2:ncol(fhw)])
    
    #read the currency table
    month=substr(as.character(date[i]),1,7)
    jpy=currency[grep(month,as.character(currency$DATE))[1],"JPY"]
    jpy=1/jpy*100
    hwm[i,"jpy"]=jpy
  }
  
  hwm$arrival=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"arrival"]
  hwm$arrival1=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"arrival1"]
  hwm$arrival12=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"arrival12"]
  
  
  predict.no=1
  
  #stand for predict
  hwm.p=hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]
  #stand for training
  hwm.t=hwm[1:nrow(hwm)-predict.no,]
  
  # best 1
  #hwfit=lm(arrival~qz+lyjg+jd+arrival1+arrival12,hwm.t) #share baidu fit
  # best 2
  
  hwfit=lm(arrival~jg+yh+arrival12+jd+gwgl,hwm.t)#9898
  
  
  
  summary(hwfit)
  mse=sum(((fitted(hwfit)-hwm.t$arrival)/hwm.t$arrival)^2)^(1/2)
  cat("lag=",lag," mse=",mse,"\n")
  error=(fitted(hwfit)-hwm.t$arrival)/hwm.t$arrival
  hwm.t$predict=fitted(hwfit)
  hwm.t$error=error
  
  par(mfrow=c(1,1),mar=c(4,4,0,0))
  mycolor=rainbow(10)
  barplot(error,names.arg=hwm.t$date,ylab="ERROR")
  #  png(file="error.png")
  #  dev.off()
  plot(hwm.t$date,hwm.t$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(50000,400000))
  legend("bottomright",legend=c("official", "predict"),lty=c(1,1),col=mycolor[c(1,2)])
  lines(hwm.t$date,fitted(hwfit),type="b",pty=2,col=mycolor[2])
  
  hwm.p$arrival=predict(hwfit,newdata=hwm.p)
  hwm.p
  hwm[(nrow(hwm)-predict.no+1):nrow(hwm),]=hwm.p
  
  mycolor=rainbow(10)
  plot(hwm$date,hwm$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(50000,400000))
  legend("bottomright",legend=c("official", "predict"),lty=c(1,1),col=mycolor[c(1,2)])
  lines(hwm$date,c(fitted(hwfit),hwm.p$arrival),type="b",pty=2,col=mycolor[2])
  
  jp.p=jp
  jp.p[which(jp.p$date==as.Date("2015-03-01")),"arrival"]=hwm.p$arrival
  jp.p=jp.p[70:162,]
  
  plot(jp.p$date,jp.p$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(50000,400000))
  legend("topleft",legend=c("japan tourist number"),lty=c(1),col=mycolor[c(1)])
  
  jp.p[,1:2]
  hwm.t[,c("date","arrival","predict","error")]
  hwm[,c("date","arrival")]
  
  
  
  plot(hwm$date,hwm$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(50000,500000))
  lines(hwm$date,hwm$jg*14,type="b",lty=1,xlab="date",col=mycolor[2])
  lines(hwm$date,hwm$yh*8.4,type="b",lty=1,xlab="date",col=mycolor[3])
  lines(hwm$date,hwm$arrival12,type="b",lty=1,xlab="date",col=mycolor[4])
  lines(hwm$date,hwm$jd*11,type="b",lty=1,xlab="date",col=mycolor[6])
  lines(hwm$date,hwm$gwgl*5,type="b",lty=1,xlab="date",col=mycolor[7])
  lines(hwm$date,hwm$jpy*10000,type="b",lty=1,xlab="date",col=mycolor[9])
  legend("topleft",legend=c("当月日本旅游人数", "日本旅游价格指数","日本樱花指数","12月前人数","日本酒店指数","日本购物攻略指数","汇率"),lty=c(1,1),col=mycolor[c(1,2,3,4,6,7,9)])


lines(hwm$date,hwm$pc1*10,type="b",lty=1,xlab="date")
lines(hwm$date,hwm$pc2,type="b",lty=1,xlab="date")
lines(hwm$date,hwm$pc3*10,type="b",lty=1,xlab="date")



  
  #test

  hwfit=lm(arrival~jg+yh+arrival12+jd,hwm.t)#9898
  hwfit=lm(arrival~jg+yh+jd+gwgl,hwm.t)#9898
  hwfit=lm(arrival~jg+yh+arrival12,hwm.t)#9898
  hwfit=lm(arrival~pc1++pc2+pc3+arrival12,hwm.t)#9898 
  
hwfit=lm(arrival~jg+yh+arrival12+jd+gwgl,hwm.t)#9898


  summary(hwfit)
  confint(hwfit)
    
  # judge the assumption
  par(mfrow=c(2,2),mar=c(4,4,0,0))
  plot(hwfit)
  
  gvmodel=gvlma(hwfit)
  summary(gvmodel)
  
  #multicollinearily
  sqrt(vif(hwfit))
  
  
  cor(hwm$arrival,hwm[,2:ncol(hwm)])
  corrgram(hwm,order=T,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)
    
  par(mfrow=c(1,1),mar=c(4,4,0,0))
  
  leaps=regsubsets(arrival~qz+jg+jd+arrival1+arrival12+lyqz+yh+gw+sp,data=hwm,nbest=5)
  leaps=regsubsets(arrival~jg+yh+arrival12+jd+gwgl,data=hwm,nbest=2)
  leaps=regsubsets(arrival~qz+ly+y+tq+jd+hl+lyqz+jg+dt+lvgl+yh+zyx+gwgl+gw+lyjd+sp+arrival1+arrival12+jpy,data=hwm,nbest=3)
  plot(leaps,scale="adjr2")

  save(file="japan.Rdata",japan,jp,hotword,hwm,currency)
  load("japan.Rdata")









######################################################################
#2# select the best factor
## read the all of the hot word in the recent year
if (T){
  hotword=read.csv("japan-hotword.csv",header=T,na.strings="",stringsAsFactor=F)
  hotword$date=as.Date(hotword$date,"%m/%d/%y")
  hotword$sumplace=rowSums(hotword[,which(names(hotword)=="东京"):ncol(hotword)])
  head(hotword)
  plot(hotword$date,hotword$sumplace)
  hw=hotword[,c(1:6,8:17,ncol(hotword))]
  names(hw)=c("date","qz","ly","y","tq","jd","hl","lyqz","jg","dt","lvgl","yh","zyx","gwgl","gw","lyjd","sp")
  head(hw)
  
  
  
  ## change the daily date to monthly with different LAG
  ## lag 1 day means month=(30+1+.....29)/30
  ## lag 2 days means month=(29+30+1+2+....28)/30
  lag=0
  
  for (lag in 0:0) {
    
    hwm=hw[1,]
    hwm$jpy=0
    
    date=c(as.Date("2014-04-01"),as.Date("2014-05-01"),as.Date("2014-06-01"),
           as.Date("2014-07-01"),as.Date("2014-08-01"),as.Date("2014-09-01"),
           as.Date("2014-10-01"),as.Date("2014-11-01"),as.Date("2014-12-01"),
           as.Date("2015-01-01"),as.Date("2015-02-01"))
    
    for (i in 1:length(date)){
      location=which(hw$date==as.Date(date[i]))
      begin=max(1,location-lag)
      end=min(nrow(hw),begin+30)
      hwm[i,"date"]=date[i]
      hwm[i,2:ncol(hw)]=colSums(hw[begin:end,2:ncol(hw)])/(end-begin+1)
    }
    
    
    for (i in 1:length(date)){
      dpm=jp[which(jp$date==as.Date(date[i])),"dpm"]
      location=which(hw$date==as.Date(date[i]))
      begin=max(1,location-lag)
      end=min(nrow(hw),begin+dpm-1)
      
      hwm[i,"date"]=date[i]
      hwm[i,2:ncol(hw)]=colSums(hw[begin:end,2:ncol(hw)])
      
      
      #read the currency table
      month=substr(as.character(date[i]),1,7)
      
      jpy=currency[grep(month,as.character(currency$DATE))[1],"JPY"]
      jpy=1/jpy*100
      hwm[i,"jpy"]=jpy

    }
    
    
    
    
    hwm$arrival=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"arrival"]
    hwm$arrival1=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"arrival1"]
    hwm$arrival12=jp[which(jp$date==date[1]):(which(jp$date==date[1])+length(date)-1),"arrival12"]
    
    
    #hwfit=lm(arrival~qz+ly+y+tq+jd+hl+lyqz+jg+dt+lvgl+yh+zyx+gwgl+gw+lyjd+sp+arrival1+arrival12,hwm) #share baidu fit
    #hwfit=lm(log(arrival)~log(jg)+log(arrival1)+log(arrival12),hwm) #share baidu fit
    #sp not effective
    #hwfit=lm(arrival~qz+jg+jd+sp+arrival1+arrival12,hwm) #share baidu fit
    #huilv not xianzhu
    #hwfit=lm(arrival~y+jg+dt+sp+arrival1+arrival12,hwm) #share baidu fit
    
    hwfit=lm(arrival~lyqz+jg+yh+gwgl+gw+sp+arrival1+arrival12,hwm) #share baidu fit
    
    
    hwfit=lm(arrival~qz+jg+jd+arrival1+arrival12,hwm) #share baidu fit
    hwfit=lm(arrival~qz+jg+jd+arrival1+arrival12+sp+lyqz+yh+gw,hwm) #9983
    hwfit=lm(arrival~qz+jg+jd+arrival1+arrival12+lyqz+yh+gw,hwm) #9925
    hwfit=lm(arrival~jg+jd+arrival1+arrival12+yh+gw,hwm)#991
    hwfit=lm(arrival~qz+jg+jd+arrival1+arrival12+yh+gw,hwm)#9921
    hwfit=lm(arrival~qz+jg+jd+arrival1+arrival12+yh+gw+sp,hwm)#9981
    hwfit=lm(arrival~qz+jg+jd+arrival12+yh+gw+sp,hwm)#9981
    hwfit=lm(arrival~jg+yh+arrival12+jd+gwgl+jpy,hwm)#currency 9901
    
    #best series
    hwfit=lm(arrival~jg+yh+arrival12+jd+gw,hwm)#971
    # for now the best
    hwfit=lm(arrival~jg+yh+arrival12+jd+gwgl,hwm)#9898
    
    
    
    
    summary(hwfit)
    confint(hwfit)
    
   
    
    # judge the assumption
    par(mfrow=c(2,2),mar=c(4,4,0,0))
    plot(hwfit)
    
    gvmodel=gvlma(hwfit)
    summary(gvmodel)
    
    #multicollinearily
    sqrt(vif(hwfit))
    
    
    
    
    
    mse=sum(((fitted(hwfit)-hwm$arrival)/hwm$arrival)^2)^(1/2)
    cat("lag=",lag," mse=",mse,"\n")
    error=(fitted(hwfit)-hwm$arrival)/hwm$arrival
    error
    
    par(mfrow=c(1,1),mar=c(4,4,0,0))
    mycolor=rainbow(10)
    barplot(error,names.arg=date)
    #  png(file="error.png")
    #  dev.off()
    plot(hwm$date,hwm$arrival,type="b",lty=1,xlab="date",col=mycolor[1],ylim=c(50000,400000))
    legend("topright",legend=c("official", "predict"),lty=c(1,1),col=mycolor[c(1,2)])
    lines(hwm$date,fitted(hwfit),type="b",pty=2,col=mycolor[2])
  }
  
  cor(hwm$arrival,hwm[,2:ncol(hwm)])
  corrgram(hwm,order=T,lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)
  
  
  par(mfrow=c(1,1),mar=c(4,4,0,0))
  leaps=regsubsets(arrival~qz+ly+y+tq+jd+hl+lyqz+jg+dt+lvgl+yh+zyx+gwgl+gw+lyjd+sp+arrival1+arrival12+jpy,data=hwm,nbest=3)
  leaps=regsubsets(arrival~qz+jg+jd+arrival1+arrival12+lyqz+yh+gw+sp,data=hwm,nbest=5)
  leaps=regsubsets(arrival~jg+yh+arrival12+jd+gwgl,data=hwm,nbest=2)
  plot(leaps,scale="adjr2")
  
  save(file="japan.Rdata",japan,hotword,hwm,jp)
  load("japan.Rdata")
  
}


##########################################
#2# define the macro
no.dim=dim(index)
no.var=no.dim[2]
no.sample=no.dim[1]

#define the collom position
begin=6
end=no.var
no.index=3
no.set=(end-begin+1)/no.index #18

#turn off the data to numeric
for (i in begin:end){
  for (j in 1:no.sample){
    # check the non numeric
    if (!is.numeric(index[j,i])){
      cat(i,j,index[j,i],"\n")
      index[j,i]=as.numeric(index[j,i])
    }

    if (is.na(index[j,i])){
      cat(i,j,index[j,i],"\n")
      if ((j==1)) index[j,i]=index[j+1,i]
      if (!(j==1)) index[j,i]=index[j-1,i]
    }
    
    if (index[j,i]<(mean(index[,i])/100)){
      cat(i,j,index[j,i],mean(index[,i]),"\n")
      index[j,i]=mean(index[,i])
    }
  }
  #cat(mean(index[,i]))
}


##### 04HUFZEEPr
index=index[order(index$Date),]
# sum.baidu
sum.baidu=apply(index[seq(begin,end-2,no.index)],1,mean)
index=cbind(index,sum.baidu)

sum.qh=apply(index[seq(begin+1,end-1,no.index)],1,mean)
index=cbind(index,sum.qh)

sum.sogou=apply(index[seq(begin+2,end,no.index)],1,mean)
index=cbind(index,sum.sogou)

end=end+3
no.var=no.var+3



##########################################
#3# define the switch
period=7
lm.no=29
test.no=floor(no.sample/period)-lm.no

#############################################
#4# turn the data according to week and month
# calculate index in 7 day mean:period index


# smooth the data to 7 or 30 days average
period.index=index[1:floor(no.sample/period),]
for (i in 1:floor(no.sample/period)){
  location=(i-1)*period+1
  period.index[i,c(1,2)]=index[location,c(1,2)]
  period.index[i,3:end]=apply(index[location:(location+period-1),3:end],2,mean)
  
}

#produce the test data set for evaluation
tindex=period.index[(lm.no+1):(lm.no+test.no),]
pindex=period.index[1:lm.no,]

##########################################
#5# have a look at the data

if (0){
 
  opar=par(no.readonly=TRUE)
  op <- par(mar = rep(0, 4))   
  
  target=pindex#define the target data
  for (i in 1:(no.set+1)){
    par(mfrow=c(7,1),mar=c(4,4,0,0))
    mycolor=rainbow(10)
    position=begin+(i-1)*no.index
    plot(target$Date,target$baidupc,type="b",col=mycolor[1],ylab="baidu pc")
    plot(target$Date,target$sb,type="b",col=mycolor[2],ylab="baidu share")
    plot(target$Date,target$sq,type="b",col=mycolor[3],ylab="360 share")
    plot(target$Date,target$ss,type="b",col=mycolor[4],ylab="sogou share")
    plot(target$Date,target[,begin+(i-1)*no.index],type="b",col=mycolor[5],xlab="baidu Date",ylab=names(index)[begin+(i-1)*no.index])
    plot(target$Date,target[,begin+(i-1)*no.index+1],type="b",col=mycolor[6],xlab="360 Date",ylab=names(index)[begin+(i-1)*no.index+1])
    plot(target$Date,target[,begin+(i-1)*no.index+2],type="b",col=mycolor[7],xlab="sogou Date",ylab=names(index)[begin+(i-1)*no.index+2])
    #plot(target$Date[c(1:60)],target$zhaopinb[c(1:60)],type="b",col=mycolor[6])
    #abline(index[2],zhaopinb)
    #abline(index[2],zhaopinq)
    #abline(index[2],zhaopins)
    #cor(target$baidupc,target$sb)
  
    if (0){
      jpeg(paste(i,".jpg",sep=""))
      #dev.off
    }
    dev.new()
  }
  
  par(opar)
}


################################################
#11# the real prediction with percent+LM method

#10.1 scale the data : sindex stand for the scaled index
sindex=period.index[,c(2,3,4,5,end-2,end-1,end)]
#use the history market share
weeks=dim(sindex)[1];

#####################may be this line should be modified################
sindex[2:weeks,c("sb","sq","ss")]=sindex[1:(weeks-1),c("sb","sq","ss")]

# scaled
if (1) {
  sindex$sum.baidu=scale(sindex$sum.baidu,scale=TRUE,center=TRUE)+10
  sindex$sum.qh=scale(sindex$sum.qh,scale=TRUE,center=TRUE)+10
  sindex$sum.sogou=scale(sindex$sum.sogou,scale=TRUE,center=TRUE)+10
}

sindex[length(sindex)+1]=sindex$sb*sindex$sum.baidu/(sindex$sb*sindex$sum.baidu+sindex$sq*sindex$sum.qh+sindex$ss*sindex$sum.sogou)
names(sindex)[length(sindex)]="psb" #predict percent of baidu
sindex[length(sindex)+1]=sindex$sq*sindex$sum.qh/(sindex$sb*sindex$sum.baidu+sindex$sq*sindex$sum.qh+sindex$ss*sindex$sum.sogou)
names(sindex)[length(sindex)]="psq" #predict percent of baidu
sindex[length(sindex)+1]=sindex$ss*sindex$sum.sogou/(sindex$sb*sindex$sum.baidu+sindex$sq*sindex$sum.qh+sindex$ss*sindex$sum.sogou)
names(sindex)[length(sindex)]="pss" #predict percent of baidu

# means the final predict result
sindex[length(sindex)+1]=sindex$sb
names(sindex)[length(sindex)]="fsb" #predict percent of baidu
sindex[length(sindex)+1]=sindex$sq
names(sindex)[length(sindex)]="fsq" #predict percent of baidu
sindex[length(sindex)+1]=sindex$ss
names(sindex)[length(sindex)]="fss" #predict percent of baidu

#produce the test data set for evaluation
stindex=sindex[42:50,]#41
spindex=sindex[1:41,]#10
#produce the test data set for evaluation
stindex=sindex[(lm.no+1):(lm.no+test.no),]
spindex=sindex[1:lm.no,]


# start the linear migrite with spindex
sbfit=lm(sb~psb,spindex) #share baidu fit
summary(sbfit)
sqfit=lm(sq~psq,spindex) #share baidu fit
ssfit=lm(ss~pss,spindex) #share baidu fit


for (i in 1:nrow(stindex)){

  stindex[i,"fsb"]=predict(sbfit,newdata=stindex[i,])
  stindex[i,"fsq"]=predict(sqfit,newdata=stindex[i,])
  stindex[i,"fss"]=predict(ssfit,newdata=stindex[i,])
  
  #scaled to 1
  sumfs=sum(stindex[i,c("fsb","fsq","fss")])
  stindex[i,c("fsb","fsq","fss")]=stindex[i,c("fsb","fsq","fss")]/sumfs
    
  #update the information
  if (!i==nrow(stindex)) {
    #------------------------------------------------------------------#
    #update sb sq ss    #this is the key assumption
    
    stindex[i+1,c("sb","sq","ss")]=stindex[1,c("sb","sq","ss")]
    stindex[i+1,c("sb","sq","ss")]=stindex[i,c("fsb","fsq","fss")]
    
    #------------------------------------------------------------------#
        
    #recalculate the psb, psq,pss 
    stindex[i+1,c("psb","psq","pss")]=stindex[i+1,c("sb","sq","ss")]*stindex[i+1,c("sum.baidu","sum.qh","sum.sogou")]/sum(stindex[i+1,c("sb","sq","ss")]*stindex[i+1,c("sum.baidu","sum.qh","sum.sogou")])
  }
}



par(mfrow=c(3,2),mar=c(4,4,0,0))
mycolor=rainbow(10)
plot(spindex$Date,spindex$sb,type="b",col=mycolor[1],lty=1,ylab="share baidu",xlab=paste(lm.no,"weeks LM"),ylim=c(0.4,0.8))
legend("topright",legend=c("real", "percent", "LM"),lty=c(1,1,1),col=mycolor[c(1,2,8)])
lines(spindex$Date,spindex$psb,type="l",col=mycolor[2])
lines(spindex$Date,fitted(sbfit),type="l",col=mycolor[8])
plot(stindex$Date,stindex$fsb,type="p",col=mycolor[1],ylim=c(0.4,0.8),ylab="share baidu",xlab=paste(test.no,"weeks LM"))
lines(stindex$Date,stindex$psb,type="l",col=mycolor[2])

plot(spindex$Date,spindex$sq,type="b",col=mycolor[1],lty=1,ylab="share 360",xlab=paste(lm.no,"weeks LM"),ylim=c(0.1,0.4))
legend("topright",legend=c("real", "percent", "LM"),lty=c(1,1,1),col=mycolor[c(1,2,8)])
lines(spindex$Date,spindex$psq,type="l",col=mycolor[2])
lines(spindex$Date,fitted(sqfit),type="l",col=mycolor[8])
plot(stindex$Date,stindex$fsq,type="p",col=mycolor[1],ylim=c(0.1,0.4),ylab="share 360",xlab=paste(test.no,"weeks LM"))
lines(stindex$Date,stindex$psq,type="l",col=mycolor[2])

plot(spindex$Date,spindex$ss,type="b",col=mycolor[1],lty=1,ylab="share sogou",xlab=paste(lm.no,"weeks LM"),ylim=c(0,0.2))
legend("topright",legend=c("real", "percent", "LM"),lty=c(1,1,1),col=mycolor[c(1,2,8)])
lines(spindex$Date,spindex$pss,type="l",col=mycolor[2])
lines(spindex$Date,fitted(ssfit),type="l",col=mycolor[8])
plot(stindex$Date,stindex$fss,type="p",col=mycolor[1],ylim=c(0,0.2),ylab="share sogou",xlab=paste(test.no,"weeks LM"))
lines(stindex$Date,stindex$pss,type="l",col=mycolor[2])

print("The predict shares are as followed")
print(stindex[c("fsb","fsq","fss")])
print(stindex[c("Date","fsb","fsq","fss")])                                                                                   
