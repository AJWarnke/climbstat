rm(list=ls(all=TRUE))
# Set Language
Sys.setenv(LANGUAGE="en")
# Set Working Directory
if(Sys.info()["nodename"]=="LAPTOP-E2DNT490"){
  setwd("C:/Users/Arne/Dropbox/Privat/Sport/Klettern")
  .libPaths("C:/Users/Arne/Dropbox/Work/Computer/R Packages")
}

# Load packages
library("RSQLite")
library(quantreg)
library(RColorBrewer)
library(np)
#
source("Blog/Code/Climb_R_0_Main.r")
#
dat = dat[which(dat$height>0 & dat$weight>0),]
#
dat_w = dat[which(dat$height>=150 & dat$height<=180),]
dat_w = dat_w[which(dat_w$weight>=40 & dat_w$weight<=100),]
dat_w$bmi = dat_w$weight/((dat_w$height/100)^2)
dat_w = subset(dat_w,bmi>18.5 & bmi<=23)
dat_w = subset(dat_w,sex==1)
#
dat_m = dat[which(dat$height>=165 & dat$height<=190),]
dat_m = dat_m[which(dat_m$weight>=40 & dat_m$weight<=100),]
dat_m$bmi = dat_m$weight/((dat_m$height/100)^2)
dat_m = subset(dat_m,bmi>18.5 & bmi<=25)
# For height focus on men only (sex == 0)
dat_m = subset(dat_m,sex==0)
#dat_m = dat_m[which(dat_m$height>=160 & dat_m$height<=195),]
# Get intervals for height
#dat_m$height2 = cut(dat_m$height,10)
dat_m$weight_std = (dat_m$weight-mean(dat_m$weight))/sd(dat_m$weight)
dat_m$bmi_std = (dat_m$bmi-mean(dat_m$bmi))/sd(dat_m$bmi)
dat_m$height_std = (dat_m$height-mean(dat_m$height))/sd(dat_m$height)
#
dat_w$weight_std = (dat_w$weight-mean(dat_w$weight))/sd(dat_w$weight)
dat_w$bmi_std = (dat_w$bmi-mean(dat_w$bmi))/sd(dat_w$bmi)
dat_w$height_std = (dat_w$height-mean(dat_w$height))/sd(dat_w$height)
#
#
#dat_m = dat_m[which(dat_m$height>quantile(dat_m$height,.005) & dat_m$height<quantile(dat_m$height,.995)),]
#dat_m = dat_m[which(dat_m$weight>quantile(dat_m$weight,.005) & dat_m$weight<quantile(dat_m$weight,.995)),]
#
# A data.frame with max ascent by method_id, user_id and year
#dat_m2 = aggregate(grade_index~year+method_id+user_id+sex+age,dat_m,max)
dat_m2 = aggregate(grade_id~method_id+user_id+height+weight+bmi+bmi_std+height_std+weight_std,dat_m,max)
dat_w2 = aggregate(grade_id~method_id+user_id+height+weight+bmi+bmi_std+height_std+weight_std,dat_w,max)
dat_m2 = merge(dat_m2,grades_label,"grade_id",all.x=T)
dat_w2 = merge(dat_w2,grades_label,"grade_id",all.x=T)
# Start only with RP and focus on 7a and higher
dat_m2 = subset(dat_m2,method_id==1 & grade_index2>=7)
dat_m2$grade = factor(dat_m2$grade)
# 
dat_w2 = subset(dat_w2,method_id==1 & grade_index2>=7)
dat_w2$grade = factor(dat_w2$grade)
#
#dat_m2 = dat_m2[which(dat_m2$user_id%in%sample(unique(dat_m2$user_id),5000,replace=F)),]
#
np_bw_m = npregbw(formula=grade_index2~height+bmi,data=dat_m2)
np_bw_w = npregbw(formula=grade_index2~height+bmi,data=dat_w2)
#
np_bw_m2 = npregbw(formula=grade_index2~height+weight,data=dat_m2)
#
model_np_m = npreg(bws=np_bw_m)
model_np_m2 = npreg(bws=np_bw_m2)
model_np_w = npreg(bws=np_bw_w)
#

for(i in seq(0,355,5)){
	eval(parse(text=paste0("png('Blog/graphs/height_bmi/females/females_",i,".png',width = 800, height = 600)")))
	plot(model_np_w,view="fixed",theta=i,xlab="Height (in cm)",ylab="Body Mass Index (BMI)",zlab="Grade (7 = 7a)",main="Height, BMI and Rock Climbing Performance (Females)",cex.lab=1.5,cex.axis=1.3)
	dev.off()
}

for(i in seq(0,355,5)){
	eval(parse(text=paste0("png('Blog/graphs/height_bmi/males/males_",i,".png',width = 800, height = 600)")))
	plot(model_np_m,view="fixed",theta=i,xlab="Height (in cm)",ylab="Body Mass Index (BMI)",zlab="Grade (7 = 7a, 7.5 = 7b+, 8 = 8a)",main="Height, BMI and Rock Climbing Performance (Males)",cex.lab=1.5,cex.axis=1.3)
	dev.off()
}

for(i in seq(0,355,5)){
	eval(parse(text=paste0("png('Blog/graphs/height_weight/males/males_",i,".png',width = 800, height = 600)")))
	plot(model_np_m2,view="fixed",theta=i,xlab="Height (in cm)",ylab="Weight (kg)",zlab="Grade (7 = 7a, 7.5 = 7b+, 8 = 8a)",main="Height, Weight and Rock Climbing Performance (Males)",cex.lab=1.5,cex.axis=1.3)
	dev.off()
}


#par(mar=c(5.1,4.1,4.1,2.1)
dev.new(width=6, height=3)
par(mar=c(4.2,1,1,0.5))
lab <- as.numeric(names(table(dat_m$height)))
lab[which(lab%%5!=0)] = NA
height_plot = barplot(table(dat_m$height),yaxt="n",xaxt="n",xlab="Height (in cm)",space=rep(.5,length(table(dat_m$height))),col="lightblue",cex.lab=1.5)
axis(1,lab,at=height_plot,cex.axis=1.5)

#
dev.new(width=10, height=4)
par(mar=c(4.2,2,2,.5))
plot(NA,xlab="Height (in cm)",yaxt="n",xaxt="n",xlim=c(155,200),ylim=c(16,26),main="Maximum Performance by Height",cex.lab=1.3)
abline(v=seq(155,200,1),lwd=.1,col="lightgrey",lty=3)
abline(h=seq(16,26,1),lwd=.1,col="lightgrey",lty=3)
lines(155:200,tapply(dat_m$grade_index,dat_m$height,max),type="b",pch=16,cex=1.15,col="blue",lwd=1.25)
axis(1,labels=seq(155,200,5),at=seq(155,200,5),cex.axis=1.1)
axis(2,labels=as.character(grades_label[which(grades_label$grade_index%in%c(16:26)),"grade"]),at=16:26,cex.axis=1.1)
text(185,26,pos=4,"Adam Ondra",font=3)
text(170,20,pos=3,"Stefano Ghisolfi",font=3)
arrows(170,21,170,23.5,lwd=.1,lty=1,length=.1)
#lines(175,24,type="p",col="darkred",col="darkred")
text(175,18,pos=3,"Dani Andrada",font=3)
arrows(175,19,175,23.5,lwd=.1,lty=1,length=.1)
text(173,24.5,pos=3,"Magnus Midtbø \n Sachi Amma",font=3)
arrows(173,24.25,173,24.75,lwd=.25,lty=1,length=.1,code=1)
text(180,20,pos=3,"Jernej Kruder",font=3)
arrows(180,21,180,23.5,lwd=.1,lty=1,length=.1)
text(184,18,pos=3,"Pirmin Bertle",font=3)
arrows(184,19,184,23.5,lwd=.1,lty=1,length=.1)
#
dev.new(width=10, height=5)
par(mar=c(4.2,2,2,.5))
plot(NA,xlab="Height (in cm)",yaxt="n",xaxt="n",xlim=c(155,200),ylim=c(4,26),main="Maximum Performance by Height",cex.lab=1.3)
#abline(v=seq(155,200,1),lwd=.1,col="lightgrey",lty=3)
#abline(h=seq(4,26,2),lwd=.1,col="lightgrey",lty=3)
with(dat_m2,lines(jitter(height),jitter(grade_index),cex=.1,col="gray",type="p"))
axis(1,labels=seq(155,200,5),at=seq(155,200,5),cex.axis=1.1)
axis(2,labels=as.character(grades_label[which(grades_label$grade_index%in%c(4:26)),"grade"]),at=4:26,cex.axis=1.1)
rq_curve=rq(grade_index~height+I(height^2)+I(height^3),data=dat_m2,tau=.995)
curve(coef(rq_curve)[1]+coef(rq_curve)[2]*x+coef(rq_curve)[3]*(x^2)+coef(rq_curve)[4]*(x^3),155,200,add=T,col=brewer.pal(9,"Spectral")[9],lwd=2)
text(155,22.75,pos=4,"99.5th percentile",col=brewer.pal(9,"Spectral")[9],font=3)
rq_curve=rq(grade_index~height+I(height^2)+I(height^3),data=dat_m2,tau=.95)
curve(coef(rq_curve)[1]+coef(rq_curve)[2]*x+coef(rq_curve)[3]*(x^2)+coef(rq_curve)[4]*(x^3),155,200,add=T,col=brewer.pal(9,"Spectral")[8],lwd=2)
text(155,20,pos=4,"95th percentile",col=brewer.pal(9,"Spectral")[8],font=3)
rq_curve=rq(grade_index~height+I(height^2)+I(height^3),data=dat_m2,tau=.75)
curve(coef(rq_curve)[1]+coef(rq_curve)[2]*x+coef(rq_curve)[3]*(x^2)+coef(rq_curve)[4]*(x^3),155,200,add=T,col=brewer.pal(9,"Spectral")[7],lwd=2)
text(155,16.75,pos=4,"75th percentile",col=brewer.pal(9,"Spectral")[7],font=3)
lm_curve=lm(grade_index~height+I(height^2)+I(height^3),data=dat_m2)
curve(coef(lm_curve)[1]+coef(lm_curve)[2]*x+coef(lm_curve)[3]*(x^2)+coef(lm_curve)[4]*(x^3),155,200,add=T,col=brewer.pal(9,"Spectral")[4],lwd=5)
text(155,13.25,pos=4,"Average",col=brewer.pal(9,"Spectral")[4],font=3)
rq_curve=rq(grade_index~height+I(height^2)+I(height^3),data=dat_m2,tau=.25)
curve(coef(rq_curve)[1]+coef(rq_curve)[2]*x+coef(rq_curve)[3]*(x^2)+coef(rq_curve)[4]*(x^3),155,200,add=T,col=brewer.pal(9,"Spectral")[3],lwd=2)
text(155,9.5,pos=4,"25th percentile",col=brewer.pal(9,"Spectral")[3],font=3)




nop=npreg(grade_index~height,data=subset(dat_m2,grade_index>=20))


plot(NA,xlab="Height (in cm)",yaxt="n",xaxt="n",xlim=c(160,195),ylim=c(16,26),main="Maximum Performance by Height",cex.lab=1.3)
abline(v=seq(160,195,1),lwd=.1,col="lightgrey",lty=3)
abline(h=seq(16,26,1),lwd=.1,col="lightgrey",lty=3)
lines(160:195,with(subset(dat_m,method_id==3),tapply(grade_index,height,max)),type="b",pch=16,cex=1.15,col="blue",lwd=1.25)
axis(1,labels=seq(160,195,2),at=seq(160,195,2),cex.axis=1.1)
axis(2,labels=as.character(grades_label[which(grades_label$grade_index%in%c(16:26)),"grade"]),at=16:26,cex.axis=1.1)


brewer.pal(5,"Spectral")[5]




plot(tapply(dat_m$grade_index,dat_m$height2,mean),type="b",xlab="Height",yaxt="n",xaxt="n")
axis(1,labels=as.numeric( sub("\\((.+),.*", "\\1", levels(dat_m$height2))),at=1:length(tapply(dat_m$grade_index,dat_m$height2,mean)))
axis(2,labels=as.character(grades_label[which(grades_label$grade_id%in%unique(dat_m$grade_id)),"grade"]),at=1:length(unique(dat_m$grade_id)))

plot(tapply(dat_m$grade_index,dat_m$height,mean),type="b",xlab="Height",yaxt="n",xaxt="n")
axis(1,labels=min(dat_m$height):max(dat_m$height),at=1:length(tapply(dat_m$grade_index,dat_m$height,mean)))
axis(2,labels=as.character(grades_label[which(grades_label$grade_id%in%unique(dat_m$grade_id)),"grade"]),at=1:length(unique(dat_m$grade_id)))

plot(tapply(dat_m$grade_index,dat_m$height,max),type="b",xlab="Height",yaxt="n",xaxt="n")
axis(1,labels=min(dat_m$height):max(dat_m$height),at=1:length(tapply(dat_m$grade_index,dat_m$height,max)))
axis(2,labels=as.character(grades_label[which(grades_label$grade_id%in%unique(dat_m$grade_id)),"grade"]),at=1:length(unique(dat_m$grade_id)))

plot(tapply(dat_m$grade_index,dat_m$height,function(x) quantile(x,.95)),type="b",xlab="Height",yaxt="n",xaxt="n")
axis(1,labels=min(dat_m$height):max(dat_m$height),at=1:length(tapply(dat_m$grade_index,dat_m$height,max)))
axis(2,labels=as.character(grades_label[which(grades_label$grade_id%in%unique(dat_m$grade_id)),"grade"]),at=grades_label[which(grades_label$grade_id%in%unique(dat_m$grade_id)),"grade_id"])




plot(tapply(dat_m$age,dat_m$grade,mean),type="b",ylab="Age",xaxt="n")
axis(1,labels=grades_label$grade,at=1:length(unique(dat_m$grade_id)))

plot(tapply(dat_m$age,dat_m$grade,function(x) quantile(x,.95)),type="b",ylab="Age",xaxt="n")
axis(1,labels=grades_label$grade,at=1:length(unique(dat_m$grade_id)))


plot(tapply(dat_m$grade_index,dat_m$age,max),type="b",xlab="Age",yaxt="n",xaxt="n",ylab="Grade")
axis(2,labels=grades_label$grade,at=grades_label$grade_index)
axis(1,labels=sort(unique(dat_m$age)),at=1:length(unique(dat_m$age)))

plot(tapply(dat_m$grade_index,dat_m$age,function(x) quantile(x,.95)),type="b",xlab="Age",yaxt="n",xaxt="n",ylab="Grade")
axis(2,labels=grades_label$grade,at=grades_label$grade_index)
axis(1,labels=sort(unique(dat_m$age)),at=1:length(unique(dat_m$age)))

plot(tapply(dat_m$grade_index,dat_m$age,function(x) quantile(x,.99)),type="b",xlab="Age",yaxt="n",xaxt="n",ylab="Grade")
axis(2,labels=grades_label$grade,at=grades_label$grade_index)
axis(1,labels=sort(unique(dat_m$age)),at=1:length(unique(dat_m$age)))

plot(tapply(dat_m$grade_index,dat_m$age,median),type="b",xlab="Age",yaxt="n",xaxt="n",ylab="Grade")
axis(2,labels=grades_label$grade,at=grades_label$grade_index)
axis(1,labels=sort(unique(dat_m$age)),at=1:length(unique(dat_m$age)))


k=1
list_hw = list()
for(i in seq(.05,.95,.05)){
	list_hw[[k]] = list()
	list_hw[[k]]=rq(grade_index~height_std+bmi_std,data=dat_m2,tau=i)
	k=k+1
}