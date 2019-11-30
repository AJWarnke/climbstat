grades_label = data.frame(
	"grade_id"=c(13,15,17,21,23,25,29,31,33,36,38,40,42,44,46,49,51,53,55,57,59,62,64,66,68,70,72,75,77,79,81,83),
	"grade"=c("3a","3b","3c","4a","4b","4c","5a","5b","5c","6a","6a+","6b","6b+","6c","6c+","7a","7a+","7b","7b+","7c","7c+","8a","8a+","8b","8b+","8c","8c+","9a","9a+","9b","9b+","9c"),
	"gradeus"=c("","","5.4","5.5","5.6","5.7","5.8","5.9","5.10a","5.10b","5.10c","5.10d","5.11a","5.11b","5.11c","5.11d","5.12a","5.12b","5.12c","5.12d","5.13a","5.13b","5.13c","5.13d","5.14a","5.14b","5.14c","5.14d","5.15a","5.15b","5.15c","5.15d"),
	"grade_index2"=c(3,3+1/3,3+2/3,4,4+1/3,4+2/3,5,5+1/3,5+2/3,6,6+1/6,6+1/3,6.5,6+2/3,6+5/6,7,7+1/6,7+1/3,7.5,7+2/3,7+5/6,8,8+1/6,8+1/3,8.5,8+2/3,8+5/6,9,9+1/6,9+1/3,9.5,9+2/3))
grades_label$grade_index = 1:dim(grades_label)[1]
#
fa = read.table("../data/first_ascents.txt",header=T,sep="\t")
#
rp = subset(fa,style="Redpoint" & grade_x_ascent==1 & ascensionist!="Fred Rouhling")
rp = rp[order(rp$sex,rp$year),]

tmp_x = min(rp$year):max(rp$year)

png("../graphs/rp_time.png",width=1000,height=600)
par(mar=c(10, 12, 4, 6) + 0.1)
plot(min(rp$year):max(rp$year),rep(NA,length(tmp_x)),ylim=c(7,9.7),yaxt="n",xaxt="n",xlab="Year",ylab="",cex.axis=1.5)
axis(1,seq(min(rp$year),max(rp$year),2),at=seq(min(rp$year),max(rp$year),2),las=2)
axis(2,grades_label$gradeus,at=grades_label$grade_index2,las=2,cex.lab=1.35)
abline(h=grades_label$grade_index2,lwd=.1,col="lightgray",lty=2,cex.lab=1.35)
#
with(subset(rp,sex=="male"),lines(year,grade_index2,lwd=2,col="blue",pch=15,lty=3,type="b",cex=1.5))
with(subset(rp,sex=="female"),lines(year,grade_index2,lwd=2,col="red",pch=15,lty=3,type="b",cex=1.5))

par(new=TRUE)
axis(4,grades_label$grade,at=grades_label$grade_index2,las=2)
legend("bottom",c("females","males"),lwd=c(2,2),col=c("red","blue"),lty=c(3,3),cex=c(1.5),pch=c(15,15))
dev.off()
#