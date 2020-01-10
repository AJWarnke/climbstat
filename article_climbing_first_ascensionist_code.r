grades_label = data.frame(
	"grade_id" = c(13,15,17,21,23,25,29,31,33,36,38,40,42,44,46,49,51,53,55,57,59,62,64,66,68,70,72,75,77,79,81,83),
	"grade" = c("3a","3b","3c","4a","4b","4c","5a","5b","5c","6a","6a+","6b","6b+","6c","6c+","7a","7a+","7b","7b+","7c","7c+","8a","8a+","8b","8b+","8c","8c+","9a","9a+","9b","9b+","9c"),
	"gradeus" = c("","","5.4","5.5","5.6","5.7","5.8","5.9","5.10a","5.10b","5.10c","5.10d","5.11a","5.11b","5.11c","5.11d","5.12a","5.12b","5.12c","5.12d","5.13a","5.13b","5.13c","5.13d","5.14a","5.14b","5.14c","5.14d","5.15a","5.15b","5.15c","5.15d"),
	"grade_index2" = c(3,3+1/3,3+2/3,4,4+1/3,4+2/3,5,5+1/3,5+2/3,6,6+1/6,6+1/3,6.5,6+2/3,6+5/6,7,7+1/6,7+1/3,7.5,7+2/3,7+5/6,8,8+1/6,8+1/3,8.5,8+2/3,8+5/6,9,9+1/6,9+1/3,9.5,9+2/3))
grades_label$grade_index = 1:dim(grades_label)[1]
#
fa = read.table("../data/first_ascents.txt", header = TRUE, sep = "\t", encoding = "UTF-8")
#
rp = subset(fa,style = "Redpoint" & grade_x_ascent == 1 & ascensionist != "Fred Rouhling")
rp = rp[order(rp$sex,rp$year), ]
#
rp = rbind(rp,rep(NA, dim(rp)[2]))
rp[dim(rp)[1], "year"] = as.numeric(format(Sys.time(), "%Y"))
#
tmp_x = min(rp$year):max(rp$year)
#
# Export data
export_data = rp[, c("year", "ascensionist", "route", "crag", "gradeus", "grade", "sex", "confirmed", "source")]
export_data$source = as.character(export_data$source)
export_data = export_data[!is.na(export_data$ascensionist), ]
export_data[which(grepl("Fernandez", export_data$ascensionist)), "source"] = "https://www.hardclimbs.info/climbs/chilam-balam"
export_data[which(export_data$year == 2012), "source"] = "https://www.planetmountain.com/en/news/interviews/adam-ondra-makes-his-change.html"
export_data[which(export_data$year == 2017), "source"] = "https://www.planetmountain.com/en/news/interviews/adam-ondra-climbs-worlds-first-9c-at-flatanger-in-norway.html"
write.csv(export_data, file = "../data/ascensionist_table.csv", row.names = FALSE)
########################################
#### One graph 
png(paste0("../graphs/ascensionists_over_time.png"),width=1000,height=600)
	par(mar=c(8, 6, 4, 4) + 0.1)
	plot((min(rp$year)):(max(rp$year)), rep(NA,length(tmp_x)), ylim=c(6.9, 9.75), yaxt="n", xaxt="n", xlab="", ylab="", cex.axis=1.5)
	mtext(side=3, line=2, cex=1.7, "First Ascensionists over Time (Females and Males)")
	mtext(side=3, line=0.5, cex=1, "climbstat.blogspot.com", font=3)	
	mtext(side = 1, line=5, cex=1.5, "Year")
	mtext(side = 2, line=4, cex=1.5, "Grades (YDS)")
	axis(1,seq(min(rp$year),max(rp$year),2),at=seq(min(rp$year),max(rp$year),2),las=2)
	axis(2,grades_label$gradeus,at=grades_label$grade_index2,las=2,cex.lab=1.35)
	abline(h=grades_label$grade_index2,lwd=.1,col="lightgray",lty=2,cex.lab=1.35)
	#
	with(subset(rp,sex=="male"),lines(year,grade_index2,lwd=2,col="blue",pch=15,lty=3,type="b",cex=1.5))
	with(subset(rp,sex=="female"),lines(year,grade_index2,lwd=2,col="red",pch=15,lty=3,type="b",cex=1.5))

	par(new=TRUE)
	axis(4,grades_label$grade,at=grades_label$grade_index2,las=2)
	legend("bottom",c("males","females"),lwd=c(2,2),col=c("blue","red"),lty=c(3,3),cex=c(1.5),pch=c(15,15))
	corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
	par(xpd = TRUE) #Draw outside plot area
	text(x = corners[2] + 3.75, y = mean(corners[3:4]), "Grades (French)", srt = 270, cex = 1.5)
dev.off()
########################################
#### Males for GIF
k=1
for(i in 1:dim(rp[which(rp$sex == "male"), ])[1]) {
	png(paste0("../graphs/ascensionists/male_",k,".png"),width=1000,height=600)
		par(mar=c(8, 6, 4, 4) + 0.1)
		plot((min(rp$year)):(max(rp$year)), rep(NA,length(tmp_x)), ylim=c(6.9, 9.75), yaxt="n", xaxt="n", xlab="", ylab="", cex.axis=1.5)
		mtext(side=3, line=2, cex=1.7, "First Ascensionists over Time (Males)")
		mtext(side=3, line=0.5, cex=1, "climbstat.blogspot.com", font=3)	
		mtext(side = 1, line=5, cex=1.5, "Year")
		mtext(side = 2, line=4, cex=1.5, "Grades (YDS)")
		axis(1,seq(min(rp$year),max(rp$year),2),at=seq(min(rp$year),max(rp$year),2),las=2)
		axis(2,grades_label$gradeus,at=grades_label$grade_index2,las=2,cex.lab=1.35)
		abline(h=grades_label$grade_index2,lwd=.1,col="lightgray",lty=2,cex.lab=1.35)
		#
		with(subset(rp,sex=="male"),lines(year,grade_index2,lwd=2,col="blue",pch=15,lty=3,type="b",cex=1.5))
		lines(rp[which(rp$sex == "male"), ][k,"year"], rp[which(rp$sex == "male"), ][k,"grade_index2"], col="orange", pch=15, type="p", cex=2)
		#with(subset(rp,sex=="female"),lines(year,grade_index2,lwd=2,col="red",pch=15,lty=3,type="b",cex=1.5))

		par(new=TRUE)
		axis(4,grades_label$grade,at=grades_label$grade_index2,las=2)
		#legend("bottom",c("males","females"),lwd=c(2,2),col=c("blue","red"),lty=c(3,3),cex=c(1.5),pch=c(15,15))
		#
		#with(subset(rp, route=="Chilam Balam"), text(year, grade_index2, labels = ascensionist, pos=3))
		#with(subset(rp, route=="Change"), text(year, grade_index2, labels = ascensionist, pos=3))
		#with(subset(rp, route=="Silence"), text(year, grade_index2, labels = ascensionist, pos=3))
		#with(subset(rp, route=="Action Directe"), text(year, grade_index2, labels = ascensionist, pos=3))
		if(k == dim(rp[which(rp$sex == "male"), ])[1]){
			text(rp[which(rp$sex == "male"), ][k,"year"] - 1, rp[which(rp$sex == "male"), ][k,"grade_index2"], 
				labels = paste0(rp[which(rp$sex == "male"), ][k,"ascensionist"]," ","(",rp[which(rp$sex == "male"), ][k,"grade"]," ",rp[which(rp$sex == "male"), ][k,"year"],")"), pos=2, cex = 1.25)
		} else if (k == 1) {
			text(rp[which(rp$sex == "male"), ][k,"year"] + 5, rp[which(rp$sex == "male"), ][k,"grade_index2"], 
				labels = paste0(rp[which(rp$sex == "male"), ][k,"ascensionist"]," ","(",rp[which(rp$sex == "male"), ][k,"grade"]," ",rp[which(rp$sex == "male"), ][k,"year"],")"), pos=1, cex = 1.25)				
		} else if ((k == 2) | grepl("Rouhling", rp[which(rp$sex == "male"), ][k,"ascensionist"])) {
			text(rp[which(rp$sex == "male"), ][k,"year"], rp[which(rp$sex == "male"), ][k,"grade_index2"] + .05, 
				labels = paste0(rp[which(rp$sex == "male"), ][k,"ascensionist"],"\n","(",rp[which(rp$sex == "male"), ][k,"grade"]," ",rp[which(rp$sex == "male"), ][k,"year"],")"), pos=3, cex = 1.25)
		} else {
			text(rp[which(rp$sex == "male"), ][k,"year"]+.25, rp[which(rp$sex == "male"), ][k,"grade_index2"]-.25, 
				labels = paste0(rp[which(rp$sex == "male"), ][k,"ascensionist"],"\n","(",rp[which(rp$sex == "male"), ][k,"grade"]," ",rp[which(rp$sex == "male"), ][k,"year"],")"), pos=4, cex = 1.25)
		}
		corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
		par(xpd = TRUE) #Draw outside plot area
		text(x = corners[2] + 3.75, y = mean(corners[3:4]), "Grades (French)", srt = 270, cex = 1.5)
	dev.off()
	k = k + 1
}
#
########################################
# Females for GIF
k=1
for(i in 1:dim(rp[which(rp$sex == "female"), ])[1]) {
	png(paste0("../graphs/ascensionists/female_",k,".png"),width=1000,height=600)
		par(mar=c(8, 6, 4, 4) + 0.1)
		plot((min(rp$year)):(max(rp$year)), rep(NA,length(tmp_x)), ylim=c(6.9, 9.75), yaxt="n", xaxt="n", xlab="", ylab="", cex.axis=1.5)
		mtext(side=3, line=2, cex=1.7, "First Ascensionists over Time (Females)")
		mtext(side=3, line=0.5, cex=1, "climbstat.blogspot.com", font=3)	
		mtext(side = 1, line=5, cex=1.5, "Year")
		mtext(side = 2, line=4, cex=1.5, "Grades (YDS)")
		axis(1,seq(min(rp$year),max(rp$year),2),at=seq(min(rp$year),max(rp$year),2),las=2)
		axis(2,grades_label$gradeus,at=grades_label$grade_index2,las=2,cex.lab=1.35)
		abline(h=grades_label$grade_index2,lwd=.1,col="lightgray",lty=2,cex.lab=1.35)
		#
		with(subset(rp,sex=="female"),lines(year,grade_index2,lwd=2,col="blue",pch=15,lty=3,type="b",cex=1.5))
		lines(rp[which(rp$sex == "female"), ][k,"year"], rp[which(rp$sex == "female"), ][k,"grade_index2"], col="orange", pch=15, type="p", cex=2)
		#with(subset(rp,sex=="female"),lines(year,grade_index2,lwd=2,col="red",pch=15,lty=3,type="b",cex=1.5))

		par(new=TRUE)
		axis(4,grades_label$grade,at=grades_label$grade_index2,las=2)
		#legend("bottom",c("males","females"),lwd=c(2,2),col=c("blue","red"),lty=c(3,3),cex=c(1.5),pch=c(15,15))
		#
		#with(subset(rp, route=="Chilam Balam"), text(year, grade_index2, labels = ascensionist, pos=3))
		#with(subset(rp, route=="Change"), text(year, grade_index2, labels = ascensionist, pos=3))
		#with(subset(rp, route=="Silence"), text(year, grade_index2, labels = ascensionist, pos=3))
		#with(subset(rp, route=="Action Directe"), text(year, grade_index2, labels = ascensionist, pos=3))
		if(k == dim(rp[which(rp$sex == "female"), ])[1]){
			text(rp[which(rp$sex == "female"), ][k,"year"] - 1, rp[which(rp$sex == "female"), ][k,"grade_index2"], 
				labels = paste0(rp[which(rp$sex == "female"), ][k,"ascensionist"]," ","(",rp[which(rp$sex == "female"), ][k,"grade"]," ",rp[which(rp$sex == "female"), ][k,"year"],")"), pos=2, cex = 1.25)
		} else 	if(k == (dim(rp[which(rp$sex == "female"), ])[1] - 1)){
			text(rp[which(rp$sex == "female"), ][k,"year"], rp[which(rp$sex == "female"), ][k,"grade_index2"] - .125, 
				labels = paste0(rp[which(rp$sex == "female"), ][k,"ascensionist"],"\n","(",rp[which(rp$sex == "female"), ][k,"grade"]," ",rp[which(rp$sex == "female"), ][k,"year"],")"), pos=1, cex = 1.25)
		} else if (k == 1) {
			text(rp[which(rp$sex == "female"), ][k,"year"] + 5, rp[which(rp$sex == "female"), ][k,"grade_index2"], 
				labels = paste0(rp[which(rp$sex == "female"), ][k,"ascensionist"]," ","(",rp[which(rp$sex == "female"), ][k,"grade"]," ",rp[which(rp$sex == "female"), ][k,"year"],")"), pos=1, cex = 1.25)				
		} else {
			text(rp[which(rp$sex == "female"), ][k,"year"]+.25, rp[which(rp$sex == "female"), ][k,"grade_index2"]-.25, 
				labels = paste0(rp[which(rp$sex == "female"), ][k,"ascensionist"],"\n","(",rp[which(rp$sex == "female"), ][k,"grade"]," ",rp[which(rp$sex == "female"), ][k,"year"],")"), pos=4, cex = 1.25)
		}
		corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
		par(xpd = TRUE) #Draw outside plot area
		text(x = corners[2] + 3.75, y = mean(corners[3:4]), "Grades (French)", srt = 270, cex = 1.5)
	dev.off()
	k = k + 1
}
#