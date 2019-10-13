rm(list=ls(all=TRUE))
# Set Language
Sys.setenv(LANGUAGE="en")
#
source("system.r")
#
plot_line = function(user=x,col=1) {
	tmp_data = which(coef_growth$user_id==user)
	tmp = c(coef_growth$min_experience[tmp_data]:coef_growth$max_experience[tmp_data])
	lines(tmp, as.numeric(fixef(reg_gr)) + coef_growth$intercept[tmp_data] + coef_growth$exp[tmp_data] * tmp + coef_growth$exp2[tmp_data] * (tmp^2) + coef_growth$exp3[tmp_data] * (tmp^3) ,lwd=5 ,col=brewer.pal(8,"Set1")[col])
}
#
options(tibble.width = Inf)
options(width=250)
# Load packages
library(RSQLite)
library(quantreg)
library(RColorBrewer)
library(np)
library(dplyr)
library(speedglm)
library(rqPen)
library(reshape2)
library(lme4)
#
library(multiwayvcov)
library(lmtest)
#
library(doParallel)
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)
#
source("Climb_R_0_Main_new.r")
# 
gr = dat
#
gr = gr %>% filter(Style=="Redpoint")
gr = gr %>% filter(Trad==0 & FA==0)
gr = gr %>% filter(grade_index2>=5)
gr = gr %>% filter(year %in% c(2000:2017))
gr = gr %>% filter(started %in% c(1995:2014))
#
gr = gr %>% filter(!is.na(birth))
#
gr = gr %>% mutate(experience = year - started)
gr = gr %>% filter(experience >=0)
#gr = gr %>% filter(experience <=12)
gr = gr %>% mutate(experience2 = experience*experience)
gr = gr %>% mutate(experience3 = experience*experience*experience)
gr = gr %>% mutate(experience4 = experience*experience*experience*experience)
# 
gr = gr %>% group_by(user_id) %>% mutate(count_years = n_distinct(year))
gr = gr %>% filter(count_years >= 3)
#
gr = gr %>% group_by(user_id) %>% mutate("min_experience" = min(experience))
gr = gr %>% group_by(user_id) %>% mutate("max_experience" = max(experience))
gr = gr %>% filter(min_experience >= 0)
#
#gr = left_join(gr, gr %>% group_by(user_id) %>% filter(experience==1) %>% mutate(max_experience_year1 = max(grade_index2)))
#gr = gr %>% group_by(user_id) %>% mutate(max_experience_year1 = max(max_experience_year1))
#gr = gr %>% filter(is.finite(max_experience_year1))
#
gr = gr %>% mutate(birth_year = as.numeric(format(as.Date(as.POSIXct(birth,format="%Y-%m-%d")),format="%Y")))
gr = gr %>% mutate(start_age = started - birth_year)
#
gr = gr %>% filter(start_age>=5 & start_age<20)
#gr = gr %>% filter(start_age>=5 & start_age<40)
#
#gr$start_age2 = NA
#gr$start_age2[which(gr$start_age<=11)] = 1
#gr$start_age2[which(gr$start_age>11 & gr$start_age<=15)] = 2
#gr$start_age2[which(gr$start_age>15 & gr$start_age<=20)] = 3
#gr$start_age2[which(gr$start_age>20 & gr$start_age<=25)] = 4
#gr$start_age2[which(gr$start_age>25 & gr$start_age<=30)] = 5
#gr$start_age2[which(gr$start_age>30)] = 6 
# Only get three hightest ascents by user and year
gr = gr %>% group_by(user_id,year) %>% mutate("year_rank" = dense_rank(desc(grade_index2)))
gr = gr %>% filter(year_rank <= 3)
#
#
gr = subset(gr, sex == 0)
#
dim(gr)
length(unique(gr$user_id))
#
#
gr = gr %>%  arrange(user_id,year)
gr = gr[,which(!colnames(gr)%in%c("method_id","route_id","Style","date","crag","sector","name","country","rating","FA","gradeus","grade_index"))]
#
#
reg_gr = lmer(grade_index2 ~ 1 + ( experience + experience2  + experience3 | user_id ), data=gr, control = lmerControl(calc.derivs = FALSE))
#
#
coef_growth = data.frame(ranef(reg_gr)$'user_id')
colnames(coef_growth) = c("intercept","exp","exp2","exp3")
coef_growth$user_id = rownames(coef_growth)
coef_growth = merge(coef_growth,gr[!duplicated(gr$user_id),c("user_id","sex","experience","experience2","experience3","start_age","min_experience","max_experience")],"user_id",all.x=T,all.y=F)
coef_growth = coef_growth[complete.cases(coef_growth),]
#
png("Growth_elites.png",width=800,height=600)
	par(mar=c(5.1, 4.1, 4.1, 4.1))
	plot(NA,xlim=c(0,20),ylim=c(5,9.15),xlab="Experience",ylab="Grade",yaxt="n",xaxt="n",cex.lab=1.5)
	axis(1, seq(0,20,1), at = seq(0,20,1), las=1,cex.axis=1.25)
	#axis(2, c("5a","6a","6b+","7a","7b+","8a","8b+","9a"), at=grades_label[which(grades_label$grade%in%c("5a","6a","6b+","7a","7b+","8a","8b+","9a")),"grade_index2"])
	axis(2, grades_label$grade, at=grades_label$grade_index2, las=2,cex.axis=1.25)
	#
	abline(h=grades_label$grade_index2,lwd=.1,col="lightgray",lty=3)
	#
	for(i in 1:dim(coef_growth)[1]){
		tmp = c(coef_growth$min_experience[i]:coef_growth$max_experience[i])
		lines(tmp, as.numeric(fixef(reg_gr)) + coef_growth$intercept[i] + coef_growth$exp[i] * c(tmp) + coef_growth$exp2[i] * (tmp^2) + coef_growth$exp3[i] * (tmp^3) ,lwd=.01 ,col="lightskyblue1" )
		#curve(coef(reg_fe)[1] + coef(reg_fe)[2] * x + coef(reg_fe)[3] * x^2, 0, 10, lwd=5, col="darkred" ,lty=1 ,add=T)
	}
	#
	#
	ondra = which(coef_growth$user_id==1476)
	tmp = c(coef_growth$min_experience[ondra]:coef_growth$max_experience[ondra])
	lines(tmp, as.numeric(fixef(reg_gr)) + coef_growth$intercept[ondra] + coef_growth$exp[ondra] * tmp + coef_growth$exp2[ondra] * (tmp^2) + coef_growth$exp3[ondra] * (tmp^3) ,lwd=5 ,col=brewer.pal(6,"Set1")[1])
	#
	ghisolfi = which(coef_growth$user_id==22437)
	tmp = c(coef_growth$min_experience[ghisolfi]:coef_growth$max_experience[ghisolfi])
	lines(tmp,as.numeric(fixef(reg_gr)) + coef_growth$intercept[ghisolfi] + coef_growth$exp[ghisolfi] * tmp + coef_growth$exp2[ghisolfi] * tmp^2  + coef_growth$exp3[ghisolfi] * tmp^3 ,lwd=5 ,col=brewer.pal(6,"Set1")[2])
	#64723
	bouin = which(coef_growth$user_id==64723)
	tmp = c(coef_growth$min_experience[bouin]:coef_growth$max_experience[bouin])
	lines(tmp,as.numeric(fixef(reg_gr)) + coef_growth$intercept[bouin] + coef_growth$exp[bouin] * tmp + coef_growth$exp2[bouin] * tmp^2  + coef_growth$exp3[bouin] * tmp^3 ,lwd=5 ,col=brewer.pal(6,"Set1")[3])
	#
	rullo = which(coef_growth$user_id==53883)
	tmp = c(coef_growth$min_experience[rullo]:coef_growth$max_experience[rullo])
	lines(tmp, as.numeric(fixef(reg_gr)) + coef_growth$intercept[rullo] + coef_growth$exp[rullo] * tmp + coef_growth$exp2[rullo] * (tmp^2) + coef_growth$exp3[rullo] * (tmp^3) ,lwd=5 ,col=brewer.pal(6,"Set1")[4])
	#	
	puigblanque = which(coef_growth$user_id==1051)
	tmp = c(coef_growth$min_experience[puigblanque]:coef_growth$max_experience[puigblanque])
	lines(tmp, as.numeric(fixef(reg_gr)) + coef_growth$intercept[puigblanque] + coef_growth$exp[puigblanque] * tmp + coef_growth$exp2[puigblanque] * (tmp^2) + coef_growth$exp3[puigblanque] * (tmp^3) ,lwd=5 ,col=brewer.pal(6,"Set1")[5])
	#
	schab = which(coef_growth$user_id==18008)
	tmp = c(coef_growth$min_experience[schab]:coef_growth$max_experience[schab])
	lines(tmp, as.numeric(fixef(reg_gr)) + coef_growth$intercept[schab] + coef_growth$exp[schab] * tmp + coef_growth$exp2[schab] * (tmp^2) + coef_growth$exp3[schab] * (tmp^3) ,lwd=5 ,col=brewer.pal(6,"Set1")[6])
	#	
#
	legend("bottom",c("Adam Ondra","Stefano Ghisolfi","Sébastien Bouin","Jorge Diaz-Rullo","Ramón Julian Puigblanque","Piotr Schab"),col=brewer.pal(6,"Set1"),lwd=5)
	par(new=TRUE)
	axis(4,grades_label$gradeus,at=grades_label$grade_index2,las=2,cex.axis=1.25)
dev.off()
#



png("Growth_elites2.png",width=800,height=600)
	par(mar=c(5.1, 4.1, 4.1, 4.1))
	plot(NA,xlim=c(0,20),ylim=c(5,9.15),xlab="Experience",ylab="Grade",yaxt="n",xaxt="n",cex.lab=1.5)
	axis(1, seq(0,20,1), at = seq(0,20,1), las=1,cex.axis=1.25)
	#axis(2, c("5a","6a","6b+","7a","7b+","8a","8b+","9a"), at=grades_label[which(grades_label$grade%in%c("5a","6a","6b+","7a","7b+","8a","8b+","9a")),"grade_index2"])
	axis(2, grades_label$grade, at=grades_label$grade_index2, las=2,cex.axis=1.25)
	#
	abline(h=grades_label$grade_index2,lwd=.1,col="lightgray",lty=3)
	#
	for(i in 1:dim(coef_growth)[1]){
		tmp = c(coef_growth$min_experience[i]:coef_growth$max_experience[i])
		lines(tmp, as.numeric(fixef(reg_gr)) + coef_growth$intercept[i] + coef_growth$exp[i] * c(tmp) + coef_growth$exp2[i] * (tmp^2) + coef_growth$exp3[i] * (tmp^3) ,lwd=.01 ,col="lightskyblue1" )
		#curve(coef(reg_fe)[1] + coef(reg_fe)[2] * x + coef(reg_fe)[3] * x^2, 0, 10, lwd=5, col="darkred" ,lty=1 ,add=T)
	}
	#
	# Sachi Amma
	plot_line(user=30100,col=1)
	#
	# Daniel Woods
	plot_line(user=4102,col=2)
	#
	# Mathieu Bouyoud
	plot_line(user=8345,col=3)
	#
	# David Graham 
	plot_line(user=118,col=4)
	#	
	# Domen Škofic
	plot_line(user=27591,col=5)
	#
	# Jernej Kruder 
	plot_line(user=9886,col=6)
	#
	# Joe Kinder 
	plot_line(user=476,col=7)
	#
	# Daniel Jung 
	plot_line(user=3155,col=8)
	#
	legend("bottom",c("Sachi Amma","Daniel Woods","Mathieu Bouyoud","David Graham ","Domen Škofic","Jernej Kruder","Joe Kinder ","Daniel Jung"),col=brewer.pal(8,"Set1"),lwd=5)
	par(new=TRUE)
	axis(4,grades_label$gradeus,at=grades_label$grade_index2,las=2,cex.axis=1.25)
dev.off()

