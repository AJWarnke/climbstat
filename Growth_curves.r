rm(list=ls(all=TRUE))
# Set Language
Sys.setenv(LANGUAGE="en")
# Set Working Directory
if(Sys.info()["nodename"]=="H423DF72" | Sys.info()["nodename"]=="H5CD9074XD1"){
	setwd("C:/Users/WarnkeAJ/OneDrive - BASF/Projects/Other/Run")
	.libPaths(c("C:/Users/WarnkeAJ/Documents/R/R-3.4.1/library", .libPaths()))
}
#
if(Sys.info()["nodename"]=="HD39L3K2"){
	#setwd("Z:/Run/")
	setwd("C:/Run")
	#.libPaths(c("C:/Program Files/R/R-3.4.1/library", .libPaths()))
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
source("Data_Prep_8a.r")
# 
gr = dat
#
gr = gr %>% filter(Style=="Redpoint")
gr = gr %>% filter(Trad==0 & FA==0)
gr = gr %>% filter(grade_index2>=5)
gr = gr %>% filter(year %in% c(2000:2017))
gr = gr %>% filter(started %in% c(1995:2013))
#
gr = gr %>% filter(!is.na(birth))
#
gr = gr %>% mutate(experience = year - started)
gr = gr %>% filter(experience >=0)
gr = gr %>% filter(experience <=12)
gr = gr %>% mutate(experience2 = experience*experience)
gr = gr %>% mutate(experience3 = experience*experience*experience)
gr = gr %>% mutate(experience4 = experience*experience*experience*experience)
# 
gr = gr %>% group_by(user_id) %>% mutate(count_years = n_distinct(year))
gr = gr %>% filter(count_years >= 3)
#
gr = gr %>% group_by(user_id) %>% mutate("min_experience" = min(experience))
gr = gr %>% group_by(user_id) %>% mutate("max_experience" = max(experience))
gr = gr %>% filter(min_experience >= 0 & min_experience <= 5)
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
dim(gr)
length(unique(gr$user_id))
#
#
gr = gr %>%  arrange(user_id,year)
gr = gr[,which(!colnames(gr)%in%c("method_id","route_id","Style","date","crag","sector","name","country","rating","FA","gradeus","grade_index"))]
#
#
reg_gr = lmer(grade_index2 ~ 1 + ( 1 + experience + experience2 | sex ) + ( experience + experience2 | user_id ), data=gr, control = lmerControl(calc.derivs = FALSE))

#
#
coef_growth = data.frame(ranef(reg_gr)$'user_id')
colnames(coef_growth) = c("intercept","exp","exp2")
#
coef_growth_sex = data.frame(ranef(reg_gr)$'sex')
colnames(coef_growth_sex) = c("intercept_sex","exp_sex","exp2_sex")
coef_growth_sex$sex = rownames(coef_growth_sex)
#
#
coef_growth$user_id = rownames(coef_growth)
coef_growth = merge(coef_growth,gr[!duplicated(gr$user_id),c("user_id","sex","experience","experience2","start_age","min_experience","max_experience")],"user_id",all.x=T,all.y=F)
coef_growth = merge(coef_growth,coef_growth_sex,"sex",all=T)
#
reg_lm_female = lm(grade_index2 ~ experience + experience2 + experience3, data=subset(gr,sex==1))
reg_lm_male = lm(grade_index2 ~ experience + experience2 + experience3, data=subset(gr,sex==0))
#reg_fe = lm(grade_index2 ~ experience + experience2 + as.factor(user_id), data=gr)
reg_lm_joint = lm(grade_index2 ~ experience*sex + experience2*sex + experience3*sex, data=gr)
vcov_user <- cluster.vcov(reg_lm_joint, gr$user_id)
coeftest(reg_lm_joint,vcov_user)
#
#
coef_growth = coef_growth[complete.cases(coef_growth),]
#
png("Growth.png",width=800,height=600)
	plot(NA,xlim=c(0,12),ylim=c(5,9),xlab="Experience",ylab="Grade",yaxt="n",xaxt="n",cex.lab=1.5)
	axis(1, seq(0,12,1), at = seq(0,12,1), las=1,cex.axis=1.25)
	#axis(2, c("5a","6a","6b+","7a","7b+","8a","8b+","9a"), at=grades_label[which(grades_label$grade%in%c("5a","6a","6b+","7a","7b+","8a","8b+","9a")),"grade_index2"])
	axis(2, grades_label$grade, at=grades_label$grade_index2, las=2,cex.axis=1.25)
	#
	abline(h=grades_label$grade_index2,lwd=.1,col="lightgray",lty=3)
	#
	for(i in 1:dim(coef_growth)[1]){
		tmp = c(coef_growth$min_experience[i]:coef_growth$max_experience[i])
		lines(tmp,as.numeric(fixef(reg_gr)) + ( coef_growth$intercept[i] + coef_growth$intercept_sex[i]) + ( coef_growth$exp[i] + coef_growth$exp_sex[i] ) * c(tmp) + ( coef_growth$exp2[i] + coef_growth$exp2_sex[i] ) * (tmp^2) ,lwd=.01 ,col="lightskyblue1" )
		#curve(coef(reg_fe)[1] + coef(reg_fe)[2] * x + coef(reg_fe)[3] * x^2, 0, 10, lwd=5, col="darkred" ,lty=1 ,add=T)
	}


	#curve(as.numeric(fixef(reg_gr)) + mean(coef_growth$intercept) + mean(coef_growth$exp) * x + mean(coef_growth$exp2) * x^2, 0, 17, lwd=5, col="blue" ,lty=1 ,add=T)
	curve(coef(reg_lm_female)[1] + coef(reg_lm_female)[2] * x + coef(reg_lm_female)[3] * x^2 + coef(reg_lm_female)[4] * x^3, 0, 12, lwd=5, col="red" ,lty=1 ,add=T)
	curve(coef(reg_lm_male)[1] + coef(reg_lm_male)[2] * x + coef(reg_lm_male)[3] * x^2 + coef(reg_lm_male)[4] * x^3, 0, 12, lwd=5, col="blue" ,lty=1 ,add=T)
	legend("bottom",c("males","females"),col=c("blue","red"),lwd=5,cex=1.5)
dev.off()
#
#
ondra = which(coef_growth$user_id==1476)
tmp = c(coef_growth$min_experience[ondra]:coef_growth$max_experience[ondra])
lines(tmp,as.numeric(fixef(reg_gr)) + ( coef_growth$intercept[ondra] + coef_growth$intercept_sex[ondra]) + ( coef_growth$exp[ondra] + coef_growth$exp_sex[ondra] ) * c(tmp) + ( coef_growth$exp2[ondra] + coef_growth$exp2_sex[ondra] ) * (tmp^2) ,lwd=5 ,col="purple")
with(subset(gr,user_id==1476),lines(experience,grade_index2,type="p",pch=15,col="purple"))
#
library("optimx")
reg_gr1 = lmer(grade_index2 ~ 1 + (1+experience+experience2|sex) + (1+experience+experience2|user_id) + (1+experience+experience2|start_age2),data=gr,control = lmerControl(calc.derivs = FALSE))
reg_gr2 = glmer(grade_index2 ~ 1 + (1+experience|sex) + (1+experience|user_id) + (1+experience|start_age2),family=Gamma(link="log"),data=gr,control = glmerControl(calc.derivs = FALSE))
#
coef_growth = data.frame(ranef(reg_gr1)$'user_id')
colnames(coef_growth)[1] = "intercept_user"
#
coef_growth_age = data.frame(ranef(reg_gr1)$'start_age2')
colnames(coef_growth_age)[1] = "intercept_age"
colnames(coef_growth_age)[2] = "experience_age"
colnames(coef_growth_age)[3] = "experience2_age"
coef_growth_age$start_age2 = rownames(coef_growth_age)
#
coef_growth_sex = data.frame(ranef(reg_gr1)$'sex')
colnames(coef_growth_sex)[1] = "intercept_sex"
colnames(coef_growth_sex)[2] = "experience_sex"
colnames(coef_growth_sex)[3] = "experience2_sex"
coef_growth_sex$sex = rownames(coef_growth_sex)
#coef_growth$intercept = fixef(reg_gr1)["(Intercept)"]
#coef_growth$coef_start_age = fixef(reg_gr1)["start_age"]
#coef_growth$coef_sex = fixef(reg_gr1)["sex"]
coef_growth$user_id = rownames(coef_growth)
coef_growth = merge(coef_growth,gr[!duplicated(gr$user_id),c("user_id","start_age2","sex")],"user_id",all.x=T,all.y=F)
coef_growth = merge(coef_growth,coef_growth_age,"start_age2",all=T)
coef_growth = merge(coef_growth,coef_growth_sex,"sex",all=T)
#
coef_growth$intercept_total = fixef(reg_gr1)["(Intercept)"] + coef_growth$intercept_user + coef_growth$intercept_sex*coef_growth$sex + coef_growth$intercept_age*coef_growth$start_age2
#
plot(NA,xlim=c(0,10),ylim=c(6,10))
for(i in 1:dim(coef_growth)[1]){
	curve(coef_growth$intercept_total[i] + (coef_growth$experience[i]+coef_growth$experience_age[i]+coef_growth$experience_sex[i])*x + (coef_growth$experience2[i]+coef_growth$experience2_age[i]+coef_growth$experience2_sex[i])*x^2,0,10,lwd=.01,col="lightgray",add=T)
}
#
for(i in 1:length(unique(gr$start_age2))){
	with(subset(coef_growth,start_age2==unique(gr$start_age2)[i] & sex==0),
		curve(mean(intercept_total) + mean(experience+experience_age+experience_sex) * x + mean(experience2+experience2_age+experience2_sex) * x^2,0,10,lwd=3,col=rev(brewer.pal(6,"Blues"))[i],add=T)
	)
	with(subset(coef_growth,start_age2==unique(gr$start_age2)[i] & sex==1),
		curve(mean(intercept_total) + mean(experience+experience_age+experience_sex) * x + mean(experience2+experience2_age+experience2_sex) * x^2,0,10,lwd=3,col=rev(brewer.pal(6,"Reds"))[i],add=T)
	)
}
#
plot_click <- par(ask=TRUE)
sample_user_id = sample(unique(as.character(gr$user_id)),size=10,replace=F)
for (i in sample_user_id){
    plot(NA,xlim=c(0,10),ylim=c(6,10))
	with(subset(gr,user_id==i),{
		lines(experience,grade_index2,type="p",pch=16,col="blue")
	})
	lines(subset(gr,user_id==i)$experience,predict(reg_gr1,newdata=subset(gr,user_id==i)),type="b","red",lwd=2,lty=2)
}
par(plot_click)


#
#
#
library(caret)
set.seed(998)


gr2 = aggregate(grade_index2~user_id+sex+height+weight+OS+Flash+year+Trad+started+experience+birth_year+start_age,gr,max)



cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)

trainIndex <- createDataPartition(gr2$grade_index2, p = .75, list = FALSE, times=1)
training <- gr2[ trainIndex,]
testing  <- gr2[-trainIndex,]

train_rf = train(grade_index2~. , data = training, method = "rf",  number = 10, repeats = 10, verbose=TRUE,allowParallel=TRUE,importance=T)



stopCluster(cl)



library(snow)
z=vector('list',4)
z=1:4
system.time(lapply(z,function(x) Sys.sleep(1)))
cl <- makePSOCKcluster(detectCores())
system.time(clusterApply(cl, z,function(x) Sys.sleep(1)))
stopCluster(cl)