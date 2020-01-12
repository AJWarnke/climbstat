rm(list=ls(all=TRUE))
# Set Language
Sys.setenv(LANGUAGE="en")
#
source("system.r")
#
plot_line = function(user=x,col=1) {
	tmp_data = which(coef_growth$user_id==user)
	tmp = c(coef_growth$min_experience[tmp_data]:coef_growth$max_experience[tmp_data])
	lines(tmp, as.numeric(fixef(reg_gr)) + coef_growth$intercept[tmp_data] + coef_growth$exp[tmp_data] * tmp + coef_growth$exp2[tmp_data] * (tmp^2) + coef_growth$exp3[tmp_data] * (tmp^3) ,lwd=5 ,col=brewer.pal(8,"Set3")[col])
}
#
options(tibble.width = Inf)
options(width=250)
# Load packages
library(RSQLite)
library(quantreg)
library(RColorBrewer)
library(np)
library(plyr)
library(dplyr)
library(speedglm)
library(rqPen)
library(reshape2)
library(lme4)
library(xgboost)
library(randomForest)
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
gr = climbs
#
gr = gr %>% filter(Trad==0 & FA==0)
gr = gr %>% filter(grade_index2>=5)
gr = gr %>% filter(year %in% c(2000:2017))
gr = gr %>% filter(started %in% c(1995:2014))
#
gr = gr %>% filter(!is.na(birth))
#
gr = gr %>% mutate(experience = year - started)
gr = gr %>% filter(experience >=0)
gr = gr %>% filter(weight >0)
gr = gr %>% filter(height >100)
gr$bmi = gr$weight/((gr$height/100)^2)
gr = gr %>% filter(bmi>17 & bmi<35)
#
#
gr = gr %>% mutate(birth_year = as.numeric(format(as.Date(as.POSIXct(birth,format="%Y-%m-%d")),format="%Y")))
gr = gr %>% mutate(age = year - birth_year)
gr = gr %>% mutate(start_age = started - birth_year)
gr = gr %>% filter(start_age >0)
gr = gr %>% filter(age >= 6 & age<=60)
gr = gr %>% filter(grade_index2>=6)
#

# Only get three hightest ascents by user and year
gr = gr %>% group_by(user_id,Style) %>% mutate("rank" = dense_rank(desc(grade_index2)))
gr = gr %>% filter(rank <= 1)
#
gr = gr %>% group_by(user_id,Style) %>% filter(row_number(user_id) == 1)
#
gr = gr %>%  arrange(user_id)

#
tmp_start_age = sapply( seq(5,30,5), function(x) as.numeric(gr$start_age>x))
colnames(tmp_start_age) = paste0("start_age_above_", seq(5,30,5))
gr = cbind(data.frame(gr),tmp_start_age)
# paste(paste0("start_age_above_", seq(5,30,5)),collapse=" + ")
subset(gr,user_id==1476)
#
xgb = xgboost(data = data.matrix(gr[,c("sex","experience","age","start_age","bmi","weight","height","Style")]), label = gr$grade_index2, nrounds = 50, max_depth = 3)
xgb.importance( model = xgb )
#
#rf = randomForest(factor(grade_index2) ~ . , data=data.matrix(gr[,c("sex","experience","age","start_age","bmi","weight","height","grade_index2","Style")]))
#varImpPlot(rf, type=2)
#
summary(lm(grade_index2 ~ sex + experience + age + bmi +  height + weight + Style + start_age_above_5 + start_age_above_10 + start_age_above_15 + start_age_above_20 + start_age_above_25 + start_age_above_30, data= gr))
#
