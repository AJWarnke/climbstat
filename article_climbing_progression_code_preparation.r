# Connect to database
con <- dbConnect(drv=RSQLite::SQLite(), dbname="database.sqlite")
# list all tables
tables <- dbListTables(con)
# exclude sqlite_sequence (contains technical table information)
tables <- tables[tables != "sqlite_sequence"]
#
## create a data.frame for each table
lDataFrames <- vector("list", length=length(tables))
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}
#
dbDisconnect(con)
# Information about individual climbers/boulderer
climber = lDataFrames[[4]]
dim(climber)
# Drop irrelevant columns
climber = climber[,c("id","sex","height","weight","started","birth")]
names(climber)[which(names(climber)=="id")] = "user_id"
#
# climb_type=0 : climbing, climb_type=1 : bouldering
# OS: method_id == 3, Flash:  method_id == 2, RP: method_id == 1
ascents = lDataFrames[[1]]
ascents = subset(ascents,climb_type==0) # only rope climbing
ascents = subset(ascents,method_id<=3) # OS, Flash, RP
ascents = ascents[which(ascents$"repeat"==0),] # no repeats
ascents = ascents[which(ascents$climb_try==0),] # no repeats
# what is yellow_id, perhaps downvote or so?
ascents = ascents[which(ascents$exclude_from_ranking==0),] # no excluded routes
ascents$OS = as.numeric(ascents$method_id==3)
ascents$Flash = as.numeric(ascents$method_id==2)
ascents$date = format(as.Date(as.POSIXct(ascents$date, origin="1970-01-01")),format="%Y%m")
#
ascents$Style = factor("Redpoint",levels=c("Redpoint","Flash","Onsight"))
ascents$Style[which(ascents$method_id==2)] = "Flash"
ascents$Style[which(ascents$method_id==3)] = "Onsight"
#
table(lDataFrames[[1]]$notes)
ascents$Trad = as.numeric(grepl("Traditional",ascents$notes))
ascents$FA = as.numeric(grepl("First Ascent",ascents$notes))
ascents$Difficulty = factor("Normal",levels=c("Normal","Soft","Hard"))
ascents$Difficulty[which(grepl("Soft",ascents$notes))] = "Soft"
ascents$Difficulty[which(grepl("Hard",ascents$notes))] = "Hard"
ascents$Go2 = as.numeric(grepl("Second Go",ascents$notes))
#
ascents = ascents[,c("id","user_id","grade_id","method_id","OS","Flash","Go2","Style","date","year","crag_id","crag","sector_id","sector","name","country","rating","Trad","FA","Difficulty")]
names(ascents)[which(names(ascents)=="id")] = "route_id"
#
climbs = merge(ascents,climber,by="user_id",all=F)
#
#
grades_label = data.frame(
	"grade_id"=c(13,15,17,21,23,25,29,31,33,36,38,40,42,44,46,49,51,53,55,57,59,62,64,66,68,70,72,75,77,79,81,83),
	"grade"=c("3a","3b","3c","4a","4b","4c","5a","5b","5c","6a","6a+","6b","6b+","6c","6c+","7a","7a+","7b","7b+","7c","7c+","8a","8a+","8b","8b+","8c","8c+","9a","9a+","9b","9b+","9c"),
	"gradeus"=c("","","5.4","5.5","5.6","5.7","5.8","5.9","5.10a","5.10b","5.10c","5.10d","5.11a","5.11b","5.11c","5.11d","5.12a","5.12b","5.12c","5.12d","5.13a","5.13b","5.13c","5.13d","5.14a","5.14b","5.14c","5.14d","5.15a","5.15b","5.15c","5.15d"),
	"grade_index2"=c(3,3+1/3,3+2/3,4,4+1/3,4+2/3,5,5+1/3,5+2/3,6,6+1/6,6+1/3,6.5,6+2/3,6+5/6,7,7+1/6,7+1/3,7.5,7+2/3,7+5/6,8,8+1/6,8+1/3,8.5,8+2/3,8+5/6,9,9+1/6,9+1/3,9.5,9+2/3))
grades_label$grade_index = 1:dim(grades_label)[1]
#
climbs = merge(climbs,grades_label,"grade_id",all.x=T)
climbs = subset(climbs,!is.na(grade_index))
# Focus on ascents 6a and harder
#climbs = climbs[which(climbs$grade_id>=36 & !(climbs$grade_id%in%c(71,73))),] # 71,73 just a very few ascents
#
# https://www.8a.nu/scorecard/paolo-antoniotti/routes/?AscentClass=0&AscentListViewType=0&GID=176da43c4512d0a8a6e64f89793650ab
climbs = climbs[which(climbs$route_id!=2938504),] 
# 9a Onsight by deactivited climber
climbs = climbs[which(climbs$user_id!=32658),] 
# Seems to be advertisment
climbs = climbs[which(climbs$user_id!=47989),] 
#
#
#climbs2 = aggregate(grade_id~method_id+user_id+height+weight+bmi+bmi2,climbs,max)
#climbs2 = merge(climbs2,grades_label,"grade_id",all.x=T)
#
#
boulder = lDataFrames[[1]]
boulder = subset(boulder,climb_type==1) # only bouldering
#boulder = subset(boulder,method_id<=3) # OS, Flash, RP
boulder = boulder[which(boulder$"repeat"==0),] # no repeats
boulder = boulder[which(boulder$climb_try==0),] # no repeats
# what is yellow_id, perhaps downvote or so?
boulder = boulder[which(boulder$exclude_from_ranking==0),]
#
boulder$Flash = as.numeric(boulder$method_id==1)
boulder$date = format(as.Date(as.POSIXct(boulder$date, origin="1970-01-01")),format="%Y%m")
#
boulder$Style = factor("Redpoint",levels=c("Redpoint","Flash"))
boulder$Style[which(boulder$method_id==1)] = "Flash"

#
table(lDataFrames[[1]]$notes)
boulder$Traverse = as.numeric(grepl("Traverse",boulder$notes))
boulder$Trad = as.numeric(grepl("Traditional",boulder$notes))
boulder$FA = as.numeric(grepl("First Ascent",boulder$notes))
boulder$Difficulty = factor("Normal",levels=c("Normal","Soft","Hard"))
boulder$Difficulty[which(grepl("Soft",boulder$notes))] = "Soft"
boulder$Difficulty[which(grepl("Hard",boulder$notes))] = "Hard"
boulder$Go2 = as.numeric(grepl("Second Go",boulder$notes))
#
boulder = boulder[,c("id","user_id","grade_id","method_id","Flash","Go2","Style","date","year","crag_id","crag","sector_id","sector","name","country","rating","Trad","FA","Difficulty")]
names(boulder)[which(names(boulder)=="id")] = "route_id"
#
boulder = merge(boulder,climber,by="user_id",all=F)
#
#
grades_label = data.frame(
	"grade_id"=c(13,15,17,21,23,25,29,31,33,36,38,40,42,44,46,49,51,53,55,57,59,62,64,66,68,70,72,75,77,79,81,83),
	"grade"=c("3a","3b","3c","4a","4b","4c","5a","5b","5c","6a","6a+","6b","6b+","6c","6c+","7a","7a+","7b","7b+","7c","7c+","8a","8a+","8b","8b+","8c","8c+","9a","9a+","9b","9b+","9c"),
	"gradeus"=c("","","5.4","5.5","5.6","5.7","5.8","5.9","5.10a","5.10b","5.10c","5.10d","5.11a","5.11b","5.11c","5.11d","5.12a","5.12b","5.12c","5.12d","5.13a","5.13b","5.13c","5.13d","5.14a","5.14b","5.14c","5.14d","5.15a","5.15b","5.15c","5.15d"),
	"grade_index2"=c(3,3+1/3,3+2/3,4,4+1/3,4+2/3,5,5+1/3,5+2/3,6,6+1/6,6+1/3,6.5,6+2/3,6+5/6,7,7+1/6,7+1/3,7.5,7+2/3,7+5/6,8,8+1/6,8+1/3,8.5,8+2/3,8+5/6,9,9+1/6,9+1/3,9.5,9+2/3))
grades_label$grade_index = 1:dim(grades_label)[1]
#
grades_bouldern = data.frame(
	"grade_id" = c(13,15,17,21,23,25,29,31,33,36,38,40,42,44,46,49,51,53,55,57,59,62,64,66,68,70,72,75),
	"grade_index2" = c(3,3+1/3,3+2/3,4,4+1/3,4+2/3,5,5+1/3,5+2/3,6,6+1/6,6+1/3,6.5,6+2/3,6+5/6,7,7+1/6,7+1/3,7.5,7+2/3,7+5/6,8,8+1/6,8+1/3,8.5,8+2/3,8+5/6,9),
	"font" = c("3A","3B","3C","4A","4B","4C","5A","5B","5C","6A","6A+","6B","6B+","6C","6C+","7A","7A+","7B","7B+","7C","7C+","8A","8A+","8B","8B+","8C","8C+","9A"),
	"v" = c("VB","VB","VB","V0-","V0-/V0","V0","V0+","V1","V1/V2","V2","V2/V3","V3","V3/V4","V4","V4/V5","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17")
)
grades_bouldern$grade_index = 1:dim(grades_bouldern)[1]
#
boulder = merge(boulder,grades_bouldern,"grade_id",all.x=T)
boulder = subset(boulder,!is.na(grade_index))
#
climbs$climb = 1
boulder$boulder = 1 