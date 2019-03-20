dt.time.loc.event[,notest := event ]
dt.time.loc.event$notest[!is.na(dt.time.loc.event$istest)] <- NA


dt.time.loc.event[,locate_n_earplug := NA ]
for(itr in 1:length(dt.time.loc.event$event)){
  if(str_detect(dt.time.loc.event$event[itr], "^where-|^s-e-")){
    dt.time.loc.event$locate_n_earplug[itr] <- dt.time.loc.event$event[itr]
  }
}

#require(ggraptR)
#ggraptR()

#### Food ####
dt.time.loc.event$notest[!is.na(dt.time.loc.event$locate_n_earplug)] <- NA
sort((dt.time.loc.event$notest)) #  unique
dt.time.loc.event[,food:=notest]
dt.time.loc.event$food[!str_detect(dt.time.loc.event$food, "^f-")] <- NA
dt.food <- dt.time.loc.event[!is.na(food)]#sort(table(dt.time.loc.event$food))
dt.food <- dt.food[, .(inears,location.long,location,location.short,time,event)] 
dt.food[,food1c:=as.character(str_extract_all( event,"^f-[\\p{Alphabetic}]{1,30}"))]
dt.food[,f1c.count:=.N,by=food1c]
dt.food <- dt.food[f1c.count>4]
###### FOOD extract quantities #######
dt.food[,food1cQuant:=NA]
dt.food[,food1cQuant:=as.character(str_extract( event,"(?<=[.-])[/1234567890g.]{1,4}(($)|(?=[.-]))"))]
dt.food$food1cQuant[dt.food$food1cQuant == "character(0)"]<-NA
dt.food[,food1cQuantgrmsoz:=str_detect(food1cQuant,"[g]"),by=food1c]
dt.food$food1cQuantgrmsoz[is.na(dt.food$food1cQuantgrmsoz)]<-FALSE
for (itr in 1:length(dt.food$food1cQuant)) {
  worked_string<-dt.food$food1cQuant[itr]
  if(!is.na(worked_string)){
    if(str_detect(worked_string,"[/]")){
      worked_string<-  as.numeric( str_extract_all( worked_string,"[1234567890]{1,3}(?=[/])"))/
      as.numeric( str_extract_all( worked_string,"(?<=[/])[1234567890]{1,3}"))
    }  
    if(str_detect(worked_string,"[\\p{Alphabetic}]")){
      worked_string<-str_extract( worked_string,"[1234567890]{1,4}")
    } 
  } else {
    if(sum(dt.food$food1cQuantgrmsoz[dt.food$food1c==dt.food$food1c[itr]] ,na.rm = T)>0){
      dt.food$food1cQuantgrmsoz[itr]<-TRUE
    } else {
      worked_string<-1
    }
}
  dt.food$food1cQuant[itr]<-worked_string
}
dt.food[,food1cMed:=as.character(median(as.numeric(food1cQuant),na.rm = T)),by=food1c]
dt.food$food1cQuant<-as.numeric(dt.food$food1cQuant)
dt.food$food1cMed<-as.numeric(dt.food$food1cMed)
for (itr in 1:length(dt.food$food1cQuant)) {
  if(is.na(dt.food$food1cQuant[itr])) {
    dt.food$food1cQuant[itr]<-dt.food$food1cMed[itr]
  }
  if(dt.food$food1cQuantgrmsoz[itr]){
    dt.food$food1cQuant[itr]<-dt.food$food1cQuant[itr]/dt.food$food1cMed[itr]
  }
}
#why not just always impute over medians? if no median detected then 1?
#setting to 1 must precede medianization
#why not .0.5 instead of -.5 or .1/2

ggplot(dt.food, aes(x=time)) + geom_histogram(stat="bin", position="stack", alpha=1, bins=200) + geom_path(stat="bin", position="identity", linetype="dashed", bins=100, pad=TRUE) + facet_grid(food1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + xlab("time") + ylab("count")
ggplot(dt.food, aes(x=time)) + geom_histogram(stat="bin", position="stack", alpha=1, bins=1000) + facet_grid(food1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + xlab("time") + ylab("count")
ggplot(dt.food, aes(y=food1cQuant, x=time)) + geom_path(stat="identity", position="identity", alpha=0.5) + facet_grid(food1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + xlab("time") + ylab("food1cQuant")

png(filename = paste0("fud.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.food, aes(y=food1c, x=time)) + 
  geom_point(aes(colour=food1c),
             stat="identity", position="identity", alpha=0.5, size=3) +
  geom_line(aes(colour=food1c), stat="identity", position="identity", alpha=0.5) +
  theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) +
  scale_size(range=c(1, 1)) + xlab("time") + ylab("f1c.count")
dev.off()

png(filename = paste0("food_weighd_density.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.food, aes(x = time, weights = food1cQuant)) + geom_density(adjust = 1/16) + facet_grid(food1c ~ .) 
dev.off()

#### Drug ####
dt.time.loc.event[,drug:=notest]
dt.time.loc.event$drug[!str_detect(dt.time.loc.event$drug, "^d-")]<-NA
dt.drug<-dt.time.loc.event[!is.na(drug)]#sort(table(dt.time.loc.event$drug))
dt.drug<-dt.drug[, .(inears,location.long,location,location.short,time,event)] 
dt.drug[,drug1c:=as.character(str_extract_all( event,"^d-[\\p{Alphabetic}]{1,30}"))]
dt.drug[,f1c.count:=.N,by=drug1c]
dt.drug<-dt.drug[f1c.count>4]

  
png(filename = paste0("drug.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.drug, aes(y=drug1c, x=time)) + 
  geom_point(aes(colour=drug1c),
             stat="identity", position="identity", alpha=0.5, size=3) +
  geom_line(aes(colour=drug1c), stat="identity", position="identity", alpha=0.5) +
  theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) +
  scale_size(range=c(1, 1)) + xlab("time") + ylab("f1c.count")
dev.off()

png(filename = paste0("drug_weighd_density.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.drug, aes(x = time, weights = 1)) + geom_density(adjust = 1/8) + facet_grid(drug1c ~ .) 
dev.off()

#### Activity ####
dt.time.loc.event[,act:=notest]
dt.time.loc.event$act[!str_detect(dt.time.loc.event$act, "^a-")]<-NA
dt.act<-dt.time.loc.event[!is.na(act)]#sort(table(dt.time.loc.event$act))
dt.act<-dt.act[, .(inears,location.long,location,location.short,time,event)] 
dt.act[,act1c:=as.character(str_extract_all( event,"^a-[\\p{Alphabetic}]{1,30}"))]
dt.act[,a1c.count:=.N,by=act1c]
dt.act<-dt.act[a1c.count>4]
#unique(dt.act$act1c)

###### Activity extract quantities #######
dt.act[,act1cQuant:=NA]
dt.act[,act1cQuant:=as.character(str_extract( event,"(?<=[.-])[/1234567890.]+[hm]"))]
dt.act$act1cQuant[dt.act$act1cQuant == "character(0)"]<-NA
#dt.act[,act1cQuantgrmsoz:=str_detect(act1cQuant,"[hm]"),by=act1c]
#dt.act$act1cQuantgrmsoz[is.na(dt.act$act1cQuantgrmsoz)]<-FALSE

#str_extract_all( "a-internethypnotized-1.5h","(?<=[.-])[/1234567890.hm]+(($)|(?=[.-]))")

for (itr in 1:length(dt.act$act1cQuant)) {
  worked_string<-dt.act$act1cQuant[itr]
  if(!is.na(worked_string)){
    if(str_detect(worked_string,"[/]")){
      worked_string<-  as.numeric( str_extract_all( worked_string,"[1234567890]{1,3}(?=[/])"))/
        as.numeric( str_extract_all( worked_string,"(?<=[/])[1234567890]{1,3}"))
    }  
    if(str_detect(worked_string,"[h]")){
      worked_string<-str_extract( worked_string,"[1234567890.]{1,5}")
    } 
    if(str_detect(worked_string,"[m]")){
      worked_string<-as.numeric(str_extract( worked_string,"[1234567890.]{1,5}"))/60
    } 
  } else {
    if(sum(dt.act$act1cQuantgrmsoz[dt.act$act1c==dt.act$act1c[itr]] ,na.rm = T)>0){
      dt.act$act1cQuantgrmsoz[itr]<-TRUE
    } else {
      worked_string<-1
    }
  }
  dt.act$act1cQuant[itr]<-worked_string
}
dt.act[,act1cMed:=as.character(median(as.numeric(act1cQuant),na.rm = T)),by=act1c]
for (itr in 1:length(dt.act$act1cQuant)) {
  if(is.na(dt.act$act1cQuant[itr])) {
    dt.act$act1cQuant[itr]<-dt.act$act1cMed[itr]
  }
}
#why not just always impute over medians? if no median detected then 1h?
#setting to 1 must precede medianization? no?
dt.act$act1cQuant<-as.numeric(dt.act$act1cQuant)

png(filename = paste0("actviolin.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.act, aes(y=time, x=as.factor(act1c))) +
  geom_violin(stat="ydensity", position="dodge", alpha=0.5, trim=TRUE, scale="area") +
  geom_boxplot(stat="boxplot", position="dodge", alpha=0.5, width=0.2) + theme_grey() +
  theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) +
  xlab("as.factor(act1c)") + ylab("time")
dev.off()

png(filename = paste0("act.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.act, aes(y=act1c, x=time)) + 
  geom_point(aes(colour=act1c),
             stat="identity", position="identity", alpha=0.5, size=3) +
  geom_line(aes(colour=act1c), stat="identity", position="identity", alpha=0.5) +
  theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) +
  scale_size(range=c(1, 1)) + xlab("time") + ylab("f1c.count")
dev.off()

png(filename = paste0("act_weighd_density.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.act, aes(x = time, weights = act1cQuant)) + geom_density(adjust = 1/8) + facet_grid(act1c ~ .) 
dev.off()

maxtime<-max(dt.act$time)
png(filename = paste0("act_weighd_density_recent.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.act, aes(x = time, weights = act1cQuant)) + geom_density(adjust = 1/8) +
  facet_grid(act1c ~ .) + xlim(maxtime - 20, maxtime)
dev.off()

#### State Symptom ####
dt.time.loc.event[,state:=notest]
dt.time.loc.event$state[!str_detect(dt.time.loc.event$state, "^s-")] <- NA
DtSy <- dt.time.loc.event[!is.na(state)]#sort(table(dt.time.loc.event$state))
DtSy <- DtSy[, .(inears,location.long,location,location.short,time,event)] 
DtSy[,St1c:=as.character(str_extract_all( event,"^s-[\\p{Alphabetic}]{1,30}"))]
DtSy[,s1c.count:=.N,by=St1c]
DtSy <- DtSy[s1c.count>4]


png(filename = paste0("statusviolin.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(DtSy, aes(y=time, x=as.factor(St1c))) +
  geom_violin(stat="ydensity", position="dodge", alpha=0.5, trim=TRUE, scale="area") +
  geom_boxplot(stat="boxplot", position="dodge", alpha=0.5, width=0.2) + theme_grey() +
  theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) +
  xlab("as.factor(St1c)") + ylab("time")
dev.off()

png(filename = paste0("status.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(DtSy, aes(y=St1c, x=time)) + 
  geom_point(aes(colour=St1c),
             stat="identity", position="identity", alpha=0.5, size=3) +
  geom_line(aes(colour=St1c), stat="identity", position="identity", alpha=0.5) +
  theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) +
  scale_size(range=c(1, 1)) + xlab("time") + ylab("f1c.count")
dev.off()

###### Status extract quantities #######
DtSy[,St1cQuant:=NA]
DtSy[,St1cQuant:=as.character(str_extract( event,"(?<=[.-])[/1234567890g.]{1,4}(($)|(?=[.-]))"))]
DtSy$St1cQuant[DtSy$St1cQuant == "character(0)"] <- NA
DtSy[,St1cDuration:=as.character(str_extract( event,"(?<=[.-])[1234567890]*?[./]*?[1234567890]+[hm]"))]
DtSy$St1cDuration[DtSy$St1cDuration == "character(0)"] <- NA

for (itr in 1:length(DtSy$St1cDuration)) {
  worked_string <- DtSy$St1cDuration[itr]
  if(!is.na(worked_string)){
    if(str_detect(worked_string,"[/]")){
      worked_string <- as.numeric( str_extract_all( worked_string,"[1234567890]{1,3}(?=[/])"))/
        as.numeric( str_extract_all( worked_string,"(?<=[/])[1234567890]{1,3}"))
    }  
    if(str_detect(worked_string,"[\\p{Alphabetic}]")){
      worked_string <- str_extract( worked_string,"[.1234567890]{1,5}")
    } 
  }
  worked_string <- round(as.numeric(worked_string),3)
  DtSy$St1cDuration[itr] <- worked_string
}

#DtSy[,St1cMed:=as.character(median(as.numeric(St1cQuant),na.rm = T)),by=St1c]
DtSy$St1cQuant <- as.numeric(DtSy$St1cQuant)
DtSy$St1cDuration <- as.numeric(DtSy$St1cDuration)
 
DtSy$St1cQuant[is.na(DtSy$St1cQuant)] <- 3

#ggplot(DtSy, aes(y=St1cQuant, x=time)) + geom_path(stat="identity", position="identity", alpha=0.5) + facet_grid(St1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + xlab("time") + ylab("St1cQuant")
ggplot(DtSy, aes(y=St1cQuant, x=time)) + geom_point(stat="identity", position="identity", alpha=0.5, size=3) + geom_line(stat="identity", position="identity", alpha=0.5) + facet_grid(St1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + scale_size(range=c(1, 1)) + xlab("time") + ylab("St1cQuant")

# explicitly stated hours are only average of 1.5 <- 31/19. Stated becuse short! 
# but I expect 2 records to last most of 16h day so 8 each and 5 something actual? 
# LATER remove at sleep cause symptoms tend at start so 3 hours cut off?  NO! MUST BE NA! ELSE PRE_SLEEP_CAUSES GET GREAT VALUES
# but reduce to 75%? LATER or 3 day median? at 3 hour windows
  
# or maybe have list of percents and times because if its only slighty less i don't notice
# for every St.1c, for every row, check row further ahead, 
# if time-distance between the two points is higher than max bounds * 2


# sinc eline graphing instead of density Duration no longer figures into degree calculation but I kept the code just incase
# ONLY FOR LINE GRAPH WE WILL IGNORE IT cause scatter + line is really good enough 
# add time to this row's, subtract from other's add event of 0 with new times
# WARNING ! BAD DESIGN MEANS INE GRAPH WILL LOOK LIKE DURATION BOUNDS NEVER RUN PAST OTHER EVENTS' TIMES
# THEREFORE MENTIONING SHORT RUN INSIDE OTHERS WILL NOT PRODUCE  THE EXPECTED SPIKE IN A MOUNTAIN
# ALSO LAZINESS MEANS INPUT DURATION WILL NOT EFFECT LINE GRAPH OUTSIDE OF WHERE 0 DUMMIES END
DtSy[,St1cStart:=NA]
DtSy[,St1cEnd:=NA]
DtSyT<-data.table()

#i<-333
#should duration be calculated ahead of time?
for (i in 1:length(DtSy$St1cDuration)) {
  
  if(!is.na(DtSy$St1cDuration[i])){
    DtSy$St1cStart[i] <- DtSy$time[i] - DtSy$St1cDuration[i] * hour.constant / 2
    DtSy$St1cEnd[i] <- DtSy$time[i] + DtSy$St1cDuration[i] * hour.constant / 2 
    #CheckStart  <- DtSy$time[i] - DtSy$St1cDuration[i] * hour.constant  
    #CheckEnd <- DtSy$time[i] + DtSy$St1cDuration[i] * hour.constant 
  } else { # if duration is written down its not changed irregardless if other same symptom test are within that duration.
    #If its not then it must be calculated
    DtSy$St1cStart[i] <- DtSy$time[i] - 12 * hour.constant / 2
    DtSy$St1cEnd[i] <- DtSy$time[i] + 12 * hour.constant / 2  
}
for (i in 1:length(DtSy$St1cDuration)) {
 
  if(!is.na(DtSy$St1cDuration[i])){
    DtSy$St1cStart[i] <- DtSy$time[i] - DtSy$St1cDuration[i] * hour.constant / 2
    DtSy$St1cEnd[i] <- DtSy$time[i] + DtSy$St1cDuration[i] * hour.constant / 2 
    #CheckStart  <- DtSy$time[i] - DtSy$St1cDuration[i] * hour.constant  
    #CheckEnd <- DtSy$time[i] + DtSy$St1cDuration[i] * hour.constant 
  } else { # if duration is written down its not changed irregardless if other same symptom test are within that duration.
    #If its not then it must be calculated
    DtSy$St1cStart[i] <- DtSy$time[i] - 12 * hour.constant / 2
    DtSy$St1cEnd[i] <- DtSy$time[i] + 12 * hour.constant / 2 
    CheckStart <- DtSy$time[i] - 12 * hour.constant
    CheckEnd <- DtSy$time[i] + 12 * hour.constant

    inbounds.end <- DtSy[DtSy$time[i] < time  & St1c == DtSy$St1c[i]]
    inbounds.end <- inbounds.end[min(inbounds.end$time) == time]
    inbounds.start <- DtSy[DtSy$time[i] > time & St1c == DtSy$St1c[i]]
    inbounds.start <- inbounds.start[max(inbounds.start$time) == time]
    
    if( (inbounds.end$time[1] < CheckStart)  ){
      DtSy$St1cEnd[i] <- DtSy$time[i] + (inbounds.end$time[1] - DtSy$time[i]) / 2
    }
    if( (inbounds.start$time[1] > CheckStart)  ){   
      DtSy$St1cStart[i] <- DtSy$time[i] - (DtSy$time[i] - inbounds.start$time[1]) / 2
    }
    DtSy$St1cDuration[i] <- (DtSy$St1cEnd[i] - DtSy$St1cStart[i]) /  hour.constant
     }
    # add symptoms of strength 0 for those times I did not use mysym and cause using line graphs now
    # conundrum; how to respect user input duration in placing not exactly zeroeth points
    if(nrow(inbounds.end) < 0){
      inbounds.end <- inbounds.end[min(inbounds.end$time) == time]
      DtSy$St1cEnd[i] <- (inbounds.end$time[1] - DtSy$time[i]) / 2  + DtSy$time[i]
    }
    if(nrow(inbounds.start) < 0){
      dt.one.event <- DtSy[i]
      dt.one.event$time <- CheckStart
      l = list(DtSyT,dt.one.event)
      DtSyT<-rbindlist(l, use.names=TRUE)
     }

  }
}
DtSy$St1cDuration <- round(DtSy$St1cDuration,2)
DtSy[,St1cImpact:=St1cQuant]#St1cDuration* because now using line graph instead of density
maxtime <- max(DtSy$time)+.8


ggplot(DtSy, aes(y=St1cImpact, x=time)) + geom_point(stat="identity", position="identity", alpha=0.5, size=3) + geom_line(stat="identity", position="identity", alpha=0.5) + facet_grid(St1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + scale_size(range=c(1, 1)) + xlab("time") + ylab("St1cQuant")
ggplot(DtSy, aes(y=St1cQuant, x=time)) + geom_point(stat="identity", position="identity", alpha=0.5, size=3) + geom_line(stat="identity", position="identity", alpha=0.5) + facet_grid(St1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + scale_size(range=c(1, 1)) + xlab("time") + ylab("St1cQuant")+ xlim(maxtime-20,maxtime )
  
png(filename = paste0("status_weighd_density.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(DtSy, aes(x = time, weights = St1cImpact)) + geom_density(adjust = 1/8) + facet_grid(St1c ~ .) 
dev.off()

png(filename = paste0("status_weighd_density_limd.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(DtSy, aes(x = time, weights = St1cImpact)) + geom_density(adjust = 1/8) +
  facet_grid(St1c ~ .)  + xlim(maxtime-20,maxtime )
dev.off()
# + expand_limits(x = c(first.day, last.day)) 

#### dt recombine #####

require(data.table)
require(ggplot2)
# update dt.time.loc.event to current standards
dt.test <- dt.time.loc.event[!is.na(istest)][checksum>7]
columns.after <- which(names(dt.test)=="checksum") -1
columns.before <- which(names(dt.test)=="istest") +1 
dt.temporary <- dt.test[, (((columns.after:columns.before))),with=FALSE]
dt.test$test1cQuant <- apply(dt.temporary,1, min, na.rm=TRUE)
# m19lc and m20lc should merge 

# DtSy dt.drug dt.food dt.act
# label datapoints by type
# create a single "impact" for all
# are these events of interest as cause or effect
# combine the datasets 

dt.test[,category:="test"]; DtSy[,category:="state"];dt.food[,category:="food"];dt.drug[,category:="drug"];dt.act[,category:="act"];
dt.test[,first_level_of_event:=istest];DtSy[,first_level_of_event:=St1c];dt.food[,first_level_of_event:=food1c];dt.drug[,first_level_of_event:=drug1c];dt.act[,first_level_of_event:=act1c];
dt.test[,impact:=test1cQuant];DtSy[,impact:=St1cImpact];dt.food[,impact:=food1cQuant];dt.drug[,impact:=1];dt.act[,impact:=act1cQuant];

bad.activites <- c("a-internethypnotized", "a-wank", "a-rockingchair", "a-daydreaming","a-AMV") 
bad.state <- c(  "s-fart"     ,   "s-dizzy"     ,    "s-headache" ,   "s-nauseous"  ,  "s-eyeshurt" ,  
               "s-rocking" ,    "s-sleep"   ,    "s-daydreaming" ,    "s-stomach"  ,    
               "s-compulsion" , "s-lung"  ) #unique(DtSy$state1c)
good.state <- c("s-excited","s-energetic")
dt.test[,cause:=FALSE];DtSy[,cause:=TRUE];dt.food[,cause:=TRUE];dt.drug[,cause:=TRUE];dt.act[,cause:=TRUE];
dt.test[,Effect_of_interest:= TRUE];DtSy[,Effect_of_interest:=F];dt.food[,Effect_of_interest:=FALSE];dt.drug[,Effect_of_interest:=FALSE];
dt.act[,Effect_of_interest:= F] #first_level_of_event %in% bad.activites];

#dt.recombined <- dt.test[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)] 
dt.recombined <- rbindlist(list(dt.test[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)],
                              DtSy[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)],
                              dt.act[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)],
                              dt.drug[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)],
                              dt.food[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)]
), use.names=TRUE, fill=FALSE, idcol=TRUE)
summary(dt.recombined)
dt.recombined <- dt.recombined[order(time)]
dt.recombined$impact <- as.numeric(dt.recombined$impact)
dt.recombined[,orgquant:=NA]
dt.recombined$orgquant[dt.recombined$category == "state"] <- as.numeric(DtSy$St1cQuant) #length(DtSy$St1cQuant);length(dt.recombined$orgquant[dt.recombined$category == "state"])
