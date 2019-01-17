dt.time.loc.event[,notest := event ]
dt.time.loc.event$notest[!is.na(dt.time.loc.event$istest)]<-NA


dt.time.loc.event[,locate_n_earplug := NA ]
for(itr in 1:length(dt.time.loc.event$event)){
  if(str_detect(dt.time.loc.event$event[itr], "^where-|^s-e-")){
    dt.time.loc.event$locate_n_earplug[itr]<-dt.time.loc.event$event[itr]
  }
}

#library(ggraptR)
#ggraptR()

#### Food ####
dt.time.loc.event$notest[!is.na(dt.time.loc.event$locate_n_earplug)]<-NA
sort((dt.time.loc.event$notest)) #  unique
dt.time.loc.event[,food:=notest]
dt.time.loc.event$food[!str_detect(dt.time.loc.event$food, "^f-")]<-NA
dt.food<-dt.time.loc.event[!is.na(food)]#sort(table(dt.time.loc.event$food))
dt.food<-dt.food[, .(inears,location.long,location,location.short,time,event)] 
dt.food[,food1c:=as.character(str_extract_all( event,"^f-[\\p{Alphabetic}]{1,30}"))]
dt.food[,f1c.count:=.N,by=food1c]
dt.food<-dt.food[f1c.count>4]
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
  facet_grid(act1c ~ .)  + xlim(maxtime-20,maxtime )
dev.off()
#### State Symptom ####
dt.time.loc.event[,state:=notest]
dt.time.loc.event$state[!str_detect(dt.time.loc.event$state, "^s-")]<-NA
dt.state<-dt.time.loc.event[!is.na(state)]#sort(table(dt.time.loc.event$state))
dt.state<-dt.state[, .(inears,location.long,location,location.short,time,event)] 
dt.state[,state1c:=as.character(str_extract_all( event,"^s-[\\p{Alphabetic}]{1,30}"))]
dt.state[,s1c.count:=.N,by=state1c]
dt.state<-dt.state[s1c.count>4]


png(filename = paste0("statusviolin.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.state, aes(y=time, x=as.factor(state1c))) +
  geom_violin(stat="ydensity", position="dodge", alpha=0.5, trim=TRUE, scale="area") +
  geom_boxplot(stat="boxplot", position="dodge", alpha=0.5, width=0.2) + theme_grey() +
  theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) +
  xlab("as.factor(state1c)") + ylab("time")
dev.off()

png(filename = paste0("status.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.state, aes(y=state1c, x=time)) + 
  geom_point(aes(colour=state1c),
             stat="identity", position="identity", alpha=0.5, size=3) +
  geom_line(aes(colour=state1c), stat="identity", position="identity", alpha=0.5) +
  theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) +
  scale_size(range=c(1, 1)) + xlab("time") + ylab("f1c.count")
dev.off()

###### Status extract quantities #######
dt.state[,state1cQuant:=NA]
dt.state[,state1cQuant:=as.character(str_extract( event,"(?<=[.-])[/1234567890g.]{1,4}(($)|(?=[.-]))"))]
dt.state$state1cQuant[dt.state$state1cQuant == "character(0)"]<-NA
dt.state[,state1cDuration:=as.character(str_extract( event,"(?<=[.-])[1234567890]*?[./]*?[1234567890]+[hm]"))]
dt.state$state1cDuration[dt.state$state1cDuration == "character(0)"]<-NA

for (itr in 1:length(dt.state$state1cDuration)) {
  worked_string<-dt.state$state1cDuration[itr]
  if(!is.na(worked_string)){
    if(str_detect(worked_string,"[/]")){
      worked_string<-  as.numeric( str_extract_all( worked_string,"[1234567890]{1,3}(?=[/])"))/
        as.numeric( str_extract_all( worked_string,"(?<=[/])[1234567890]{1,3}"))
    }  
    if(str_detect(worked_string,"[\\p{Alphabetic}]")){
      worked_string<-str_extract( worked_string,"[.1234567890]{1,5}")
    } 
  }
  worked_string<-round(as.numeric(worked_string),3)
  dt.state$state1cDuration[itr]<-worked_string
}

#dt.state[,state1cMed:=as.character(median(as.numeric(state1cQuant),na.rm = T)),by=state1c]
dt.state$state1cQuant<-as.numeric(dt.state$state1cQuant)
dt.state$state1cDuration<-as.numeric(dt.state$state1cDuration)
 
dt.state$state1cQuant[is.na(dt.state$state1cQuant)]<-3

#ggplot(dt.state, aes(y=state1cQuant, x=time)) + geom_path(stat="identity", position="identity", alpha=0.5) + facet_grid(state1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + xlab("time") + ylab("state1cQuant")
ggplot(dt.state, aes(y=state1cQuant, x=time)) + geom_point(stat="identity", position="identity", alpha=0.5, size=3) + geom_line(stat="identity", position="identity", alpha=0.5) + facet_grid(state1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + scale_size(range=c(1, 1)) + xlab("time") + ylab("state1cQuant")

# explicitly stated hours are only average of 1.5 <- 31/19. Stated becuse short! 
#but I expect 2 records to last most of 16h day so 5? 
#LATER remove at sleep cause symptoms tend at start so 3 hours cut off?  NO! MUST BE NA! ELSE PRE_SLEEP_CAUSES GET GREAT VALUES
# but reduce to 75%? LATER or 3 day median? at 3 hour windows
  
#or maybe have list of percents and times because if its only slighty less i don't notice
#for every state.1c, for every row, check row further ahead, 
#if time-distance between the two points is higher than max bounds * 2

#ONLY FOR LINE GRAPH WE WILL IGNORE IT cause scatter + line is really good enough 
# add time to this row's, subtract from other's add event of 0 with new times
dt.state[,state1cStart:=NA]
dt.state[,state1cEnd:=NA]
 
#i<-333
for (i in 1:length(dt.state$state1cDuration)) {

  if(!is.na(dt.state$state1cDuration[i])){
    dt.state$state1cStart[i] <- dt.state$time[i] - dt.state$state1cDuration[i] * hour.constant / 2
    dt.state$state1cEnd[i] <- dt.state$time[i] + dt.state$state1cDuration[i] * hour.constant / 2 
  } else { 
    dt.state$state1cStart[i] <-  dt.state$time[i] - 8 * hour.constant / 2
    dt.state$state1cEnd[i] <- dt.state$time[i] + 8 * hour.constant / 2 
    CheckStart <- dt.state$time[i] - 8 * hour.constant
    CheckEnd <- dt.state$time[i] + 8 * hour.constant
    
    inbounds.end <- dt.state[dt.state$time[i] < time & CheckEnd > time & state1c == dt.state$state1c[i]]
    inbounds.start <- dt.state[dt.state$time[i] > time & CheckStart < time & state1c == dt.state$state1c[i]]
    
    if(nrow(inbounds.end) > 0){
      inbounds.end<-inbounds.end[min(inbounds.end$time) == time]
      dt.state$state1cEnd[i] <- (inbounds.end$time[1] - dt.state$time[i]) / 2  + dt.state$time[i]
    }
    if(nrow(inbounds.start) > 0){
      inbounds.start<-inbounds.start[max(inbounds.start$time) == time]
      dt.state$state1cStart[i] <- dt.state$time[i] - (dt.state$time[i] - inbounds.start$time[1]) / 2
    }
    dt.state$state1cDuration[i] <- (dt.state$state1cEnd[i] - dt.state$state1cStart[i]) /  hour.constant
  }
}
dt.state$state1cDuration<-round(dt.state$state1cDuration,2)
dt.state[,state1cImpact:=state1cDuration*state1cQuant]
maxtime<-max(dt.state$time)

ggplot(dt.state, aes(y=state1cImpact, x=time)) + geom_point(stat="identity", position="identity", alpha=0.5, size=3) + geom_line(stat="identity", position="identity", alpha=0.5) + facet_grid(state1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + scale_size(range=c(1, 1)) + xlab("time") + ylab("state1cQuant")
ggplot(dt.state, aes(y=state1cQuant, x=time)) + geom_point(stat="identity", position="identity", alpha=0.5, size=3) + geom_line(stat="identity", position="identity", alpha=0.5) + facet_grid(state1c ~ .) + theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + scale_size(range=c(1, 1)) + xlab("time") + ylab("state1cQuant")+ xlim(maxtime-20,maxtime )
  
png(filename = paste0("status_weighd_density.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.state, aes(x = time, weights = state1cImpact)) + geom_density(adjust = 1/8) + facet_grid(state1c ~ .) 
dev.off()

png(filename = paste0("status_weighd_density_limd.png"), width = 1366/itemNlargeConst,
    height = 768/itemNlargeConst)
ggplot(dt.state, aes(x = time, weights = state1cImpact)) + geom_density(adjust = 1/8) +
  facet_grid(state1c ~ .)  + xlim(maxtime-20,maxtime )
dev.off()
# + expand_limits(x = c(first.day, last.day)) 

