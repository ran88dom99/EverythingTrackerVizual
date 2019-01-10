# update dt.time.loc.event to current standards
dt.test<-dt.time.loc.event[!is.na(istest)][checksum>7]
columns.after<-which(names(dt.test)=="checksum") -1
columns.before<-which(names(dt.test)=="istest") +1 
dt.temporary<-dt.test[, (((columns.after:columns.before))),with=FALSE]
dt.test$test1cQuant<- apply(dt.temporary,1, min, na.rm=TRUE)

# dt.state dt.drug dt.food dt.act
# label datapoints by type
# create a single "impact" for all
# are these events of interest as cause or effect
# combine the datasets 

dt.test[,category:="test"]; dt.state[,category:="state"];dt.food[,category:="food"];dt.drug[,category:="drug"];dt.act[,category:="act"];
dt.test[,first_level_of_event:=istest];dt.state[,first_level_of_event:=state1c];dt.food[,first_level_of_event:=food1c];dt.drug[,first_level_of_event:=drug1c];dt.act[,first_level_of_event:=act1c];
dt.test[,impact:=test1cQuant];dt.state[,impact:=state1cImpact];dt.food[,impact:=food1cQuant];dt.drug[,impact:=1];dt.act[,impact:=act1cQuant];

bad.activites<-c("a-internethypnotized", "a-wank", "a-rockingchair", "a-daydreaming","a-AMV") 
bad.state<-c(  "s-fart"     ,   "s-dizzy"     ,    "s-headache" ,   "s-nauseous"  ,  "s-eyeshurt" ,  
               "s-rocking" ,    "s-sleep"   ,    "s-daydreaming" ,    "s-stomach"  ,    
               "s-compulsion" , "s-lung"  ) #unique(dt.state$state1c)
good.state<-c("s-excited","s-energetic")
dt.test[,cause:=FALSE];dt.state[,cause:=TRUE];dt.food[,cause:=TRUE];dt.drug[,cause:=TRUE];dt.act[,cause:=TRUE];
dt.test[,Effect_of_interest:=TRUE];dt.state[,Effect_of_interest:=TRUE];dt.food[,Effect_of_interest:=FALSE];dt.drug[,Effect_of_interest:=FALSE];
dt.act[,Effect_of_interest:=first_level_of_event %in% bad.activites];

#dt.recombined<-dt.test[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)] 
dt.recombined<-rbindlist(list(dt.test[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)],
                              dt.state[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)],
                              dt.act[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)],
                              dt.drug[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)],
                              dt.food[, .(inears,location.long,location,location.short,time,event,first_level_of_event,impact,category,Effect_of_interest,cause)]
                              ), use.names=TRUE, fill=FALSE, idcol=TRUE)
summary(dt.recombined)
dt.recombined<-dt.recombined[order(time)]
dt.recombined$impact <- as.numeric(dt.recombined$impact)

# for every cause 
# and interesting effects 
# for a set of durations/windows 
# for set of lengths untill windows of durations start but never more than windows
# and finaly for window overlap
# or spearman
# compared to what windows exactly? #
causes.unique <- as.vector(unique(dt.recombined[cause==TRUE , .(first_level_of_event)])[[1]])
effects.unique <- as.vector(unique(dt.recombined[Effect_of_interest==TRUE , .(first_level_of_event)])[[1]])
window.lengths <- c(.5,1,10,30,60,180,600,1200,6000,15000) * minute.constant
window.lengths <- round(window.lengths, digits = 5)
compare.window <- 3

for (cause in causes.unique) {
  for (effect in effects.unique) {
    for (window in window.lengths) {
      for (incubation in window.lengths[window.lengths<window]) {
        selection <- dt.recombined$first_level_of_event==cause | dt.recombined$first_level_of_event==effect
        dt.worked <- dt.recombined[selection]
        dt.worked$cause <- dt.worked$first_level_of_event==cause
        dt.worked$effect <- dt.worked$first_level_of_event==effect
        
        dt.worked[,impacted:=0]
        dt.worked[,inzone:=FALSE]
        for (each.cauz in which(dt.worked$cause)) {
          
          start.window <- dt.worked[each.cauz]$time + incubation
          end.window <- dt.worked[each.cauz]$time + incubation + window
          mpakt <- dt.worked[each.cauz]$impact
          if( length( dt.worked[time > start.window & time < end.window]$impact ) >0)
          dt.worked[time > start.window & time < end.window]$impacted <- dt.worked[time > start.window & time < end.window]$impacted + mpakt
          
          start.window <- dt.worked[each.cauz]$time + incubation -  window - compare.window
          end.window <- dt.worked[each.cauz]$time + incubation + window + compare.window
          if( length( dt.worked[time > start.window & time < end.window]$impact ) >0)
            dt.worked[time > start.window & time < end.window]$inzone <- TRUE
  
        }
        cause.count<-sum(dt.worked$cause)
        effect.count<-sum(dt.worked$effect)
        dt.worked<-dt.worked[inzone == T & effect == T]
        comparison.count <- sum(dt.worked$impacted==0)
        
        try({
        corr<-cor.test( dt.worked$impact, dt.worked$impacted,
                        alternative="two" , method = "pearson" , exact=F )
        if(is.na(corr)) next()
        a  <- corr$conf.int[1]
        b <- corr$conf.int[2]
        highest.effect <- 0
        if(sign(a)==sign(b)){
          if(sign(a)>0)
            highest.effect<-min(a,b)
          if(sign(a)<0)
            highest.effect<-max(a,b)
        }
        highest.effect<-round(highest.effect, digits = 3)
        
        print(paste(
          highest.effect , cause, cause.count, effect, effect.count, window,
          incubation, comparison.count
          )  )
        })
        
      }
    }
  }
}

