#for every unique first level 
# ----  probably should keep these seperate ----
require(data.table)
require(stringr)
NotesExtract <- function(event_type="s-headache", missed.strings=dt.missedstring, recombined=dt.recombined )
  {
#event_type<- "s-headache"; missed.strings=dt.missedstring;recombined=dt.recombined
#if usr do not want both data objects use NA for the one usr do not want.  


if(!is.na(recombined)){
# for every extra of type-name added as connected note
recombined <- recombined[first_level_of_event == event_type] 
recombined[,org.event:=event]
recombined[,event:=str_replace(org.event,event_type,"")]
recombined[,event:=str_replace(event,"[.-]","")]
recombined <- recombined[str_detect(event, "[aqwszxcderfvbgtyhnmjuiklop]{2,}")]
#  add this string to output.dt
recombined <- recombined[,.(inears,location.long,location,location.short,time,event,
                            impact,orgquant)]
output.dt <- recombined
}

if(!is.na(missed.strings)){
output.dt <- missed.strings[1]
event_type <- str_replace(event_type,".+-","")
# for every . connected chunk in dt.missedstring itr<-2
for(itr in 1:length(dt.missedstring$event)){
#  if it contains the event_type string 
  if(str_detect(dt.missedstring$event[itr],event_type)){
#   add it to output.dt  
    output.dt <- rbindlist(list(output.dt, dt.missedstring[itr])
                              , use.names=TRUE, fill=FALSE, idcol=FALSE)
  }
}
output.dt$time <- round(as.numeric(output.dt$time), digits = 4)
#str(output.dt);str(recombined)
output.dt <- output.dt[-1]

if(!is.na(recombined)){
  #combine and sort recombine and output.dt
output.dt <- rbindlist(list(output.dt,recombined)
                     , use.names=T, fill=T, idcol=T)
}

}

output.dt <- output.dt[order(output.dt$time)]
return(output.dt)
} 
 
WordCounts <- function(extranotes = extranotes){
forwordcount <- vector(mode = "character")
for (itr in c(1:30)) {
  forwordcount <- append(forwordcount, word(extranotes$event, itr, sep = fixed('.')))
}
forwordcount <- forwordcount[!is.na(forwordcount)]
forwordcount <- forwordcount[forwordcount != ""]
forwordcount <- forwordcount[!(forwordcount %in% c("0","1","2","3","4","5","6","7","8","9"))]
wat <- (table(forwordcount))
return(wat[order(-wat)])
}

# create vector of word or character counts in each string and glue it to un-de-NAed wordcount vector to create datatable
# then undecided data table manip to make some kind of importance score
