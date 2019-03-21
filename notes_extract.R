#for every unique first level 
# ----  probably should keep these seperate ----
require(data.table)
require(stringr)
NotesExtract <- function(event_type="s-headache", missed.strings=dt.missedstring, recombined=dt.recombined )
  {
#event_type<- "s-headache"; missed.strings=dt.missedstring;recombined=dt.recombined
#if usr do not want both data objects use NA for the one usr do not want.  


if(any(!is.na(recombined))){
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

if(any(!is.na(missed.strings))){
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

if(any(!is.na(recombined))){
  #combine and sort recombine and output.dt
output.dt <- rbindlist(list(output.dt,recombined)
                     , use.names=T, fill=T, idcol=T)
}

}

output.dt <- output.dt[order(output.dt$time)]
return(output.dt)
} 

 require(data.table)
WordCounts <- function(extranotes = extranotes, percent.of.string = T, nameed = "headache") {
forwordcount <- vector(mode = "character")
lcount <- length(extranotes$time)
for (itr in c(1:30)) {
  forwordcount <- append(forwordcount, word(extranotes$event, itr, sep = fixed('.')))
}
if(!percent.of.string){
forwordcount <- forwordcount[!is.na(forwordcount)]
forwordcount <- forwordcount[forwordcount != ""]
forwordcount <- forwordcount[!(forwordcount %in% c("0","1","2","3","4","5","6","7","8","9"))]
wat <- (table(forwordcount))
return(wat[order(-wat)])
}
events.count <- length(extranotes$event)
relatv.wordcount <- data.table(forwordcount, 1:events.count)
relatv.wordcount <- relatv.wordcount[!is.na(forwordcount)]
relatv.wordcount <- relatv.wordcount[forwordcount != ""]
relatv.wordcount <- relatv.wordcount[!(forwordcount %in% c("0","1","2","3","4","5","6","7","8","9"))]
relatv.wordcount[,relatv:=.N, by=V2]
relatv.wordcount[,relatv:=1/relatv]
relatv.wordcount[,count:=round(sum(relatv), digits = 2), by=forwordcount]
outit <- relatv.wordcount[, .SD[1], by=forwordcount] 
outit <- outit[,.(forwordcount,count)]

outittoper <-  data.table(
  forwordcount = nameed,
  count = lcount
)
l = list(outittoper,outit[order(-count)])
return( rbindlist(l, use.names=TRUE))
}
 

# create vector of word or character counts in each string and glue it to un-de-NAed wordcount vector to create datatable
# then undecided data table manip to make some kind of importance score

