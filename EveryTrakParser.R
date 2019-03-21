source("runfirst.R")
require(lubridate)
require(dplyr)
require(stringr)

# read file 3 ways
fileName <- 'diet journal.txt'
singleString1 <- readChar(fileName, file.info(fileName)$size)
# singleString2 <- paste(readLines(fileName), collapse=" ")
# singleString3 <- scan(fileName, what="character", sep=NULL)
# 1 best because \r\n included but 3 comes pre chopped
# grep can not deal with variable lookahead
# convert "# notes notes" to "#.notes.notes"

pound.notes <- str_locate_all(singleString1,"#.+\r\n")[[1]]
poundreplacement <- str_replace_all( str_sub(singleString1, pound.notes), " ", ".")
for (itr in 1:length(poundreplacement)) {
  str_sub(singleString1, start = pound.notes[itr,1], end  = pound.notes[itr,2]) <- poundreplacement[itr]
}#str_sub(singleString1, pound.notes),1:2


# remove date spaces then chop by spaces
str_extract_all(singleString1,"[1234567890]{1,2}:[1234567890]{1,2}[[:blank:]](AM|PM)[ ][1234567890]{1,2}/[1234567890]{1,2}/[1234567890]{4}")
singleString1 <- str_replace_all(singleString1,"(?<=[1234567890]{1,2}:[1234567890]{1,2})[ ](?=AM|PM [1234567890]{1,2}/[1234567890]{1,2}/[1234567890]{4})","")
singleString1 <- str_replace_all(singleString1,"(?<=[1234567890]{1,2}:[1234567890]{1,2}(AM|PM))[ ](?=[1234567890]{1,2}/[1234567890]{1,2}/[1234567890]{4})","")
str_extract_all(singleString1,"[1234567890]{1,2}:[1234567890]{1,2}(AM|PM)[1234567890]{1,2}/[1234567890]{1,2}/[1234567890]{4}")
split_date_txt <- str_replace_all(singleString1,"(\r)|(\n)"," ")
split_date_txt <- str_split(split_date_txt,"[ ](?=[1234567890]{1,2}:[1234567890]{1,2}(AM|PM)[1234567890]{1,2}/[1234567890]{1,2}/[1234567890]{4})")[[1]]
split_space_txt <- str_split(split_date_txt," ")
#character vector chopped by space creating list of characters

minute.constant<-1/(24*60)
hour.constant<-1/(24)
require(data.table)
short.timer<-2
vec_times<-vector()
where.long="Florida";where="campground";where.short="outside";inears="s-e-in"
dt.time.loc.event<-  data.table(
  inears = inears,
  location.long = where.long,
  location = where,
  location.short = where.short,
  time = "0",
  event = "sleep"
)
dt.missedstring <-  data.table(
  inears = inears,
  location.long = where.long,
  location = where,
  location.short = where.short,
  time = "0",
  event = "what??waswrittenhere?"
)
tripple.check <- data.table( 
  event = "what??waswrittenhere?"
)

#error.test<-"12:45AM12/17/2018"; itr<-774
first_posix_datetime<-parse_date_time("4:00AM9/19/2018", c("%I:%M p! %m %d %y") )
for(itr in 2:length(split_date_txt)){
  vec_times[itr] <- parse_date_time(split_space_txt[[itr]][1], c("%I:%M p! %m %d %y") ) - first_posix_datetime

  
  if(length(split_space_txt[[itr]])<2) next()
  for(initr in 2:length(split_space_txt[[itr]])){
    string.worked<-split_space_txt[[itr]][initr]
    if(is.na(string.worked)) next()
    split_space_txt[[itr]][initr]<-string.worked
    if(str_detect(string.worked, "^s-e-")) inears<-string.worked
      if(str_detect(string.worked, "^where-long")) where.long<-string.worked
      if(str_detect(string.worked, "^where-(?!((long)|(short)))")) where<-string.worked
      if(str_detect(string.worked, "^where-short")) {
        where.short<-string.worked
        short.timer<-vec_times[itr]
      } 
    if(vec_times[itr]>(short.timer+3*hour.constant)) where.short<-"unknown"
    
    dt.one.event <- data.table( #to tripple check what may have slipped past
      event = string.worked)
    l = list(tripple.check,dt.one.event)
    tripple.check<-rbindlist(l, use.names=TRUE)
    
    cork.form<-str_detect(string.worked, "(^.{1,4}-)|([1234567890;]{2,}[\\p{Alphabetic}]{2})|([1234567890]{1,}[pRrNnmb]{1}([ ]|$))|(m5x3dm)|(SET)|(where-)|(s-e-)")
    if(cork.form){
    dt.one.event <- data.table(
      inears = inears,
      location.long = where.long,
      location = where,
      location.short = where.short,
      time = vec_times[itr] + (initr-2) * minute.constant,
      event = string.worked
    )
    l = list(dt.time.loc.event,dt.one.event)
    dt.time.loc.event<-rbindlist(l, use.names=TRUE)
    }

    
    if(!cork.form & nchar(string.worked)>2){
      dt.one.event <- data.table(
        inears = inears,
        location.long = where.long,
        location = where,
        location.short = where.short,
        time = vec_times[itr] + (initr-2) * minute.constant,
        event = string.worked
      )
      l = list(dt.missedstring,dt.one.event)
      dt.missedstring <- rbindlist(l, use.names=TRUE)
    }
  }
}

#LOTS OF STUFF WAS MOVED TO TESTANALYSISPLOT.R 
#things expected here may be there

#all state like location and computer must be applied to other data sources and reaplied to 
#this one in case something went out of order

#### input from mySymptoms ####
#must add many extra columns again
mysym.file<-"mySymptomsDiary.csv"
mySymptoms<-read.csv(mysym.file, header = F, quote = "")
  
dt.mysymp<-  data.table(
  inears = inears,
  location.long = where.long,
  location = where,
  location.short = where.short,
  time = "0",
  event = "sleep"
)

  zeroth_posix_datetime<-parse_date_time(" 0:01", c("%H:%M") )
Clean <- function(x){
  if(str_detect(x,"Duration:")){
    durat <- as.numeric( parse_date_time(x, c("%H:%M") ) - zeroth_posix_datetime) * 60
    return(paste0(durat,"m"))
  }
  x<- str_replace_all(x , "\"", "")
  x<- str_replace_all(x , "Notes:", "")
  x<- str_replace_all(x , "Intensity:", "")
  x<- str_replace_all(x , "Duration:", "")
  x<- str_replace_all(x , fixed("40m."), "")
  x<- str_replace_all(x , fixed("[ x "), "")
  x<- str_replace_all(x , fixed(" ]"), "")
  x<- str_replace_all(x , " ", ".")
  x<- str_replace_all(x , fixed(".."), ".")
  x<- str_replace_all(x , fixed(".."), ".")
  x<- str_replace_all(x , fixed("-."), "-")
  return(x)
}  
Detect <- function(x){
  if(str_detect(x,"Duration:")){ return(T)}
  if(str_detect(x,"Intensity:")){ return(T)}
  if(str_detect(x,fixed("[ x "))) return(T) 
  if(str_detect(x,"Notes:")) return(T)
  return(F)
}
  

first_posix_datetime<-parse_date_time("4:00AM9/19/2018", c("%I:%M p! %m %d %y") )
  paste(mySymptoms[1,2],mySymptoms[1,1])
  parse_date_time(" 15:03 10/31/2018", c("%H:%M %m %d %y") )
  vec_times<-vector(length = length(mySymptoms[,1]), mode = "numeric")
 
#get all possible symptoms for symptom filling  
allpossiblesymptoms<-mySymptoms[mySymptoms$V3==" Symptom",]
allpossiblesymptoms<-(allpossiblesymptoms[,4:length(allpossiblesymptoms)])
dimsallposs<-dim(allpossiblesymptoms)
allSyms<-c()
for(itrc in 1:dimsallposs[2]){
  for(itrr in 1:dimsallposs[1]){
    if(!is.na(allpossiblesymptoms[itrr,itrc])){
     if(!Detect(allpossiblesymptoms[itrr,itrc])){
      allSyms<-union(allSyms, allpossiblesymptoms[itrr,itrc])
     }
    }
  }
}
allSyms<-setdiff(allSyms,"")
    
abrv<-c(" Bowel Movement", "e-",
        " Breakfast", "f-",
        " Dinner", "f-",
        " Drink", "f-",
        " Energy", "s-",
        " Exercise", "a-",
        " Lunch", "f-",
        " Medication", "d-",
        " Other", "e-",
        " Sleep Quality", "s-",
        " Snack", "f-",
        " Stress", "a-",
        " Supplements", "d-",
        " Symptom", "s-")
  
for (itr in 1:length(mySymptoms[,1])) {
  if(mySymptoms[itr,2]=="") next()
  work.vec<-mySymptoms[itr,]
  work.vec <- work.vec[!is.na(work.vec)]
  work.vec <- work.vec[work.vec != ""]
  work.vec <- work.vec[work.vec != " "]
  vec_times[itr]  <- (parse_date_time(paste(work.vec[2],work.vec[1]), c("%H:%M %m %d %y") )) - (first_posix_datetime) 
  
  #turns out notes are for last item always
  #noters<-""
  #if(sum(str_detect(work.vec,"Notes:"))>0) 
  #  noters <- work.vec[which.max(str_detect(work.vec,"Notes:"))]

  sympt.fill<-F; 
  if(work.vec[3] == " Symptom") {
    sympt.fill<-T ; unfillSyms<-allSyms}
  
  categ<-abrv[which.max(abrv %in% work.vec[3]) + 1]
  wrt.str <- categ
  lwv<-length(work.vec)
  if(lwv<4) next()
  for (itrc in 4:lwv) {
    wrt.str <- paste(wrt.str,Clean(work.vec[itrc]), sep =".")
    
    if(sympt.fill & !Detect(work.vec[itrc])){
      unfillSyms<-setdiff(unfillSyms,work.vec[itrc]) }
    
    if((itrc+1) <= lwv){
    if(!Detect(work.vec[itrc+1])) {
      dt.one.event <- data.table(
        inears = "unk",
        location.long = "unk",
        location = "unk",
        location.short = "unk",
        time = vec_times[itr] ,
        event = Clean(wrt.str)
      )
      l = list(dt.mysymp,dt.one.event)
      dt.mysymp<-rbindlist(l, use.names=TRUE)
      wrt.str <- categ
    }
    }
    
  }
      dt.one.event <- data.table(
      inears = "unk",
      location.long = "unk",
      location = "unk",
      location.short = "unk",
      time = vec_times[itr] ,
      event = Clean(wrt.str)
    )
    l = list(dt.mysymp, dt.one.event)
    dt.mysymp <- rbindlist(l, use.names=TRUE) 
    
    if(sympt.fill){
       for (itru in unfillSyms) {
         dt.one.event <- data.table(
           inears = "unk",
           location.long = "unk",
           location = "unk",
           location.short = "unk",
           time = vec_times[itr] ,
           event = Clean(paste(categ,Clean(itru),"0", sep ="."))
         )
         l = list(dt.mysymp, dt.one.event)
         dt.mysymp <- rbindlist(l, use.names=TRUE) 
      }
      
      }
    
  #if(mySymptoms[itr,3]=="Bowel Movement") string.worked<-paste("e-bowel",mySymptoms[itr,5],CleanNotes(mySymptoms[itr,6]), sep = ".")
}
  
    
  unique(dt.mysymp$event)
  #dt.mysymp | (event %in% c("f-foj","f-ensure")) 
  #most of these ar fixed by str_to_lower_case
  conversion<-c("s-Eyestrain", "s-eyeshurt" ,
                "s-Daydream" , "s-daydreaming",
                "s-Sleep"        , "s-sleep",
                "s-Neckache"    , "s-neckache", 
                "s-Stomach"     , "s-stomach",
                "s-Daydreaming"  , "s-daydreaming",
                "s-Tired"       , "s-tired",
                "s-Nausea"       ,  "s-nauseous"  ,
                "s-Compulsion"   , "s-compulsion",
                "s-Lung"        , "s-lung" ,
                "s-Rocking"      , "s-rocking"  ,
                "s-Energetic"    , "s-energetic",
                "s-Sleepy"      , "s-sleep",
                "vitamin.c", "vitC")
  for(itr in 1:length(dt.mysymp$event)){ #itr<-3
    detected.convert  <- str_detect(dt.mysymp$event[itr],fixed(conversion))
     which.detect <- which.max( detected.convert )
    if(sum( detected.convert )>0) {
      str_sub(dt.mysymp$event[itr], 
              str_locate(dt.mysymp$event[itr],fixed(conversion[which.detect])) )  <-
        conversion[(which.detect %% 2) + which.detect]
    }#dt.mysymp$event[itr]
     dt.mysymp$event[itr]<-str_to_lower(dt.mysymp$event[itr])
  }
    

#### redo unchanging states, combine sources ####  
  l = list(dt.time.loc.event, dt.mysymp )
  dt.time.loc.event <- rbindlist(l, use.names=TRUE, idcol = "source") 
  dt.time.loc.event$time <- as.numeric(dt.time.loc.event$time)
  dt.time.loc.event <- dt.time.loc.event[order(dt.time.loc.event$time)]
  
for(itr in 1:length(dt.time.loc.event$time)){ #itr<-4
  string.worked <- dt.time.loc.event$event[itr]
  if(str_detect(string.worked, "^s-e-")) inears<-string.worked
  if(str_detect(string.worked, "^where-long")) where.long<-string.worked
  if(str_detect(string.worked, "^where-(?!((long)|(short)))")) where<-string.worked
  if(str_detect(string.worked, "^where-short")) {
    where.short<-string.worked
    short.timer<-dt.time.loc.event$time[itr]
  } 
  if(dt.time.loc.event$time[itr]>(short.timer+3*hour.constant)) where.short<-"unknown"
  
  dt.time.loc.event$inears[itr] <- inears 
  dt.time.loc.event$location.long[itr] <- where.long
  dt.time.loc.event$location[itr] <- where
  dt.time.loc.event$location.short[itr] <- where.short 
}
  