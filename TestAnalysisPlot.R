
# location long and medium as. part of time lables # HARD 
# periodicity, how does time of day affet it all?  6.78 %% 1
# how do I geal with event and count data?
# feature creation based on what window orindicator creates the biggest difference
# after accounting for time?
# to explain by location maybe use faceting?

# linesupdown
# better grid overall?
# keep dates in dt; itl be easier to read
# how are my scores today? picture;includes uncolored mainscatter and 3 marginal 1 bot 2 right (over 2months and over last 2 days)
# maybe this should follow cause-effect analysis?

# event cause effect analysis graphs?
# compare as bivariate scatterplots AND timeseries one above the other
# then there is q-q scatter over time with a line for each hourly "batch" or maybe just day
# genius settiing 00:00 at 4am around here; day can be used without having to claculate bins 

# unique() tester of all regexes

abrv.fullname<-c(  "SET","SETCARDS", "minuteseconds", "False" ,
                   "VR","HBverbal", "auto"  , "True" , 
                   "vzs","HBVizualScore", "auto" , "True" , 
                   "vzl","HBVizualLevel", "auto" , "True" , 
                   "n","HBnumbermemory", "auto" , "True" , 
                   "r","HBreaction", "auto" , "False" , 
                   "sc","shortcardsmed", "special", "True" ,
                   "sc","shortcardsmax", "special", "True" , 
                   "lc","TimetoMemorizeCards", "special", "False" , 
                   "p","pagesread" ,  "auto" , "True" , 
                   "m5x3dm" , "TimetoMultiplyDice5x3" , "minuteseconds" , "False" ,
                   "sp1b" , "Self-Paced 1-Back" , "auto" , "True" , 
                   "sp2b" , "Self-Paced 2-Back" , "auto" , "True" , 
                   "sp3b" , "Self-Paced 3-Back" , "auto" , "True" , 
                   "srt" , "Simple Reaction Time" , "auto" , "True" ,
                   "cad" , "Cued Attention (dot)" , "auto" , "True" , 
                   "cod" , "Coding" , "auto" , "True" , 
                   "crt" , "Choice Reaction Time" , "auto" , "True" ,  
                   "vm" , "Visual Matching" , "auto" , "True" , 
                   "vpa" , "Visuospatial Paired Associates" , "auto" , "True" , 
                   "Rbm" , "Reaction HumanBench" , "auto" , "False" , 
                   "gng" , "gonogo" , "auto" , "True",  
                   "af" , "AttentionFocus" , "auto" , "True",
                   "cw" , "ColorWord" , "auto" , "True" ,
                   "bss" , "BackwardSpatialSpan" , "auto" , "True" , 
                   "fss" , "Forward Spatial Span" , "auto" , "True" ,
                   "dr" , "DesignRecognition" , "auto" , "True" , 
                   "ft" , "FingerTapping" , "auto" , "True" ,
                   "sort" , "sort" , "auto" , "True" ,
                   "mr" , "MentalRotation" , "auto" , "True" ,  
                   "vbds" , "VizualBackwardDigitSpan" , "auto" , "True" , 
                   "vfds" , "VisualForwardDigitSpan" , "auto" , "True" ,
                   "vlwrrw" , "VisualLearningRealWord" , "auto" , "True" , 
                   "vl2rw" , "VisualLearning2RealWord" , "auto" , "True" , 
                   "vlwrnw" , "VisualLearningNonsenseWord" , "auto" , "True" ,
                   "vl2nw" , "VisualLearning2NonsenseWord" , "auto" , "True" ) ###last formaly named

unnamedmeansautomated<-c(    "abds" , "abds" , "auto" , "True" ,  
                             "bpm" , "bpm" , "auto" , "True" , 
                             "dt" , "dt" , "auto" , "True" ,  
                             "oct" , "oct" , "auto" , "True" , 
                             "odd" , "odd" , "auto" , "True" , 
                             "poly" , "poly" , "auto" , "True" , 
                             "sp" , "sp" , "auto" , "True" )

#what it takes me to parse above input string
df.test<-data.table(abreviation=I("parseingstring"), name=I("testnames"), parsing=I("auto") ,greater.is.better=I("True"))
df.tests<-df.test
#itr<-4
for(itr in 1:length(abrv.fullname)){
  df.test[1 , as.numeric(((itr - 1 ) %% 4) + 1)] <- abrv.fullname[itr]
  if(((itr - 1) %% 4) + 1 == 4){
    df.tests<-rbindlist(list(df.tests,df.test))
  }
}
df.tests<-df.tests[-1,]


dt.time.loc.event$time<-as.numeric(dt.time.loc.event$time)

for(itr in length(dt.time.loc.event$event):1){
  dt.time.loc.event$istest[itr]<-NA 
  if(str_detect(dt.time.loc.event$event[itr],"(?<=^[-;1234567890]{1,20})[\\p{Alphabetic}]{1,}.{1,}$")) {
    dt.time.loc.event$istest[itr]<-( str_extract_all( dt.time.loc.event$event[itr],"(?<=^[-;1234567890]{1,20})[\\p{Alphabetic}]{1,}.{1,}$")[[1]])  
  }
  if(str_detect(dt.time.loc.event$event[itr],"(?<=^[-;1234567890]{1,2})[p]{1,}$")) {
    dt.time.loc.event$istest[itr]<-( str_extract_all( dt.time.loc.event$event[itr],"(?<=^[-;1234567890]{1,2})[p]{1,}$")[[1]])  
  }
}  

#automaticaly register tests
new.tests<-setdiff(sort(unique(dt.time.loc.event$istest)), df.tests$abreviation)
for(itr in 1:length(new.tests)){
  df.test<-data.table(abreviation=I(new.tests[itr]), name=I(new.tests[itr]), parsing=I("auto") ,greater.is.better=I("True"))
  df.tests<-rbindlist(list(df.tests,df.test))
}
df.tests$greater.is.better<-as.logical(df.tests$greater.is.better)

#create all test columns
first.auto.column <- length(names(dt.time.loc.event)) + 1 #length(df.tests$name)
for(itr in 1:length(df.tests$name)){
  dt.time.loc.event[, df.tests$name[itr]:= vector(mode = "logical", length = length(dt.time.loc.event$time))]
}

#automatic test column filling
for(itrauto in first.auto.column:length(dt.time.loc.event[1,])){
  itrauto.col.df <- itrauto - first.auto.column +1
  if(df.tests$parsing[itrauto.col.df]=="auto"){
    for(itr in length(dt.time.loc.event$event):1){
      dt.time.loc.event[[itrauto]][itr]<-NA
      target.string<-paste0("^[1234567890]{1,}(?=",df.tests$abreviation[itrauto.col.df],"$)")
      if(str_detect(dt.time.loc.event$event[itr],target.string)) {
        dt.time.loc.event[[itrauto]][itr]<-as.numeric((str_extract_all(dt.time.loc.event$event[itr],target.string)[[1]]))
      }  
    }  
    if(df.tests$greater.is.better[itrauto.col.df]==FALSE){
      dt.time.loc.event[[itrauto]] <- dt.time.loc.event[[itrauto]] * -1
    }
  } 
}



unique(dt.time.loc.event[[itrauto]])
names(dt.time.loc.event)[itrauto]


for(itr in length(dt.time.loc.event$event):1){
  #
  dt.time.loc.event$SETCARDS[itr]<-NA
  if(str_detect(dt.time.loc.event$event[itr],"^[;1234567890]{1,5}SET$")) {
    dt.time.loc.event$SETCARDS[itr] <-  -60  * as.numeric( str_extract_all(dt.time.loc.event$event[itr],"(?<=(;|^))[1234567890]{1,2}(?=SET)")[[1]])
    minutes.hold<-as.numeric( str_extract_all(dt.time.loc.event$event[itr],"^[1234567890]{1,2}(?=;[1234567890]{1,2}SET)")[[1]]) 
    if(length(minutes.hold)>0){
      dt.time.loc.event$SETCARDS[itr] <- dt.time.loc.event$SETCARDS[itr] - minutes.hold
    }
  }
  
  dt.time.loc.event$TimetoMultiplyDice5x3[itr]<-NA
  if(str_detect(dt.time.loc.event$event[itr],"^[;1234567890]{1,5}m5x3dm$")) {
    dt.time.loc.event$TimetoMultiplyDice5x3[itr] <-  -60  * as.numeric( str_extract_all(dt.time.loc.event$event[itr],"(?<=(;|^))[1234567890]{1,2}(?=m5x3dm)")[[1]])
    minutes.hold<-as.numeric( str_extract_all(dt.time.loc.event$event[itr],"^[1234567890]{1,2}(?=;[1234567890]{1,2}m5x3dm)")[[1]]) 
    if(length(minutes.hold)>0){
      dt.time.loc.event$TimetoMultiplyDice5x3[itr] <- dt.time.loc.event$TimetoMultiplyDice5x3[itr] - minutes.hold
    }
  }
  
  dt.time.loc.event$shortcardsmed[itr]<-NA
  if(str_detect(dt.time.loc.event$event[itr],"[1234567890]{1,}[SsCc]{2}")) {
    dt.time.loc.event$shortcardsmed[itr]<-as.numeric(median(as.numeric(str_extract_all(dt.time.loc.event$event[itr],"[1234567890]{1,}(?=(-[1234567890]{1,}){0,4}[SsCc]{2})")[[1]])))
  }
  dt.time.loc.event$shortcardsmax[itr]<-NA
  if(str_detect(dt.time.loc.event$event[itr],"[1234567890]{1,}[SsCc]{2}")) {
    dt.time.loc.event$shortcardsmax[itr]<-as.numeric(max(as.numeric(str_extract_all(dt.time.loc.event$event[itr],"[1234567890]{1,}(?=(-[1234567890]{1,}){0,4}[SsCc]{2})")[[1]])))
  }
  dt.time.loc.event$TimetoMemorizeCards[itr]<-NA
  if(str_detect(dt.time.loc.event$event[itr],"^[1234567890;]{1,}m(20|19)[LlCc]{2}$")) {
    dt.time.loc.event$TimetoMemorizeCards[itr] <- -60 *  
      as.numeric( str_extract_all(dt.time.loc.event$event[itr],"(?<=(;|^))[1234567890]{1,2}(?=m(20|19)[LlCc]{2})")[[1]])
    minutes.hold<-as.numeric( str_extract_all(dt.time.loc.event$event[itr],"^[1234567890]{1,2}(?=;[1234567890]{1,2}m(20|19)[LlCc]{2})")[[1]])
    if(length(minutes.hold)>0){
      dt.time.loc.event$TimetoMemorizeCards[itr] <- dt.time.loc.event$TimetoMemorizeCards[itr] - minutes.hold
    }
  } 
}

unique(dt.time.loc.event$istest)
#remove columns with too few datapoints 5 atm
for(itr in length(dt.time.loc.event[1,]):1){
  colnonNA<-sum(!is.na(dt.time.loc.event[[itr]]))
  print(colnonNA)
  if(colnonNA<10){
    dt.time.loc.event[[itr]] <- NULL
  }
} 

dt.time.loc.event$checksum<-apply(dt.time.loc.event,1,function(x) sum(!is.na(x)))
cbound.test<-dt.time.loc.event

dt.time.loc.event$event[str_detect(dt.time.loc.event$event,"[1234567890]-[1234567890]")]


cbound.test$checksum<-apply(cbound.test,1,function(x) sum(!is.na(x)))
print(dt.time.loc.event[dt.time.loc.event$checksum==10]$event)
if(length(dt.time.loc.event[dt.time.loc.event$checksum==11]$event)>0) stop("3 test columns recieved marks from one event")
if(length(dt.time.loc.event[dt.time.loc.event$checksum==6]$event)>0) stop("fewer than 5 columns filled in test dataset")
print(dt.time.loc.event[dt.time.loc.event$checksum==8]$event)
inspect.bad.checksum<-dt.time.loc.event[dt.time.loc.event$checksum==7]
dt.time.loc.event[,m20lc:=NULL]
dt.time.loc.event[,m19lc:=NULL]
####plot everything against time####

require(dplyr)

source("EveryTrak_functions.R")
source("eda_by_lukereding.R")

main.directory<-getwd()
setwd(paste0(getwd(),"/output"))

if(F){
  plotit<-cbound.test[!is.na(shortcardsmed),]   
  #ExpectVS_ActDensityRainbow(x=as.numeric(cbound.test[!is.na(shortcards),]$time),y=cbound.test[!is.na(shortcards),]$shortcards, eff.thrs.stp=.001,autzoom.plot="no")
  ggplot(plotit, aes(y=shortcardsmed, x=time)) + 
    geom_point(stat="identity", position="jitter", alpha=0.5, size=3) + 
    geom_smooth(stat="smooth", position="identity", method="auto", formula=y ~ x, se=TRUE, n=80, level=0.95, span=0.75) + 
    theme_grey() + theme(text=element_text(family="sans", face="plain", color="#000000", size=15, hjust=0.5, vjust=0.5)) + 
    scale_size(range=c(1, 3)) + xlab("time") + ylab("shortcardsmed")
}

if(T){# I use this to look for anomalies
  
  #input.folder<-"badbooks"
  #output.name<-paste(input.folder,"4",sep = "") ###only after selection
  
  cols_tosee<-union( c("time" ), intersect(df.tests$name,names(dt.time.loc.event)) )
  usrs_stats <-  cbound.test
  
  #source("EDA_dif_functions.R")
  
  colnamecombos1<-vector();colnamecombos2<-vector()
  len.cols_tosee<-length(cols_tosee) #to prevent too many pics under each chunk
  qrtcuts<-c(1,round(len.cols_tosee/3),round((2*len.cols_tosee)/3),len.cols_tosee)
  
  gotwd<-getwd();gotwd<-"C:/Users/Dm/Desktop/EverythingTrackerVizual/output"
  do.call(file.remove, list(list.files(gotwd, full.names = TRUE)))
  for(col_name1 in c("time")){
    for(col_name2 in cols_tosee){
      if(sum(colnamecombos1[colnamecombos2==col_name1]==col_name2)>0) next()
      if(col_name1==col_name2) next()
      colnamecombos1 <- c(colnamecombos1,col_name1)
      colnamecombos2 <- c(colnamecombos2,col_name2)
      summary(get(col_name1,usrs_stats))
      summary(get(col_name2,usrs_stats))
      if(F){try({
        gc()
        ExpectVS_ActDensityRainbow(get(col_name1,usrs_stats),
                                   get(col_name2,usrs_stats),col_name1,col_name2)
      })}
      
      file.remove(paste0(col_name2,"_over_time_loc.png")) #col_name2<-"vbds"  #col_name2<-"vfds"
      itemNlargeConst<-1.6
      png(filename = paste0(col_name2,"_over_time_loc.png"), width = 1366/itemNlargeConst,
          height = 768/itemNlargeConst)
      print(col_name2)
      PlotScatrNOneDense()
      dev.off()
    }
  }
}


#(unique(dt.time.loc.event$event))

####quick correlations####
col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
cols_tosee <- union( c("time" ), intersect(df.tests$name,names(dt.time.loc.event)) )

dt.tests.day.mean<-dt.time.loc.event
dt.tests.day.mean$time <- round(dt.tests.day.mean$time)
#dt.time.loc.event[,timerounded  := round(time), by=time]
if(T){ #remove unessesary columns?  not in the main dataset!!
  cols.remove <- c(1:length(dt.time.loc.event[1,]))[!(names(dt.time.loc.event) %in% cols_tosee)]
  cols.remove <- sort(cols.remove,decreasing = T)
  for(itr in cols.remove){
    print(names(dt.tests.day.mean[[itr]]))
    dt.tests.day.mean[[itr]]<-NULL
  }
}
before.daymeans<-length(names(dt.tests.day.mean))
for (itr in 2:length(cols_tosee)) {
  dt.tests.day.mean[,meanNDCG  := mean(get(cols_tosee[itr]),na.rm = T), by=time]#createmeans
  names(dt.tests.day.mean)<-c(names(dt.tests.day.mean)[1:(length(dt.tests.day.mean)-1)],paste0(cols_tosee[itr],".day.mean"))
}

#remove multiple entries of single day
dt.tests.day.mean<-dt.tests.day.mean[,.SD[1], by=time]
#remove old columns
dt.tests.day.mean[,names(dt.tests.day.mean)[2:before.daymeans] := NULL]


if(T){ # no need to run twice
  require(corrplot)
  #
  M <- cor(dt.tests.day.mean,method = "spearman",use = "pairwise.complete.obs" )
  
  M[is.na(M)]<-0 #does running correlation  analysis over an existing correlation matrix to propagate trust work or are there better methods in R?  
  png(filename = "AACorTests.png", width = 1480, height = 1480)
  corrplot(M, method = "ellipse",order = "hclust", addrect = 5, col = col1(100))
  dev.off()
  M2 <- cor(M,method = "pearson",use = "pairwise.complete.obs" )
  png(filename = "AACorTestsTrustProp.png", width = 1480, height = 1480)
  corrplot(M2, method = "ellipse",order = "hclust", addrect = 5, col = col1(100))
  dev.off()
  
  require(DataExplorer)#is ths actualy it?
  create_report(dt.tests.day.mean, output_file = "report.html", output_dir = getwd(),
                y = NULL, config = list())
  create_report(as.data.frame(M), output_file = "report2.html", output_dir = getwd(),
                y = NULL, config = list())
  
  
  require(tidyr)
  png(filename = "edaeda_oncorrmat.png", width = 1480, height = 1480)
  eda(M,plot = T)
  dev.off()
  edaedaCorrMat<-eda(M,plot = F)
  png(filename = "edaeda.png", width = 1480, height = 1480)
  eda(dt.tests.day.mean,plot = T)
  dev.off()
  edaeda<-eda(dt.tests.day.mean,plot = F)
}
# PCA Factor Analysis on missing values
# islands instead of days
# two more eda
# adjust for effect of learning curve
# adjust for lack of common points ie 1/10 probability bars
# propagate trust
# pca and efa on missing data 
# correlation between measures but how to set windows? do I use mean or median inside a window? 


# SPARSE I AM LOOKING FOR THE WORD SPARSE !! SPARSE ANALYSIS?
