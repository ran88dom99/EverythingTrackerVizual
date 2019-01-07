library(tidyquant)
PlotScatrNOneDense <- function(){
  usrs_stats_nonas<-usrs_stats[!is.na(get(col_name1,usrs_stats))]
  usrs_stats_nonas<-usrs_stats_nonas[!is.na(get(col_name2,usrs_stats_nonas))]
  print( ggplot(usrs_stats_nonas,aes(x=get(col_name1),y=get(col_name2))) +
           geom_smooth(color="blue", alpha=0.2, size=0.6) +
           geom_point(aes(colour=location),stat="identity", alpha=0.9, size=2.5)+
           geom_line(color="blue", alpha=0.3)+# position="jitter",
           xlab(col_name1) + ylab(col_name2) + 
           geom_line(aes(y=rollmean(get(col_name2,usrs_stats_nonas), 5, na.pad=TRUE)))
         )  
                     

  #length(rollmean(get(col_name2,usrs_stats_nonas), 5, na.pad=TRUE))
  #length(get(col_name2,usrs_stats_nonas))
  #line graphs between nas
  
  #col_name2<-"pagesread""shortcardsmax"
  #geom_line(aes(color="median",y=rollmedian(get(col_name2,usrs_stats_nonas), 5, na.pad=TRUE)))) 
  #geom_line(aes(y=rollmean(get("SETCARDS",usrs_stats), 5, na.pad=TRUE)))
  #+ geom_vline(xintercept=as.numeric(mydata$datefield[120]), linetype=4)
  #geom_ma(mapping = NULL, data = NULL, position = "identity",
  #                 na.rm = TRUE, show.legend = NA, inherit.aes = TRUE, ma_fun = SMA,
  #                 n = 5, wilder = FALSE, ratio = NULL, v = 1, wts = 1:n )  +
}
if(F){
print( ggplot(usrs_stats,aes(x=get(col_name1),y=get(col_name2))) +
         geom_smooth(color="blue", alpha=0.2, size=0.6) +
         geom_point(aes(colour=location),stat="identity", alpha=0.9, size=2.5)+
         geom_line(color="blue", alpha=0.3)+# position="jitter",
         xlab(col_name1) + ylab(col_name2) + 
         geom_line(aes(y=))
)  
}
PlotScatrNDenseVarsPass <- function(data,x,y){
  print( ggplot(data=data,aes(x=x,y=y)) +
           geom_point(stat="identity", position="jitter", alpha=.9, size=.7)+
           geom_density2d(stat="density2d", position="identity",color="green") +
           geom_smooth(color="blue") )
}
#```xlab(col_name1) + ylab(col_name2) +

