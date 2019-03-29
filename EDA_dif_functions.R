#The function that draws 2d unexpectedness rainbows.
## ##TODO;

#categorical variables
# names
# zooms
#  removal is straight forward but hclust resort? thats much more difficult

#not doing
#distance is not included in calculation of continuous variable displacement statistic.
#probability is not included in any way; Some kind of local probability test?
#joint actual vs expect should be easier than serious K-S
#darkness hue as well as rainbow? ggplot again
# bin then chiSqr for probability of entire joint distribution to just be accidental, as adition to spearman and displacement

#x,y z animate a,b facet
#animate and facet probably needs ggplot
#all functions would have to calculate on unknown number of variables
#intake would have to be a df, get the column names too

#```{r}
#binned 2d frequency.
source("runfirst.R")
require(MASS)
require(ggplot2)

if(F){df<-exp.df}
DIY2D<-function(df,nbins=200){
  # OPTION 5: The Hard Way (DIY) # rbloggers 5 ways density
  # http://stackoverflow.com/questions/18089752/r-generate-2d-histogram-from-raw-data
  x.sv.nbin <- nbins
  y.sv.nbin <- nbins
  if(x.is.categorical) x.sv.nbin <- min(nbins,length(unique(x)))
  if(y.is.categorical) y.sv.nbin <- min(nbins,length(unique(y)))

  x.bin <- seq(floor(min(df[,1])), ceiling(max(df[,1])), length=x.sv.nbin)
  y.bin <- seq(floor(min(df[,2])), ceiling(max(df[,2])), length=y.sv.nbin)

  freq <-  as.data.frame(table(findInterval(df[,1], x.bin),findInterval(df[,2], y.bin)))
  freq[,1] <- as.numeric(freq[,1])
  freq[,2] <- as.numeric(freq[,2])

  freq2D <- matrix(0, nrow=x.sv.nbin, ncol=y.sv.nbin)
  freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]
  return(freq2D)
}
#percent of points moved between distributions
displacement2D<-function(one.df,two.df,numbins=200){
  #percent of points moved between distributions
  one.fq<-DIY2D(one.df,nbins = numbins)
  two.fq<-DIY2D(two.df,nbins = numbins)
  dif.fq<-(one.fq-two.fq)
  dif.fq[dif.fq<0]<-0
  #summary(as.vector(dif.fq))
  sum(dif.fq)/sum(one.fq)
}
#sample/permute 2 variables multiple times to get displacemnt from joint distribution causeble by randomness
expectedSampleDisplacement<-function(x,y,numitrs=60,numbins=200){
  #sample/permute 2 variables multiple times to determine displacemnt from joint distribution causeble by randomness
   sum.disp <- 0
  for(itr in 1:numitrs){
    exp.df <- data.frame(sample(x), sample(y))
    exp.df2 <- data.frame(sample(x), sample(y))
    one.fq<-DIY2D(exp.df,nbins = numbins)
    two.fq<-DIY2D(exp.df2,nbins = numbins)
    #sum.fq<-sum.fq+(one.fq[,]+two.fq[,])
    dif.fq<-(one.fq-two.fq)
    dif.fq[dif.fq<0]<-0
    sum.disp<-sum.disp+(sum(dif.fq)/sum(one.fq))
  }
  #expi.fq<<-round(sum.fq/(numitrs*2))
  return(sum.disp/numitrs)
}
#sample/permute 2d data multiple times to determine displacemnt by actual vs expected joint distributions
actVSexpDisplacement<-function(x,y,numitrs=60,numbins=200){
  #sample/permute 2d data multiple times to determine displacemnt by actual vs expected joint distributions
  #sum.fq <- diag(numbins)*0
  sum.disp <- 0
  for(itr in 1:numitrs){
    act.df <- data.frame(x,y)
    exp.df <- data.frame(sample(x), sample(y))
    names(exp.df)<-c("x","y")

    one.fq<-DIY2D(act.df,nbins = numbins)
    two.fq<-DIY2D(exp.df,nbins = numbins)
    #sum.fq<-sum.fq+(one.fq[,]+two.fq[,])
    dif.fq<-(one.fq-two.fq)
    dif.fq[dif.fq<0]<-0
    sum.disp<-sum.disp+(sum(dif.fq)/sum(one.fq))
  }
  #image(one.fq,col=topo.colors(12),  xlab=xlab2, ylab=ylab)col = rainbow(100),
  #expi.fq<<-round(sum.fq/(numitrs*2))
  return(sum.disp/numitrs)
}

#for probability and categorical/interval and mass effect and speed
#must 2d bin and compare prob individual then max improb from one? or bonferoni correction?
#then sum positive against Expected for effect size
if(F){
x <- get(col_name2,usrs_stats)
y <- get("simplskew_user_rating",usrs_stats)
act.df <- data.frame(x,y)
exp.df <- data.frame(sample(x), sample(y))
names(exp.df)<-c("x","y")
one.fq<-DIY2D(act.df,nbins = numbins)
two.fq<-DIY2D(exp.df,nbins = numbins)
dif.fq<-(one.fq-two.fq)
summary(as.vector(one.fq));summary(as.vector(two.fq));summary(as.vector(dif.fq));
sum(as.vector(one.fq));sum(as.vector(two.fq));sum(as.vector(dif.fq));

xlab="x"
ylab="y"
eff.thrs.stp=.01
perc.windw=.2
rm.xs.fct=T
perc.lcl=F
autzoom.plot="no"
perc.trim=.1
dif.fq<-PP2$z
graphplot=T
}
ZoomBandWid<-function(dif.fq,perc.trim=.05,graphplot=F){
  #based DIY2D

  norm.disp.fq <- dif.fq #- freq.most.common
  norm.disp.fq <- abs(norm.disp.fq[,])
  for(dig.it in 1:50) {
    if(round(quantile(norm.disp.fq, probs = .95),digits = dig.it)>0) break()
  }
  #dig.it=dig.it+1#quantile(dif.fq, probs = .95)
  #quantile(dif.fq, probs = .99)
  #quantile(dif.fq, probs = .95)
  #quantile(dif.fq, probs = .90)
  #quantile(dif.fq, probs = .80)
  #(mean(norm.disp.fq,na.rm = T));.03*200^2
  #atables<-summary(as.factor(round(dif.fq,digits=dig.it)))
  #mxind<-which.max(atables[1:(length(atables)-1)])
  #freq.most.common<-as.numeric(names(mxind))
  #plot(density(as.vector(norm.disp.fq)))

  norm.disp.fq <- round(norm.disp.fq,digits=dig.it)
  #summary(as.vector(norm.disp.fq))

  x1<-apply(norm.disp.fq, 1,sum)
  #summary(as.vector(x1));summary(as.vector(y1))
  for(dig.it in 1:50) {
    if(round(mean(x1, na.rm = T),digits = dig.it)>0) break()
  } #quantile(x1, probs = .95)
  x1 <- round(x1,digits=dig.it)


  x2<-vector(mode = "numeric", length = 0)

  for (itr in 1:length(x1)) {
    x2<-append(x2, seq.int(from=itr, to=itr, length.out =  x1[itr]))
  }
#summary(as.vector(x2));summary(as.vector(y2))

  require("hdrcde")
  if(graphplot==F){
  hdr.x<-hdr(x2, prob = c(((1-perc.trim)*100),99,90,50,75))}
  if(graphplot==T) {
  hdr.x<-hdr.den(x2, prob = c(((1-perc.trim)*100),99,90,50,75))}
  #hdr.x.rng<<-c(min(hdr.x$hdr[1,])/length(x1),max(hdr.x$hdr[1,])/length(x1))
  #hdr.y.rng<<-c(min(hdr.y$hdr[1,])/length(y1),max(hdr.y$hdr[1,])/length(y1))
  hdr.x.rng<-c(floor(min(hdr.x$hdr[1,],na.rm = T)),ceiling(max(hdr.x$hdr[1,],na.rm = T)))

  pp2x.rng<-c(1,length(dif.fq[1,])); pp2y.rng<-c(1,length(dif.fq[,1]))
  hdr.x.rng<-c(max(hdr.x.rng[1],pp2x.rng[1]),min(hdr.x.rng[2],pp2x.rng[2]))
  return(hdr.x.rng)
  }
if(F){
  x <- get(col_name2,usrs_stats)
  y <- get("simplskew_user_rating",usrs_stats)
  act.df <- data.frame(x,y)
  exp.df <- data.frame(sample(x), sample(y))
  names(exp.df)<-c("x","y")
  one.fq<-DIY2D(act.df,nbins = numbins)
  two.fq<-DIY2D(exp.df,nbins = numbins)
  dif.fq<-(one.fq-two.fq)
  summary(as.vector(one.fq));summary(as.vector(two.fq));summary(as.vector(dif.fq));
  sum(as.vector(one.fq));sum(as.vector(two.fq));sum(as.vector(dif.fq));

  xlab="x"
  ylab="y"
  eff.thrs.stp=.01
  perc.windw=.2
  rm.xs.fct=T
  perc.lcl=F
  autzoom.plot="no"
  perc.trim=.1
  dif.fq<-PP2$z
  graphplot=T
  count.vars.graph=20
} #split and otherwise rework previous functions to work with single variables at a time
#bounds to remove least changed columns. bounds only made for x. use t() Categorical Plan
ZoomCategSort<-function(dif.fq,count.vars.graph=20,graphplot=F ){
  #based DIY2D
#binning difference density like  before but predefined # of bins this time
#don't forget the names

  #output must use previous ggplot code for lables
  norm.disp.fq <- dif.fq #- freq.most.common
  norm.disp.fq <- abs(norm.disp.fq[,])
  for(dig.it in 1:50) {
    if(round(quantile(norm.disp.fq, probs = .95),digits = dig.it)>0) break()
  }
  norm.disp.fq <- round(norm.disp.fq,digits=dig.it)
  #summary(as.vector(norm.disp.fq))

  x1<-apply(norm.disp.fq, 1,sum)
  for(dig.it in 1:50) {
    if(round(mean(x1, na.rm = T),digits = dig.it)>0) break()
  }
  x1 <- round(x1,digits=dig.it)
  x2<-vector(mode = "numeric", length = 0)
  for (itr in 1:length(x1)) {
    x2<-append(x2, seq.int(from=itr, to=itr, length.out =  x1[itr]))
  }

  #here difference from continous starts
  #sortbyimpact
  rank.x <- rank(-x1)
  tf.rankpass <- rank.x <= count.vars.graph

  #heirarchical clustering maybe later?
  dist.norm.disp.fq <- dist(norm.disp.fq) # method="man" # is a bit better
  hclust.norm.disp.fq <- hclust(dist.norm.disp.fq, method = "ward.D2")
  if(graphplot) plot(hclust.norm.disp.fq)
  x5<-hclust.norm.disp.fq$order[hclust.norm.disp.fq$order %in% which(tf.rankpass)]
 return(x5)
}

#```
#
#```{r}
if(F){cols_tosee<-c("mean_user_rating","sd_user_rating","simplskew_user_rating","count_user_rating","user_median_popular_rank","user_90th_quant_popular_rank","user_more_popular_item","touches_by_user","trimmed_prob_unimodal","skew_user_rating","unqUsersTouchd","MenAD_user_rating","usr_rat_outliers" )
  x <- iris$Sepal.Width
  y <- (iris$Species)
  x <- get("skew_user_rating",usrs_stats)#user_more_popular_itemget("MenAD_user_ratinguser_more_popular_item",usrs_stats) count_user_rating
  y <- get("simplskew_user_rating",usrs_stats)#mean_user_ratingget(,usrs_stats)"skew_user_rating" user_90th_quant_popular_rank
  x <- diamonds$cut
  y <- diamonds$color
  source("runfirst.R"); require(MASS); require(dplyr); require(ggplot2)
  xlab="x"; ylab="y"; eff.thrs.stp=.001; perc.windw=.1;rm.xs.fct=T;perc.lcl=F;autzoom.plot="yes";detail=200
  ExpectVS_ActDensityRainbow(x,y, eff.thrs.stp=.001)

}



ExpectVS_ActDensityRainbow <- function(x,y,xlab="x", ylab="y", eff.thrs.stp=.01, perc.windw=.1,rm.xs.fct=T,perc.lcl=F,autzoom.plot="yes",detail=200){
  print(paste(xlab,ylab)); require(reshape2)
  if(!(autzoom.plot %in% c("both","no","yes"))) {stop("autzoom.plot not acceptable")}
  if(!is.vector(x) && !is.factor(x)) {stop("x not vector or factor")}
  if(is.vector(x)) {x.is.categorical<<-F}
  if(is.factor(x)) {
    x.is.ordinal<<-is.ordered(x)
    x.is.categorical<<-T
    x.levels<<-levels(x)
    x<<-as.numeric(x)
  }
  if(!is.vector(y) && !is.factor(y)) stop("y not vector or factor")
  if(is.vector(y)) y.is.categorical<<-F
  if(is.factor(y)){
    y.is.ordinal<<-is.ordered(y)
     y.is.categorical<<-T
     y.levels<<-levels(y)
     y<<-as.numeric(y)
  }
  if(length(unique(x))<2 || length(unique(y))<2) stop("no variance at all in at least one vector")
  if(length(x)!=length(y)) stop("vectors unequal length")
  if(!is.character(xlab) || !is.character(ylab)) stop("lables not character")

  #x<-as_tibble(x)
  #y<-as_tibble(y)
  #x<-as.vector(x)
  #y<-as.vector(y, mode = "character")
  #y<-as.vector(y, mode = "numeric")


  lx<-length(x)
  nonadf<-na.omit(data.frame(x,y))
  x<-nonadf$x;y<-nonadf$y
  #really a solution?
  if( bandwidth.nrd(x)==0){
    warning("too many of single type in x")
    atables<-summary(as.factor(x))
    remove.these<-as.numeric(names(atables)[1])
    x[x==remove.these]<-NA
    try(rm("atables"));
  }
  if( bandwidth.nrd(y)==0){
    warning("too many of single type in y")
    atables<-summary(as.factor(y))
    remove.these<-as.numeric(names(atables)[1])
    y[y==remove.these]<-NA
    try(rm("atables"));
  }
  nonadf<-na.omit(data.frame(x,y))
  x<-nonadf$x;y<-nonadf$y


  percent.left <- round((length(x)/lx),digits = 4)
  ranSmpDisp <- expectedSampleDisplacement(x,y,numitrs=60,numbins=detail)
  actExpDisp <- actVSexpDisplacement(x,y,numitrs=60,numbins=detail)
  adjActDisp <- round(actExpDisp-ranSmpDisp,digits = 4)
  spearmanRho <- round(cor(x,y,method = "spearman"),digits=4)
  print(paste0(percent.left," percent left showing displacement ",adjActDisp," and spearman ",spearmanRho))
  if(eff.thrs.stp>(adjActDisp) && (eff.thrs.stp)>(spearmanRho^2)) return(NULL)
  xlab2<-paste0(xlab," of which ",percent.left,"% d",adjActDisp," spear",spearmanRho)

  try(rm("nonadf"));gc()

    actual <- kde2d(x, y, n = detail)
    if(length(x)<1700){
      warning("less than 1700 dp")
      expectJoint <- kde2d(sample(x,size=2000,replace = T), sample(y,size=2000,replace = T), n = detail)
    } else {
      expectJoint <- kde2d(sample(x), sample(y), n = detail)
    }
    if(perc.lcl) {
      PP2<-expectJoint;PP2$z<-((actual$z[,]-expectJoint$z[,])/(actual$z[,]+expectJoint$z[,]));
    }
    if(!perc.lcl)  {
      PP2<-expectJoint;PP2$z<-((actual$z[,]-expectJoint$z[,]));
    }
    #summary(as.vector(PP2))

    if(autzoom.plot=="both"||autzoom.plot=="no"){
      image(PP2, col = rainbow(100), xlab=xlab2, ylab=ylab)
      }
    if(autzoom.plot=="both"||autzoom.plot=="yes"){
      nonadf<-data.frame(x,y)
      if(!x.is.categorical && !x.is.ordinal){
      hdr.x.rng<-ZoomBandWid(PP2$z,perc.trim=perc.windw)
      nonadf<-nonadf[nonadf$x > PP2$x[hdr.x.rng[1]],]
      nonadf<-nonadf[nonadf$x < PP2$x[hdr.x.rng[2]],]
      }
      if(!y.is.categorical && !y.is.ordinal){
      hdr.y.rng<-ZoomBandWid(t(PP2$z),perc.trim=perc.windw)#zomming in!
      nonadf<-nonadf[nonadf$y > PP2$y[hdr.y.rng[1]],]
      nonadf<-nonadf[nonadf$y < PP2$y[hdr.y.rng[2]],]
      }
      x<-nonadf$x;y<-nonadf$y

    if(!x.is.categorical || !y.is.categorical){
      bandw.fr.dens<-c(bandwidth.nrd(x)/(1+.6*x.is.categorical),bandwidth.nrd(y)/(1+.6*y.is.categorical))

    actual <- kde2d(x, y, h=bandw.fr.dens, n = detail)
    if(length(x)<1700){
      warning("less than 1700 dp")
      expectJoint <- kde2d(sample(x,size=2000,replace = T), sample(y,size=2000,replace = T), h=bandw.fr.dens, n = detail)
    } else {
      expectJoint <- kde2d(sample(x), sample(y), h=bandw.fr.dens, n = detail)
    }
    if(perc.lcl) {
      PP2<-expectJoint;PP2$z<-((actual$z[,]-expectJoint$z[,])/(actual$z[,]+expectJoint$z[,]));
    }
    if(!perc.lcl)  {
      PP2<-expectJoint;PP2$z<-((actual$z[,]-expectJoint$z[,]));
    }
    #summary(as.vector(PP2))
    image(PP2, col = rainbow(100), xlab=xlab2, ylab=ylab)


    melted.PP2Z<-melt(PP2$z)


    # Show the area only
    ggplot(melted.PP2Z, aes(x=Var1, y=Var2) ) +
      stat_density_2d(aes(color = value), geom = "polygon")

    ggplot(melted.PP2Z, aes(x = Var1, y = Var2, colour=value)) +
      geom_point(aes(fill = value))+
      theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2)) +
      scale_color_gradientn(colours = rainbow(10)) #scale_color_gradient2( low="blue", mid="white", high="red", space ="Lab"  )#scale_color_brewer(palette="Spectral") #scale_fill_gradient(guide = guide_colourbar(), low = "#ffffff", high = "#56B1F7",

#    ggplot(melted.PP2Z, aes(x = Var1, y = Var2, colour=value)) +
#      geom_raster(aes(fill = value))+
#     scale_color_gradient2( low="blue", mid="white", high="red", space ="Lab"  )#scale_color_brewer(palette="Spectral") #scale_fill_gradient(guide = guide_colourbar(), low = "#ffffff", high = "#56B1F7",                          #space = "Lab")#scale_fill(colours=rainbow(2)) scale_color_brewer(palette="Spectral") +scale_fill_brewer(palette="Spectral")
    }
    if(x.is.categorical && y.is.categorical){ #whoops not difference just density
      actual <- DIY2D(data.frame(x,y))
      if(length(x)<1700){
        warning("less than 1700 dp")
        expectJoint <- DIY2D(data.frame(sample(x,size=2000,replace = T), sample(y,size=2000,replace = T)))
      } else {
        expectJoint <- DIY2D(data.frame(sample(x), sample(y)))
      }
      if(perc.lcl) {
        PP2<-expectJoint;PP2<-((actual[,]-expectJoint[,])/(actual[,]+expectJoint[,]));
      }
      if(!perc.lcl)  {
        PP2<-expectJoint;PP2<-((actual[,]-expectJoint[,]));
      }

      melted.PP2Z<-melt(PP2)

      ggplot(melted.PP2Z, aes(x = Var1, y = Var2, colour=value)) +
        geom_raster(aes(fill = value))+
        theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2)) +
        scale_fill_gradient2( low="blue", mid="white", high="red", space ="Lab"  )
        #scale_fill_gradientn(colours = rainbow(7)) #scale_color_gradient2( low="blue", mid="white", high="red", space ="Lab"  )#scale_color_brewer(palette="Spectral") #scale_fill_gradient(guide = guide_colourbar(), low = "#ffffff", high = "#56B1F7",


      #ggplot() +
       # geom_bin2d(aes(x = x.levels[x],y = y.levels[y],fill = log10(..count..)),data=,drop = FALSE) +
       # theme(axis.text.x=element_text(size=7, angle=270,hjust=0.95,vjust=0.2)) +
       # scale_fill_gradientn(colours = rainbow(3))
#        scale_fill_gradient2( low="blue", mid="white", high="red", space ="Lab" )#guide = guide_colourbar(),low = '#ffffff',high = '#c90700')
    }
    }
    if(round(sum(as.vector(PP2$z)), digits = 5) !=0) warning("difference between expected and actual does not sum to 0")
}


PlotScatrNDense <- function(){
  usrs_stats_nonas<-usrs_stats[!is.na(get(col_name1,usrs_stats))]
  usrs_stats_nonas<-usrs_stats_nonas[!is.na(get(col_name2,usrs_stats_nonas))]
  print( ggplot(usrs_stats_nonas,aes(x=get(col_name1),y=get(col_name2))) +
           geom_point(stat="identity", position="jitter", alpha=0.3, size=1)+
           geom_density2d(stat="density2d", position="identity",color="green") +
           xlab(col_name1) + ylab(col_name2) + geom_smooth(color="blue") +
           stat_density_2d(mapping = aes(x = get(col_name1)[sample(1:length(get(col_name1)))],
                                         y = get(col_name2)),
                           color = "red", n = 100, h = NULL,
                           na.rm = FALSE, show.legend = F, inherit.aes = T) )
}
