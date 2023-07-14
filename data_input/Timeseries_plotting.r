# Time-series Functions.r
# --------------------------------
#
# Author: Michael Mueller
#
# Date:   12.12.2016
#
# Last modified: -
#
# --------------------------------



### ---------------------------------------------------------------------------------------------------

### Plot functions

# Time series

plot_ts <- function(figname,date,yyy,window_width,xrange_unixTS,yrange,xlabString,ylabString,legend_str){
  
  #
  
  if(!window_width%in%c("day","week","month","year","all","all_day2day")){
    stop("Error 1 in plot function.")
  }
  
  #
  
  if(!is.matrix(yyy)){
    yyy <- matrix(yyy,ncol=1)
  }
  
  n_yyy <- dim(yyy)[2]
  
  
  # Date limits
  
  if(!is.null(xrange_unixTS)){
    xrange_date_min <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[1]
    xrange_date_max <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[2]
    
    id   <- which(date>=xrange_date_min & date<=xrange_date_max)
    n_id <- length(id)
    
    if(n_id>0){
      date <- date[id]
      yyy  <- yyy[id,]
    }else{
      stop()
    }
  }
  
  # Window width x
  
  if(is.null(window_width)){
    stop() 
  }
  if(window_width=="all"){
    intervals <- seq(strptime("20080101000000","%Y%m%d%H%M%S",tz="UTC"),strptime("20170101000000","%Y%m%d%H%M%S",tz="UTC"),length.out = 2)
  }
  if(window_width=="all_day2day"){
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + 86400
    intervals  <- seq(date_first,date_last,length.out = 2)
  }
  if(window_width=="year"){
    year_first <- as.numeric(strftime(min(date),"%Y",tz="UTC"))
    year_last  <- as.numeric(strftime(max(date),"%Y",tz="UTC")) + 1
    date_first <- strptime(paste(year_first,"0101000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(paste(year_last, "0101000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    intervals  <- seq(date_first,date_last,by="years")
  }
  if(window_width=="month"){
    year_first  <- as.numeric(strftime(min(date),"%Y",tz="UTC"))
    month_first <- as.numeric(strftime(min(date),"%m",tz="UTC"))
    year_last   <- as.numeric(strftime(max(date),"%Y",tz="UTC"))
    month_last  <- as.numeric(strftime(max(date),"%m",tz="UTC"))
    
    if(month_last==12){
      month_last <- 1
      year_last  <- year_last + 1
    }
    
    date_first <- strptime(paste(year_first,sprintf("%02.0f",month_first),"01000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(paste(year_last, sprintf("%02.0f",month_last), "01000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    intervals  <- seq(date_first,date_last,by="months")
  }
  if(window_width=="week"){
    delta_days <- ((as.numeric(strftime(min(date),"%w",tz="UTC"))-1)+7)%%7
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") - delta_days*86400
    delta_days <- ((as.numeric(strftime(max(date),"%w",tz="UTC"))-1)+7)%%7
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + (7-delta_days)*86400
    intervals  <- seq(date_first,date_last,by="weeks")
  }
  if(window_width=="day"){
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + 86400
    intervals  <- seq(date_first,date_last,by="days")
  }
  
  n_intervals <- length(intervals)-1
  
  
  # Window width y
  
  if(is.null(yrange)){
    yrange <- c(min(yyy,na.rm=T),max(yyy,na.rm=T))
  }
  
  
  # Plot
  
  def_par <- par()
  pdf(file = figname, width=16, height=8, onefile=T, pointsize=12, colormodel="srgb")
  par(mai=c(1,1,0.1,0.1))
  
  for(ith_interval in 1:n_intervals){
    id   <- which(date>=intervals[ith_interval] & date<intervals[ith_interval+1])
    n_id <- length(id)
    
    if(n_id<1){
      next
    }
    
    if(window_width=="all"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="2 years")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="years")
    }
    if(window_width=="all_day2day"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],length.out=5)
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],length.out=5)
    }
    if(window_width=="year"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="months")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="months")
    }
    if(window_width=="month"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="7 days")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="days")
    }
    if(window_width=="week"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="2 days")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="days")
    }
    if(window_width=="day"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="4 hours")
      xticksLab <- c(strftime(xticks[1],"%d/%m/%Y",tz="UTC"),strftime(xticks[2:length(xticks)],"%H:%M",tz="UTC"))
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="hours")
    }
    
    n_x_grids <- length(x_grid)
    
    plot(c(intervals[ith_interval],intervals[ith_interval+1]),
         c(0,0),
         ylim=yrange,
         t="l",
         col="gray50",
         lwd=1,
         lty=1,
         xaxt="n",
         xlab=xlabString,
         ylab=ylabString,
         cex.axis=1.25,
         cex.lab=1.25)
    
    for(ith_x_grid in 1:n_x_grids){
      lines(c(x_grid[ith_x_grid],x_grid[ith_x_grid]),c(-1e9,1e9),col="gray80",lwd=1,lty=1)
    }
    
    axis(side=1,at=xticks,labels=xticksLab,cex.axis=1.25,cex.lab=1.25)
    
    for(ith_yyy in 1:1){
      lines(date[id],yyy[id,ith_yyy],lty=1, lwd=1, col=ith_yyy)
    }
    
    if(n_yyy>1){
      for(ith_yyy in 2:n_yyy){
        lines(date[id],yyy[id,ith_yyy],lty=1, lwd=1, col=ith_yyy)
      }
    }
    
    par(family="mono")
    legend("topright",legend=legend_str,col=1:n_yyy,lwd=2,lty=1,cex=1.25,bg="white")
    par(family="")
    
    
  }
  
  dev.off()
  par(def_par)
  
  
}


# Time series : 2 different y-axes

plot_ts_2YAxes <- function(figname,date,yyy1,yy2,window_width,xrange_unixTS,yrange1,yrange2,xlabString,ylabString1,ylabString2,legend_str){
  
  #
  
  if(!window_width%in%c("day","week","month","year","all","all_day2day")){
    stop("Error 1 in plot function.")
  }
  
  #
  
  if(!is.matrix(yyy1)){
    yyy1 <- matrix(yyy1,ncol=1)
  }
  
  if(!is.matrix(yyy2)){
    yyy2 <- matrix(yyy2,ncol=1)
  }
  
  if(dim(yyy1)[2]>4){
    stop()
  }
  if(dim(yyy2)[2]>4){
    stop()
  }
  
  n_yyy1 <- dim(yyy1)[2]
  n_yyy2 <- dim(yyy2)[2]
  
  
  # Date limits
  
  if(!is.null(xrange_unixTS)){
    xrange_date_min <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[1]
    xrange_date_max <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[2]
    
    id   <- which(date>=xrange_date_min & date<=xrange_date_max)
    n_id <- length(id)
    
    if(n_id>0){
      date <- date[id]
      yyy1 <- yyy1[id,]
      yyy2 <- yyy2[id,]
    }else{
      stop()
    }
  }
  
  # Window width x
  
  if(is.null(window_width)){
    stop() 
  }
  if(window_width=="all"){
    intervals <- seq(strptime("20080101000000","%Y%m%d%H%M%S",tz="UTC"),strptime("20170101000000","%Y%m%d%H%M%S",tz="UTC"),length.out = 2)
  }
  if(window_width=="all_day2day"){
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + 86400
    intervals  <- seq(date_first,date_last,length.out = 2)
  }
  if(window_width=="year"){
    year_first <- as.numeric(strftime(min(date),"%Y",tz="UTC"))
    year_last  <- as.numeric(strftime(max(date),"%Y",tz="UTC")) + 1
    date_first <- strptime(paste(year_first,"0101000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(paste(year_last, "0101000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    intervals  <- seq(date_first,date_last,by="years")
  }
  if(window_width=="month"){
    year_first  <- as.numeric(strftime(min(date),"%Y",tz="UTC"))
    month_first <- as.numeric(strftime(min(date),"%m",tz="UTC"))
    year_last   <- as.numeric(strftime(max(date),"%Y",tz="UTC"))
    month_last  <- as.numeric(strftime(max(date),"%m",tz="UTC"))
    
    if(month_last==12){
      month_last <- 1
      year_last  <- year_last + 1
    }
    
    date_first <- strptime(paste(year_first,sprintf("%02.0f",month_first),"01000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(paste(year_last, sprintf("%02.0f",month_last), "01000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    intervals  <- seq(date_first,date_last,by="months")
  }
  if(window_width=="week"){
    delta_days <- ((as.numeric(strftime(min(date),"%w",tz="UTC"))-1)+7)%%7
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") - delta_days*86400
    delta_days <- ((as.numeric(strftime(max(date),"%w",tz="UTC"))-1)+7)%%7
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + (7-delta_days)*86400
    intervals  <- seq(date_first,date_last,by="weeks")
  }
  if(window_width=="day"){
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + 86400
    intervals  <- seq(date_first,date_last,by="days")
  }
  
  n_intervals <- length(intervals)-1
  
  
  # Window width y
  
  if(is.null(yrange1)){
    yrange1 <- c(min(yyy1,na.rm=T),max(yyy1,na.rm=T))
  }
  if(is.null(yrange2)){
    yrange2 <- c(min(yyy2,na.rm=T),max(yyy2,na.rm=T))
  }
  
  # Plot
  
  def_par <- par()
  pdf(file = figname, width=16, height=8, onefile=T, pointsize=20, colormodel="srgb")
  par(mai=c(1,1,0.1,1))
  
  for(ith_interval in 1:n_intervals){
    id   <- which(date>=intervals[ith_interval] & date<intervals[ith_interval+1])
    n_id <- length(id)
    
    if(n_id<1){
      next
    }
    
    if(window_width=="all"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="2 years")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="years")
    }
    if(window_width=="all_day2day"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],length.out=5)
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],length.out=5)
    }
    if(window_width=="year"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="months")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="months")
    }
    if(window_width=="month"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="7 days")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="days")
    }
    if(window_width=="week"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="2 days")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="days")
    }
    if(window_width=="day"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="4 hours")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="hours")
    }
    
    n_x_grids <- length(x_grid)
    
    plot(c(intervals[ith_interval],intervals[ith_interval+1]),
         c(0,0),
         ylim=yrange1,
         t="l",
         col="gray50",
         lwd=1,
         lty=1,
         xaxt="n",
         xlab=xlabString,
         ylab=ylabString1,
         cex.axis=1.25,
         cex.lab=1.25)
    
    for(ith_x_grid in 1:n_x_grids){
      lines(c(x_grid[ith_x_grid],x_grid[ith_x_grid]),c(-1e9,1e9),col="gray80",lwd=1,lty=1)
    }
    
    axis(side=1,at=xticks,labels=xticksLab,cex.axis=1.25,cex.lab=1.25)
    
    for(ith_yyy1 in 1:n_yyy1){
      lines(date[id],yyy1[id,ith_yyy1],lty=1, lwd=1, col=ith_yyy1)
    }
    
    par(new=T)
    
    plot(c(intervals[ith_interval],intervals[ith_interval+1]),
         c(0,0),
         ylim=yrange2,
         t="l",
         col="gray50",
         lwd=1,
         lty=1,
         xaxt="n",
         xlab="",
         ylab="",
         xaxt="n",
         yaxt="n")
    
    for(ith_yyy2 in 1:n_yyy2){
      lines(date[id],yyy2[id,ith_yyy2],lty=1, lwd=1, col=4+ith_yyy2)
    }
    
    mtext(text = ylabString2, side = 4,line = 2.5,cex=1.25)
    axis(side = 4,cex.lab=1.25,cex.axis=1.25)
    
    par(family="mono")
    legend("topright",legend=legend_str,col=(c(1:n_yyy1,4+(1:n_yyy2)))[1:length(legend_str)],lwd=2,lty=1,cex=1.25,bg="white")
    par(family="")
    
  }
  
  dev.off()
  par(def_par)
  
  
}


# Time series : Network

plot_ts_NETWORK <- function(figname,date,yyy,yyy_FLAG,window_width,xrange_unixTS,yrange,xlabString,ylabString,legend_str,gap){
  
  #
  
  if(!window_width%in%c("day","week","month","year","all","all_day2day")){
    stop("Error 1 in plot function.")
  }
  
  #
  
  if(!is.matrix(yyy)){
    yyy <- matrix(yyy,ncol=1)
  }
  
  n_yyy <- dim(yyy)[2]
  
  
  # Date limits
  
  if(!is.null(xrange_unixTS)){
    xrange_date_min <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[1]
    xrange_date_max <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[2]
    
    id   <- which(date>=xrange_date_min & date<=xrange_date_max)
    n_id <- length(id)
    
    if(n_id>0){
      date <- date[id]
      yyy  <- yyy[id,]
    }else{
      stop()
    }
  }
  
  # Window width x
  
  if(is.null(window_width)){
    stop() 
  }
  if(window_width=="all"){
    intervals <- seq(strptime("20080101000000","%Y%m%d%H%M%S",tz="UTC"),strptime("20170101000000","%Y%m%d%H%M%S",tz="UTC"),length.out = 2)
  }
  if(window_width=="all_day2day"){
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + 86400
    intervals  <- seq(date_first,date_last,length.out = 2)
  }
  if(window_width=="year"){
    year_first <- as.numeric(strftime(min(date),"%Y",tz="UTC"))
    year_last  <- as.numeric(strftime(max(date),"%Y",tz="UTC")) + 1
    date_first <- strptime(paste(year_first,"0101000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(paste(year_last, "0101000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    intervals  <- seq(date_first,date_last,by="years")
  }
  if(window_width=="month"){
    year_first  <- as.numeric(strftime(min(date),"%Y",tz="UTC"))
    month_first <- as.numeric(strftime(min(date),"%m",tz="UTC"))
    year_last   <- as.numeric(strftime(max(date),"%Y",tz="UTC"))
    month_last  <- as.numeric(strftime(max(date),"%m",tz="UTC"))
    
    if(month_last==12){
      month_last <- 1
      year_last  <- year_last + 1
    }
    
    date_first <- strptime(paste(year_first,sprintf("%02.0f",month_first),"01000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(paste(year_last, sprintf("%02.0f",month_last), "01000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    intervals  <- seq(date_first,date_last,by="months")
  }
  if(window_width=="week"){
    delta_days <- ((as.numeric(strftime(min(date),"%w",tz="UTC"))-1)+7)%%7
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") - delta_days*86400
    delta_days <- ((as.numeric(strftime(max(date),"%w",tz="UTC"))-1)+7)%%7
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + (7-delta_days)*86400
    intervals  <- seq(date_first,date_last,by="weeks")
  }
  if(window_width=="day"){
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + 86400
    intervals  <- seq(date_first,date_last,by="days")
  }
  
  n_intervals <- length(intervals)-1
  
  
  # Window width y
  
  if(is.null(yrange)){
    yrange <- c(min(yyy,na.rm=T),max(yyy,na.rm=T))
  }
  
  
  # Plot
  
  def_par <- par()
  pdf(file = figname, width=16, height=8, onefile=T, pointsize=12, colormodel="srgb")
  par(mai=c(1,1,0.1,0.1))
  
  for(ith_interval in 1:n_intervals){
    id   <- which(date>=intervals[ith_interval] & date<intervals[ith_interval+1])
    n_id <- length(id)
    
    if(n_id<1){
      next
    }
    
    if(window_width=="all"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="2 years")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="years")
    }
    if(window_width=="all_day2day"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],length.out=5)
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],length.out=5)
    }
    if(window_width=="year"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="months")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="months")
    }
    if(window_width=="month"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="7 days")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="days")
    }
    if(window_width=="week"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="2 days")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="days")
    }
    if(window_width=="day"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="4 hours")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="hours")
    }
    
    n_x_grids <- length(x_grid)
    
    plot(c(intervals[ith_interval],intervals[ith_interval+1]),
         c(0,0),
         ylim=yrange,
         t="l",
         col="gray50",
         lwd=1,
         lty=1,
         xaxt="n",
         xlab=xlabString,
         ylab=ylabString,
         cex.axis=1.25,
         cex.lab=1.25)
    
    for(ith_x_grid in 1:n_x_grids){
      lines(c(x_grid[ith_x_grid],x_grid[ith_x_grid]),c(-1e9,1e9),col="gray80",lwd=1,lty=1)
    }
    
    axis(side=1,at=xticks,labels=xticksLab,cex.axis=1.25,cex.lab=1.25)
    
    for(ith_yyy in 1:n_yyy){
      
      # Time
      date_tmp     <- date[id]
      yyy_tmp      <- yyy[id,ith_yyy]
      yyy_FLAG_tmp <- yyy_FLAG[id,ith_yyy]
      
      #
      
      # id_ok        <- which(!is.na(yyy_tmp) & !is.na(yyy_FLAG_tmp))
      # date_tmp_A   <- date_tmp[id_ok]
      # yyy_tmp_A    <- yyy_tmp[id_ok]
      # 
      # if(length(id_ok)>1){
      #   
      #   id_gap   <- which(diff(as.numeric(difftime(time1=date_tmp_A,time2=date_tmp_A[1],units="secs",tz="UTC")))>15*60)
      #   n_id_gap <- length(id_gap)
      #   
      #   if(n_id_gap>0){
      #     date_tmp_A <- c(date_tmp_A,date_tmp_A[id_gap]+1)
      #     yyy_tmp_A  <- c(yyy_tmp_A,rep(NA,n_id_gap))
      # 
      #     oo       <- order(date_tmp_A)
      # 
      #     date_tmp_A <- date_tmp_A[oo]
      #     yyy_tmp_A  <- yyy_tmp_A[oo]
      # 
      #   }
      #   lines(date_tmp_A,yyy_tmp_A, lty=1, lwd=1, col="gray80")
      # }
      
      #
      
      id_ok        <- which(!is.na(yyy_tmp) & !is.na(yyy_FLAG_tmp) & yyy_FLAG_tmp==1)
      date_tmp_A   <- date_tmp[id_ok]
      yyy_tmp_A    <- yyy_tmp[id_ok]
      
      if(length(id_ok)>1){
        
        id_gap   <- which(diff(as.numeric(difftime(time1=date_tmp_A,time2=date_tmp_A[1],units="secs",tz="UTC")))>gap)
        n_id_gap <- length(id_gap)
        
        if(n_id_gap>0){
          date_tmp_A <- c(date_tmp_A,date_tmp_A[id_gap]+1)
          yyy_tmp_A  <- c(yyy_tmp_A,rep(NA,n_id_gap))
          
          oo       <- order(date_tmp_A)
          
          date_tmp_A <- date_tmp_A[oo]
          yyy_tmp_A  <- yyy_tmp_A[oo]
          
        }
        lines(date_tmp_A,yyy_tmp_A, lty=1, lwd=1, col=rainbow(n_yyy)[ith_yyy])
      }
    }
    
    par(family="mono")
    legend("topright",legend=legend_str,col=rainbow(n_yyy),lwd=2,lty=1,cex=0.6,bg="white")
    par(family="")
    
  }
  
  dev.off()
  par(def_par)

}


# Time series

plot_ts_LP8atREF <- function(figname,date,yyy,window_width,xrange_unixTS,yrange,xlabString,ylabString,legend_str){
  
  #
  
  if(!window_width%in%c("day","week","month","year","all","all_day2day")){
    stop("Error 1 in plot function.")
  }
  
  #
  
  if(!is.matrix(yyy)){
    yyy <- matrix(yyy,ncol=1)
  }
  
  n_yyy <- dim(yyy)[2]
  
  colors <- c(rainbow(n_yyy-1),"black")
  
  
  # Date limits
  
  if(!is.null(xrange_unixTS)){
    xrange_date_min <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[1]
    xrange_date_max <- strptime("19700101000000","%Y%m%d%H%M%S",tz="UTC") + xrange_unixTS[2]
    
    id   <- which(date>=xrange_date_min & date<=xrange_date_max)
    n_id <- length(id)
    
    if(n_id>0){
      date <- date[id]
      yyy  <- yyy[id,]
    }else{
      stop()
    }
  }
  
  # Window width x
  
  if(is.null(window_width)){
    stop() 
  }
  if(window_width=="all"){
    intervals <- seq(strptime("20080101000000","%Y%m%d%H%M%S",tz="UTC"),strptime("20170101000000","%Y%m%d%H%M%S",tz="UTC"),length.out = 2)
  }
  if(window_width=="all_day2day"){
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + 86400
    intervals  <- seq(date_first,date_last,length.out = 2)
  }
  if(window_width=="year"){
    year_first <- as.numeric(strftime(min(date),"%Y",tz="UTC"))
    year_last  <- as.numeric(strftime(max(date),"%Y",tz="UTC")) + 1
    date_first <- strptime(paste(year_first,"0101000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(paste(year_last, "0101000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    intervals  <- seq(date_first,date_last,by="years")
  }
  if(window_width=="month"){
    year_first  <- as.numeric(strftime(min(date),"%Y",tz="UTC"))
    month_first <- as.numeric(strftime(min(date),"%m",tz="UTC"))
    year_last   <- as.numeric(strftime(max(date),"%Y",tz="UTC"))
    month_last  <- as.numeric(strftime(max(date),"%m",tz="UTC"))
    
    if(month_last==12){
      month_last <- 1
      year_last  <- year_last + 1
    }
    
    date_first <- strptime(paste(year_first,sprintf("%02.0f",month_first),"01000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(paste(year_last, sprintf("%02.0f",month_last), "01000000",sep=""),"%Y%m%d%H%M%S",tz="UTC")
    intervals  <- seq(date_first,date_last,by="months")
  }
  if(window_width=="week"){
    delta_days <- ((as.numeric(strftime(min(date),"%w",tz="UTC"))-1)+7)%%7
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") - delta_days*86400
    delta_days <- ((as.numeric(strftime(max(date),"%w",tz="UTC"))-1)+7)%%7
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + (7-delta_days)*86400
    intervals  <- seq(date_first,date_last,by="weeks")
  }
  if(window_width=="day"){
    date_first <- strptime(strftime(min(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC")
    date_last  <- strptime(strftime(max(date),"%Y%m%d000000",tz="UTC"),"%Y%m%d%H%M%S",tz="UTC") + 86400
    intervals  <- seq(date_first,date_last,by="days")
  }
  
  n_intervals <- length(intervals)-1
  
  
  # Window width y
  
  if(is.null(yrange)){
    yrange <- c(min(yyy,na.rm=T),max(yyy,na.rm=T))
  }
  
  
  # Plot
  
  def_par <- par()
  pdf(file = figname, width=16, height=8, onefile=T, pointsize=12, colormodel="srgb")
  par(mai=c(1,1,0.1,0.1))
  
  for(ith_interval in 1:n_intervals){
    id   <- which(date>=intervals[ith_interval] & date<intervals[ith_interval+1])
    n_id <- length(id)
    
    if(n_id<1){
      next
    }
    
    if(window_width=="all"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="2 years")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="years")
    }
    if(window_width=="all_day2day"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],length.out=5)
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],length.out=5)
    }
    if(window_width=="year"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="months")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="months")
    }
    if(window_width=="month"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="7 days")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="days")
    }
    if(window_width=="week"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="2 days")
      xticksLab <- strftime(xticks,"%d/%m/%Y",tz="UTC")
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="days")
    }
    if(window_width=="day"){
      xticks    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="4 hours")
      xticksLab <- c(strftime(xticks[1],"%d/%m/%Y",tz="UTC"),strftime(xticks[2:length(xticks)],"%H:%M",tz="UTC"))
      x_grid    <- seq(intervals[ith_interval],intervals[ith_interval+1],by="hours")
    }
    
    n_x_grids <- length(x_grid)
    
    plot(c(intervals[ith_interval],intervals[ith_interval+1]),
         c(0,0),
         ylim=yrange,
         t="l",
         col="gray50",
         lwd=1,
         lty=1,
         xaxt="n",
         xlab=xlabString,
         ylab=ylabString,
         cex.axis=1.25,
         cex.lab=1.25)
    
    for(ith_x_grid in 1:n_x_grids){
      lines(c(x_grid[ith_x_grid],x_grid[ith_x_grid]),c(-1e9,1e9),col="gray80",lwd=1,lty=1)
    }
    
    axis(side=1,at=xticks,labels=xticksLab,cex.axis=1.25,cex.lab=1.25)
    
    
    if(n_yyy>1){
      for(ith_yyy in 1:(n_yyy-1)){
        lines(date[id],yyy[id,ith_yyy],lty=1, lwd=1, col=colors[ith_yyy])
      }
    }
    
    for(ith_yyy in n_yyy:n_yyy){
      lines(date[id],yyy[id,ith_yyy],lty=1, lwd=2, col=colors[ith_yyy])
    }
    
    par(family="mono")
    legend("topright",legend=legend_str,col=colors,lwd=4,lty=1,cex=1.25,bg="white")
    par(family="")
    
    
  }
  
  dev.off()
  par(def_par)
  
  
}

