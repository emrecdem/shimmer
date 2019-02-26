# Explorative code used with AX3 experiments
rm(list=ls())
graphics.off()
fname_left = "/media/sf_sharedfolder/Emotion/handclapping_experiment/37727_0000000026_left.resampled.csv"
fname_right = "/media/sf_sharedfolder/Emotion/handclapping_experiment/39434_0000000026_right.resampled.csv"
claptimes =  "/media/sf_sharedfolder/Emotion/handclapping_experiment/handclaptimes.csv"

claps = read.csv(claptimes)
claptimes = paste0("2019-02-26 ",as.character(claps$Time),sep = "")
claps$Time = as.POSIXlt(claptimes,formalArgs="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")

for (hand in c("left","right")) {
  if (hand == "left") {
    acc = read.csv(fname_left)
  } else if (hand == "right") {
    acc = read.csv(fname_right)
  }
  acc$Time = as.POSIXlt(acc$Time,formalArgs="%Y-%m-%d %H:%M:%S",tz="Europe/Amsterdam")

  bigwindowsize =  4 #size in seconds
  smallwindowsize = 100 # size in samples
  
  for (i in 1:nrow(claps)) {
    plotdim = 4 # number of plotting columns
    plotdim2 = plotdim^2
    if ((i+(plotdim2-1))/plotdim2 == round((i+(plotdim2-1))/plotdim2)) {
      x11()
      par(mfrow=c(plotdim,plotdim))
    }
    window = which(acc$Time < claps$Time[i]+(bigwindowsize/2) & acc$Time > claps$Time[i]-(bigwindowsize/2))
    MX = max(acc$Accel.X..g.[window]) # center smallwindow around max of bigwindow 
    peak = which(acc$Accel.X..g.[window] == MX)
    window = window[(peak-smallwindowsize):(peak+smallwindowsize)]
    index = 1:length(window)
    plot(index,acc$Accel.X..g.[window],type="l",xlab="time",ylab="Acceleration (g)",
         main=paste0("clap ",i," wrist ",hand),col="blue",ylim=c(-4,8))
    lines(index,acc$Accel.Y..g.[window],type="l",col="red")
    lines(index,acc$Accel.Z..g.[window],type="l",col="green")
    legend("topleft",legend = c("X","Y","Z"),col=c("blue","red","green"),lty=c(1,1,1),cex=0.4)
    
  }
}