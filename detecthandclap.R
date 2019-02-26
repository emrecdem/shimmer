rm(list=ls())
graphics.off()
#==============================================
# Key input parameters from user:
#==============================================
# specify workfolder:
path = "/media/sf_sharedfolder/Emotion/accelerometer_data"
# assumption is that there is a 'myresults' folder in this folder
# in which the extracted features are stored.
#==============================================

outputfolder = paste0(path,"/myresults")
handclapfolder = paste0(path,"/handclaps") # create folder in which the handclaptimes are stored
if (dir.exists(handclapfolder) == FALSE) dir.create(handclapfolder)
# extract relevant information from filenames in the 'myresults' folder
fnamesfull = dir(outputfolder,full.names = TRUE)
fnames = dir(outputfolder)
getloc = function(x) return(unlist(strsplit(x,"_"))[4])
getid = function(x) return(unlist(strsplit(x,"_"))[6])
getses = function(x) return(unlist(strsplit(x,"_"))[5])
id = as.character(sapply(X = fnames,FUN=getid))
loc = as.character(sapply(X = fnames,FUN=getloc)) #loc = location
ses = as.character(sapply(X = fnames,FUN=getses)) #ses = session
wrist = loc %in% c("nondomwrist","domwrist")
uid = unique(id)
for (idi in uid) { # loop through individuals
  uses = unique(ses[which(id == idi)])  
  for (sesi in uses) { # loop over sessions
    twowrists = which(ses == sesi & id ==idi & wrist == TRUE)
    if (length(twowrists) > 1) { # if a pair is found load the data
      loc1 = loc[twowrists[1]]
      if (loc1 == "nondomwrist") {
        nondomwrist = read.csv(fnamesfull[twowrists[1]])
        domwrist = read.csv(fnamesfull[twowrists[2]])
      } else {
        nondomwrist = read.csv(fnamesfull[twowrists[2]])
        domwrist = read.csv(fnamesfull[twowrists[1]])
      }
      # fuse the data from both wrists
      S = merge(nondomwrist,domwrist,by=c("numtime"),suffixes = c(".nd",".dm"),all = TRUE)
      S$time.nd = as.POSIXlt(S$time.nd)
      S$time.dm = as.POSIXlt(S$time.dm)
      # Create plot of the timeseries and the matching handclaps in red:
      x11();
      matchingpeaks = which(S$accWR_peak.nd == 1 & S$accWR_peak.dm== 1)
      par(mfrow=c(2,1))
      plot(S$time.nd,S$accWR_mean.nd,type="l",main=paste0("nondominant wrist ",idi," ",sesi),
           ylim=c(0,5),ylab="acceleration (g)",xlab="time")
      lines(S$time.nd,S$accWR_max.nd,type="l",col="blue")
      lines(S$time.nd[matchingpeaks],rep(1,length(matchingpeaks)),type="p",col="red",pch=20)
      plot(S$time.dm,S$accWR_mean.dm,type="l",main=paste0("dominant wrist ",idi," ",sesi),
           ylim=c(0,5),ylab="acceleration (g)",xlab="time")
      lines(S$time.dm,S$accWR_max.dm,type="l",col="blue")
      lines(S$time.dm[matchingpeaks],rep(1,length(matchingpeaks)),type="p",col="red",pch=20)
      # Write matching handclap timestamps to a csv file:
      peaks = data.frame(timestamp=S$time.nd[matchingpeaks])
      write.csv(peaks,file=paste0(handclapfolder,"/PossibleHandClapTimes_",idi,"_",sesi,".csv"),row.names = FALSE)
    }
  }
}