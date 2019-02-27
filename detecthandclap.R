rm(list=ls())
graphics.off()
#==============================================
# Key input parameters from user:
#==============================================
# specify workfolder:
path = "/media/sf_sharedfolder/Emotion/accelerometer_data"
# assumption is that there is a 'myresults' folder in this folder
# in which the extracted features are stored.
do.plot = TRUE # Create plot on screen (for testing). change to FALSE to turn off / change to TRUE to turn on
#==============================================

outputfolder = paste0(path,"/myresults")
handclapfolder = paste0(path,"/handclaps") # create folder in which the handclaptimes are stored
if (dir.exists(handclapfolder) == FALSE) dir.create(handclapfolder)
# extract relevant information from filenames in the 'myresults' folder
fnamesfull = dir(outputfolder,full.names = TRUE)
fnames = dir(outputfolder)
getid = function(x) return(unlist(strsplit(x,"_"))[4])
getloc = function(x) return(unlist(strsplit(x,"_"))[2])
getses = function(x) return(unlist(strsplit(x,"_"))[3])
getpart = function(x) return(unlist(strsplit(x,"_"))[7])
id = as.character(sapply(X = fnames,FUN=getid))
loc = as.character(sapply(X = fnames,FUN=getloc)) #loc = location
ses = as.character(sapply(X = fnames,FUN=getses)) #ses = session
part = as.character(sapply(X = fnames,FUN=getpart)) #part = partA or PartB
wrist = loc %in% c("nondomwrist","domwrist")
uid = unique(id)
for (idi in uid) { # loop through individuals
  uses = unique(ses[which(id == idi)])
  for (sesi in uses) { # loop over sessions
    upart = unique(part[which(id == idi & ses == sesi)])  
    for (parti in upart) { # loop over parts
      twowrists = which(ses == sesi & id ==idi & wrist == TRUE & part == parti)
      if (length(twowrists) == 2) { # if a pair is found load the data
        loc1 = loc[twowrists[1]]
        if (loc1 == "nondomwrist") {
          nondomwrist = read.csv(fnamesfull[twowrists[1]])
          domwrist = read.csv(fnamesfull[twowrists[2]])
        } else {
          nondomwrist = read.csv(fnamesfull[twowrists[2]])
          domwrist = read.csv(fnamesfull[twowrists[1]])
        }
        # fuse the data from both wrists
        S = merge(nondomwrist,domwrist,by=c("numerictime"),suffixes = c(".nd",".dm"),all = TRUE) #nd =nondominant, dm=dominant
        S$time.nd = as.POSIXlt(S$time.nd)
        S$time.dm = as.POSIXlt(S$time.dm)
        if (do.plot == TRUE) {
          # Create plot of the timeseries and the matching handclaps in red:
          x11();
          matchingpeaks = which(S$acc_peak.nd == 1 & S$acc_peak.dm== 1)
          
          par(mfrow=c(2,1))
          plot(S$time.nd,S$acc_mean.nd,type="l",main=paste0("nondominant wrist ",idi," ",sesi),
               ylim=c(0,5),ylab="acceleration (g)",xlab="time")
          lines(S$time.nd,S$acc_max.nd,type="l",col="blue")
          lines(S$time.nd[matchingpeaks],rep(1,length(matchingpeaks)),type="p",col="red",pch=20)
          plot(S$time.dm,S$acc_mean.dm,type="l",main=paste0("dominant wrist ",idi," ",sesi),
               ylim=c(0,5),ylab="acceleration (g)",xlab="time")
          lines(S$time.dm,S$acc_max.dm,type="l",col="blue")
          lines(S$time.dm[matchingpeaks],rep(1,length(matchingpeaks)),type="p",col="red",pch=20)
          # Write matching handclap timestamps to a csv file:
          peaks = data.frame(timestamp=S$time.nd[matchingpeaks])
          print(paste0("Number of handclaps: ",nrow(peaks)))
          write.csv(peaks,file=paste0(handclapfolder,"/PossibleHandClapTimes_",idi,"_",sesi,".csv"),row.names = FALSE)
        }
      }
    }
  }
}
