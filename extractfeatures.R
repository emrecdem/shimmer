# Data exploration Shimmer device - 20 February 2019 (by: Vincent van Hees)
rm(list=ls()) # WARNING: this will empty your workspace, comment this out if you do not want that
graphics.off() # WARNING: this will close all open figures, comment this out if you do not want that
#==============================================
# Key input parameters from user:
#==============================================
# specify location of data folder and output folder:
path = "/media/sf_sharedfolder/Emotion"
datafolder = paste0(path,"/accelerometer_data/datamichel")
outputfolder = paste0(path,"/accelerometer_data/myresults")
# epoch size in seconds to which data will be aggregated:
epochsize = 1 
# whether or not to plot some data on screen (mainly useful for testing purposes)
do.plot = TRUE # change to FALSE to turn off / change to TRUE to turn on

#====================================================
# call packages and declare functions
list.of.packages <- c("signal", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(signal)
library(data.table)
get_enmo =  function(x,y,z) {
  # enmo: euclidean norm minus one, with negative values rounded to zero
  enmo = pmax(0,sqrt(x^2 + y^2 + z^2) - 9.81)
}
p50 = function(x) return(quantile(x,probs = 0.50))
p75 = function(x) return(quantile(x,probs = 0.75))
p99 = function(x) return(quantile(x,probs = 0.99))
addvarEnmoWR = function(x,varname = "") {
  # replace variable "enmo" with a new name
  # this is needed for the output of the aggregate function
  x[varname] = x$enmo_WR
  x = x[,-which(colnames(x) == "enmo_WR")]
  return(x)
}
addvarEnmoLN = function(x,varname = "") {
  # replace variable "enmo" with a new name
  # this is needed for the output of the aggregate function
  x[varname] = x$enmo_LN
  x = x[,-which(colnames(x) == "enmo_LN")]
  return(x)
}

#====================================================
# main code:
fnames = dir(datafolder)
blocksize = 500000 # If your machine runs out of memory then lower this value.
for (fi in 1:length(fnames)) {
  timer0 = Sys.time()
  fname = paste0(datafolder,"/",fnames[fi])
  cat(paste0("\n",fname))
  varname_tmp = read.csv(file = fname,nrows = 1, skip=1,sep = "\t",
                         header = FALSE,stringsAsFactors = FALSE)
  unit = read.csv(file = fname,nrows = 1, skip=2,sep = "\t",
                  header = FALSE,stringsAsFactors = FALSE)
  varname_tmp = as.character(varname_tmp)
  data2store = c()
  currentblock = 3
  stopprocess = FALSE
  fnameshort_withoutext = unlist(strsplit(fnames[fi],"[.]cs"))[1]
  # id = unlist(strsplit(fnameshort_withoutext,"_"))[2]
  while (stopprocess == FALSE) {
    # load data
    D = c()
    try(expr={D = data.table::fread(file = fname,nrows = blocksize, skip=currentblock,sep = "\t",
                                    header = FALSE,stringsAsFactors = FALSE)},silent=TRUE)
    if (length(D) == 0) {
      stopprocess =TRUE
    } else {
      if (nrow(D) < 1000) {
        stopprocess =TRUE
      } else { # if there is data, continue with processing:
        D = as.data.frame(D)
        currentblock = currentblock + blocksize + 1 
        cat(" .")
        # assign names to columns
        colnames(D) = varname_tmp
        # add timestamp
        D$timestamp = as.POSIXlt(D[,1]/1000,origin="1970-1-1",tz="Europe/Amsterdam")
        varname = c(varname_tmp,"timestamp")
        # sort column names (variable names)
        D = D[,sort(colnames(D))]
        varname = sort(varname)
        
        # extract serial number from variable names
        sn.keepsearching = TRUE
        vi = 1
        while (sn.keepsearching == TRUE) {
          split_varname = unlist(strsplit(varname[vi],"_"))
          if (length(split_varname) > 1) {
            sn = split_varname[1]
            sn.keepsearching = FALSE
          }
          vi = vi + 1
          if (vi >= length(varname)) {
            cat("\nError: ID not found in variable names")
            sn.keepsearching = FALSE
          }
        }
        # derive attachment location
        if (sn == "CD5D" | sn == "C9BB") {
          bodyside = "domwrist"
        } else if (sn == "D910" | sn == "D821") {
          bodyside = "nondomwrist"
        } else if (sn == "D977" | sn == "DA9F") {
          bodyside = "chest"
        }
        #remove all identifiers from varnames, to make code below more generic
        for (jj in 1:length(varname)) {
          partname = unlist(strsplit(varname[jj],"_"))[2] #assumption that name is structured as letters_letters_variablename
          tmp = unlist(strsplit(varname[jj],paste0("_",partname,"_")))
          if (length(tmp) > 1) varname[jj] = tmp[2]
        }
        colnames(D) = varname
        # if ((currentblock-blocksize + 1 ) < 10) print(varname)
        # extract sample frequency from timestamps and number of data points
        duration_data_secs = as.numeric(difftime(D$timestamp[nrow(D)],D$timestamp[1],units = "secs"))
        sf = round((nrow(D)-1) / duration_data_secs)
        #-------------------------------
        # calculate enmo features (magnitude of acceleration)
      
        enmo_WR = get_enmo(x=D$Accel_WR_X_CAL,y=D$Accel_WR_Y_CAL,z=D$Accel_WR_Z_CAL)
        enmo_LN = get_enmo(x=D$Accel_LN_X_CAL,y=D$Accel_LN_Y_CAL,z=D$Accel_LN_Z_CAL)
        
        # remove low frequency component, probably related to imperfect calibration
        lb = 0.2 # lower boundary of the filter
        n = 4 # filter order
        bf = signal::butter(n,c(lb/(sf/2)),type=c("high")) #creating filter coefficients
        # also ignore direction of acceleration now signal is high pass filtered
        enmo_WR = abs(signal::filter(bf,enmo_WR))
        enmo_LN = abs(signal::filter(bf,enmo_LN))
        
        
        # downsample
        FiveSecIndex = round(round(as.numeric(D$timestamp)/epochsize)*epochsize)
        df_kin = data.frame(enmo_WR=enmo_WR,enmo_LN=enmo_LN,
                            pitch_LN = D$Euler_9DOF_Pitch_LN_CAL, pitch_WR = D$Euler_9DOF_Pitch_WR_CAL,
                            roll_LN = D$Euler_9DOF_Roll_LN_CAL, roll_WR = D$Euler_9DOF_Roll_WR_CAL,
                            yaw_LN = D$Euler_9DOF_Yaw_LN_CAL, yaw_WR = D$Euler_9DOF_Yaw_WR_CAL,
                            time=D$timestamp,numtime=FiveSecIndex)
        if ("GSR_Skin_Conductance_CAL" %in% varname) {
          do.skinsensors = TRUE  
        } else {
          do.skinsensors = FALSE
        }
        if (do.skinsensors == TRUE) {
          df_skin = data.frame(GSR_Skin_Conductance = D$GSR_Skin_Conductance_CAL,
                              GSR_Skin_Resistance = D$GSR_Skin_Resistance_CAL,
                              time=D$timestamp, numtime=FiveSecIndex)
          Oskin = aggregate(x = df_skin,by = list(df_skin$numtime),mean)
        }
        
        O50 = aggregate(x = df_kin,by = list(df_kin$numtime),p50)
        O75 = aggregate(x = df_kin,by = list(df_kin$numtime),p75)
        O99 = aggregate(x = df_kin,by = list(df_kin$numtime),p99)
        Omean = aggregate(x = df_kin,by = list(df_kin$numtime),mean)
        Omax = aggregate(x = df_kin,by = list(df_kin$numtime),max)
        Ostd = aggregate(x = df_kin,by = list(df_kin$numtime),sd)
        
        O50 = addvarEnmoWR(x = O50, varname="enmoWR_p50")
        O50 = addvarEnmoLN(x = O50, varname="enmoLN_p50")
        O75 = addvarEnmoWR(x = O75, varname="enmoWR_p75")
        O75 = addvarEnmoLN(x = O75, varname="enmoLN_p75")
        O99 = addvarEnmoWR(x = O99, varname="enmoWR_p99")
        O99 = addvarEnmoLN(x = O99, varname="enmoLN_p99")
        Omean = addvarEnmoWR(x = Omean, varname="enmoWR_mean")
        Omean = addvarEnmoLN(x = Omean, varname="enmoLN_mean")
        Omax = addvarEnmoWR(x = Omax, varname="enmoWR_max")
        Omax = addvarEnmoLN(x = Omax, varname="enmoLN_max")
        Ostd = addvarEnmoWR(x = Ostd, varname="enmoWR_std")
        Ostd = addvarEnmoLN(x = Ostd, varname="enmoLN_std")
        
        agData = data.frame(time=O50$time,
                            numtime=O50$numtime,
                            accWR_percentile50=O50$enmoWR_p50,
                            accWR_percentile75=O75$enmoWR_p75,
                            accWR_percentile99=O99$enmoWR_p99,
                            accWR_mean=Omean$enmoWR_mean,
                            accWR_max=Omax$enmoWR_max,
                            accWR_std=Ostd$enmoWR_std,
                            accLN_percentile50=O50$enmoLN_p50,
                            accLN_percentile75=O75$enmoLN_p75,
                            accLN_percentile99=O99$enmoLN_p99,
                            accLN_mean=Omean$enmoLN_mean,
                            accLN_max=Omax$enmoLN_max,
                            accLN_std=Ostd$enmoLN_std,
                            pitch_LN_mean = Omean$pitch_LN,
                            pitch_WR_mean = Omean$pitch_WR,
                            roll_LN_mean = Omean$roll_LN,
                            roll_WR_mean = Omean$roll_WR,
                            yaw_LN_mean = Omean$yaw_LN,
                            yaw_WR_mean = Omean$yaw_WR)
        # identify peaks in the magnitude of acceleration (enmo)
        agData$accWR_peak = 0
        agData$accLN_peak = 0
        # peaks are defined here as:
        # (maximum values in an epoch at least 200% the 99th percentile of that epoch
        # AND with a value above 2 m/s2) OR with max acceleration above 10
        peakindexWR = which(((agData$accWR_max-agData$accWR_percentile50) > 
                             ((agData$accWR_percentile99-agData$accWR_percentile50)*2) & agData$accWR_max > 2) |
                            agData$accWR_max > 10)
        agData$accWR_peak[peakindexWR] = 1 
        peakindexLN = which(((agData$accLN_max-agData$accLN_percentile50) > 
                               ((agData$accLN_percentile99-agData$accLN_percentile50)*2) & agData$accLN_max > 2) |
                              agData$accLN_max > 10)
        agData$accLN_peak[peakindexLN] = 1 
        if (do.skinsensors == TRUE) {
          agData$GSR_Skin_Conductance_mean = Oskin$GSR_Skin_Conductance 
          agData$GSR_Skin_Resistance_mean = Oskin$GSR_Skin_Resistance
        }
        # plot low resolution data on screen:
        if (do.plot == TRUE) {
          x11()
          par(mfrow=c(2,1))
          plot(agData$time,agData$accWR_mean,type="l",ylim=c(0,max(agData$accWR_max)),main="accWR",
               col="blue",xlab="timestamp",ylab="acceleration (m/s2)")
          lines(agData$time,agData$accWR_max,type="l",col="green")
          lines(agData$time[peakindexWR],agData$accWR_max[peakindexWR],col="red",type="p",pch=20,cex=0.5)
          legend("topleft",legend = c("mean","max"),col=c("blue","green"),lty=c(1,1))
          
          plot(agData$time,agData$accLN_mean,type="l",ylim=c(0,max(agData$accLN_max)),main="accLN",
               col="blue",xlab="timestamp",ylab="acceleration (m/s2)")
          lines(agData$time,agData$accLN_max,type="l",col="green")
          lines(agData$time[peakindexLN],agData$accLN_max[peakindexLN],col="red",type="p",pch=20,cex=0.5)
          legend("topleft",legend = c("mean","max"),col=c("blue","green"),lty=c(1,1))
        }
        if (length(data2store) == 0) {
          data2store = agData
        } else {
          data2store = rbind(data2store,agData)
        }
      }
    }
    graphics.off()
  }
  write.csv(data2store,file = paste0(outputfolder,"/shim_aggre_",bodyside,"_",fnameshort_withoutext,".csv"),row.names = FALSE)
  timer1 = Sys.time()
  cat("\nTime elapsed:")
  print(timer1-timer0)
}