# Data exploration Shimmer device - February 2019 (by: Vincent van Hees)
rm(list=ls()) # WARNING: this will empty your workspace, comment this out if you do not want that
graphics.off() # WARNING: this will close all open figures, comment this out if you do not want that
#=============================================================
# Key input parameters from user:
#=============================================================
# specify location of data folder and output folder:
path = "/media/sf_sharedfolder/Emotion/accelerometer_data"
datafolder = paste0(path,"/datamichel")
outputfolder = paste0(path,"/myresults")

AccelerometerType = "WR" # Choose which of the two accelerometers to use, alternatively use "LN"
DeviceSerialNumbers_dominantwrist = c("CD5D", "C9BB")
DeviceSerialNumbers_nondominantwrist = c("D910", "D821")
DeviceSerialNumbers_chest = c("D977", "DA9F")
epochsize = 1 # Epoch size in seconds to which data will be aggregated:
do.plot = TRUE # Create plot on screen (for testing). change to FALSE to turn off / change to TRUE to turn on

#----------------------------------------------------------------------
# Abbreviations:
# - WR:  Wide range accelerometer (see Shimmer documentation)
# - LN:  Low noise accelerometer now  (see Shimmer documentation)
# - Yaw, roll and pitch: Euler angles provided by Shimmer software (see Shimmer documention)
# - enmo: refers to metric used for calculating magnitude of acceleration (abbreviation only
#         used in code, not in variable names.
#         In the variable name it is just called acceleration).
#         The metric entails: Eucliden Norm of three axes Minus One with negatie values rounded to zero 
#
# Units:
# - peak variables are indicated by 1 (peak) 0 (not a peak).
# - all other variables are in m/s2, unless otherwise indicated.
#=============================================================
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
addvarEnmo = function(x,varname = "") {
  # replace variable "enmo" with a new name
  # this is needed for the output of the aggregate function
  x[varname] = x$enmo
  x = x[,-which(colnames(x) == "enmo")]
  return(x)
}
options(digits.secs=12) # to get more precise fractions of seconds
#====================================================
# main code:
if (dir.exists(outputfolder) == FALSE) dir.create(outputfolder)
fnames = dir(datafolder)
bodyside = "bodysideunknown"
do.call = TRUE # Do callibration? (not functional at the moment)
if (do.call == TRUE) epochsize = 30
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
  data2store = Dstore = sfstore = c() # initialize objects
  endlastblock = 3
  stopprocess = FALSE
  fnameshort_withoutext = unlist(strsplit(fnames[fi],"[.]cs"))[1]
  blocki = 1
  while (stopprocess == FALSE) {
    # load data
    D = c()
    try(expr={D = data.table::fread(file = fname,nrows = blocksize, skip=endlastblock,sep = "\t",
                                    header = FALSE,stringsAsFactors = FALSE)},silent=TRUE)
    if (length(D) == 0) {
      stopprocess =TRUE
    } else {
      if (nrow(D) < 1000) {
        stopprocess =TRUE
      } else { # if there is data, continue with processing:
        D = as.data.frame(D)
        endlastblock = (blocki * blocksize) + 3
        blocki = blocki + 1
        cat(" .")
        # assign names to columns
        colnames(D) = varname_tmp
        # add POSIX timestamp to data.frame
        D$timestamp = as.POSIXlt(D[,1]/1000,origin="1970-1-1",tz="Europe/Amsterdam")
        varname = c(varname_tmp,"timestamp")
        # Sort column names (variable names)
        D = D[,sort(colnames(D))]
        varname = sort(varname)
        # extract device serial number from variable names
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
        # derive attachment location from device serial number
        if (sn %in% DeviceSerialNumbers_dominantwrist) {
          bodyside = "domwrist"
        } else if (sn %in% DeviceSerialNumbers_nondominantwrist) {
          bodyside = "nondomwrist"
        } else if (sn %in% DeviceSerialNumbers_chest) {
          bodyside = "chest"
        }
        
        #remove all device identifiers from varnames, to allow for more generic code
        for (jj in 1:length(varname)) {
          partname = unlist(strsplit(varname[jj],"_"))[2] #assumption that name is structured as letters_letters_variablename
          tmp = unlist(strsplit(varname[jj],paste0("_",partname,"_")))
          if (length(tmp) > 1) varname[jj] = tmp[2]
        }
        colnames(D) = varname
        # extract sample frequency from timestamps and number of data points
        duration_data_secs = as.numeric(difftime(D$timestamp[nrow(D)],D$timestamp[1],units = "secs"))
        sf = round((nrow(D)-1) / duration_data_secs)
        sfstore = c(sfstore,sf)
        # Now we know the sample rate, we can assess how many rows there are too many in this data block for
        # integer number of feature extraction.
        # First, append rows from previous iteration. For the first iteration this will append an empty object
        D = rbind(Dstore,D) 
        # Identify how many samples there are too many and store them for the next iteration
        NRD = nrow(D)
        SamplesPerEpoch = epochsize*sf
        lastsample = ((floor(NRD/SamplesPerEpoch)) * SamplesPerEpoch)
        if (floor(NRD/SamplesPerEpoch) < 1) stopprocess = TRUE # do not continue if there is not at least one epoch with data
        Dstore = D[(lastsample+1):NRD,] # store these rows for next iteration
        D = D[1:lastsample,] # continue with the rest of the data.
        if (do.call ==  FALSE) { # if we do not want to do calibration then focus on feature extraction
          #-------------------------------
          # calculate enmo features (magnitude of acceleration)
          if (AccelerometerType == "WR") {
            enmo = get_enmo(x=D$Accel_WR_X_CAL,y=D$Accel_WR_Y_CAL,z=D$Accel_WR_Z_CAL)
          } else {
            enmo = get_enmo(x=D$Accel_LN_X_CAL,y=D$Accel_LN_Y_CAL,z=D$Accel_LN_Z_CAL)
          }
          # remove low frequency component, probably related to imperfect calibration
          lb = 0.2 # lower boundary of the filter
          n = 4 # filter order
          bf = signal::butter(n,c(lb/(sf/2)),type=c("high")) #creating filter coefficients
          # also ignore direction of acceleration now signal is high pass filtered
          enmo = abs(signal::filter(bf,enmo))
          # Calculate separate enmo specifically for peak detecion only for WR for now
          lb = 5 # lower boundary of the filter
          hb = 50 # higher bounder of the filter
          Wc = matrix(0,2,1)
          Wc[1,1] = lb / (sf/2)
          Wc[2,1] = hb / (sf/2)
          bf = signal::butter(n,Wc,type=c("pass"))
          if (AccelerometerType == "WR") {
            GX = D$Accel_WR_X_CAL; GY = D$Accel_WR_Y_CAL; GZ = D$Accel_WR_Z_CAL
          } else {
            GX = D$Accel_LN_X_CAL; GY = D$Accel_LN_Y_CAL; GZ = D$Accel_LN_Z_CAL
          }
          GX = abs(signal::filter(bf,GX))
          GY = abs(signal::filter(bf,GY))
          GZ = abs(signal::filter(bf,GZ))
          enmopeak = get_enmo(x=GX,y=GY,z=GZ)
        }
        # downsample
        if (epochsize < 1) {
          FiveSecIndex = round(round(as.numeric(D$timestamp)/epochsize*10,digits=1) 
                               * epochsize*10,digits=1)
        } else {
          FiveSecIndex = round(round(as.numeric(D$timestamp)/epochsize) 
                               * epochsize)
        }
        if (do.call ==  FALSE) {
          if (AccelerometerType == "WR") {
            df_kin = data.frame(enmo=enmo,
                                pitch = D$Euler_9DOF_Pitch_WR_CAL,
                                roll = D$Euler_9DOF_Roll_WR_CAL,
                                yaw = D$Euler_9DOF_Yaw_WR_CAL,enmopeak = enmopeak,
                                time=D$timestamp,numerictime=FiveSecIndex)
          } else {
            df_kin = data.frame(enmo=enmo,
                                pitch = D$Euler_9DOF_Pitch_LN_CAL,
                                roll = D$Euler_9DOF_Roll_LN_CAL,
                                yaw = D$Euler_9DOF_Yaw_LN_CAL,enmopeak = enmopeak,
                                time=D$timestamp,numerictime=FiveSecIndex)
          }
        } else { # variables needed for accelerometer calibration assessment
          if (AccelerometerType == "WR") {
            df_kin = data.frame(Accel_X = D$Accel_WR_X_CAL, Accel_Y = D$Accel_WR_Y_CAL, Accel_Z = D$Accel_WR_Z_CAL,
                                time=D$timestamp,numerictime=FiveSecIndex)
          } else {
            df_kin = data.frame(Accel_X = D$Accel_LN_X_CAL, Accel_Y = D$Accel_LN_Y_CAL, Accel_Z = D$Accel_LN_Z_CAL,
                                time=D$timestamp,numerictime=FiveSecIndex)
          }
        }
        if ("GSR_Skin_Conductance_CAL" %in% varname & do.call ==  FALSE) {
          do.skinsensors = TRUE  
        } else {
          do.skinsensors = FALSE
        }
        if (do.skinsensors == TRUE) {
          df_skin = data.frame(GSR_Skin_Conductance = D$GSR_Skin_Conductance_CAL,
                               GSR_Skin_Resistance = D$GSR_Skin_Resistance_CAL,
                               time=D$timestamp, numerictime=FiveSecIndex)
          # ignore first half epoch to only get full epochs
          firstfullepoch = unique(df_skin$numerictime)[2]
          df_skin = df_skin[which(df_skin$numerictime >= firstfullepoch),]
          # aggregate
          Oskin = aggregate(x = df_skin,by = list(df_skin$numerictime),mean)
        }
        # ignore first half epoch to only get full epochs
        firstfullepoch = unique(df_kin$numerictime)[2]
        df_kin = df_kin[which(df_kin$numerictime >= firstfullepoch),]
        # aggregate
        Omean = aggregate(x = df_kin,by = list(df_kin$numerictime),mean)
        if (do.call == FALSE) {
          Ostd = aggregate(x = df_kin[,c("enmo","numerictime")],by = list(df_kin$numerictime),sd)
        } else {
          Ostd = aggregate(x = df_kin[,c("Accel_X","Accel_Y","Accel_Z","numerictime")],by = list(df_kin$numerictime),sd)
        }
    
        if (do.call ==  FALSE) { # additional aggregations if calibration is not done
          O50 = aggregate(x = df_kin[,c("enmopeak","numerictime","time")], by = list(df_kin$numerictime),p50)
          O75 = aggregate(x = df_kin[,c("enmopeak","numerictime")], by = list(df_kin$numerictime),p75)
          O99 = aggregate(x = df_kin[,c("enmopeak","numerictime")], by = list(df_kin$numerictime),p99)
          Omax = aggregate(x = df_kin[,c("enmopeak","numerictime")], by = list(df_kin$numerictime),max)
          # update variable names
          Omean = addvarEnmo(x = Omean, varname="enmo_mean")
          Ostd = addvarEnmo(x = Ostd, varname="enmo_std")
          # Now put all relevant aggregated variables in a dataframe:
          agData = data.frame(time=O50$time,
                              secondsinrecording=O50$numerictime-O50$numerictime[1],
                              numerictime=O50$numerictime,
                              acc_percentile50=O50$enmopeak,
                              acc_percentile75=O75$enmopeak,
                              acc_percentile99=O99$enmopeak,
                              acc_max=Omax$enmopeak,
                              acc_mean=Omean$enmo_mean,
                              acc_std=Ostd$enmo_std,
                              pitch_mean = Omean$pitch,
                              roll_mean = Omean$roll,
                              yaw_mean = Omean$yaw)
          # identify peaks in the magnitude of acceleration (enmo)
          agData$acc_peak = 0
          # peaks are defined here as:
          # (maximum values in an epoch at least 200% the 99th percentile of that epoch
          # AND with a value above 2 m/s2) OR with max acceleration above 10
          peakindex = which(((agData$acc_max-agData$acc_percentile50) > 
                               ((agData$acc_percentile99-agData$acc_percentile50)*2) & agData$acc_max > 2) |
                              agData$acc_max > 5)
          agData$acc_peak[peakindex] = 1 
        } else { # when investigating autocalibration the output variables are simpler.
          agData = data.frame(time=Omean$time,
                              numerictime=Omean$numerictime,
                              Accel_X_mean = Omean$Accel_X,
                              Accel_Y_mean = Omean$Accel_Y,
                              Accel_Z_mean = Omean$Accel_Z,
                              Accel_X_std = Ostd$Accel_X,
                              Accel_Y_std = Ostd$Accel_Y,                              
                              Accel_Z_std = Ostd$Accel_Z,
                              EN = sqrt(Omean$Accel_X^2 + Omean$Accel_Y^2 + Omean$Accel_Z^2))
        }
        if (do.skinsensors == TRUE & do.call == FALSE) {
          agData$GSR_Skin_Conductance_mean = Oskin$GSR_Skin_Conductance 
          agData$GSR_Skin_Resistance_mean = Oskin$GSR_Skin_Resistance
        }
        # plot low resolution data on screen:
        if (do.plot == TRUE & do.call == FALSE) {
          x11()
          plot(agData$time,agData$acc_mean,type="l",ylim=c(0,max(agData$acc_max)),main="acc",
               col="blue",xlab="timestamp",ylab="acceleration (m/s2)")
          lines(agData$time,agData$acc_max,type="l",col="green")
          lines(agData$time[peakindex],agData$acc_max[peakindex],col="red",type="p",pch=20,cex=0.5)
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
  if (length(sfstore) > 0) {
    sfmean = round(mean(sfstore))
  } else {
    sfmean = NA
  }
  if (do.call == FALSE) {
    write.csv(data2store,file = paste0(outputfolder,"/shimmer_",
                                       bodyside,"_",fnameshort_withoutext,"_sf",sfmean,"_sernum",sn,
                                       "_acc",AccelerometerType,".csv"),row.names = FALSE)
  } else {
    spheredata = which(data2store$Accel_X_std < 0.128 & # note threshold in m/s2
                         data2store$Accel_X_std < 0.128 &
                         data2store$Accel_X_std < 0.128)
    cat(paste0("\nLength spheredata: ",length(spheredata)))
    if (length(spheredata) > 1) {
      data2store = data2store[spheredata,]
      write.csv(data2store,file = paste0(outputfolder,"/shim_calibrationcheck_",
                                         fnameshort_withoutext,"_sernum",sn,".csv"),row.names = FALSE)
      
      CalibrationError_ms2 = mean(abs(data2store$EN - 9.81))
      CalibrationError_mg = CalibrationError_ms2 / 9.81
      cat(paste0("\nCalibration error in g: ",CalibrationError_mg))
    }
  }
  timer1 = Sys.time()
  cat("\nTime elapsed:")
  print(timer1-timer0)
}