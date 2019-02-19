# Data exploration Shimmer device - 4 February 2019
rm(list=ls())
graphics.off()

library(signal)
library(zoo)
library(data.table)

get_enmo =  function(x,y,z) {
  enmo = pmax(0,sqrt(x^2 + y^2 + z^2) - 9.81)
}
get_angles = function(x,y,z,sf) {
  winsi = round(sf*5)
  if (round(winsi/2) == (winsi/2)) winsi = winsi+1
  Gxm = zoo::rollmedian(x,k=winsi,na.pad=TRUE)
  Gym = zoo::rollmedian(y,k=winsi,na.pad=TRUE)
  Gzm = zoo::rollmedian(z,k=winsi,na.pad=TRUE)
  Gxm[which(is.na(Gxm[1:1000]) ==T)] = Gxm[which(is.na(Gxm[1:1000]) ==F)[1]]
  Gym[which(is.na(Gym[1:1000]) ==T)] = Gym[which(is.na(Gym[1:1000]) ==F)[1]]
  Gzm[which(is.na(Gzm[1:1000]) ==T)] = Gzm[which(is.na(Gzm[1:1000]) ==F)[1]]
  p1 = which(is.na(Gxm) ==F); Gxm[which(is.na(Gxm) ==T)] = Gxm[p1[length(p1)]]
  p1 = which(is.na(Gym) ==F); Gym[which(is.na(Gym) ==T)] = Gym[p1[length(p1)]]
  p1 = which(is.na(Gzm) ==F); Gzm[which(is.na(Gzm) ==T)] = Gzm[p1[length(p1)]]
  anglex = (atan(Gxm / (sqrt(Gym^2 + Gzm^2)))) / (pi/180)
  angley = (atan(Gym / (sqrt(Gxm^2 + Gzm^2)))) / (pi/180)
  anglez = (atan(Gzm / (sqrt(Gxm^2 + Gym^2)))) / (pi/180)
  Gangles = cbind(anglex,angley,anglez)
  return(Gangles)
}
# averageperepoch = function(x,sf,epochsize) {
#   x2 =cumsum(c(0,x))
#   select = seq(1,length(x2),by=sf*epochsize)
#   x3 = diff(x2[round(select)]) / abs(diff(round(select)))
# }

addvarEnmo = function(x,varname = "") {
  x[varname] = x$enmo
  x = x[,-which(colnames(x) == "enmo")]
  return(x)
}
#-------------------------------------------
# main code:

# specify data location
path = "/media/sf_sharedfolder/Emotion"
datapath = paste0(path,"/accelerometer_data/2018-05-07_13.24.29_Ses9_070518_SD_Session1")
fnames = dir(datapath)
blocksize = 500000
sf = 500

for (fi in 1:length(fnames)) {
  fname = paste0(datapath,"/",fnames[fi])
  varname_tmp = read.csv(file = fname,nrows = 1, skip=1,sep = "\t",
                         header = FALSE,stringsAsFactors = FALSE)
  unit = read.csv(file = fname,nrows = 1, skip=2,sep = "\t",
                  header = FALSE,stringsAsFactors = FALSE)
  varname_tmp = as.character(varname_tmp)
  currentblock = 3
  for (block in 1) {
    D = data.table::fread(file = fname,nrows = blocksize, skip=currentblock,sep = "\t",
                          header = FALSE,stringsAsFactors = FALSE)
    D = as.data.frame(D)
    currentblock = currentblock + blocksize + 1 
    # make sure that columnnames are correct
    colnames(D) = varname_tmp
    # add timestamp
    D$timestamp = as.POSIXlt(D[,1]/1000,origin="1970-1-1",tz="Europe/Amsterdam")
    varname_tmp = c(varname_tmp,"timestamp")
    # sort
    D = D[,sort(colnames(D))]
    varname = sort(varname_tmp)
    id = unlist(strsplit(varname[3],"_"))[1] #ID is in all the variable names
    #remove all idifiers from varnames, because to make code below more generic
    for (jj in 1:length(varname)) {
      tmpA = unlist(strsplit(varname[jj],"_partA_"))
      tmpB = unlist(strsplit(varname[jj],"_partB_"))
      if (length(tmpA) > 1) {
        varname[jj] = tmpA[2]
      } else if (length(tmpB) > 1) {
          varname[jj] = tmpB[2]
      }
    }
    colnames(D) = varname
    enmo = get_enmo(x=D$Accel_WR_X_CAL,y=D$Accel_WR_Y_CAL,z=D$Accel_WR_Z_CAL)
    # Remove low frequency component, probably related to imperfect calibration
    lb = 0.2
    n = 4
    bf = signal::butter(n,c(lb/(sf/2)),type=c("high")) #creating filter coefficients
    enmo = signal::filter(bf,enmo)
    # ignore direction of acceleration, now signal is high pass filtered
    enmo = abs(enmo)
    epochsize = 1 # epoch size in seconds
    
    Gangles = get_angles(x=D$Accel_WR_X_CAL,y=D$Accel_WR_Y_CAL,z=D$Accel_WR_Z_CAL,sf=sf)
    # downsample
    FiveSecIndex = round(round(as.numeric(D$timestamp)/epochsize)*epochsize)
    df_kin = data.frame(enmo=enmo,
                        # anglex=Gangles[,1],angley=Gangles[,2],anglez=Gangles[,3], #not using angles at the moment
                        time=D$timestamp,index=FiveSecIndex)
    
    p50 = function(x) return(quantile(x,probs = 0.50))
    p75 = function(x) return(quantile(x,probs = 0.75))
    p99 = function(x) return(quantile(x,probs = 0.99))
    
    O50 = aggregate(x = df_kin,by = list(df_kin$index),p50)
    O75 = aggregate(x = df_kin,by = list(df_kin$index),p75)
    O99 = aggregate(x = df_kin,by = list(df_kin$index),p99)
    Omean = aggregate(x = df_kin,by = list(df_kin$index),mean)
    Omax = aggregate(x = df_kin,by = list(df_kin$index),max)
    
 
    
    O50 = addvarEnmo(x = O50, varname="enmo_p50")
    O75 = addvarEnmo(x = O75, varname="enmo_p75")
    O99 = addvarEnmo(x = O99, varname="enmo_p99")
    Omean = addvarEnmo(x = Omean, varname="enmo_mean")
    Omax = addvarEnmo(x = Omax, varname="enmo_max")

    agData = data.frame(time=O50$time,
                      index=O50$index,
                      p50=O50$enmo_p50,
                      p75=O75$enmo_p75,
                      p99=O99$enmo_p99,
                      mean=Omean$enmo_mean,
                      max=Omax$enmo_max)
    agData$peak = 0
    # peaks are defined as:
    # maximum values in an epoch at least 200% the 99th percentile of that epoch
    # AND with a value above 2
    # OR with max acceleration above 10
    peakindex = which(((agData$max-agData$p50) > ((agData$p99-agData$p50)*2) & agData$max > 2) |
                        agData$max > 10)
                        
    agData$peak[peakindex] = 1 
    # plot low resolution data on screen:
    x11()
    plot(agData$time,agData$mean,type="l",ylim=c(0,max(agData$max)),col="blue")
    lines(agData$time,agData$max,type="l",col="green")
    lines(agData$time[peakindex],agData$max[peakindex],col="red",type="p",pch=20,cex=0.5)
    legend("topleft",legend = c("mean","max"),col=c("blue","green"),lty=c(1,1))
    
    # # plot high resolution data:
    # mainname = "Accel_WR"
    # print(mainname)
    # pdf(file=paste0(path,"/plots/accplot_Accel_WR_",block,"_file_",id,".pdf"))
    # plot(df_kin$time,df_kin$enmo,type="l",col="black",lwd=1,main=mainname,xlab="time",ylab="ENMO")
    # if(length(peakindex) > 0) lines(df_kin$time[peakindex*epochsize*sf],df_kin$enmo[peakindex*epochsize*sf],col="red",type="p",pch=20)
    # dev.off()
  }
}