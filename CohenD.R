##BiocManager::install('lsr') ##Install package if your device did not have it
library(lsr) #required for CohenD execution
Control <- read.delim("/Users/littlevanilla/Downloads/interpolate channels excluded/CV_excluded_interpolate/CV_stroop.txt", sep="\t", header = TRUE)
Stress <- read.delim("/Users/littlevanilla/Downloads/interpolate channels excluded/SV_excluded_interpolate/SV_stroop.txt", sep="\t", header = TRUE)
#Control_amuse[is.na(Control_amuse)]=0
MD <- read.csv("MD_Stroop.csv")

CAlpha <- Control[Control$Freq_int_name=='alpha/all',] #Dissect the file into different EEG-band
SAlpha <- Stress[Stress$Freq_int_name=='alpha/all',]
CohenAlpha<-c()
for (i in (5:23)){ #runloop to calculate mean value of EEG band per channel
  s<-na.omit(SAlpha[,i])
  c<-na.omit(CAlpha[,i])
  t<-cohensD(s,c)
  if (!is.na(MD[i, 2]) && MD[i, 2] < 0) {
    t <- t * (-1)  # Update t if condition is met #CohenD function does not return directional values
  }
  CohenAlpha<-c(CohenAlpha,t)
}
CBeta <- Control[Control$Freq_int_name=='beta/all',] #Dissect the file into different EEG-band
SBeta <- Stress[Stress$Freq_int_name=='beta/all',]
CohenBeta<-c()
for (i in (5:23)){ #runloop to calculate mean value of EEG band per channel
  s<-na.omit(SBeta[,i])
  c<-na.omit(CBeta[,i])
  t<-cohensD(s,c)
  if (!is.na(MD[i, 3]) && MD[i, 3] < 0) {
    t <- t * (-1)  # Update t if condition is met
  }
  CohenBeta<-c(CohenBeta,t)
}
CLBeta <- Control[Control$Freq_int_name=='low-beta/all',] #Dissect the file into different EEG-band
SLBeta <- Stress[Stress$Freq_int_name=='low-beta/all',]
CohenLBeta<-c()
for (i in (5:23)){ #runloop to calculate mean value of EEG band per channel
  s<-na.omit(SLBeta[,i])
  c<-na.omit(CLBeta[,i])
  t<-cohensD(s,c)
  if (!is.na(MD[i, 4]) && MD[i, 4] < 0) {
    t <- t * (-1)  # Update t if condition is met
  }
  CohenLBeta<-c(CohenLBeta,t)
}
CHBeta <- Control[Control$Freq_int_name=='high-beta/all',] #Dissect the file into different EEG-band
SHBeta <- Stress[Stress$Freq_int_name=='high-beta/all',]
CohenHBeta<-c()
for (i in (5:23)){ #runloop to calculate mean value of EEG band per channel
  s<-na.omit(SHBeta[,i])
  c<-na.omit(CHBeta[,i])
  t<-cohensD(s,c)
  if (!is.na(MD[i, 5]) && MD[i, 5] < 0) {
    t <- t * (-1)  # Update t if condition is met
  }
  CohenHBeta<-c(CohenHBeta,t)
}
CGamma <- Control[Control$Freq_int_name=='gamma/all',] #Dissect the file into different EEG-band
SGamma <- Stress[Stress$Freq_int_name=='gamma/all',]
CohenGamma<-c()
for (i in (5:23)){ #runloop to calculate mean value of EEG band per channel
  s<-na.omit(SGamma[,i])
  c<-na.omit(CGamma[,i])
  t<-cohensD(s,c)
  if (!is.na(MD[i, 6]) && MD[i, 6] < 0) {
    t <- t * (-1)  # Update t if condition is met
  }
  CohenGamma<-c(CohenGamma,t)
}
CTheta <- Control[Control$Freq_int_name=='theta/all',] #Dissect the file into different EEG-band
STheta <- Stress[Stress$Freq_int_name=='theta/all',]
CohenTheta<-c()
for (i in (5:23)){ #runloop to calculate mean value of EEG band per channel
  s<-na.omit(STheta[,i])
  c<-na.omit(CTheta[,i])
  t<-cohensD(s,c)
  if (!is.na(MD[i, 7]) && MD[i, 7] < 0) {
    t <- t * (-1)  # Update t if condition is met
  }
  CohenTheta<-c(CohenTheta,t)
}
CDelta <- Control[Control$Freq_int_name=='delta/all',] #Dissect the file into different EEG-band
SDelta <- Stress[Stress$Freq_int_name=='delta/all',]
CohenDelta<-c()
for (i in (5:23)){ #runloop to calculate mean value of EEG band per channel
  s<-na.omit(SDelta[,i])
  c<-na.omit(CDelta[,i])
  t<-cohensD(s,c)
  if (!is.na(MD[i, 7]) && MD[i, 7] < 0) {
    t <- t * (-1)  # Update t if condition is met
  }
  CohenDelta<-c(CohenDelta,t)
}
##Please note that CohenD returns values as absolute in this package##
Channel <- MD[,1]
Cohen<-cbind(Channel,CohenAlpha,CohenBeta,CohenLBeta,CohenHBeta,CohenGamma,CohenTheta, CohenDelta)
colnames(Cohen)<-c('Channel','Alpha','Beta','Low-beta','High-beta','Gamma','Theta','Delta')
write.csv(Cohen,'CohenD_Stroop.csv',row.names = FALSE)
