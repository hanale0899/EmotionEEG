BiocManager::install('tidyr')
BiocManager::install('dyplr')
library(tidyr)
library(dplyr)
df <- read.delim("/Users/littlevanilla/Downloads/interpolate channels excluded/SV_excluded_interpolate/SV_stroop.txt", sep="\t", header = TRUE)
#Control_amuse[is.na(Control_amuse)]=0

Channel <- colnames(df[5:23])
Alpha <- df[df$Freq_int_name=='alpha/all',] #Dissect the file into different EEG-band
CAAm <- c()
for (i in (5:23)){ #runloop to calculate mean value of EEG band per channel
  t<-na.omit(Alpha[,i])
  m <- mean(t)
 CAAm <- c(CAAm, m)
}
Beta <- df[df$Freq_int_name=='beta/all',]
CABm <- c()
for (i in (5:23)){
  t<-na.omit(Beta[,i])
  m <- mean(t)
  CABm <- c(CABm, m)
}
Lowbeta <- df[df$Freq_int_name=='low-beta/all',]
CALm <- c()
for (i in (5:23)){
  t<-na.omit(Lowbeta[,i])
  m <- mean(t)
  CALm <- c(CALm, m)
}
Highbeta <- df[df$Freq_int_name=='high-beta/all',]
CAHm <- c()
for (i in (5:23)){
  t<-na.omit(Highbeta[,i])
  m <- mean(t)
  CAHm <- c(CAHm, m)
}
Gamma <- df[df$Freq_int_name=='gamma/all',]
CAGm <- c()
for (i in (5:23)){
  t<-na.omit(Gamma[,i])
  m <- mean(t)
  CAGm <- c(CAGm, m)
}
Theta <- df[df$Freq_int_name=='theta/all',]
CATm <- c()
for (i in (5:23)){
  t<-na.omit(Theta[,i])
  m <- mean(t)
  CATm <- c(CATm, m)
}
Delta <- df[df$Freq_int_name=='delta/all',]
CADm <- c()
for (i in (5:23)){
  t<-na.omit(Delta[,i])
  m <- mean(t)
  CADm <- c(CADm, m)
}
MCA<-cbind(Channel,CAAm,CABm, CALm, CAHm, CAGm, CATm, CADm) #combine results into a summary file
colnames(MCA)<-c('Channel','Alpha','Beta','Low-beta','High-beta','Gamma','Theta', 'Delta')
write.csv(MCA,'StressStroop_Mean.csv',row.names=FALSE)

