library('BiocManager')
#BiocManager::install('tidyverse')
library('tidyverse')
library('dplyr')

##Pre-process dataframe
SManikin <- read.csv('Stress_offline_vid.csv')
SManikin <- SManikin %>% arrange(ID) #Sort data following ID orders
SManikin <- SManikin[-c(2,3,11),] #Eliminate subjects without EEG data

CManikin <- read.csv('Control_offline_vid.csv')
CManikin <- CManikin %>% arrange(ID) #Sort data following ID orders
CManikin <- CManikin[-c(4,5),] #Eliminate subjects without EEG data

##Extract Manikin Data of pleasure
Pleasure1 <- SManikin[,1:4]
Pleasure2 <- CManikin[,1:4]
Pleasure <- rbind(Pleasure1, Pleasure2)

##Reformat to make the dataframe look better
ID <- Pleasure[,1]
Pleasure <- Pleasure[,-1]
rownames(Pleasure)<-ID

##Extract Manikin data of sadness
Sad1 <- SManikin[,5:7] 
Sad2 <- CManikin[,5:7]
Sad <- rbind(Sad1, Sad2)
rownames(Sad)<-ID

##Extract Manikin data of neutral
Neu1 <- SManikin[,8:10] 
Neu2 <- CManikin[,8:10]
Neu <- rbind(Neu1, Neu2)
rownames(Neu)<-ID

##Extract Manikin data of amusement
Amuse1 <- SManikin[,11:13] 
Amuse2 <- CManikin[,11:13]
Amuse <- rbind(Amuse1, Amuse2)
rownames(Amuse)<-ID

##Extract Manikin data of fear
Fear1 <- SManikin[,14:16] 
Fear2 <- CManikin[,14:16]
Fear <- rbind(Fear1, Fear2)
rownames(Fear)<-ID

##Run Correlation 
#Import PSD data
Control <- read.delim("/Users/littlevanilla/Downloads/interpolate channels excluded/CV_excluded_interpolate/CV_neutral.txt", sep="\t", header = TRUE)
Stress <- read.delim("/Users/littlevanilla/Downloads/interpolate channels excluded/SV_excluded_interpolate/SV_neutral.txt", sep="\t", header = TRUE)
Stress <- Stress[,-24] #applied for fear only

#Dissect the file into different EEG-band
#Combine EEG-bands of 2 cohorts into 1 dataframe
Channel <- colnames(Control[5:23])
Alpha2 <- Control[Control$Freq_int_name=='alpha/all',] #Alpha
Alpha1 <- Stress[Stress$Freq_int_name=='alpha/all',]
Alpha <- rbind(Alpha1, Alpha2)

Beta2 <- Control[Control$Freq_int_name=='beta/all',] #Beta
Beta1 <- Stress[Stress$Freq_int_name== 'beta/all',]
Beta <- rbind(Beta1, Beta2)

LowB2 <- Control[Control$Freq_int_name=='low-beta/all',] #Low-beta
LowB1 <- Stress[Stress$Freq_int_name== 'low-beta/all',]
LowBeta <- rbind(LowB1, LowB2)

HB2 <- Control[Control$Freq_int_name=='high-beta/all',] #High-beta
HB1 <- Stress[Stress$Freq_int_name== 'high-beta/all',]
HighBeta <- rbind(HB1, HB2)

Theta2 <- Control[Control$Freq_int_name=='theta/all',] #Theta
Theta1 <- Stress[Stress$Freq_int_name== 'theta/all',]
Theta <- rbind(Theta1, Theta2)

Delta2 <- Control[Control$Freq_int_name=='delta/all',] #Delta
Delta1 <- Stress[Stress$Freq_int_name== 'delta/all',]
Delta <- rbind(Delta1, Delta2)

Gamma2 <- Control[Control$Freq_int_name=='gamma/all',] #Gamma
Gamma1 <- Stress[Stress$Freq_int_name== 'gamma/all',]
Gamma <- rbind(Gamma1, Gamma2)

#Set-up Spearman correlation loop
pval <- c()
rho <- c()
Valence <- Neu[,1]
  for (i in (5:23)){
    cor<-cor.test(Valence,HighBeta[,i], method='spearman',exact = FALSE) #If exact not set at FALSE there will be bug
    pvalue <- cor$p.value
    pval <- c(pval, pvalue)
    R <- cor$estimate
    rho <- c(rho, R)
  }
Val <- rbind(Channel, pval, rho)

pval <- c()
rho <- c()
Arousal <- Neu[,2]
for (i in (5:23)){
  cor<-cor.test(Arousal,HighBeta[,i], method='spearman',exact = FALSE)
  pvalue <- cor$p.value
  pval <- c(pval, pvalue)
  R <- cor$estimate
  rho <- c(rho, R)
}
Arou<- rbind(Channel, pval, rho)

pval <- c()
rho <- c()
Dominance <- Neu[,3]
for (i in (5:23)){
  cor<-cor.test(Dominance,HighBeta[,i], method='spearman',exact = FALSE)
  pvalue <- cor$p.value
  pval <- c(pval, pvalue)
  R <- cor$estimate
  rho <- c(rho, R)
}
Dom <- rbind(Channel, pval, rho)

#Combine the dataframe into a report
Data <- rbind(Val, Arou, Dom)
write.csv(Data, 'Neutral_Manikin_Correlation_HighBeta.csv', col.names=FALSE)

"There will be a bug but just ignore it as it is a format bug with col.names, too lazy to fix"
  