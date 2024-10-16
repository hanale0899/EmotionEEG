library(dplyr)
#data <- list.files(path = '.', pattern = ".txt$", full.names = TRUE)

##set up path for saving files
mypath<-"C:/Users/hoang/OneDrive - VietNam National University - HCM INTERNATIONAL UNIVERSITY/Documents/Tài liệu RA_513/EMOTION/analyzedata"

##Load psd files
ControlArith <- read.delim("Pic_control_arithmatic_2023-02-27_221831.txt")
StressArith <- read.delim("Pic_stress_arithmatic_2023-02-27_222107.txt")

##Eliminate EOG channels
ControlArith<- ControlArith %>% select(-contains("EOG")) 
StressArith<- StressArith %>% select(-contains("EOG"))

##Create new dataframes specific for each EEG band
#Control variables
CAalpha <- ControlArith[ControlArith$Freq_int_name == "alpha/all",]
CAlowbeta <- ControlArith[ControlArith$Freq_int_name == "low-beta/all",]
CAhighbeta <- ControlArith[ControlArith$Freq_int_name == "high-beta/all",]
CAbeta <- ControlArith[ControlArith$Freq_int_name == "beta/all",]
CAgamma <- ControlArith[ControlArith$Freq_int_name == "gamma/all",]
CAtheta <- ControlArith[ControlArith$Freq_int_name == "theta/all",]
CAdelta <- ControlArith[ControlArith$Freq_int_name == "delta/all",]
#Stressed variables
SAalpha <- StressArith[StressArith$Freq_int_name == "alpha/all",]
SAlowbeta <- StressArith[ControlArith$Freq_int_name == "low-beta/all",]
SAhighbeta <- StressArith[ControlArith$Freq_int_name == "high-beta/all",]
SAbeta <- StressArith[StressArith$Freq_int_name == "beta/all",]
SAgamma <- StressArith[ControlArith$Freq_int_name == "gamma/all",]
SAtheta <- StressArith[ControlArith$Freq_int_name == "theta/all",]
SAdelta <- StressArith[ControlArith$Freq_int_name == "delta/all",]

##modifydata, eliminate unnecessary information_adapt
CAalphaPlus = select(CAbeta, -1,-2,-3,-4)
rownames(CAalphaPlus)<-NULL
SAalphaPlus = select(SAbeta, -1,-2,-3,-4)
rownames(SAalphaPlus)<-NULL

##Test distribution:Normali Distribution is validated for t-test
#library(rstatix)
#features <- c()
#pvals <- c()

#for(i in 1:21){
#  phtk1 <- shapiro.test(as.numeric(unlist(getDataPart(CAalphaPlus[i])))) 
#  phtk2 <- shapiro.test(as.numeric(unlist(getDataPart(SAalphaPlus[i])))) 
#  print(column)
#  print(phtk1)
#  print(phtk2)
#  pvals <- c(pvals, phtk1$p.value,phtk2$p.value)
#}

#p_values_tukey <- data.frame(features, pvalues)

##loopofanalysis
#Create empty lists
M<-c()
Names<-colnames(CAalphaPlus)
pvalues<-c()
#Run loop t-test
for (i in 1:21)
{
  K<-as.numeric(unlist(getDataPart(CAalphaPlus[i])))
  P<-as.numeric(unlist(getDataPart(SAalphaPlus[i])))
  md<-mean(K,na.rm=TRUE)-mean(P,na.rm=TRUE)
  get_pval <- t.test(K, P, paired=FALSE, var.equal=FALSE)$p.value
  M<-c(M,md)
  pvalues<-c(pvalues,get_pval)
}
#createdataframe with t-test results
df<-data.frame(Names,M,pvalues)
#creatfile_adapt
write.table(df,paste0(mypath,"/","AB",".txt"),sep='\t',quote=F,row.names=FALSE)


##Adapt means change the name accordingly to the band you wish to analyze