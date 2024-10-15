Control <- read.csv('/Users/littlevanilla/Downloads/interpolate channels excluded/CV_excluded_interpolate/ControlPleasure_mean.csv')
Stress <- read.csv('/Users/littlevanilla/Downloads/interpolate channels excluded/SV_excluded_interpolate/StressPleasure_mean.csv')

md <- c() #create_empty_list
for (i in (2:8)){
  dif<-as.data.frame(Stress[,i]-Control[,i])
  md<-c(md,dif)
}
mdadj <-  as.data.frame(do.call(cbind, md)) #Convert_nested_lists_into_dataframe_with_each var_match_a_list_vertically
colnames(mdadj)<-c('Alpha','Beta','Low-beta','High-beta','Gamma','Theta','Delta')
mdadj<-cbind(Channel, mdadj)
write.csv(mdadj,'MD_Pleasure.csv', row.names = FALSE)