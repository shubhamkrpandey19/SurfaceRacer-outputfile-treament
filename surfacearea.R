library(readr)
library(dplyr)
library(openxlsx)
library(readr)
library ("berryFunctions")
library(readr)
folder='C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Desktop\\spikeprotein\\surface_racer_5.0\\asn_exposure_5.0'
for (j in 1:6) {test1 <- read_table2(paste0("C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Desktop\\spikeprotein\\surface_racer_5.0\\asn_exposure_5.0\\test",j,'.txt'), 
                     col_names = FALSE)
#View(test1)
a=which( test1$X3=='C'|test1$X3=='CA'|test1$X3=='N')
if(length(a)!=0){
test1=test1[-a,]}
b=which( test1$X4=='LIG'|test1$X4=='UNK'|test1$X1=='CAVITY')
if(length(b)!=0){
test1=test1[-b,]}
par=unique(test1$X4)
for (k in c(20,50,100)) {
data3=NULL
for (i in par) {
  data=test1[test1$X4==i,]
  data1=data%>%group_by(paste0(X5,X6))%>%summarise(sum1=sum(X11))
  data2=data.frame(Residures=i,data1)
  colnames(data2)=c('Residue','Chain and number of residue','Surface_area')
data3=rbind(data3,data2)
}
data3$Count=NA

sorted=data3[data3$Surface_area>=k,]
sorted1=sorted%>%group_by(Residue)%>%summarise(Count=n())
x1=data.frame(Residue=sorted1$Residue,x=NA,Surface_area=NA,Count=sorted1$Count)
colnames(x1)=colnames(data3)
data3[nrow(data3)+1,] <- NA
data5=rbind(data3,x1)
#data4=data3%>%group_by(Residue)%>%summarise(SA=sum(Surface_area))
#data4=data.frame(data4,Ex=NA)
#colnames(data4)=colnames(data3)
#data5=rbind(data3,data4)
library(ggplot2)
ggplot(x1,aes(x=Residue,y=factor(Count)))+geom_point(color='red')+ylab('Count')
ggsave(paste0('C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Desktop\\spikeprotein\\surface_racer_5.0\\asn_exposure_5.0\\','test_',j,'_',k,'.jpeg'),width = 4,height = 2.5,units = 'in',dpi = 320)
write.xlsx(data5,paste0('C:\\Users\\skpandey1\\OneDrive - The University of Alabama\\Desktop\\spikeprotein\\surface_racer_5.0\\asn_exposure_5.0\\','test_',j,'_',k,'.xlsx'), overwrite = TRUE)

}
}