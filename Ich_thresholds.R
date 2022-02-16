#exersise with function
library(RColorBrewer) #for custom colors
#library(pheatmap) #for pheatmap
#library(vegan) #for Shannon and Simpson
#library(reldist) #for gini index 
#library(OTUtable) #for pielou 
library(textshape) #for flatten
library(reshape2) #for melt function
library(ggplot2) #for ggplot type heatmap

make_series<-function(start_n, end_n, step_n, series, thresh_Ich){
Ich_series=c()
for (clones in seq(start_n,end_n, step_n)){
  Ich=c()
  for (i in 1:series){
    values=rbeta(clones,i,series+1-i)
    fractions<-values/sum(values)
    selected<-fractions[fractions>=thresh_Ich]
    Ich=append(Ich,sum(selected))
  }
  Ich_series=cbind(Ich_series,Ich)
}
colnames(Ich_series)<-seq(start_n, end_n, step_n) #make proper col names
return(Ich_series)
}
#define color palette for the heatmap
coul <- colorRampPalette(brewer.pal(8, "YlGnBu"))(20)
#generate data for the heatmap
#first a standard threshold used throuout the manuscript
Ich_series<-make_series(50,500,25,20, 0.005)
melted<-melt(Ich_series) #or any other series down the script
colnames(melted)<-c("Series","Clones","Values")
#note: theme_void() controls appearance of the scale values
ggplot(melted, aes(Series, Clones, fill=Values))+
  geom_tile()+
  labs(title="ICh", x="Series", y="Clones")+ 
  scale_fill_viridis_c(alpha=0.9)#+
 # theme_void()
#we can also decrease the threshold and see how heatmap changes
Ich_series<-make_series(50,500,25,20, 0.0001)
melted<-melt(Ich_series) #or any other series down the script
colnames(melted)<-c("Series","Clones","Values")
ggplot(melted, aes(Series, Clones, fill=Values))+
  geom_tile()+
  labs(title="low threshold", x="Series", y="Clones")+ 
  scale_fill_viridis_c(alpha=0.9)#+
#and we can increase the threshold
Ich_series<-make_series(50,500,25,20, 0.01)
melted<-melt(Ich_series) #or any other series down the script
colnames(melted)<-c("Series","Clones","Values")
ggplot(melted, aes(Series, Clones, fill=Values))+
  geom_tile()+
  labs(title="High threshold", x="Series", y="Clones")+ 
  scale_fill_viridis_c(alpha=0.9)#+
#as you can see threshold influences the end result of the Ich value,
#which tends to the binary responce at high index of the series (low skewing)