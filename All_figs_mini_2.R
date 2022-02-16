# 2021 12 14 R script for manuscript on CH - Frontiers in Medicine
# Written by L. Bystrykh, modified by M. Belderbos
# Run script in base R, as some functions don't work in R Studio
#examples surface 3D in https://rviews.rstudio.com/2020/12/14/plotting-surfaces-with-r/

####Load the required packages----
library(ggplot2)
library(dplyr)
#library(plotly) #for interactive 3D
library(plot3D) #for mesh and static 3D
library(RColorBrewer) #for custom colors
#library(pheatmap) #for pheatmap
library(vegan) #for Shannon and Simpson
library(reldist) #for gini index 
library(OTUtable) #for pielou 
library(textshape) #for flatten
#library(googlesheets4)
library(reshape) #for melt

#### beta distribution probability density plot data for 3D----
#This is the version of showing beta distribution series as a surface plot
#for this we need a meshgrid
M<-mesh(seq(1,20,length.out=20), #series ID
        seq(0,1,length.out=20)) #beta distribution values
y <-M$y
x <-M$x
z=c()
for (i in 1:20){
  z=rbind(z,dbeta(seq(0,1,length.out=20),i,21-i))
}

####Figure 1A  as static 3D ----
surf3D(x, y, z, 
       colvar = z, 
       colkey = list(side = 4,plot = TRUE,length = 0.5,width =
                                            0.8,dist = 0.01, shift = 0, addlines = FALSE, col.clab = NULL, cex.clab = par("cex.lab"), side.clab
                                          = NULL, line.clab = NULL, adj.clab = NULL, font.clab = NULL), 
     	 shade = 0.5,
       box = TRUE, bty = "b2", phi = 20, theta = 150, 
       col = gg2.col(100), border = "black", facets = TRUE, clab = NULL,  
       xlab = "Sizes", ylab = "Series", zlab = "Probability density",
       drawlabels = TRUE)

####Fig1B----
#make series for variable clone numbers and alpha and beta, 
Ich_series=c()
for (clones in seq(50,500, 25)){
Ich=c()
for (i in 1:20){
  values=rbeta(clones,i,21-i)
  fractions<-values/sum(values)
  selected<-fractions[fractions>=0.005]
  Ich=append(Ich,sum(selected))
}
Ich_series=cbind(Ich_series,Ich)
}
#rename column names, it will be shown on the figure
colnames(Ich_series)<-seq(50,500,25) #make proper col names
coul <- colorRampPalette(brewer.pal(8, "YlGnBu"))(20)
melted<-melt(Ich_series) #or any other series down the script
colnames(melted)<-c("Skewing","Clones","Values")
ggplot(melted, aes(Skewing, Clones, fill=Values))+
  geom_tile()+
  labs(title="ICh", x=element_blank(), y=element_blank())+
  scale_fill_viridis_c(alpha=0.9)+
  theme_void() #this line defines whether to show coord values or not

#### Figure 1C,D,E ---- Shannon index for the same set of data
# make function with switch for index selection
indexes=function(data,type){
  switch(type,
         Shannon=diversity(data, index="shannon"), 
         Simpson=diversity(data, index="simpson"),
         Pielou=pielou(data),
         Gini=gini(data))
}
#make function for data series
data_series=function(kind){
  data=c()
  for (clones in seq(50,500, 25)){
    dat=c()
    for (i in 1:20){
      values=rbeta(clones,i,21-i)
      indx=indexes(values, kind)
      dat=append(dat, indx)
    }
    data=cbind(data,dat)
  }
  colnames(data)<-seq(50,500,25) 
  return(data)
}
#generate data series for Shannon, Simpson, Gini, Pielou indexes
Sha_series<-data_series("Shannon")
Sim_series<-data_series("Simpson")
Pie_series<-data_series("Pielou")
Gin_series<-data_series("Gini")
#visualise any of them as below or use series above
name="Pielou"
series<-data_series(kind=name)
#reformat data with melt, I cannot fix the warning
melted<- melt(series)   #or any other series down the script
colnames(melted)<-c("Skewing","Clones","Values")
ggplot(melted, aes(Skewing, Clones, fill=Values))+
  geom_tile()+
  labs(title=name, x=element_blank(), y=element_blank())+
  scale_fill_viridis_c(alpha=0.9)+
  theme_void()

#### Figure 2: lm for Ich_series-----
#here we check how Ich index can be explained by other indexes
#first we have to convert data from table format to a simple series
#this is called "flatten"
Ich<-as.double(flatten(as.list(Ich_series)))
Sha<-as.double(flatten(as.list(Sha_series)))
Sim<-as.double(flatten(as.list(Sim_series)))
Pie<-as.double(flatten(as.list(Pie_series)))
Gin<-as.double(flatten(as.list(Gin_series)))
#here we run a trivial linear model where we check whether Ich can be explained
#note: 0 means ignore intercept (there is no meaning for it)
fit<-lm(Ich~ +Sha+Sim+ Pie+ Gin)
#here we check which parameters (if any) were essential
summary(fit)
anova(fit)
#both summaries say that Shannon, then Simpson explained the best
####Figure 2A----
#the same in ggplot style
data<-data.frame(cbind(Ich,predict(fit)))
colnames(data)<-c("Observed_Ich","Predicted_Ich")
ggplot(data,aes(x=Observed_Ich, y=Predicted_Ich))+
  geom_point(size = 2, color = "#274ba0")+
  geom_smooth(color = "black", fill = "#59C7EB")+
  theme_bw()  

#and in ggplot style
data2<-data.frame(cbind(Ich,Sha))
colnames(data2)<-c("Ich","Shannon")
ggplot(data2, aes(x=Ich, y=Shannon))+
  geom_point(size = 2, color = "#274ba0")+
  geom_smooth(color = "black", fill = "#59C7EB")+
  theme_bw()

