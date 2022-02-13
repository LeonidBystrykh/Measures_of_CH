#This script generates all figures for the manuscript
#Some figures are replicated multiple times as different versions
#Exact figure in the script might differ in details from the publication
#due to extra editing steps and adaptations
#Note that the basic settings for the data series remain the same through all figures
# By L.Bystrykh Feb 2022

library(plotly) #for interactive 3D
library(plot3D) #for mesh and static 3D
library(RColorBrewer) #for custom colors
library(pheatmap) #for pheatmap
library(vegan) #for Shannon and Simpson
library(reldist) #for gini index 
library(OTUtable) #for pielou 
library(textshape) #for flatten
library(reshape2) #for melt function
library(ggplot2) #for ggplot type heatmap

####check dbeta function for any selected alpha and beta values----
# dbeta function is probability density function
# it gives a shape of the distribution, not the values
span<-seq(0,1,length.out=100) #number of points in the series
plot(span,dbeta(span,10,10), "l", 
     xlab="Values",
     ylab="Probability density") #span, alpha and beta

#### beta distribution probability density plot data for 3D----
#Prepare coordinates for 3D using mesh:
M<-mesh(seq(1,20,length.out=20),
        seq(0,1,length.out=20))
x<-M$y
y<-M$x
#Make a series of beta distributions with increasing alpha and decreasing beta
# the sizes of the series are the same as numbers of the series to keep figure square shape
z=c()
for (i in 1:20){
  z=rbind(z,dbeta(seq(0,1,length.out=20),i,21-i))
}
####This is static version of the data----
surf3D(x, y, z, colvar = z, colkey = TRUE, 
       box = TRUE, bty = "b", phi = 35, theta = 105)

####This plot is interactive----
# more options in https://plotly.com/r/figure-labels/
data<-x
series<-y
density<-z
fig1A<-plot_ly(x = ~data, y = ~series, z = ~density) %>% 
  add_surface() %>%
  layout(title = 'Probability density')
fig1A

#### Fig1B---- 
#We will da a 2D plot with variable clone sizes (50 to 500 step 25) and 
# variable beta distribution (20x20). These numbers remain the same 
#for all tested indexes
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
#two versions of the heatmaps
#if you want different colors use brewer.pal
coul <- colorRampPalette(brewer.pal(8, "YlGnBu"))(20)
#version 1 basic R function heatmap()
fig1B<-heatmap(Ich_series, scale="none", Rowv=NA, Colv=NA,
               col=coul,
        xlab="Population size",
        ylab="Beta distribution series",
        main="Ich",
        RowSideColors=coul)
#version 2, pheatmap() from pheatmap library
fig1b<-pheatmap(Ich_series,
         scale="none",
         cluster_rows = F,
         cluster_cols=F,
         border_color=F,
         labels_row=c(1:20),
         main="Ich",
         ylab="y" #does not work
         )
#version 3. use ggplot type of a heatmap
#use those lines in all other heatmap options below
melted<-melt(Ich_series) #or any other series down the script
colnames(melted)<-c("Series","Clones","Values")
ggplot(melted, aes(x=Series, y=Clones, fill=Values))+
  geom_tile()+
  scale_fill_gradient(low="blue", high="yellow")+
  labs(title="Ich")
#end of ggplot2 lines

#### Fig1C ---- all elements in the same order
#try Shannon index for the same set of data
Sha_series=c()
for (clones in seq(50,500, 25)){
  Sha=c()
  for (i in 1:20){
    values=rbeta(clones,i,21-i)
    indx=diversity(values, index="shannon") #or simpson
    Sha=append(Sha, indx)
  }
  Sha_series=cbind(Sha_series,Sha)
}
colnames(Sha_series)<-seq(50,500,25) #make proper col names
#version 2, pheatmap() from pheatmap library
fig1c<-pheatmap(Sha_series,
                scale="none",
                cluster_rows = F,
                cluster_cols=F,
                border_color=F,
                labels_row=c(1:20),
                main="Shannon", #or Simpson
                ylab="y" #does not work
)
melted<-melt(Sha_series) #or any other series down the script
colnames(melted)<-c("Series","Clones","Values")
ggplot(melted, aes(x=Series, y=Clones, fill=Values))+
  geom_tile()+
  scale_fill_gradient(low="blue", high="yellow")+
  labs(title="Shannon")
#the same for simpson
Sim_series=c()
for (clones in seq(50,500, 25)){
  Sim=c()
  for (i in 1:20){
    values=rbeta(clones,i,21-i)
    indx=diversity(values, index="simpson") #or simpson
    Sim=append(Sim, indx)
  }
  Sim_series=cbind(Sim_series,Sim)
}
colnames(Sim_series)<-seq(50,500,25) #make proper col names
#use it for the heatmap as above
fig1c<-pheatmap(Sim_series,
                scale="none",
                cluster_rows = F,
                cluster_cols=F,
                border_color=F,
                labels_row=c(1:20),
                main="Simpson", 
                ylab="y" #does not work
)
melted<-melt(Sim_series) #or any other series down the script
colnames(melted)<-c("Series","Clones","Values")
ggplot(melted, aes(x=Series, y=Clones, fill=Values))+
  geom_tile()+
  scale_fill_gradient(low="blue", high="yellow")+
  labs(title="Simpson")
#try Pielou index
#pielou(c(1,2,3,4)) #just a test
Pie_series=c()
for (clones in seq(50,500, 25)){
  Pie=c()
  for (i in 1:20){
    values=rbeta(clones,i,21-i)
    indx=pielou(values) #or simpson
    Pie=append(Pie, indx)
  }
  Pie_series=cbind(Pie_series,Pie)
}
colnames(Pie_series)<-seq(50,500,25) 
fig1c<-pheatmap(Pie_series,
                scale="none",
                cluster_rows = F,
                cluster_cols=F,
                border_color=F,
                labels_row=c(1:20),
                main="Pielou", #or Simpson
                ylab="y" #does not work
)
melted<-melt(Pie_series) #or any other series down the script
colnames(melted)<-c("Series","Clones","Values")
ggplot(melted, aes(x=Series, y=Clones, fill=Values))+
  geom_tile()+
  scale_fill_gradient(low="blue", high="yellow")+
  labs(title="Pielou")
#at last: try Gini index
#gini(c(1,2,3,4)) #check
Gin_series=c()
for (clones in seq(50,500, 25)){
  Gin=c()
  for (i in 1:20){
    values=rbeta(clones,i,21-i)
    indx=gini(values) #or simpson
    Gin=append(Gin, indx)
  }
  Gin_series=cbind(Gin_series,Gin)
}
colnames(Gin_series)<-seq(50,500,25) 
fig1c<-pheatmap(Gin_series,
                scale="none",
                cluster_rows = F,
                cluster_cols=F,
                border_color=F,
                labels_row=c(1:20),
                main="Gini", #or Simpson
                ylab="y" #does not work
)
melted<-melt(Gin_series) #or any other series down the script
colnames(melted)<-c("Series","Clones","Values")
ggplot(melted, aes(x=Series, y=Clones, fill=Values))+
  geom_tile()+
  scale_fill_gradient(low="blue", high="yellow")+
  labs(title="Pielou")


#####try lm for Ich_series-----
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
fit<-lm(Ich~0 +Sha+Sim+ Pie+ Gin)
#here we check which parameters (if any) were essential
summary(fit)
anova(fit)
#both summaries say that Shannon, then Simpson explained the best
#some technical plots if you want
#par(mfrow=c(2,2))
#plot(fit)
#delete previous pictures, it will reset the screen format
#check how close are predicted values to the observed
plot(Ich, predict(fit), main="Predicted to observed Ich")
#the same in ggplot style
data<-data.frame(cbind(Ich,predict(fit)))
colnames(data)<-c("Observed_Ich","Predicted_Ich")
ggplot(data,aes(x=Observed_Ich, y=Predicted_Ich))+
  geom_point()+
  geom_smooth()
#plot Shannon vs Ich for the same data  
plot(Ich, Sha, main="Shannon versus Ich")
#and in ggplot style
data2<-data.frame(cbind(Ich,Sha))
colnames(data2)<-c("Ich","Shannon")
ggplot(data2,aes(x=Ich, y=Shannon))+
  geom_point()+
  geom_smooth()+
  labs(title="Clonal index vs Shannon index")
