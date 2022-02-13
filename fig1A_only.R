#3D for beta distribution other way
# by L.Bystrykh Feb 2022.

library(plotly)

size=50 #length of the density distribution series
series=20
z=c() #probabioity density itself
#generate series of probabioity densities with increasing alpha (i) and
#decreasing beta (21-i) at the same time
for (i in 1:series){
  z=rbind(z,dbeta(seq(0,1,length.out=size),i,series+1-i))
}
#plot one result as a single probability density in 2D
plot(1:size, z[3,],"l")
#add all series to the same plot 
for (i in 1:series){
  points(1:size, z[i,], "l")
}

#try 3d without meshgrid
# use old sizes value (length of a single probability distribution)
#and series value
#this loop will stitch coordinates for series (x), sizes (y) and 
#make probability density distribution in zet (taken from the previous calculation)
Threedee=c()
for (i in 1:series){
  x=1:size
  y=rep(i,size)
  zet=z[i,]
  block=cbind(x,y,zet)
  Threedee=rbind(Threedee, block)
}
#this is an interactive 3D picture if you run the script
D3D=data.frame(Threedee)
plot_ly(D3D, x = ~D3D[,1], y = ~D3D[,2], z = ~D3D[,3],
        type='scatter3d', mode='lines', name='line',
        line = list(color = 'rgb(231, 99, 50)'
                    ) )%>% 
  layout(scene = list(xaxis = list(title = 'Sizes'),
                      yaxis = list(title = 'Series'),
                      zaxis = list(title = 'Probabilty density'))
         )

  