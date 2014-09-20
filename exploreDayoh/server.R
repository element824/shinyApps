
# load libraries
library(ggplot2)
library(ggmap)
library(RgoogleMaps)
library(RColorBrewer)

load(file="zipPlt.Rda")

shinyServer(function(input,output){
  
  output$censusplt = renderPlot({
    
     censusvar=input$pickcensusvar
     
     # get Dayton area map
     x=get_googlemap(center="Dayton",maptype=c("roadmap"),zoom=9)
     
     # choropleth map of variable
     p=ggmap(x)
     p=p+geom_polygon(data=zipPlt,aes_string(x="long",y="lat",group="id",fill=censusvar),color="black",alpha=0.4)
     p=p+xlab("")+ylab("")
     p=p+scale_fill_manual(values=rainbow(5))+theme_bw(20)+theme(axis.text=element_blank())
     p=p+theme(legend.title=element_blank(),plot.title=element_text(face="bold"))
     print(p)
    
  })
  
  output$zdataplt = renderPlot({
    
    zdatavar=input$pickzdatavar
    
    # get Dayton area map
    x=get_googlemap(center="Dayton",maptype=c("roadmap"),zoom=9)
    
    # choropleth map of variable
    p=ggmap(x)
    p=p+geom_polygon(data=zipPlt,aes_string(x="long",y="lat",group="id",fill=zdatavar),color="black",alpha=0.4)
    p=p+xlab("")+ylab("")
    p=p+scale_fill_manual(values=rainbow(5))+theme_bw(20)+theme(axis.text=element_blank())
    p=p+theme(legend.title=element_blank(),plot.title=element_text(face="bold"))
    print(p)
    
  })  
  
})