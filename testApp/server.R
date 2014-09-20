
library(dplyr)

data(mtcars)

#mtcarsagg=mtcars%>%group_by(cyl)%>%summarize(hp_mean=mean(hp),mpg_mean=mean(mpg))
mtcarsagg=mtcars%>%select(cyl,hp,mpg)%>%group_by(cyl)%>%summarise_each(funs(mean))


shinyServer(function(input,output){
  
  output$tbl <- renderTable({
      mtcarsagg
  })
  
})