
zschurl="http://www.zillow.com/dayton-oh/schools/#/dayton-oh/schools/bb=-84.622879%2C39.427177%2C-83.782425%2C40.134266&regionId=31184&zoom=9"

shinyUI(fluidPage(
  h1("Explore Dayton, OH Area"),
  tabsetPanel(
    tabPanel("Census View",
        column(4,
            wellPanel(
              h2("Census Variables"),
              selectInput(inputId="pickcensusvar",label="Pick Census Variable",
                          choices=c("Median Income" = "medIncLvl",
                                    "Median Age" = "medAgeLvl",
                                    "% white population" = "pctwhiteLvl",
                                    "% black population" = "pctblackLvl",
                                    "% asian population" = "pctasianLvl"),
                          selected=c("medIncLvl")),
              br(),
              p("The data here has been extracted from ",
                 a(href="http://www.census.gov/data/developers/data-sets.html",target="_blank","US Census data API"),
                ". The source code for extracting data used in these maps is available ",
                a(href="https://github.com/notesofdabbler/blog_notesofdabbler/blob/master/exploreCensus/exploreDaytonArea.R",target="_blank","here.")),
              p(em("Note: This maps is not based on a live data feed. A snapshot of data taken on Sep 20, 2014 is shown here")),
              p("A nice zip code level census information can be accessed in the following US Census ",
                a(href="http://factfinder2.census.gov/faces/nav/jsf/pages/index.xhtml",target="_blank","page.")),
              br(),
              br(),
              p(em("Disclaimer: The information in this website is created for exploratory purposes and I do not guarantee
                   the accuracy of information.",style="color:red"))
              )
               ),
        column(8,        
               em("There is time lag for map to appear. Your patience is appreciated.",style="color:blue"),
               plotOutput("censusplt",width="900px",height="900px")       
                     )
    ),
    tabPanel("Housing View",
        column(4,
            wellPanel(
              h2("Housing Variables"),
              selectInput(inputId="pickzdatavar",label="Pick housing variable",
                          choices=c("Median List Price" = "medListPriceLvl",
                                    "Median List Price per SqFt" = "medValSqFtLvl",
                                    "% single homes" = "pctsinglehomeLvl",
                                    "% home built after 2000" = "pctgt2000Lvl",
                                    "% homes > 2400 sqft" = "sizegt24Lvl",
                                    "% home 1800-2400 sqft" = "size1824Lvl"),
                          selected=c("medListPriceLvl")),
              br(),
              p("The data here has been extracted from ",
                a(href="http://www.zillow.com/howto/api/GetDemographics.htm",target="_blank","Zillow API"),
                " neighborhood data. The source code for extracting data used in these maps is available ",
                a(href="https://github.com/notesofdabbler/blog_notesofdabbler/blob/master/exploreCensus/exploreDaytonArea.R",target="_blank","here.")),
              p(em("Note: This map is not based on a live data feed. A snapshot of data taken on Sep 20, 2014 is shown here")),
              br(),
              br(),
              p(em("Disclaimer: The information in this website is created for exploratory purposes and I do not guarantee
                   the accuracy of information.",style="color:red"))
              
              )   
              
              ),
        column(8,
               em("There is time lag for map to appear. Your patience is appreciated.",style="color:blue"),
               plotOutput("zdataplt",width="900px",height="900px")     
               )
    ),
    tabPanel("School View",
        p("The school view is readily available in a nice format at zillow and so has not been recreated here. 
          Please following this ",a(href=zschurl,target="_blank","link")," to view school summary")     
             )
    
    ),
  wellPanel(
    p("This application is powered by ",a(href="http://shiny.rstudio.com/",target="_blank","Shiny")," developed by ",
        a(href="http://www.rstudio.com/",target="_blank","RStudio"))
    )
  ))