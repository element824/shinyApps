library(shiny)

# Define UI for miles per gallon application
shinyUI(navbarPage(
    # Application title
  "Mortgage Payment Calculator",
  
  tabPanel("About",
    h3("Tool to explore scenarios around mortgage payment"),
    p(strong("Disclaimer: This calculation tool is exploratory and I don't guarantee any accuracy of the calculation"),style="color:red"),
    p("This tool is done using ",a("Shiny",href="http://shiny.rstudio.com"),
          " webframework created by ",a("RStudio",href="http:///www.rstudio.com"),
          " and inspired by ",a("Zillow mortgage calculator",href="http://www.zillow.com/mortgage-calculator/"))
           ),
  tabPanel("Inputs",
          fluidPage(
            fluidRow(
              p(strong("Disclaimer: This calculation tool is exploratory and I don't guarantee any accuracy of the calculation"),style="color:red")
              ),
            fluidRow(
                column(3,
                  wellPanel(
                    numericInput("homeprice","Home Price ($)",250000),
                    numericInput("pctdown","% Down payment",20),
                    numericInput("nyr","Term of mortgage (years)",30),
                    numericInput("irate","Interest rate",4.5),
                  selectInput("xtraprintime","Frequency of extra payments to principal",
                              choices=c("monthly","quarterly","half-yearly","yearly")),
                  numericInput("xtraprin","Extra payment to principal per period ($)",0)
                  
                  )),
              column(3,
                wellPanel(  
                  numericInput("ptaxrate","Property tax rate (%)",1),
                  numericInput("insamt","Home Insurance ($/month)",75),
                  numericInput("hoa","HOA dues (Annual) ($)",360)),
                wellPanel(
                  numericInput("dedother","Other itemized deductions ($)",5000),
                  numericInput("dedstd","Standard deduction ($)",12600),
                  numericInput("taxrate","Tax rate (%)",25)
                )
                  ),
              column(3,
                  wellPanel(
                    numericInput("loan401K","Loan from 401K ($)",0),
                    numericInput("irate401K","Interest rate (%)",4.5),
                    numericInput("nyr401K","Payment period (yrs)",5)
                    )
                  )
              ))
               
               ),
      tabPanel("Plot",
          fluidPage(
            fluidRow(
              p(strong("Disclaimer: This calculation tool is exploratory and I don't guarantee any accuracy of the calculation"),style="color:red")
            ),
            fluidRow(
              h3(textOutput("cashoutlay"))
              ),
            fluidRow(
            column(6,
               h3("Plot of Monthly Payments"),
               br(),
               br(),
               br(),
               plotOutput("paymnthPlot")),
            column(6,
               h3("Break out of average monthly payment in a given year"),
               numericInput("payyrpick","Pick Year",1),
               h4(textOutput("avgmnthpay")),
               plotOutput("paymnthbreakout"))
            )
          )
          ),
      tabPanel("Table",
               h4("Monthly payment ($) breakout for first two years"),
               p(strong("Disclaimer: This calculation tool is exploratory and I don't guarantee any accuracy of the calculation"),style="color:red"),
               tableOutput("tblOut"))
))