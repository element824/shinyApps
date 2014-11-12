library(shiny)

# Define UI for miles per gallon application
shinyUI(navbarPage(
  # Application title
  "Mortgage Payment Calculator",
  
  tabPanel("About",
    p("The tool here helps run different scenarios on monthly payment for a house"),
    br(),
    p("Disclaimer:"),
    p("This is just a tool to explore calculation and I don't guarantee accuracy of the calculation")
    ),
  tabPanel("Inputs",
          div(class="row-fluid",
              div(class="span3",
                  wellPanel(
                    numericInput("homeprice","Home Price ($)",250000),
                    numericInput("pctdown","% Down payment",20),
                    numericInput("nyr","Term of mortgage (years)",30),
                    numericInput("irate","Interest rate",4.5),
                  selectInput("xtraprintime","Frequency of extra payments to principal",
                              choices=c("monthly","quarterly","half-yearly","yearly")),
                  numericInput("xtraprin","Extra payment to principal per period ($)",0)
                  
                  )),
              div(class="span3",
                wellPanel(  
                  numericInput("ptaxrate","Property tax rate (%)",1),
                  numericInput("insamt","Home Insurance ($/month)",75)),
                wellPanel(
                  numericInput("dedother","Other itemized deductions ($)",5000),
                  numericInput("dedstd","Standard deduction ($)",12600),
                  numericInput("taxrate","Tax rate (%)",25)
                )
                  ),
              div(class="span3",
                  wellPanel(
                    numericInput("loan401K","Loan from 401K ($)",0),
                    numericInput("irate401K","Interest rate (%)",4.5),
                    numericInput("nyr401K","Payment period (yrs)",5)
                    )
                  )
              )
               
               ),
      tabPanel("Plot",
               h4(textOutput("cashoutlay")),
               br(),
               h3("Plot of Monthly Payments"),
               plotOutput("paymnthPlot"),
               h3("Break out of average monthly payment in a given year"),
               numericInput("payyrpick","Pick Year",1),
               h4(textOutput("avgmnthpay")),
               plotOutput("paymnthbreakout")),
      tabPanel("Table",
               h4("Monthly payment ($) breakout for first two years"),
               tableOutput("tblOut"))

))