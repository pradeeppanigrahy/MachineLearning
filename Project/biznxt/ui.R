library(markdown)
library(shiny)
library(plotly)
library(gridExtra)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
#library(shinydashboard)


# shinyUI(fluidPage(
#   
#   titlePanel("Welcome!"),
#   
#   sidebarLayout(
#     
#     sidebarPanel(
#       # Inputs excluded for brevity
#     ),
#     
#     mainPanel(
#       navbarPage("BiZnXt",
#                  tabPanel("State",
#                           sidebarLayout(
#                             sidebarPanel(
#                               sliderInput("Year",
#                                           "",
#                                           min = 2010,
#                                           max = 2017,
#                                           value = 1,
#                                           step=1)
#                               
#                             ),
#                             mainPanel(
#                               plotlyOutput("plotlymapByState"),
#                               plotlyOutput("plotlybarByState")
#                               
#                             )
#                           )
#                  ),
#                  tabPanel("City",
#                           plotlyOutput("returnsbycity")
#                  ),
#                  tabPanel("Time Series Analysis",
#                           plotOutput("multistateplot"),
#                           plotOutput("multistateplot1"),
#                           plotOutput("multistateplot2"),
#                           plotOutput("multistateplot3"),
#                           plotOutput("multistateplot4")
#                  )
#                  
#       )
#     )
#   )
# ))






# pageWithSidebar(
#   
#   # Application title
#   headerPanel("Hello Shiny!"),
#   
#   mainPanel(
#   navbarPage("BiZnXt",
#              tabPanel("State",
#                       sidebarLayout(
#                         sidebarPanel(
#                           sliderInput("Year",
#                                       "",
#                                       min = 2010,
#                                       max = 2017,
#                                       value = 1,
#                                       step=1)
#                           
#                         ),
#                         mainPanel(
#                           plotlyOutput("plotlymapByState"),
#                           plotlyOutput("plotlybarByState")
#                           
#                         )
#                       )
#              ),
#              tabPanel("City",
#                       plotlyOutput("returnsbycity")
#              ),
#              tabPanel("Time Series Analysis",
#                       plotOutput("multistateplot"),
#                       plotOutput("multistateplot1"),
#                       plotOutput("multistateplot2"),
#                       plotOutput("multistateplot3"),
#                       plotOutput("multistateplot4")
#              )
#              
#   )
#   )
# )











shinyUI(fluidPage(theme = "biznxt.css",
  list(tags$head(HTML('<img class="biz_logo" src=biz.png></img> <img class="wklogo" src=wolterskluwer-logo-rgb.png></img>' ))),
  div(style="padding: 1px 0px; width: '100%';height:71px;",
      titlePanel(
        title="", windowTitle="See our market penetration"
      )
  ),
  navbarPage(" ",
             navbarMenu("Geography",
           tabPanel("State",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("Year",
                                    "",
                                    min = 2010,
                                    max = 2016,
                                    value = 1,
                                    step=1,
                                    sep = "")
                        ,width = 3

                      ),
                      mainPanel(
                        plotlyOutput("plotlymapByState")
                        #,
                        #plotlyOutput("click"),
                        #plotlyOutput("plotlybarByState")
                        #,
                        #plotlyOutput("orderstatePlot")

                      )
                    )
           ),
           tabPanel("County",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("Yearcounty",
                                    "",
                                    min = 2010,
                                    max = 2016,
                                    value = 1,
                                    step=1,
                                    sep = ""),
                        selectInput("state", "Choose a state:",
                                    list(`East Coast` = c("New York", "New Jersey", "Connecticut"),
                                         `West Coast` = c("Colorado","Washington", "Oregon", " California"),
                                         `Midwest` = c("Minnesota", "Wisconsin", "Iowa"),
                                         `South` = c("Alabama","Florida"))
                        )
                        ,width = 3
                        
                      ),
                      mainPanel(
                        plotOutput("plotbycounty")
                        ,
                        plotOutput("plotbycountyzoom")
                        #, textOutput("countyerrortext")
                      )
                    )
           ),
           tabPanel("City",
                    sidebarLayout(
                      sidebarPanel(
                        sliderInput("Yearcity",
                                    "",
                                    min = 2010,
                                    max = 2016,
                                    value = 1,
                                    step=1,
                                    sep = "")
                        ,width = 3
                        
                      ),
                      mainPanel(
                    plotlyOutput("returnsbycity")
                      )
                    )
           )),
           tabPanel("Trend Analysis"
                    ,
                    plotOutput("multistateplot"),
                    plotOutput("multistateplot1"),
                    plotOutput("multistateplot2"),
                    plotOutput("multistateplot3"),
                    plotOutput("multistateplot4")
           ),
           tabPanel("Stats",sidebarLayout(
             sidebarPanel(
               sliderInput("Yearbar",
                           "",
                           min = 2010,
                           max = 2016,
                           value = 1,
                           step=1,
                           sep = "")
               ,width = 3
               
             ),mainPanel(plotlyOutput("plotlybarByState")
                    ,
                    plotlyOutput("orderstatePlot")
                    )
             )
             ),
           tabPanel("Sector", sidebarLayout(
             sidebarPanel(
               sliderInput("Yearsector",
                           "",
                           min = 2010,
                           max = 2016,
                           value = 1,
                           step=1,
                           sep = ""),
               selectInput("statesector", "Choose a state:",
                                                 list( `States`=c("Colorado","Alabama","New York", "New Jersey", "Connecticut",
                                                       "Washington", "Oregon", " California",
                                                      "Minnesota", "Wisconsin", "Iowa"))
                           )
               ,width = 3
               
             ),
             mainPanel(
               plotlyOutput("returnsbysectorbycountry"),
               plotlyOutput("returnsbysectorbystate"),
               plotlyOutput("returnsbytaxtypebycountry"),
               plotlyOutput("returnsbytaxtypebystate")
             )
           )),
           # tabPanel("Time Series Analysis"
           #          ,
           #          plotOutput("multistateplot"),
           #          plotOutput("multistateplot1"),
           #          plotOutput("multistateplot2"),
           #          plotOutput("multistateplot3"),
           #          plotOutput("multistateplot4")
           # ),
           #tabPanel("Do you know?"
                    #,
                    #verbatimTextOutput ("doyouknowhtml")
                    # textOutput("doyouknowtext")     
           #),
           tabPanel("Our recommendations",
                    # dataTableOutput('evolvingstates'),
                    # dataTableOutput('losingstates')
                    includeHTML("./www/recommendation.html")
                    # tableOutput('evolvingstates'),
                    # tableOutput('losingstates')
                    
           ),
           navbarMenu("More",
                      tabPanel("Advanced analytics",plotlyOutput("advance3dplot")),
                      tabPanel("Predictions"),
                      tabPanel("Risk analysis"),
                      tabPanel("Modelling")
           ),
           navbarMenu("Other Products",
                      tabPanel("Sure Tax"),
                      tabPanel("CCH Access"),
                      tabPanel("STROL"),
                      tabPanel("More...")
           )
           

))
)