library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title= "Location&Variation"),
                    dashboardSidebar(
                      sidebarMenu(
                        id="tabs",
                        menuItem("Prerequistes",tabName = "prerequisite1",icon = icon("book")),
                        menuItem("Overview", tabName = "overview",icon = icon("dashboard")),
                        menuItem("Challenges", tabName = "challenges",icon = icon("cogs"))
                      )
                    ),
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "stylecssMouseExp.css")
                      ),
                      tabItems(
                        
                        #prerequiste tab content
                        
                        tabItem(tabName = "prerequisite1",
                                withMathJax(),
                                h3(strong("Background: Location and Variation")),br(),
                                h4(tags$li("The mean of a data set is the numerical average.")),
                                h4(tags$li("The median of a data set is the 50th percentile (the middle value when the numbers are put in order.")),
                                h4(tags$li("The lower quartile (also called the first quartile and abbreviated as \\(Q_l\\)) is the 25th percentile in a data set. 
                                             Thus, 25% of the values on the list of data would fall below \\(Q_l\\) and 75% would be larger than \\(Q_l\\).")),
                                h4(tags$li("The upper quartile (also called the third quartile and abbreviated as \\(Q_u\\)) is the 75th percentile in a data set. 
                                   Thus, 75% of the values on the list of data would fall below \\(Q_u\\) and 25% would be larger than \\(Q_u\\).")),
                                h4(tags$li("The interquartile range (abbreviated as IQR) is the difference between the upper quartile and the lower quartile in a data set so \\(IQR = Q_u\\) - \\(Q_l\\).
                                   The IQR provides a resistant measure of the variability of a set of data.")),
                                h4(tags$li("The standard deviation (SD) measures how far data values differ from the mean. 
                                   If there was no variability, every measurement would be the same as all being the mean value - and the standard deviation would be zero. 
                                   As a rule of thumb, about 68% of the values in a symmetric histogram come within one standard deviation of the mean.")),
                                
                                br(),
                                div(style = "text-align:center",
                                    bsButton("nextbutton", "Go to the overview", icon("wpexplorer"), size = "medium",style = "warning"))
                                
                        ),
                        
                        # First tab content
                        tabItem(tabName = "overview",
                                tags$a(href='http://stat.psu.edu/',   tags$img(src='logo.png', align = "left", width = 180)),
                                br(),
                                br(),
                                br(),
                                
                                h3(strong("About:")),
                                h4("In this App, you will explore measures of location and variation."),
                                br(),
                                h3(strong("Instructions:")),
                                h4(tags$li("Select the Location, Variation, or Random challenge before start.")),
                                h4(tags$li("Create points by clicking in the plot to add points and watch your results until you have 15 points total.")),
                                h4(tags$li("Show the summary statistics as you go along by checking the corresponding box.")),
                                h4(tags$li("There will be a message showing your results on the top of the plot.")),
                                h4(tags$li("Click Clear Points and click New Challenge to start over.")),
                                
                                div(style = "text-align:center",
                                    bsButton("start", "G O !", icon("bolt"), size = "medium",style = "warning")),
                                br(),
                                h3(strong("Acknowledgements:")),
                                h4("This app was developed and coded by Caihui Xiao and further updated by Zhiliang Zhang and Jiajun Gao."),
                                h4("This app is based on extending the idea in the WH freeman applet at http://digitalfirst.bfwpub.com/stats_applet/generic_stats_applet_6_meanmed.html."),
                                h4("Special thanks to Sitong Liu for help on some programming issues.")
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "challenges",
                                fluidPage(
                                  #buttons
                                  div(style="display: inline-block;vertical-align:top;",
                                      
                                      div(style="display: inline-block;vertical-align:top;",
                                          tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                      ),
                                      circleButton("inst1",icon = icon("info"), status = "myClass",size = "xs")
                                      #circleButton("inst1",icon = icon("question",class = "glyphicon glyphicon-question-sign"), size = "xs")
                                  ),
                                  bsPopover("inst1", " ", "Click to show the instruction for this challenge", place = "bottom"),
                                  
                                  # Add a title
                                  titlePanel("Click Points for Location and Variation"),
                                  
                                  wellPanel(style = "background-color: white;",
                                            
                                            tabsetPanel(type="tabs",
                                                        tabPanel("Location",
                                                                 verbatimTextOutput("questionforL"),
                                                                 #   tags$head(tags$style(HTML("#questionforL {font-size: 20px;}"))),
                                                                 tags$style(type='text/css', '#questionforL {font-weight:bold; font-family:Arial ;font-size: 20px;background-color: #EAF2F8; color: black;}'), 
                                                                 
                                                                 verbatimTextOutput("diff1"),
                                                                 tags$style(type='text/css', '#diff1 {background-color: #pink;}'), 
                                                                 
                                                                 fluidRow( column(4,verbatimTextOutput("mean2")),
                                                                           column(4,verbatimTextOutput("meanvalue")), 
                                                                           column(4,verbatimTextOutput("mvalue"))
                                                                 )
                                                        ),
                                                        tabPanel("Variation",
                                                                 verbatimTextOutput("questionforV"),
                                                                 tags$style(type='text/css', '#questionforV {font-weight:bold; font-family:Arial ;font-size: 20px;background-color: #EAF2F8; color: black;}'), 
                                                                 
                                                                 verbatimTextOutput("diff2"),
                                                                 
                                                                 column(6,verbatimTextOutput("iqr2")),
                                                                 column(6,verbatimTextOutput("sd2"))
                                                                 
                                                        ),
                                                        tabPanel("Random",
                                                                 verbatimTextOutput("questionforR"),
                                                                 tags$style(type='text/css', '#questionforR {font-weight:bold; font-family:Arial ;font-size: 20px;background-color: #EAF2F8; color: black;}'), 
                                                                 
                                                                 verbatimTextOutput("diff3"),
                                                                 fluidRow( column(4,style = "background-color:#ffffff;",
                                                                                  verbatimTextOutput("mean3")),
                                                                           column(4,style = "background-color:#ffffff;",
                                                                                  verbatimTextOutput("meanvalue1")), 
                                                                           column(4,
                                                                                  style = "background-color:#ffffff;",
                                                                                  verbatimTextOutput("mvalue4"))
                                                                 ),
                                                                 verbatimTextOutput("diff4"),
                                                                 column(6,style = "background-color:#ffffff;",verbatimTextOutput("iqr3")),
                                                                 column(6,style = "background-color:#ffffff;",verbatimTextOutput("sd3"))
                                                                 
                                                        )
                                            ),
                                            
                                            
                                            column(6, actionButton("bs","New Challenge", style="color: #fff; background-color: orange")),
                                            column(6, actionButton("clear", "Clear Points", style="color: #fff; background-color: orange")),
                                            
                                            
                                            # fluidRow(
                                            #   div(style="display: inline-block;vertical-align:top;",
                                            #       
                                            #       div(style="display: inline-block;vertical-align:top;",
                                            #           tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 19))
                                            #       ),
                                            #       circleButton("inst1",icon = icon("info"), status = "myClass",size = "xs")
                                            #       #circleButton("inst1",icon = icon("question",class = "glyphicon glyphicon-question-sign"), size = "xs")
                                            #   ),
                                            #   bsPopover("inst1", " ", "Click to show the instruction for this challenge", place = "bottom")
                                            #   
                                            # ),
                                            
                                            fluidRow(
                                              
                                              column(1, 
                                                     checkboxInput("median","Median",FALSE),
                                                     bsPopover("median"," ", "The Vertical Line in blue", place="Right",options = list(container = "body")),
                                                     
                                                     list(tags$head(tags$style()), 
                                                          HTML('<img src="mean.png", height= 20px')),
                                                     verbatimTextOutput("median1")),
                                              column(1,
                                                     checkboxInput("mean", "Mean",FALSE),
                                                     bsPopover("mean", " ","The Vertical Line in red", place="Right",options = list(container = "body")
                                                     ),
                                                     list(tags$head(tags$style()), 
                                                          HTML('<img src="median.png", height= 24px')),
                                                     verbatimTextOutput("mean1")),
                                              column(1, 
                                                     checkboxInput("iqr", "IQR",FALSE),
                                                     bsPopover("iqr", " ","The Horizational line in blue", place="Right",options = list(container = "body")),
                                                     
                                                     list(tags$head(tags$style()), 
                                                          HTML('<img src="sd.png", height= 20px')),
                                                     verbatimTextOutput("median")),
                                              column(1,
                                                     checkboxInput("sd", "Std Dev",FALSE),
                                                     bsPopover("sd"," ", "The Horizational line in red", place="Right",options = list(container = "body")),
                                                     
                                                     list(tags$head(tags$style()), 
                                                          HTML('<img src="iqr.png", height= 20px')),
                                                     verbatimTextOutput("mean"))
                                              
                                            )
                                  ),
                                  
                                  
                                  
                                  # conditionalPanel(condition="input.buttons == 'Location'",
                                  #                  verbatimTextOutput("questionforL"),
                                  #               #   tags$head(tags$style(HTML("#questionforL {font-size: 20px;}"))),
                                  #                  tags$style(type='text/css', '#questionforL {font-weight:bold; font-family:Arial ;font-size: 20px;background-color: white; color: black;}'), 
                                  #                  
                                  #                  verbatimTextOutput("diff1"),
                                  #                  tags$style(type='text/css', '#diff1 {background-color: #pink;}'), 
                                  #                  
                                  #                  fluidRow( column(4,verbatimTextOutput("mean2")),
                                  #                            column(4,verbatimTextOutput("meanvalue")), 
                                  #                            column(4,verbatimTextOutput("mvalue"))
                                  #                  )
                                  #                  ),
                                  # conditionalPanel(condition="input.buttons == 'Variation'",
                                  #                  verbatimTextOutput("questionforV"),
                                  #                  tags$style(type='text/css', '#questionforV {font-weight:bold; font-family:Arial ;font-size: 20px;background-color: #FDFEFE; color: black;}'), 
                                  #                  
                                  #                  verbatimTextOutput("diff2"),
                                  #                  
                                  #                  column(6,verbatimTextOutput("iqr2")),
                                  #                  column(6,verbatimTextOutput("sd2"))
                                  #                  
                                  #                  ),
                                  # conditionalPanel(condition="input.buttons == 'Random'",
                                  #                  verbatimTextOutput("questionforR"),
                                  #                  tags$style(type='text/css', '#questionforR {font-weight:bold; font-family:Arial ;font-size: 20px;background-color: #FDFEFE; color: black;}'), 
                                  #                  
                                  #                  verbatimTextOutput("diff3"),
                                  #                  fluidRow( column(4,style = "background-color:#ffffff;",
                                  #                                   verbatimTextOutput("mean3")),
                                  #                            column(4,style = "background-color:#ffffff;",
                                  #                                   verbatimTextOutput("meanvalue1")), 
                                  #                            column(4,
                                  #                                   style = "background-color:#ffffff;",
                                  #                                   verbatimTextOutput("mvalue4"))
                                  #                  ),
                                  #                  verbatimTextOutput("diff4"),
                                  #                  column(6,style = "background-color:#ffffff;",verbatimTextOutput("iqr3")),
                                  #                  column(6,style = "background-color:#ffffff;",verbatimTextOutput("sd3"))
                                  #                  
                                  #                  ),
                                  # Add a row for the main content
                                  
                                  fluidRow(
                                    
                                    # Create a space for the plot output
                                    
                                    plotOutput(
                                      "clusterPlot1", "80%", "500px", click="clusterClick"
                                      
                                    ),
                                    bsPopover("clusterPlot1"," ","Click points on the graph to create your plot", trigger = "hover",place="right")
                                    
                                  )
                                  
                                  
                                  
                                )
                        )
                      )
                    ))


