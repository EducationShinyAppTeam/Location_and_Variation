library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    titleWidth = 250,
    title = "Location and Variation",
    tags$li(
      class = 'dropdown',
      tags$a(href = "https://shinyapps.science.psu.edu/",
             icon('home', lib = 'font-awesome'))
    )
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
      menuItem("Prerequisites", tabName = "prerequisite1", icon = icon("book")),
      menuItem("Challenges", tabName = "challenges", icon = icon("cogs")),
      menuItem("Reference", tabName = "References", icon = icon("leanpub"))
    ),
    tags$div(class = "sidebar-logo",
             boastUtils::psu_eberly_logo("reversed"))
  ),
  dashboardBody(
    tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "https://educationshinyappteam.github.io/Style_Guide/theme/boast.css")
  ),
  
  tabItems(
    ## First tab content - Overview tab
    tabItem(
      tabName = "overview",

      h1("Location and Variation"),
      p("In this App, you will explore measures of location and variation."),
      br(),
      
      h2("Instructions"),
      tags$ol(
        tags$li("Select the Location, Variation, or Random challenge before start."),
        tags$li("Create points by clicking in the plot to add points and watch your results until you have 15 points total."),
        tags$li("Show the summary statistics as you go along by checking the corresponding box."),
        tags$li("There will be a message showing your results on the top of the plot."),
        tags$li("Click Clear Points and click New Challenge to start over.")
      ),
      
      div(
        style = "text-align:center",
        bsButton(
          inputId = "nextbutton",
          label = "Explore prerequisites",
          icon("wpexplorer"),
          size = "large",
          class = "circle grow"
        )
      ),
      
      #Acknowledgements
      br(),
      br(),
      h2("Acknowledgements"),
      p(
        "This app was developed and coded by Caihui Xiao and further updated by Zhiliang Zhang, Jiajun Gao, and
        Daehoon Gwak."
      ),
      p(
        "Special thanks to Sitong Liu for help on some programming issues.",
        br(),
        br(),
        br(),
        div(class = "updated", "Last Update: 6/17/2020 by DHG.")
      )
    ),
    
    ##Second tab - Prerequiste tab 
    tabItem(
      tabName = "prerequisite1",
      withMathJax(),
      h2("Prerequisites"),
      br(),
      tags$ol(
        tags$li(
          "The mean of a data set is the numerical average."
        ),
        tags$li(
          "The median of a data set is the 50th percentile (the middle value when the numbers are put in order."
        ),
        tags$li(
          "The lower quartile (also called the first quartile and abbreviated as \\(Q_l\\)) is the 25th percentile in a data set.
                                             Thus, 25% of the values on the list of data would fall below \\(Q_l\\) and 75% would be larger than \\(Q_l\\)."
        ),
        tags$li(
          "The upper quartile (also called the third quartile and abbreviated as \\(Q_u\\)) is the 75th percentile in a data set.
                                   Thus, 75% of the values on the list of data would fall below \\(Q_u\\) and 25% would be larger than \\(Q_u\\)."
        ),
        tags$li(
          "The interquartile range (abbreviated as IQR) is the difference between the upper quartile and the lower quartile in a data set so \\(IQR = Q_u\\) - \\(Q_l\\).
                                   The IQR provides a resistant measure of the variability of a set of data."
        ),
        tags$li(
          "The standard deviation (SD) measures how far data values differ from the mean.
                                   If there was no variability, every measurement would be the same as all being the mean value - and the standard deviation would be zero.
                                   As a rule of thumb, about 68% of the values in a symmetric histogram come within one standard deviation of the mean."
        )
      ),
      
      br(),
      div(
        style = "text-align:center",
        bsButton(
          inputId = "start",
          label = "GO!",
          icon("bolt"),
          size = "large",
          class = "circle grow"
        )
      )
      
    ),
    
    # Third tab - Challenge tab
    tabItem(
      tabName = "challenges",
      fluidPage(
        #buttons
        div(
          style = "display: inline-block;vertical-align:top;",
          # bsButton(
          #   inputId = "inst1", '',
          #   icon = icon("info"),
          #   status = "myClass",
          #   size = "small"
          # )
        # ),
        # bsPopover(
        #   "inst1",
        #   " ",
        #   "Click to show the instruction for this challenge",
        #   place = "bottom"
        # ),
        bsButton(
          inputId = "hints", '',
          icon = icon("question"),
          type = 'toggle',
          status = "myClass",
          size = "small"
        ),
        bsPopover(
          "hints",
          " ",
          "Click me if you want to see hints",
          place = "right"
        )
        ),
        
        
        
        # Add a title
        titlePanel("Click Points for Location and Variation"),
        
        wellPanel(
          style = "background-color: white;",
          
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Location",
              verbatimTextOutput("questionforL"),
              tags$style(
                type = 'text/css',
                '#questionforL {font-weight:bold; font-family:Arial ;font-size: 20px;background-color: #EAF2F8; color: black;}'
              ),
              
              fluidRow(
                column(6,
              #show how many plots user can input
              verbatimTextOutput("diff1"),
              tags$style(type = 'text/css', '#diff1 {background-color: white;}'))),
              
              fluidRow(
                column(4, verbatimTextOutput("mean2"), tags$style(type = 'text/css', '#mean2 {background-color: white;}')),
                column(4, verbatimTextOutput("meanvalue"), tags$style(type = 'text/css', '#meanvalue {background-color: white;}')),
                column(4, verbatimTextOutput("mvalue"), tags$style(type = 'text/css', '#mvalue {background-color: white;}'))              )
            ),
            
            tabPanel(
              "Variation",
              verbatimTextOutput("questionforV"),
              tags$style(
                type = 'text/css',
                '#questionforV {font-weight:bold; font-family:Arial ;font-size: 20px;background-color: #EAF2F8; color: black;}'
              ),
              
              
              fluidRow(
                column(6,
                       #show how many plots user can input
                       verbatimTextOutput("diff2"),
                       tags$style(type = 'text/css', '#diff2 {background-color: white;}'))),
              
              fluidRow(
              column(6, verbatimTextOutput("iqr2"), tags$style(type = 'text/css', '#iqr2 {background-color: white;}')),
              column(6, verbatimTextOutput("sd2"), tags$style(type = 'text/css', '#sd2 {background-color: white;}'))
              )
              
            ),
            
            tabPanel(
              "Random",
              verbatimTextOutput("questionforR"),
              tags$style(
                type = 'text/css',
                '#questionforR {font-weight:bold; font-family:Arial ;font-size: 20px;background-color: #EAF2F8; color: black;}'
              ),
              
              fluidRow(
                column(6,
                       #show how many plots user can input
                       verbatimTextOutput("diff3"),
                       tags$style(type = 'text/css', '#diff3 {background-color: white;}'))),
              
              fluidRow(
                column(4, tags$style(type = 'text/css', '#mean3 {background-color: white;}'),
                       verbatimTextOutput("mean3")),
                column(4, tags$style(type = 'text/css', '#meanvalue1 {background-color: white;}'),
                       verbatimTextOutput("meanvalue1")),
                column(4, tags$style(type = 'text/css', '#mvalue4 {background-color: white;}'),
                       verbatimTextOutput("mvalue4")),
                column(6, tags$style(type = 'text/css', '#iqr3 {background-color: white;}'),
                       verbatimTextOutput("iqr3")),
                column(6, tags$style(type = 'text/css', '#sd3 {background-color: white;}'),
                       verbatimTextOutput("sd3"))
              )
            )
          ),
          
          fluidRow(
          column(
            1,
            actionButton("bs", "New Challenge", style = "color: #fff; background-color: orange")
          ),
          column(
            1, offset = 5,
            actionButton("clear", "Clear Points", style = "color: #fff; background-color: orange")
          )
          )
          
          ),
        
        
        fluidRow(
        # Create a space for the plot output
        column(10, plotOutput("clusterPlot1", "100%", "500px", click =
                            "clusterClick"),
               # bsPopover(
               #   "clusterPlot1",
               #   " ",
               #   "Click points on the graph to create your plot",
               #   trigger = "hover",
               #   place = "top"
               # )
        ),
        column(2, br(), br(),
               conditionalPanel("input.hints != 0 ", id = 'hintbox',
                              checkboxInput("median", "Median", FALSE),
                              p("Blue vertical Line",style="color:blue"), br(),
                              
                              checkboxInput("mean", "Mean", FALSE),
                              p("Red vertical Line",style="color:red"), br(),
                              
                              checkboxInput("iqr", "IQR", FALSE),
                              p("Blue horizontal Line",style="color:blue"), br(),
                              
                              checkboxInput("sd", "Std Dev", FALSE),
                              p("Red horizontal Line",style="color:red")
                              )
        )
        )
      )
    ),
    tabItem(
      tabName = "References",
      withMathJax(),
      h2("References"),
      p(     #shinyjs
        class = "hangingindent",
        "Attali, D. (2020), Easily Improve the User Experience of Your Shiny Apps in Seconds.
            (v1.1), [R package]. Available from
            https://cran.r-project.org/web/packages/shinyjs/shinyjs.pdf"
      ),
      p(     #shinyBS
        class = "hangingindent",
        "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny.
            (v0.61), [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
      ),
      p(     #Boast Utilities
        class = "hangingindent",
        "Carey, R. (2019), boastUtils: BOAST Utilities. (v0.1.0),
            [R Package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
      ),
      p(     #shinydashboard
        class = "hangingindent",
        "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny'. (v0.7.1), [R Package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
      ),
      p(     #shiny
        class = "hangingindent",
        "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R. (v1.4.0),
            [R Package]. Available from https://CRAN.R-project.org/package=shiny"
      ),
      p(     #shinyWidgets
        class = "hangingindent",
        "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W.
            (2020), shinyWidgets: Custom Inputs Widgets for Shiny
            (v0.5.2), [R package]. Available from
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
      ),
      p(     #reference for ideas
        class = "hangingindent",
        " Statistical Applets (n.d.), Available from
          http://digitalfirst.bfwpub.com/stats_applet/generic_stats_applet_6_meanmed.html"
      )
      
    )
  ))
)
