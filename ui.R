library(shinydashboard)
library(shiny)
library(shinyBS)
library(boastUtils)
#Let`s begin
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    titleWidth = 250,
    title = "Location and Variation",
    tags$li(class = "dropdown",
            actionLink("info",icon("info",class = "myClass"))),
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
      menuItem("Game", tabName = "game", icon = icon("gamepad")),
      menuItem("References", tabName = "References", icon = icon("leanpub"))
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
      p("In this app, you will explore measures of location and variation."),
      br(),
      h2("Instructions"),
      tags$ol(
        tags$li("Select the Location, Variation, or Random challenge before start."),
        tags$li("Create points by clicking in the plot to add points 
                and watch your results until you have 15 points total."),
        tags$li("Show the summary statistics as you go along by clicking on the hint button 
                and checking the summaries desired."),
        tags$li("There will be a message showing your results on the top of the plot."),
        tags$li("Click Clear Points and click New Challenge to start over.")
      ),
      div(
        style = "text-align:center",
        bsButton(
          inputId = "nextbutton",
          label = "GO!",
          icon = icon("bolt"),
          size = "large"
        )
      ),
      #Acknowledgements
      br(),
      br(),
      h2("Acknowledgements"),
      p(
        "This app was originally developed and coded by Caihui Xiao. The app was further updated by 
        Zhiliang Zhang and Jiajun Gao in June 2018, and by Daehoon Gwak in June 2020.
        Special thanks to Sitong Liu for helping with some programming issues.",
        br(),
        br(),
        br(),
        div(class = "updated", "Last Update: 07/01/2020 by DG.")
      )
    ),
    ##Second tab - Prerequisites tab 
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
          "The interquartile range (abbreviated as IQR) is the difference between the upper quartile 
          and the lower quartile in a data set so \\(IQR = Q_u\\) - \\(Q_l\\). 
          The IQR provides a resistant measure of the variability of a set of data."
          ),
        tags$li(
          "The standard deviation (SD) measures how far data values differ from the mean. 
          If there was no variability, every measurement would be the same as all being the mean value
          and the standard deviation would be zero. 
          As a rule of thumb, about 68% of the values in a symmetric histogram come within one standard deviation of the mean."
          )
        ),br(),
      div(
        style = "text-align:center",
        bsButton(
          inputId = "start",
          label = "GO!",
          icon =  icon("bolt"),
          size = "large"
        )
      )
    ),
    # Third tab - Challenge tab
    tabItem(
      tabName = "game",
        # Add a title
        h2("Click Points for Location and Variation"),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              title = "Location",
              tags$strong(textOutput("questionforL")),
              br(),
              fluidRow(
                column(1,
                       bsButton(
                         inputId = "hints",
                         label = "Hint",
                         type = 'toggle',
                         size = "medium")
                ),
                column(
                  3,
                  uiOutput("mean2")
                ),
                column(
                  2,
                  uiOutput("meanvalue")
                ),
                column(
                  2, 
                  uiOutput("mvalue")
                )
              )
            ),
            tabPanel(
              title = "Variation", inputID = 'ace',
              tags$strong(uiOutput("questionforV")),
              br(),
              fluidRow(
                column(1,
                       bsButton(
                         inputId = "hints",
                         label = "Hint",
                         type = 'toggle',
                         size = "medium")
                ),
                column(
                  2,
                  uiOutput("iqr2")
                ),
                column(
                  2,
                  uiOutput("sd2")
                )
              )
            ),
            tabPanel(
              title = "Random",
              tags$strong(uiOutput("questionforR")),
              br(),
              fluidRow(
                column(1,
                       bsButton(
                         inputId = "hints",
                         label = "Hint",
                         type = 'toggle',
                         size = "medium")
                ),
                column(
                  3,
                  uiOutput("mean3")
                ),
                column(
                  2,
                  uiOutput("meanvalue1")
                ),
                column(
                  2,
                  uiOutput("mvalue4")
                ),
                column(
                  2,
                  uiOutput("iqr3")
                ),
                column(
                  2,
                  uiOutput("sd3")
                )
              )
              )
            ),
          br(),
          fluidRow(
          column(
            1,
            actionButton("bs", "New Challenge")
          ),
          column(
            1, offset = 5,
            actionButton("clear", "Clear Points")
            )
          ),
          fluidRow(
            column(12,
                   #show how many plots user can input
                   uiOutput("feedback")
                  )
          ),
        fluidRow(# Create a space for the plot output
          column(
            10,
            plotOutput(outputId = "clusterPlot1", width = "100%", height = "500px", click = "clusterClick")
          ),
          column(
            2,
            br(), br(), # to fit the height on both plot and hintboxes
            conditionalPanel("input.hints != 0 ", id = 'hintbox',
              checkboxInput("median", "Median", FALSE),
              p("Blue vertical line", class = "bluetext"),
              br(),
              checkboxInput("mean", "Mean", FALSE),
              p("Red vertical line", class = "redtext"),
              br(),
              checkboxInput("iqr", "IQR", FALSE),
              p("Blue horizontal line", class = "bluetext"),
              br(),
              checkboxInput("sd", "Std Dev", FALSE),
              p("Red horizontal line", class = "redtext")
            )
          ))
    ),
    tabItem(
      tabName = "References",
      h2("References"),
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
      p(     #reference for ideas
        class = "hangingindent",
        " Statistical Applets - Mean and Median (n.d.), Available from
          http://digitalfirst.bfwpub.com/stats_applet/generic_stats_applet_6_meanmed.html"
      )
      )
    ))
)
