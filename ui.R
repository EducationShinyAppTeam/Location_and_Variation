library(shinydashboard)
library(shiny)
library(shinyBS)
library(boastUtils)
library(shinyWidgets)
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
        tags$li("In the game, you'll first need to select which challenge 
                (Location, Variation, or Both)."),
        tags$li("Create points by clicking in the plot to add points 
                and watch your results until you have 15 points total."),
        tags$li("Show the summary statistics as you go along by clicking on 
                the hint button and checking the summaries desired."),
        tags$li("There will be a message showing your results 
                on the top of the plot."),
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
        "This app was originally developed and coded by Caihui Xiao. 
        The app was further updated by Zhiliang Zhang and Jiajun Gao in 
        June 2018, and by Daehoon Gwak in July 2020.
        Special thanks to Sitong Liu for helping with some programming issues.",
        br(),
        br(),
        br(),
        div(class = "updated", "Last Update: 07/15/2020 by DG.")
      )
    ),
    ##Second tab - Prerequisites tab 
    tabItem(
      tabName = "prerequisite1",
      withMathJax(),
      h2("Prerequisites"),
      br(),
      tags$ul(
        tags$li(
          # This is not the most productive meaning for the SAM but it is what
          # this app supports.
          "You can interpret the value of the ", 
          tags$em("sample arithmetic mean"), " of a data set as being 
          the balancing point of the data set when plotted. This value is 
          commonly denoted as \\(\\overline{x}\\) or as
          \\(SAM\\left(x_1,\\ldots,x_n\\right)\\)."
        ),
        tags$li(
          "You can interpret the value of the ", tags$em("sample median"), " of 
          a data set as being the value that cuts the ordered data set into two 
          equally sized subsets. This value is also known as the 50th percentile 
          or the second quartile. There is no single symbol that represents this 
          value; some of the more common ones include 
          \\(Q_2\\), \\(\\widetilde{x}\\), 
          and \\(Median\\left(x_1\\ldots,x_n\\right)\\)."
        ),
        tags$li(
          "You can interpret the value of the ", tags$em("first quartile"),
          " (a.k.a. the lower quartile or the 25th percentile) as being the 
          value cuts off the smallest quarter (25%) of the ordered data set 
          from the rest. Thus, 25% of the values in the data set will be 
          smaller than this value and 75% would bethis value or larger. 
          We often use \\(Q_1\\) to represent this value,sometimes \\(Q_l\\)."
        ),
        tags$li(
          "You can interpret the value of the ", tags$em("third quartile"),
          " (a.k.a. the upper quartile or the 75th percentile) as being the 
          value cuts off the smallest three-quarters (75%) of the ordered 
          data set from the rest. Thus, 75% of the values in the data set 
          will be smaller than this value and 25% would be this value or larger.
          We often use \\(Q_3\\) to represent this value, sometimes \\(Q_u\\)."
        ),
        tags$li(
          "The value of the ", tags$em("interquartile range"), " is a measure of 
          variation found by looking at the distance between the values of the
          first and third quartiles. This distance is the width of the smallest
          interval that contains that middle 50% of the ordered data set. 
          We often represent this value with the letters ", tags$em("IQR"), " 
          and with the formula \\(IQR = Q_3-Q_1\\)."
        ),
        tags$li(
          "The value of the ", tags$em("sample"), "[", tags$em("arithmetic"),"] ",
          tags$em("standard deviation"), " provides a measure for how the data 
          values differ from the each other. If the observations did not vary, 
          then each value would be the same and the value of this statistics 
          would be zero. The most common ways to represent this value include 
          \\(s\\), \\(s_x\\), and \\(SASD\\left(x_1,\\ldots,x_n\\right)\\)."
        )
      ),
      br(),
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
        h2("The Location and Variation Game"),
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
              ), br(),
              fluidRow(
                column(
                  1,
                  actionButton("bs2", "New Challenge")
                ),
                column(
                  1, offset = 5,
                  actionButton("clear2", "Clear Points")
                )
              ), br(),
              uiOutput("feedback1")
            ),
            tabPanel(
              title = "Variation",
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
              ), br(),
              fluidRow(
                column(
                  1,
                  actionButton("bs1", "New Challenge")
                ),
                column(
                  1, offset = 5,
                  actionButton("clear1", "Clear Points")
                )
              ), br(),
              uiOutput("feedback2")
            ),
            tabPanel(
              title = "Both",
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
              ), br(),
              fluidRow(
                column(
                  1,
                  actionButton("bs3", "New Challenge")
                ),
                column(
                  1, offset = 5,
                  actionButton("clear3", "Clear Points")
                )
              ), br(),
              uiOutput("feedback3")
              )
            ),
        fluidRow(# Create a space for the plot output
          column(
            10,
            plotOutput(outputId = "clusterPlot1", 
                       width = "100%", height = "500px", click = "clusterClick"),
            tags$script(HTML(
              "$(document).ready(function() 
                       { document.getElementById('clusterPlot1').
                       setAttribute('aria-label',
                       `User can create points to test their challenges`)
                       })"
            ))
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
      p(     #shinyWidgets
        class = "hangingindent",
        "Perrier, V., Meyer, F., Granjon, D., Fellows, I., and Davis, W.
            (2020), shinyWidgets: Custom Inputs Widgets for Shiny
            (v0.5.2), [R package]. Available from
            https://cran.r-project.org/web/packages/shinyWidgets/index.html"
      ),
      p(     #reference for ideas
        class = "hangingindent",
        " Statistical Applets - Mean and Median (n.d.), Available from
          http://digitalfirst.bfwpub.com/stats_applet/generic_stats_applet_6_meanmed.html"
      )
      )
    ))
)
