# Load Libraries ----
library(shinydashboard)
library(shiny)
library(shinyBS)
library(boastUtils)
library(shinyWidgets)

## App Meta Data----------------------------------------------------------------
APP_TITLE <<- "Location and Variation"
APP_DESCP  <<- paste(
  "This app provides set of challenges which allow a user to explore two common",
  "measures of location (sample arithmetic mean and sample median) and two",
  "common measures of variation (interquartile range and standard deviation."
)
## End App Meta Data------------------------------------------------------------

# Define constants and functions ----

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "yellow",
    dashboardHeader(
      titleWidth = 250,
      title = "Location and Variation",
      tags$li(class = "dropdown",
              actionLink("info",icon("info",class = "myClass"))),
      tags$li(
        class = "dropdown",
        tags$a(target = "_blank", icon("comments"),
               href = "https://pennstate.qualtrics.com/jfe/form/SV_7TLIkFtJEJ7fEPz?appName=Location_and_Variation"
        )
      ),
      tags$li(
        class = 'dropdown',
        tags$a(href = "https://shinyapps.science.psu.edu/",
               icon('home', lib = 'font-awesome'))
      )
    ),
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Challenges", tabName = "challenge", icon = icon("cogs")),
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
        ## Overview tab ----
        tabItem(
          tabName = "overview",
          h1("Location and Variation"),
          p("In this app, you will explore measures of location and variation."),
          br(),
          h2("Instructions"),
          tags$ol(
            tags$li("In the challenge, you'll first need to select which challenge
                (Location, Variation, or Both)."),
            tags$li("Create points by clicking in the plot to add points
                and watch your results until you earn 15 points total."),
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
            div(class = "updated", "Last Update: 9/21/2020 by NJH.")
          )
        ),
        ##Prerequisites tab ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          br(),
          ## We will have two set of prereq's for now. The original focused on
          ## sample statistics which will be commented out for now and a set of
          ## neutral/non-technical text will be put in place --NJH, 9/21/20
          h3("Measures of Location"),
          tags$ul(
            tags$li(tags$strong("Median: "), "the 50th percentile (half of the
                    data are below and half of the data are above this value.
                    Attempts to capture the notion of a 'typical' value."),
            tags$li(tags$strong("Mean: "), "the arithmetic average; captures the
                    concept of center when totals are relevant.")
          ),
          h3("Measures of Variation"),
          tags$ul(
            tags$li(tags$strong("Interquartile Range (IQR): "), "the difference
                    between the 75th and 25th percentiles; captures how much of
                    the number line is covered by the middle half of the data."),
            tags$li(tags$strong("Standard Deviation (SD): "), "measures the
                    typical distance between a data value and the value of the
                    mean. Typically, about 2/3 of the data in a symmetric
                    histogram is within one standard deviation of the mean.")
          ),
          h3("Sensitive vs. Robust Measures"),
          tags$ul(
            tags$li("The Mean and Standard Deviation are called ",
                    tags$strong("sensitive measures"), " because they are highly
                    affected by the values of outliers."),
            tags$li("The Median and IQR are called ",
                    tags$strong("robust measures"), " because they are relatively
                    unaffected by the values of outliers.")
          ),
          # tags$ul(
          #   tags$li(
          #     # This is not the most productive meaning for the SAM but it is what
          #     # this app supports.
          #     "You can interpret the value of the ",
          #     tags$em("sample arithmetic mean"), " of a data set as being
          # the balancing point of the data set when plotted. This value is
          # commonly denoted as \\(\\overline{x}\\) or as
          # \\(SAM\\left(x_1,\\ldots,x_n\\right)\\)."
          #   ),
          #   tags$li(
          #     "You can interpret the value of the ", tags$em("sample median"), " of
          # a data set as being the value that cuts the ordered data set into two
          # equally sized subsets. This value is also known as the 50th percentile
          # or the second quartile. There is no single symbol that represents this
          # value; some of the more common ones include
          # \\(Q_2\\), \\(\\widetilde{x}\\),
          # and \\(Median\\left(x_1\\ldots,x_n\\right)\\)."
          #   ),
          #   tags$li(
          #     "You can interpret the value of the ", tags$em("first quartile"),
          #     " (a.k.a. the lower quartile or the 25th percentile) as being the
          # value cuts off the smallest quarter (25%) of the ordered data set
          # from the rest. Thus, 25% of the values in the data set will be
          # smaller than this value and 75% would bethis value or larger.
          # We often use \\(Q_1\\) to represent this value,sometimes \\(Q_l\\)."
          #   ),
          #   tags$li(
          #     "You can interpret the value of the ", tags$em("third quartile"),
          #     " (a.k.a. the upper quartile or the 75th percentile) as being the
          # value cuts off the smallest three-quarters (75%) of the ordered
          # data set from the rest. Thus, 75% of the values in the data set
          # will be smaller than this value and 25% would be this value or larger.
          # We often use \\(Q_3\\) to represent this value, sometimes \\(Q_u\\)."
          #   ),
          #   tags$li(
          #     "The value of the ", tags$em("interquartile range"), " is a measure of
          # variation found by looking at the distance between the values of the
          # first and third quartiles. This distance is the width of the smallest
          # interval that contains that middle 50% of the ordered data set.
          # We often represent this value with the letters ", tags$em("IQR"), "
          # and with the formula \\(IQR = Q_3-Q_1\\)."
          #   ),
          #   tags$li(
          #     "The value of the ", tags$em("sample [arithmetic] standard deviation"),
          #     " provides a measure for how the data values differ from the each
          #     other. If the observations did not vary, then each value would be
          #     the same and the value of this statistics would be zero. The most
          #     common ways to represent this value include \\(s\\), \\(s_x\\),
          #     and \\(SASD\\left(x_1,\\ldots,x_n\\right)\\)."
          #   )
          # ),
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
        # Challenge tab ----
        tabItem(
          tabName = "challenge",
          # Add a title
          h2("The Location and Variation Challenges"),
          tabsetPanel(
            id = "challenge-tabset",
            type = "tabs",
            ### Location Tab ----
            tabPanel(
              title = "Location",
              br(),
              div(
                class = "largerFont",
                uiOutput("questionforL"),
              ),
              br(),
              fluidRow(
                column(1,
                       bsButton(
                         inputId = "hints1",
                         label = "Hint",
                         type = 'toggle',
                         size = "large")
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
                  bsButton(
                    inputId = "newLocationChallenge",
                    label = "New Challenge",
                    style = "default",
                    size = "large"
                  )
                ),
                column(
                  1, offset = 5,
                  bsButton(
                    inputId = "clearLocationChallenge",
                    label = "Clear Points",
                    style = "default",
                    size = "large"
                  )
                )
              ), br(),
              uiOutput("feedback1")
            ),
            ### Variation Tab ----
            tabPanel(
              title = "Variation",
              br(),
              div(
                class = "largerFont",
                uiOutput("questionforV"),
              ),
              br(),
              fluidRow(
                column(1,
                       bsButton(
                         inputId = "hints2",
                         label = "Hint",
                         type = 'toggle',
                         size = "large")
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
                  bsButton(
                    inputId = "newVariationChallenge",
                    label = "New Challenge",
                    style = "default",
                    size = "large"
                  )
                ),
                column(
                  1, offset = 5,
                  bsButton(
                    inputId = "clearVariationChallenge",
                    label = "Clear Points",
                    style = "default",
                    size = "large"
                  )
                )
              ), br(),
              uiOutput("feedback2")
            ),
            ### Both Tab ----
            tabPanel(
              title = "Both",
              br(),
              div(
                class = "largerFont",
                uiOutput("questionforR"),
              ),
              br(),
              fluidRow(
                column(1,
                       bsButton(
                         inputId = "hints3",
                         label = "Hint",
                         type = 'toggle',
                         size = "large")
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
                  bsButton(
                    inputId = "newMixedChallenge",
                    label = "New Challenge",
                    style = "default",
                    size = "large"
                  )
                ),
                column(
                  1, offset = 5,
                  bsButton(
                    inputId = "clearMixedChallenge",
                    label = "Clear Points",
                    style = "default",
                    size = "large"
                  )
                )
              ), br(),
              uiOutput("feedback3")
            )
          ), #Closes tabsetpanel
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
              conditionalPanel(
                condition = "input.hints1 != 0 || input.hints2 != 0 ||
                input.hints3 != 0",
                id = 'hintbox',
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
        ## References ----
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
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {
  challenge <- reactiveVal()
  val <- reactiveValues(x = NULL, y = NULL)

  ## Clear function ----
  clearPoints <- function() {
    val$x <- NULL
    val$y <- NULL
  }

  result <- function(feedback, success) {
    stmt <- boastUtils::generateStatement(
      session,
      verb = "scored",
      object = "shiny-tab-challenge",
      description = str_squish(feedback),
      success = success
    )

    boastUtils::storeStatement(session, stmt)

    return(feedback)
  }

  # Listen for clicks
  observe({
    # Initially this will be empty
    if (is.null(input$clusterClick)) {
      return()
    }
    isolate({
      val$x <- c(val$x, input$clusterClick$x)
      val$y <- c(val$y, input$clusterClick$y)
    })

    coords <- jsonlite::toJSON({
      data.frame(
        x = val$x,
        y = val$y
      )
    })

    stmt <- boastUtils::generateStatement(
      session,
      verb = "answered",
      object = "clusterClick",
      description = str_squish(challenge),
      interactionType = "performance",
      response = coords
    )

    boastUtils::storeStatement(session, stmt)
  })

  b <- reactiveValues(right = c(sample(1:7, 1)))
  c <- reactiveValues(right = c(sample(1:7, 1)))
  d <- reactiveValues(right = c(sample(1:14, 1)))

  observeEvent(input$newVariationChallenge, {
    b$right = sample(1:7, 1)
    c$right = sample(1:7, 1)
    d$right = sample(1:14, 1)
  })

  observeEvent(input$newLocationChallenge, {
    b$right = sample(1:7, 1)
    c$right = sample(1:7, 1)
    d$right = sample(1:14, 1)
  })

  observeEvent(input$newMixedChallenge, {
    b$right = sample(1:7, 1)
    c$right = sample(1:7, 1)
    d$right = sample(1:14, 1)
  })


  # Clear the points on 'new challenge' button click
  observeEvent(
    input$newVariationChallenge ||
    input$newLocationChallenge ||
    input$newMixedChallenge,
  {
    clearPoints()
  })

  # Clear the points on 'clear' button click
  observeEvent(
    input$clearVariationChallenge ||
    input$clearLocationChallenge ||
    input$clearMixedChallenge,
  {
    clearPoints()
  })

  # Clear the points on 'challenge-tabset' change
  observeEvent(input$`challenge-tabset`, {
    clearPoints()
  })

  observeEvent(input$info,{
    sendSweetAlert(
      session = session,
      title = "Instructions:",
      text = tags$ol(
        tags$li("Create points by clicking in the plot to add points and
                see results until you earn 15 points total."),
        tags$li("Use the hint box if you need."),
        tags$li("Click Clear Points to restart the challenge or 
                click New Challenge to try another challenge.")
      ),
      type = "info"
    )
  })

  observeEvent(input$nextbutton, {
    updateTabItems(session, "pages", "prerequisites")
  })

  observeEvent(input$start, {
    updateTabItems(session, "pages", "challenge")
  })

  ## BEGIN TODO ----
  # Consider rewriting this section using a lookup table instead of ifelse block.
  # 'Location' question
  output$questionforL <- renderText({
    if (b$right == 1) {
      challenge <<- "Challenge: Please use 15 points to make them roughly symmetric."
    }
    else if (b$right == 2) {
      challenge <<- "Challenge: Please use 15 points to make them left skewed."
    }
    else if (b$right == 3) {
      challenge <<- "Challenge: Please use 15 points to make the median less than 1
      and the mean greater than 4."
    }
    else if (b$right == 4) {
      challenge <<- "Challenge: Please use 15 points to make the median greater than 9.5
      and the mean less than 6."
    }
    else if (b$right == 5) {
      challenge <<- "Challenge: Please use 15 points to make the absolute difference between
      the mean and median at least 2."
    }
    else if (b$right == 6) {
      challenge <<- "Challenge: Please use 15 points to make them right skewed."
    }
    else if (b$right == 7) {
      challenge <<- "Challenge: Please use 15 points to make the mean greater than median."
    }
    challenge
  })
  # 'Variation' question
  output$questionforV <- renderText({
    if (c$right == 1) {
      challenge <<- "Challenge: Please use 15 points to make both the SD and IQR more than 3."
    }
    else if (c$right == 2) {
      challenge <<- "Challenge: Please use 15 points to make the IQR bigger than the SD."
    }
    else if (c$right == 3) {
      challenge <<- "Challenge: Please use 15 points to make both the SD and IQR less than 1."
    }
    else if (c$right == 4) {
      challenge <<- "Challenge: Please use 15 points to make the IQR zero and
      the SD bigger than 1."
    }
    else if (c$right == 5) {
      challenge <<- "Challenge: Please use 15 points to make the SD = zero."
    }
    else if (c$right == 6) {
      challenge <<- "Challenge: Please use 15 points to make the IQR smaller than SD."
    }
    else if (c$right == 7) {
      challenge <<- "Challenge: Please use 15 points to make both the SD and
      the IQR less than 2."
    }
    challenge
  })
  # 'Random' question
  output$questionforR <- renderText({
    if (d$right == 1) {
      challenge <<- "Challenge: Please use 15 points to make them roughly symmetric."
    }
    else if (d$right == 2) {
      challenge <<- "Challenge: Please use 15 points to make them left skewed."
    }
    else if (d$right == 3) {
      challenge <<- "Challenge: Please use 15 points to make the median less than 1
      and the mean greater than 4."
    }
    else if (d$right == 4) {
      challenge <<- "Challenge: Please use 15 points to make the median greater than 9.5
      and the mean less than 6."
    }
    else if (d$right == 5) {
      challenge <<- "Challenge: Please use 15 points to make the absolute difference
      between the mean and median at least 2."
    }
    else if (d$right == 6) {
      challenge <<- "Challenge: Please use 15 points to make both the SD and IQR more than 3."
    }
    else if (d$right == 7) {
      challenge <<- "Challenge: Please use 15 points to make the IQR bigger than the SD."
    }
    else if (d$right == 8) {
      challenge <<- "Challenge: Please use 15 points to make both the SD and IQR less than 1."
    }
    else if (d$right == 9) {
      challenge <<- "Challenge: Please use 15 points to make the IQR zero and
      the SD bigger than 1."
    }
    else if (d$right == 10) {
      challenge <<- "Challenge: Please use 15 points to make the SD = zero."
    }
    else if (d$right == 11) {
      challenge <<- "Challenge: Please use 15 points to make them right skewed."
    }
    else if (d$right == 12) {
      challenge <<- "Challenge: Please use 15 points to make the IQR smaller than SD."
    }
    else if (d$right == 13) {
      challenge <<- "Challenge: Please use 15 points to make the mean greater than median."
    }
    else if (d$right == 14) {
      challenge <<- "Challenge: Please use 15 points to make both the SD and
      the IQR less than 2."
    }
    ## END TODO ----
    challenge
  })
  # Generate the plot of the clustered points
  output$clusterPlot1 <- renderPlot({
    tryCatch({
      # Format the data as a matrix
      store = matrix(0, 150, 2)
      store1 = matrix(0, 15, 2)
      if (length(val$x) > 0 & length(val$x) < 15) {
        for (i in 1:length(val$x)) {
          store[i, 1] <- signif(val$x[i], digits = 2)
          store[i, 2] <- 1
        }
        if (length(val$x) > 1) {
          for (i in 1:length(val$x)) {
            for (j in i + 1:length(val$x)) {
              if (store[j, 1] == store[i, 1]) {
                store[j, 2] <- store[j, 2] + 1
              }
            }
          }
        }
      }
      else if (length(val$x) >= 15)
      {
        for (i in 1:15) {
          store[i, 1] <- signif(val$x[i], digits = 2)
          store[i, 2] <- 1
        }
        for (i in 1:15) {
          for (j in i + 1:15) {
            if (store[j, 1] == store[i, 1]) {
              store[j, 2] <- store[j, 2] + 1
            }
          }
        }
        for (i in 1:15) {
          store1[i, 1] <- store[i, 1]
          store1[i, 2] <- store[i, 2]
        }
      }
      # Try to cluster
      mclust2Dplot(
        data = store,
        what = "classification",
        classification = fit$classification,
        main = FALSE,
        xlim = c(-0.1, 5),
        ylim = c(0, 10),
        cex = input$opt.cex,
        cex.lab = input$opt.cexaxis
      )
    }, error = function(warn) {
      # Otherwise just plot the points and instructions
      plot(
        store,
        xlim = c(1, 10),
        ylim = c(0, 15),
        xlab = "X",
        ylab = "Frequency",
        cex = 1.5,
        cex.lab = 1.5,
        cex.axis = 1.5,
        pch = 16
      )
      output$meanvalue <- renderText({
        if (length(val$x) < 15 &
            length(val$x) >= 1  & input$median   == "TRUE") {
          paste("median:", signif(median(val$x), digits = 3))
        }
        else if (length(val$x) >= 15 & input$median   == "TRUE") {
          paste("median:", signif(median(store1[, 1]), digits = 3))
        }
      })
      output$meanvalue1 <- renderText({
        if (length(val$x) < 15 &
            length(val$x) >= 1  & input$median == "TRUE") {
          paste("median:", signif(median(val$x), digits = 3))
        }
        else if (length(val$x) >= 15 & input$median == "TRUE") {
          paste("median:", signif(median(store1[, 1]), digits = 3))
        }
      })
      if (length(val$x) < 15 &
          input$mean == "TRUE" & length(val$x) > 1) {
        abline(v = (signif(mean(val$x), digits = 3)),
               col = "red",
               lwd = "4")
      }
      else if (length(val$x) >= 15 & input$mean == "TRUE")
      {
        abline(v = (signif(mean(store1[, 1]), digits = 3)),
               col = "red",
               lwd = "4")
      }
      output$mvalue <- renderText({
        if (length(val$x) >= 1 &
            length(val$x) < 15 & input$mean == "TRUE") {
          paste("mean:", signif(mean(val$x), digits = 3))
        }
        else if (length(val$x) >= 15 & input$mean == "TRUE") {
          paste("mean:", signif(mean(store1[, 1]), digits = 3))
        }
      })
      output$mvalue4 <- renderText({
        if (length(val$x) >= 1 & length(val$x) < 15 & input$mean == "TRUE") {
          paste("mean:", signif(mean(val$x), digits = 3))
        }
        else if (length(val$x) >= 15 & input$mean == "TRUE") {
          paste("mean:", signif(mean(store1[, 1]), digits = 3))
        }
      })
      output$mean2 <- renderText({
        if (length(val$x) >= 2 &
            length(val$x) < 15 &
            input$mean == "TRUE" & input$median   == "TRUE") {
          paste("Absolute difference:", signif(abs(mean(val$x) - median(val$x)),
                                               digits = 3))
        }
        else if (length(val$x) >= 15 &
                 input$mean == "TRUE" & input$median   == "TRUE")
        {
          paste("Absolute difference:", signif(abs(
            mean(store1[, 1]) - median(store1[, 1])
          ), digits = 3))
        }
      })
      output$mean3 <- renderText({
        if (length(val$x) >= 1 &
            length(val$x) < 15 &
            input$mean == "TRUE" & input$median   == "TRUE") {
          paste("Absolute difference:", signif(abs(mean(val$x) - median(val$x)),
                                               digits = 2))
        }
        else if (length(val$x) >= 15 &
                 input$mean == "TRUE" & input$median   == "TRUE")
        {
          paste("Absolute difference:", signif(abs(
            mean(store1[, 1]) - median(store1[, 1])
          ), digits = 2))
        }
      })
      output$sd2 <- renderText({
        if (length(val$x) < 15 & input$sd == "TRUE" & length(val$x) > 1) {
          paste("SD:", signif(abs(sd(val$x)), digits = 2))
        }
        else if (length(val$x) >= 15 & input$sd == "TRUE") {
          paste("SD:", signif(abs(sd(store1[, 1])), digits = 2))
        }
      })
      output$sd3 <- renderText({
        if (length(val$x) < 15 & input$sd == "TRUE" & length(val$x) > 1) {
          paste("SD:", signif(abs(sd(val$x)), digits = 2))
        }
        else if (length(val$x) >= 15 & input$sd == "TRUE") {
          paste("SD:", signif(abs(sd(store1[, 1])), digits = 2))
        }
      })
      if (length(val$x) < 15 &
          input$sd == "TRUE" & length(val$x) > 1) {
        segments(mean(val$x) - sd(val$x),
                 0,
                 mean(val$x),
                 0,
                 lwd = "4",
                 col = "red")
        segments(mean(val$x),
                 0,
                 mean(val$x) + sd(val$x),
                 0,
                 lwd = "4",
                 col = "red")
      }
      else if (length(val$x) >= 15 & input$sd == "TRUE") {
        segments(
          mean(store1[, 1]) - sd(store1[, 1]),
          0,
          mean(store1[, 1]),
          0,
          lwd = "4",
          col = "red"
        )
        segments(
          mean(store1[, 1]),
          0,
          mean(store1[, 1]) + sd(store1[, 1]),
          0,
          lwd = "4",
          col = "red"
        )
      }
      if (length(val$x) < 15 &
          input$median  == "TRUE" & length(val$x) >= 2) {
        abline(v = (signif(median(val$x), digits = 2)),
               col = "blue",
               lwd = "4")
      }
      else if (length(val$x) >= 15 & input$median  == "TRUE") {
        abline(v = (signif(median(store1[, 1]), digits = 2)),
               col = "blue",
               lwd = "4")
      }
      output$iqr2 <- renderText({
        if (length(val$x) < 15 & input$iqr == "TRUE" & length(val$x) >= 2) {
          paste("IQR:", signif(abs(IQR(val$x)), digits = 3))
        }
        else if (length(val$x) >= 15 & input$iqr == "TRUE") {
          paste("IQR:", signif(abs(IQR(store1[, 1])), digits = 3))
        }
      })
      output$iqr3 <- renderText({
        if (length(val$x) < 15 & input$iqr == "TRUE" & length(val$x) >= 2) {
          paste("IQR:", signif(abs(IQR(val$x)), digits = 3))
        }
        else if (length(val$x) >= 15 & input$iqr == "TRUE") {
          paste("IQR:", signif(abs(IQR(store1[, 1])), digits = 3))
        }
      })
      if (length(val$x) < 15 &
          input$iqr == "TRUE" & length(val$x) >= 2) {
        segments(
          quantile(val$x, 1 / 4),
          0.5,
          median(val$x),
          0.5,
          lwd = "4",
          col = "blue"
        )
        segments(
          median(val$x),
          0.5,
          quantile(val$x, 3 / 4),
          0.5,
          lwd = "4",
          col = "blue"
        )
      }
      else if (input$iqr == "TRUE" & length(val$x) >= 15) {
        segments(
          quantile(store1[, 1], 1 / 4),
          0.5,
          median(store1[, 1]),
          0.5,
          lwd = "4",
          col = "blue"
        )
        segments(
          median(store1[, 1]),
          0.5,
          quantile(store1[, 1], 3 / 4),
          0.5,
          lwd = "4",
          col = "blue"
        )
      }
      # feedback function telling to user how many points left
      tellMePoints <- function() {
        count <- length(val$x)
        if(count < 15) {
          remaining <- 15 - count
          paste0("You still need to plot ", tags$strong(remaining), " point(s).")
        }
      }
      # feedback for 'Location' question
      output$feedback1 <- renderText({
        if (b$right == 1) {
          #clicked points are greater and equal than 15
          if (length(val$x) >= 15) {
            if (signif(median(store1[, 1]), digits = 2) ==
                signif(mean(store1[, 1]), 2)) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                     \nHint: think about the relation of mean and median of
                     a symmetic distribution.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (b$right == 2) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) >
                 signif(mean(store1[, 1]), 2))) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                     \nHint: think about the relation of mean and median.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (b$right == 3) {
          if (length(val$x) >= 15) {
            if (signif(median(val$x), digits = 2) < 1 &&
                (signif(mean(val$x) > 4, digits = 2))) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                     \nHint: you can check mean and median from the hint box.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (b$right == 4) {
          if (length(val$x) >= 15) {
            if (median(store1[, 1]) > 9.5 && mean(store1[, 1]) < 6) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                     \nHint: you can check mean and median from the hint box.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (b$right == 5) {
          if (length(val$x) >= 15) {
            if (abs(median(store1[, 1]) - mean(store1[, 1])) > 2) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                     \nHint: You can see mean and median from the hint box
                     to see the current difference.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }

        else if (b$right == 6) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) <
                 signif(mean(store1[, 1]), 2))) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                     \nHint: think about the relation of mean and median.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (b$right == 7) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) <
                 signif(mean(store1[, 1]), 2))) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                     \nHint: think about left-skewed or right-skewed", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
      })
      output$feedback2 <- renderText({
        if (c$right == 1) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) > 3 && IQR(store1[, 1]) > 3) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: you can check SD and IQR from the hint box.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 2) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1]), digits = 2) <
                signif(IQR(store1[, 1]), digits = 2)) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: think about the variation of the points.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 3) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) < 1 && IQR(store1[, 1]) < 1) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              if (sd(store1[, 1]) < 1 &&  IQR(store1[, 1]) > 1) {
                result("Sorry, IQR is more than 1. Please click Clear Points
                       and try again.", FALSE)
              }
              else if (sd(store1[, 1]) > 1 &&
                       IQR(store1[, 1]) < 1) {
                result("Sorry, SD is more than 1. Please click Clear Points
                       and try again.", FALSE)
              }
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 4) {
          if (length(val$x) >= 15) {
            if (signif(IQR(store1[, 1]), digits = 2) == 0 &&
                signif(sd(store1[, 1]) > 1, digits = 2)) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: think about the variation of the points.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 5) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1]) == 0, digits = 1)) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: think about the variation of the points.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 6) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1]), digits = 2) > signif(IQR(store1[, 1]),
                                                             digits = 2)) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: you can check SD and IQR from the hint box.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 7) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) < 2 && IQR(store1[, 1]) < 2) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              if (sd(store1[, 1]) < 2 &&  IQR(store1[, 1]) > 2) {
                result("Sorry, IQR is more than 2. Please click Clear Points
                       and try again.", FALSE)
              }
              else if (sd(store1[, 1]) > 2 &&
                       IQR(store1[, 1]) < 2) {
                result("Sorry, SD is more than 2. Please click Clear Points
                       and try again.", FALSE)
              }
            }
          }
          else {
            tellMePoints()
          }
        }
      })
      output$feedback3 <- renderText({
        if (d$right == 1) {
          if (length(val$x) >= 15) {
            if (abs(signif(median(store1[, 1]), digits = 2) ==
                    signif(mean(store1[, 1]), digits = 2))) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: think about the relation of mean and median of
                     a symmetic distribution.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 2) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) >
                 signif(mean(store1[, 1]), 2))) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: think about the relation of mean and median.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 3) {
          if (length(val$x) >= 15) {
            if (median(val$x) < 1 && mean(val$x) > 4) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: you can check mean and median from the hint box.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 4) {
          if (length(val$x) >= 15) {
            if (median(store1[, 1]) > 9.5 && mean(store1[, 1]) < 6) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: you can check mean and median from the hint box.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 5) {
          if (length(val$x) >= 15) {
            if (abs(median(store1[, 1]) - mean(store1[, 1])) > 2) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: You can see mean and median from the hint box
                     to see the current difference.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 6) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) > 3 && IQR(store1[, 1]) > 3) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: you can check SD and IQR from the hint box.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 7) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1]), digits = 2) < signif(IQR(store1[, 1]),
                                                             digits = 2)) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: you can check SD and IQR from the hint box.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 8) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) < 1 && IQR(store1[, 1]) < 1) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              if (sd(store1[, 1]) < 1 &&  IQR(store1[, 1]) > 1) {
                result("Sorry, IQR is more than 1. Please click Clear Points
                       and try again.", FALSE)
              }
              else if (sd(store1[, 1]) > 1 &&
                       IQR(store1[, 1]) < 1) {
                result("Sorry, SD is more than 1. Please click Clear Points
                       and try again.", FALSE)
              }
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 9) {
          if (length(val$x) >= 15) {
            if (signif(IQR(store1[, 1]) == 0, digits = 1) &&
                sd(store1[, 1]) > 1) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: think about the variation of the points.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 10) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1]) == 0, digits = 1)) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: think about the variation of the points.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }

        else if (d$right == 11) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) <
                 signif(mean(store1[, 1]), 2))) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: you can check SD and IQR from the hint box.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 12) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1])) > signif(IQR(store1[, 1]))) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: you can check SD and IQR from the hint box.", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 13) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) <
                 signif(mean(store1[, 1]), 2))) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              result("Sorry, Please click Clear Points and try again.
                      \nHint: think about left-skewed or right-skewed", FALSE)
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 14) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) < 2 && IQR(store1[, 1]) < 2) {
              result("Congratulations, you are right!", TRUE)
            }
            else{
              if (sd(store1[, 1]) < 2 &&  IQR(store1[, 1]) > 2) {
                result("Sorry, IQR is more than 2. Please click Clear Points
                       and try again.", FALSE)
              }
              else if (sd(store1[, 1]) > 2 &&
                       IQR(store1[, 1]) < 2) {
                result("Sorry, SD is more than 2. Please click Clear Points
                       and try again.", FALSE)
              }
            }
          }
          else {
            tellMePoints()
          }
        }
      })
    })
  })
}

# App Call----
boastUtils::boastApp(ui = ui, server = server)
