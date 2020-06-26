library(boastUtils)
#Let`s begin
server <- function(input, output, session) {
  val <- reactiveValues(x = NULL, y = NULL)
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
  })
  c <- reactiveValues(right = c(sample(1:7, 1)))
  d <- reactiveValues(right = c(sample(1:14, 1)))
  observeEvent(input$bs, {
    c$right = sample(1:7, 1)
    d$right = sample(1:14, 1)
  })
  observe({
    if (input$bs >= 0) {
      val$x <- NULL
      val$y <- NULL
    }
  })
  # Clear the points on 'clear' button click
  observe({
    if (input$clear >= 0) {
      val$x <- NULL
      val$y <- NULL
    }
  })
  observeEvent(input$nextbutton, {
    updateTabItems(session, "tabs", "prerequisite1")
  })
  observeEvent(input$start, {
    updateTabItems(session, "tabs", "game")
  })
  # observeEvent(input$inst1, {
  #   sendSweetAlert(
  #     session = session,
  #     title = "Instruction:",
  #     type = "info",
  #     tags$ol(
  #       tags$li('Select the Location, Variation, or Random challenge to start the game.'),
  #       tags$li('Create points by clicking in the plot to add points and watch your results until you have 15 points total.'),
  #       tags$li('Show summary statistics as you go along by checking the corresponding box.'),
  #       tags$li('There will be a message showing your results on the top of the plot.'),
  #       tags$li('Click Clear Points and click New Challenge to start over.')
  #             )
  #     )
  # })
  # 'Location' question
  output$questionforL <- renderText({
    if (c$right == 1) {
      "Challenge: Please use 15 points to make them roughly symmetric."
    }
    else if (c$right == 2) {
      "Challenge: Please use 15 points to make them left skewed."
    }
    else if (c$right == 3) {
      "Challenge: Please use 15 points to make the median less than 1 and the mean greater than 4."
    }
    else if (c$right == 4) {
      "Challenge: Please use 15 points to make the median greater than 9.5 and the mean less than 6."
    }
    else if (c$right == 5) {
      "Challenge: Please use 15 points to make the absolute difference between the mean and median at least 2."
    }
    else if (c$right == 6) {
      "Challenge: Please use 15 points to make them right skewed."
    }
    else if (c$right == 7) {
      "Challenge: Please use 15 points to make the mean greater than median."
    }
  })
  # 'Variation' question
  output$questionforV <- renderText({
    if (c$right == 1) {
      "Challenge: Please use 15 points to make both the SD and IQR more than 3."
    }
    else if (c$right == 2) {
      "Challenge: Please use 15 points make the IQR bigger than the SD."
    }
    else if (c$right == 3) {
      "Challenge: Please use 15 points to make both the SD and IQR less than 1."
    }
    else if (c$right == 4) {
      "Challenge: Please use 15 points to make the IQR zero and the SD bigger than 1."
    }
    else if (c$right == 5) {
      "Challenge: Please use 15 points to make the SD = zero."
    }
    else if (c$right == 6) {
      "Challenge: Please use 15 points to make the IQR smaller than SD."
    }
    else if (c$right == 7) {
      "Challenge: Please use 15 points to make both the SD and the IQR less than 2."
    }
  })
  # 'Random' question
  output$questionforR <- renderText({
    if (d$right == 1) {
      "Challenge: Please use 15 points to make them roughly symmetric."
    }
    else if (d$right == 2) {
      "Challenge: Please use 15 points to make them left skewed."
    }
    else if (d$right == 3) {
      "Challenge: Please use 15 points to make the median less than 1 and the mean greater than 4."
    }
    else if (d$right == 4) {
      "Challenge: Please use 15 points to make the median greater than 9.5 and the mean less than 6."
    }
    else if (d$right == 5) {
      "Challenge: Please use 15 points to make the absolute difference between the mean and median at least 2."
    }
    else if (d$right == 6) {
      "Challenge: Please use 15 points to make both the SD and IQR more than 3."
    }
    else if (d$right == 7) {
      "Challenge: Please use 15 points make the IQR bigger than the SD."
    }
    else if (d$right == 8) {
      "Challenge: Please use 15 points to make both the SD and IQR less than 1."
    }
    else if (d$right == 9) {
      "Challenge: Please use 15 points to make the IQR zero and the SD bigger than 1."
    }
    else if (d$right == 10) {
      "Challenge: Please use 15 points to make the SD = zero."
    }
    else if (d$right == 11) {
      "Challenge: Please use 15 points to make them right skewed."
    }
    else if (d$right == 12) {
      "Challenge: Please use 15 points to make the IQR smaller than SD."
    }
    else if (d$right == 13) {
      "Challenge: Please use 15 points to make the mean greater than median."
    }
    else if (d$right == 14) {
      "Challenge: Please use 15 points to make both the SD and the IQR less than 2."
    }
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
        cex = 2.5,
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
            length(val$x) >= 1  & input$median   == "TRUE") {
          paste("median:", signif(median(val$x), digits = 3))
        }
        else if (length(val$x) >= 15 & input$median   == "TRUE") {
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
          paste("Absolute Difference:", signif(abs(mean(val$x) - median(val$x)), digits = 3))
        }
        else if (length(val$x) >= 15 &
                 input$mean == "TRUE" & input$median   == "TRUE")
        {
          paste("Absolute Difference:", signif(abs(
            mean(store1[, 1]) - median(store1[, 1])
          ), digits = 3))
        }
      })
      output$mean3 <- renderText({
        if (length(val$x) >= 1 &
            length(val$x) < 15 &
            input$mean == "TRUE" & input$median   == "TRUE") {
          paste("Absolute Difference:", signif(abs(mean(val$x) - median(val$x)), digits = 2))
        }
        else if (length(val$x) >= 15 &
                 input$mean == "TRUE" & input$median   == "TRUE")
        {
          paste("Absolute Difference:", signif(abs(
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
        points.default(mean(val$x),
                       0,
                       pch = 16,
                       cex = 2,
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
        points.default(mean(store1[, 1]), 0, pch = 16, cex = 2)
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
        points.default(median((val$x)),
                       0.5,
                       pch = 8,
                       cex = 2,
                       col = "blue")
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
        points.default(median((store1[, 1])),
                       0.5,
                       pch = 8,
                       cex = 2,
                       col = "blue")
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
        if (length(val$x) == 14) {
          "You still need to plot one point"
        }
        else if (length(val$x) == 13) {
          "You still need to plot 2 points"
        }
        else if (length(val$x) == 12) {
          "You still need to plot 3 points"
        }
        else if (length(val$x) == 11) {
          "You still need to plot 4 points"
        }
        else  if (length(val$x) == 10) {
          "You still need to plot 5 points"
        }
        else  if (length(val$x) == 9) {
          "You still need to plot 6 points"
        }
        else   if (length(val$x) == 8) {
          "You still need to plot 7 points"
        }
        else if (length(val$x) == 7) {
          "You still need to plot 8 points"
        }
        else  if (length(val$x) == 6) {
          "You still need to plot 9 points"
        }
        else  if (length(val$x) == 5) {
          "You still need to plot 10 points"
        }
        else if (length(val$x) == 4) {
          "You still need to plot 11 points"
        }
        else  if (length(val$x) == 3) {
          "You still need to plot 12 points"
        }
        else   if (length(val$x) == 2) {
          "You still need to plot 13 points"
        }
        else  if (length(val$x) == 1) {
          "You still need to plot 14 points"
        }
      }
      # feedback for 'Location' question
      output$diff1 <- renderText({
        if (c$right == 1) {
          #clicked points are greater and equal than 15
          if (length(val$x) >= 15) {
            if (signif(median(store1[, 1]), digits = 2) == signif(mean(store1[, 1]), 2)) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about the relation of mean and median of a symmetic distribution."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 2) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) > signif(mean(store1[, 1]), 2))) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about the relation of mean and median."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 3) {
          if (length(val$x) >= 15) {
            if (signif(median(val$x), digits = 2) < 1 &&
                (signif(mean(val$x) > 4, digits = 2))) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: you can check mean and median by clicking the boxes on the top."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 4) {
          if (length(val$x) >= 15) {
            if (median(store1[, 1]) > 9.5 && mean(store1[, 1]) < 6) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: you can check mean and median by clicking the boxes on the top."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 5) {
          if (length(val$x) >= 15) {
            if (abs(median(store1[, 1]) - mean(store1[, 1])) > 2) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: You can check mean and median boxes to see the current difference."
            }
          }
          else {
            tellMePoints()
          }
        }
        
        else if (c$right == 6) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) < signif(mean(store1[, 1]), 2))) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about the relation of mean and median."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 7) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) < signif(mean(store1[, 1]), 2))) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about left-skewed or right-skewed"
            }
          }
          else {
            tellMePoints()
          }
        }
      })
      # feedback for 'Variation' question
      output$diff2 <- renderText({
        if (c$right == 1) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) > 3 && IQR(store1[, 1]) > 3) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: you can check SD and IQR by clicking the boxes on the top."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 2) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1]), digits = 2) < signif(IQR(store1[, 1]), digits = 2)) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about the variation of the points."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 3) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) < 1 && IQR(store1[, 1]) < 1) {
              "Congratulations,you are right !"
            }
            else{
              if (sd(store1[, 1]) < 1 &&  IQR(store1[, 1]) > 1) {
                "Sorry, IQR is more than 1. Please click Clear Points and try again."
              }
              else if (sd(store1[, 1]) > 1 &&
                       IQR(store1[, 1]) < 1) {
                "Sorry, SD is more than 1. Please click Clear Points and try again."
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
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about the variation of the points."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 5) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1]) == 0, digits = 1)) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about the variation of the points."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 6) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1]), digits = 2) > signif(IQR(store1[, 1]), digits = 2)) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: you can check SD and IQR by clicking the boxes on the top."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (c$right == 7) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) < 2 && IQR(store1[, 1]) < 2) {
              "Congratulations,you are right !"
            }
            else{
              if (sd(store1[, 1]) < 2 &&  IQR(store1[, 1]) > 2) {
                "Sorry, IQR is more than 2. Please click Clear Points and try again."
              }
              else if (sd(store1[, 1]) > 2 &&
                       IQR(store1[, 1]) < 2) {
                "Sorry, SD is more than 2. Please click Clear Points and try again."
              }
            }
          }
          else {
            tellMePoints()
          }
        }
      })
      # feedback for 'Random' question
      output$diff3 <- renderText({
        if (d$right == 1) {
          if (length(val$x) >= 15) {
            if (abs(signif(median(store1[, 1]), digits = 2) == signif(mean(store1[, 1]), digits = 2))) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about the relation of mean and median of a symmetic distribution."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 2) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) > signif(mean(store1[, 1]), 2))) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about the relation of mean and median."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 3) {
          if (length(val$x) >= 15) {
            if (median(val$x) < 1 && mean(val$x) > 4) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: you can check mean and median by clicking the boxes on the top."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 4) {
          if (length(val$x) >= 15) {
            if (median(store1[, 1]) > 9.5 && mean(store1[, 1]) < 6) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: you can check mean and median by clicking the boxes on the top."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 5) {
          if (length(val$x) >= 15) {
            if (abs(median(store1[, 1]) - mean(store1[, 1])) > 2) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: You can check mean and median boxes to see the current difference."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 6) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) > 3 && IQR(store1[, 1]) > 3) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: you can check SD and IQR by clicking the boxes on the top."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 7) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1]), digits = 2) < signif(IQR(store1[, 1]), digits = 2)) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: you can check SD and IQR by clicking the boxes on the top."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 8) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) < 1 && IQR(store1[, 1]) < 1) {
              "Congratulations,you are right !"
            }
            else{
              if (sd(store1[, 1]) < 1 &&  IQR(store1[, 1]) > 1) {
                "Sorry, IQR is more than 1. Please click Clear Points and try again."
              }
              else if (sd(store1[, 1]) > 1 &&
                       IQR(store1[, 1]) < 1) {
                "Sorry, SD is more than 1. Please click Clear Points and try again."
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
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about the variation of the points."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 10) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1]) == 0, digits = 1)) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about the variation of the points."
            }
          }
          else {
            tellMePoints()
          }
        }
        
        else if (d$right == 11) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) < signif(mean(store1[, 1]), 2))) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: you can check SD and IQR by clicking the boxes on the top."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 12) {
          if (length(val$x) >= 15) {
            if (signif(sd(store1[, 1])) > signif(IQR(store1[, 1]))) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: you can check SD and IQR by clicking the boxes on the top."
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 13) {
          if (length(val$x) >= 15) {
            if ((signif(median(store1[, 1]), digits = 2) < signif(mean(store1[, 1]), 2))) {
              "Congratulations,you are right !"
            }
            else{
              "Sorry, Please click Clear Points and try again. \nHint: think about left-skewed or right-skewed"
            }
          }
          else {
            tellMePoints()
          }
        }
        else if (d$right == 7) {
          if (length(val$x) >= 15) {
            if (sd(store1[, 1]) < 2 && IQR(store1[, 1]) < 2) {
              "Congratulations,you are right !"
            }
            else{
              if (sd(store1[, 1]) < 2 &&  IQR(store1[, 1]) > 2) {
                "Sorry, IQR is more than 2. Please click Clear Points and try again."
              }
              else if (sd(store1[, 1]) > 2 &&
                       IQR(store1[, 1]) < 2) {
                "Sorry, SD is more than 2. Please click Clear Points and try again."
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
