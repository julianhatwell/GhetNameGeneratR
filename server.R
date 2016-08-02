library(shiny)
source("environmentControls.R")
ghetInit()

shinyServer(
  function(input, output) {

     newGhetName <- reactive({generateName(phonLength = input$phonLength
                       , gen = input$gen
                       , randomness = input$rnd
                       )
       })
     
      ghetName <- reactive({newGhetName()})
#     dfs <- reactive({n()}-2)
#     st.err <- reactive({1/sqrt(n())})
#     sig <- reactive({ifelse(input$alt == "greater"
#                             , input$sig.level
#                             , input$sig.level/2)})
      output$phons <- renderPrint({input$phonLength})
      output$gen <- renderPrint({input$gen})
      output$rnd <- renderPrint({input$rnd})
          
      output$ghetName <- renderPrint({ghetName()$name})
#     output$crit <- renderPrint({trimws(paste("Reject the Null hypothesis and accept alternative for values >"
#                                       , round(st.err() * qt(1 - sig(), dfs()), 4)
#                                       , if (input$alt == "two.sided") {
#                                           paste("or <", round( - st.err() * qt(1 - sig(), dfs()), 4), "for a two sided test")
#                                         }
#                                       ))})
#     
#     x <- seq(-1, 1, length.out = 1000)
#     
#     output$dens <- renderPlot({
#       x_null_reject <- x[x > st.err() * qnorm(1 - sig())]
#       x_alt_beta <- x[x < st.err() * qnorm(1 - input$power)] + input$delta
#       plot(x
#           , dnorm(x, sd = st.err())
#           , type = "n"
#           , ylab = ""
#           , yaxt = "n"
#           , xlab = "Effect Size (standard deviation units)")
#       polygon(c(x_alt_beta[length(x_alt_beta)],x_alt_beta)
#               , c(0, dnorm(x_alt_beta, mean = input$delta, sd = st.err()))
#               , col = "pink"
#               , border = "transparent") 
#       polygon(c(x_null_reject[1],x_null_reject)
#               , c(0, dnorm(x_null_reject, sd = st.err()))
#               , col = "light blue"
#               , border = "transparent") 
#       points(x
#           , dnorm(x, sd = st.err())
#           , type = "l"
#           , col = "blue")
#       points(x + input$delta
#              , dnorm(x, sd = st.err())
#              , type = "l"
#              , col = "purple")
#       abline(v = st.err() * qnorm(1 - sig())
#              , lwd = 3
#              , col = "magenta")
#       abline(v = 0
#              , lwd = 1
#              , lty = 2
#              , col = "blue")
#       abline(v = input$delta
#              , lwd = 1
#              , lty = 2
#              , col = "purple")
#       legend("topleft", legend = c(expression(mu[0])
#                                    , expression(mu[a])
#                                    , "Z-statistic"
#                                    , "rejection region"
#                                    , "Type II Error region")
#              , lty = c(2, 2, 1, 1, 1)
#              , lwd = c(1, 1, 3, 10, 10)
#              , col = c("blue", "purple", "magenta", "light blue", "pink")
#              , bty = "n")
#       if (input$alt == "two.sided") {
#         points(x - input$delta
#                , dnorm(x, sd = st.err())
#                , type = "l"
#                , col = "purple")
#         abline(v = - st.err() * qnorm(1 - sig())
#                , lwd = 3
#                , col = "magenta")
#       }
#      })
    })