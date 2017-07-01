library(shiny)
library(dplyr)
library(tidyr)
shinyServer(function(input, output) {
  
  #alpha <- input$sliderAlpha
  #fw <- input$sliderFW
  
  vname <- vector(mode = "character", length = 20)
  vcoeff <- vector(mode = "integer", length = 20) 
  
  for (i in seq(fw)){
    vname[i] <- paste("a^",as.character(i-1))
    vcoeff[i] <- alpha^(i-1)
    #print(i, vname[i], vcoeff[i])
  }
  vname[1] <- "0"
  vcoeff[1] <- 0
  
  dCoeff <- tbl_df(vcoeff)
  dName <- tbl_df(vname)
  
  #output$tableVcoeff <- tbl_df(vcoeff)  
  #output$tableValpha <- tbl_df(vname)

  reactiveCoeff <- reactive({return(dCoeff) # %>%
                      #filter(carb %in% input$Category))})
#https://stackoverflow.com/questions/36203200/how-to-make-a-dataset-reactive-in-shiny
    })
  
  reactiveCoeff <- reactive({return(dCoeff) # %>%
    #filter(carb %in% input$Category))})
    #https://stackoverflow.com/questions/36203200/how-to-make-a-dataset-reactive-in-shiny
  })
  
  
  output$tableValpha <- renderTable({reactiveAlpha})
  output$tableVcoeff <- renderTable({reactiveCoeff})
  
  

  
  
  
  
  mtcars$mpgsp <- ifelse(mtcars$mpg - 20 > 0, mtcars$mpg - 20, 0)
  model1 <- lm(hp ~ mpg, data = mtcars)
  model2 <- lm(hp ~ mpgsp + mpg, data = mtcars)
  
  model1pred <- reactive({
    mpgInput <- input$sliderMPG
    predict(model1, newdata = data.frame(mpg = mpgInput))
  })
  
  model2pred <- reactive({
    mpgInput <- input$sliderMPG
    predict(model2, newdata = 
              data.frame(mpg = mpgInput,
                         mpgsp = ifelse(mpgInput - 20 > 0,
                                        mpgInput - 20, 0)))
  })
  
  output$plot1 <- renderPlot({
    mpgInput <- input$sliderMPG
    
    plot(mtcars$mpg, mtcars$hp, xlab = "Miles Per Gallon", 
         ylab = "Horsepower", bty = "n", pch = 16,
         xlim = c(10, 35), ylim = c(50, 350))
    if(input$showModel1){
      abline(model1, col = "red", lwd = 2)
    }
    if(input$showModel2){
      model2lines <- predict(model2, newdata = data.frame(
        mpg = 10:35, mpgsp = ifelse(10:35 - 20 > 0, 10:35 - 20, 0)
      ))
      lines(10:35, model2lines, col = "blue", lwd = 2)
    }
    legend(25, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16, 
           col = c("red", "blue"), bty = "n", cex = 1.2)
    points(mpgInput, model1pred(), col = "red", pch = 16, cex = 2)
    points(mpgInput, model2pred(), col = "blue", pch = 16, cex = 2)
  })
  
  output$pred1 <- renderText({
    model1pred()
  })
  
  output$pred2 <- renderText({
    model2pred()
  })
})