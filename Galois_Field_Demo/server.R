library(shiny)
library(dplyr)
library(tidyr)
library(xtable )
shinyServer(function(input, output) {
  
 

  output$t1 <- renderUI({
    
    alpha <- input$sliderAlpha
    fw <- input$sliderFW
    
    va <- venc
    vname <- vector(mode = "character", length = fw)
    vcoeff <- vector(mode = "integer", length = fw) 
    seqfw <- seq(fw)
    
    for (i in seqfw ){
      vname[i] <- paste("a^{",as.character(i-2),"}")
      vcoeff[i] <- alpha^(i-1)
      #print(i, vname[i], vcoeff[i])
    }
    
    vname[1] <- "0"
    vcoeff[1] <- 0
    va
    
    dCoeff <- tbl_df(vcoeff)
    dName <- tbl_df(vname)
    
    M <- matrix(rbind(vname, vcoeff) ,nrow=2)
    rownames(M) <- c('function',"value")
    M <- print(xtable(M, align=rep("c", ncol(M)+1)), 
               floating=FALSE, tabular.environment="array", comment=FALSE, print.results=FALSE)
    html <- paste0("$$\\huge", M, "$$")
    
    print(M)
    
    list(
      withMathJax(HTML(html))
    )
  })
    
    
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