library(shinydashboard)
library(shiny)
library(plotrix)
library(grid)
library(shinyjs)
library(shinyBS)

bank<- read.csv("questionbank.csv")
bank= data.frame(lapply(bank,as.character),stringsAsFactors = FALSE)

jsResetCode <- "shinyjs.reset= function() {history.go(0)}"

shinyServer(function(session,input, output) {
  
  output$text1 <- renderText({paste("Acknowledgement:",
                                    "This app was developed and coded by Qichao Chen. Special thanks to Yuxin Zhang, Sitong Liu and Yingjie Wang for 
                                     help on some programming issues. Also, thanks to Zibin Gao as the team partner.")})
  
  output$text2 <- renderText({ paste("Warning: You must click points in plot before you click 'Check' button.") })
  
  # Level 1
  var.l1 <- reactiveValues(x = NULL, y = NULL)
 
  
  observe({
    # Initially will be empty
    if (is.null(input$Click.l1)){
      return()
    }
    
    isolate({
      var.l1$x <- c(var.l1$x, input$Click.l1$x)
      var.l1$y <- c(var.l1$y, input$Click.l1$y)
    })
  })
  
  #fresh the whole page
  observeEvent(
    input$reset_button.l1,{js$reset()}
  )
  
  
  
  output$distPlot.l1 <- renderPlot({
    
    isolate({
      plot(c(0,1),c(0,1), type = 'n',xaxt='n', yaxt='n',ann=FALSE)
      
    })
    
    col1.l1 <- rgb(red = .0, green = 0, blue = 1, alpha = 0.3)
    draw.circle(var.l1$x[1],var.l1$y[1],input$radius.l1,col=col1.l1)
    
    
    
    
  },width=500,height = 500)
  
    # observe( 
    #    if(!is.null(input$Click.l1)){
    #       updateButton(session, "calculate.l1",disabled = FALSE)
    #       
    #    })
   
  
  
  # using points simulating prob
  
  probability.l1 <- reactiveValues(
    
    prob.c1.l1= NULL
  )
  
  
  observe({
    if(input$calculate.l1>0){
      subdivide.l1 <- 400
      
      
      x.coord.l1 <- NULL
      y.coord.l1 <- NULL
      
      
      for(i in 1:subdivide.l1){
        
        x.coord.l1 <- c(x.coord.l1,seq(from = 1/subdivide.l1, to=1, by = 1/subdivide.l1))
        y.coord.l1 <- c(y.coord.l1,rep(i/subdivide.l1,subdivide.l1))
        
      }
      
      sample.space.l1 <-data.frame(x.coord.l1,y.coord.l1)
      
      
      sample.space.l1$radius.l1 <- input$radius.l1
      sample.space.l1$x.center.l1 <- var.l1$x[1]
      sample.space.l1$y.center.l1 <- var.l1$y[1]
      
      sample.space.l1$diff.l1 <- sqrt((sample.space.l1$x.center.l1-sample.space.l1$x.coord.l1)^2+(sample.space.l1$y.center.l1-sample.space.l1$y.coord.l1)^2)
      sample.space.l1$in.c1.l1<- sample.space.l1$diff.l1 <= sample.space.l1$radius.l1
      
      prob.l1 <- mean(sample.space.l1$in.c1.l1)
      probability.l1$prob.c1.l1<-signif(prob.l1,2) 
 
    }
   
  })
  
  
  #blue botton
  observe({
  if(input$labell1==0){  
    output$labeldol1 <- renderUI({
      bsButton("dol1"," ", class = "btn-group")
    })}
  else{
    output$labeldol1 <- renderUI({
      bsButton("dol1","A", class = "btn-group")
    })
  }
   })

  output$PA.l1<- renderPrint(cat(probability.l1$prob.c1.l1))
  
  
  
  ### random choose question
  numbers.l1 <- reactiveValues( quesanswer.l1=c())
  
  observe({
    numbers.l1$quesanswer.l1=sample(1:5,1)
  })
  
  
  
  #question
  
  output$question.l1<-renderText(bank[numbers.l1$quesanswer.l1,4])
  
  
  output$answer.l1 <- renderPrint({
    if(input$calculate.l1 >0){
    
    if(probability.l1$prob.c1.l1==bank[numbers.l1$quesanswer.l1,5])
    {cat("Great! You are right!")}
    else{
      cat("Change the size of your circle to match the probability desired.")
    }
    }
    else{
      cat("\n")
    }
  })
  
  
  #level 2
  
  var.l2 <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    # Initially will be empty
    if (is.null(input$Click.l2)){
      return()
    }
    
    isolate({
      var.l2$x <- c(var.l2$x, input$Click.l2$x)
      var.l2$y <- c(var.l2$y, input$Click.l2$y)
    })
  })
  
  #fresh the whole page
  observeEvent(
    input$reset_button.l2,{js$reset()}
  )
  
  
  output$distPlot.l2 <- renderPlot({
    
    
    
    isolate({
      plot(c(0,1),c(0,1), type = 'n',xaxt='n', yaxt='n',ann=FALSE)
      
    })
    
    col1.l2 <- rgb(red = .0, green = 0, blue = 1, alpha = 0.3)
    col2.l2 <- rgb(red = 0, green = 1, blue = 0, alpha = 0.3)
    draw.circle(var.l2$x[1],var.l2$y[1],input$radius.l2,col=col1.l2)
    
    
    if (input$reset2l2 == 0){
      draw.circle(var.l2$x[2],var.l2$y[2],input$radius2.l2,col=col2.l2)
    }else{
      draw.circle(input$move.l2,input$move2.l2,input$radius2.l2,col=col2.l2)
    }
    
    
    
    
  },width=500,height = 500)
  
 
  
  
  probability.l2 <- reactiveValues(
    
    prob.c1.l2= NULL,
    prob.c2.l2= NULL,
    prob.intersection.l2= NULL,
    d.l2= NULL
    
  )
  
  observe({
    if(input$calculate.l2>0){
      
      if (input$reset2l2 == 0){
        probability.l2$d.l2 <- ((var.l2$x[1]-var.l2$x[2])^2+(var.l2$y[1]-var.l2$y[2])^2)^0.5
      }else{
        probability.l2$d.l2 <- ((var.l2$x[1]-input$move.l2)^2+(var.l2$y[1]-input$move2.l2)^2)^0.5
      }
      
      
      subdivide.l2 <- 400
      
      
      x.coord.l2 <- NULL
      y.coord.l2 <- NULL
      
      for(i in 1:subdivide.l2){
        
        x.coord.l2 <- c(x.coord.l2,seq(from = 1/subdivide.l2, to=1, by = 1/subdivide.l2))
        y.coord.l2 <- c(y.coord.l2,rep(i/subdivide.l2,subdivide.l2))
        
      }
      
      sample.space.l2 <-data.frame(x.coord.l2,y.coord.l2)
      
      
      sample.space.l2$radius.c1.l2 <- input$radius.l2
      sample.space.l2$radius.c2.l2 <- input$radius2.l2
      sample.space.l2$x.center.c1.l2<- var.l2$x[1]
      sample.space.l2$y.center.c1.l2 <- var.l2$y[1]
      
      if (input$reset2l2 == 0){
        sample.space.l2$x.center.c2.l2 <- var.l2$x[2]
        sample.space.l2$y.center.c2.l2 <- var.l2$y[2]
      }else{
        sample.space.l2$x.center.c2.l2 <- input$move.l2
        sample.space.l2$y.center.c2.l2 <- input$move2.l2
      }
      
      
      
      
      sample.space.l2$diff.c1.l2 <- sqrt((sample.space.l2$x.center.c1.l2-sample.space.l2$x.coord.l2)^2+(sample.space.l2$y.center.c1.l2-sample.space.l2$y.coord.l2)^2)
      sample.space.l2$in.c1.l2<- sample.space.l2$diff.c1.l2 <= sample.space.l2$radius.c1.l2
      
      sample.space.l2$diff.c2.l2 <- sqrt((sample.space.l2$x.center.c2.l2-sample.space.l2$x.coord.l2)^2+(sample.space.l2$y.center.c2.l2-sample.space.l2$y.coord.l2)^2)
      sample.space.l2$in.c2.l2<- sample.space.l2$diff.c2.l2 <= sample.space.l2$radius.c2.l2
      
      sample.space.l2$intersection.l2 <- (sample.space.l2$diff.c1.l2 <= sample.space.l2$radius.c1.l2) & (sample.space.l2$diff.c2.l2 <= sample.space.l2$radius.c2.l2)
      
      p1.l2<- mean(sample.space.l2$in.c1.l2)
      probability.l2$prob.c1.l2<-signif(p1.l2,2)
      
      p2.l2<- mean(sample.space.l2$in.c2.l2)
      probability.l2$prob.c2.l2<-signif(p2.l2,2)
      
      p3.l2 <- mean(sample.space.l2$intersection.l2)
      probability.l2$prob.intersection.l2<-signif(p3.l2,2)
      
      
    }
  })
  
  
  actionButton("do4l2","AandB", class = "btn-group")
  
  #button
  
  observe({
    if(input$labell2==0){
      output$labeldoBl2 <- renderUI({
        bsButton("dol2"," ", class = "btn-group")
      })
      
      output$labeldoGl2 <- renderUI({
        bsButton("do2l2"," ", class = "btn-group")
      })
      
      output$labeldoBGl2 <- renderUI({
        bsButton("do4l2"," ", class = "btn-group")
      })
      
    }
    else{
      output$labeldoBl2 <- renderUI({
        bsButton("dol2","A", class = "btn-group")
      })
      
      output$labeldoGl2 <- renderUI({
        bsButton("do2l2","B", class = "btn-group")
      })
      
      output$labeldoBGl2 <- renderUI({
        bsButton("do4l2","AandB", class = "btn-group")
      })
    }
  })
  
  output$PA.l2<- renderPrint(cat(probability.l2$prob.c1.l2))
  output$PB.l2<- renderPrint(cat(probability.l2$prob.c2.l2))
  output$AB.l2<- renderPrint(cat(probability.l2$prob.intersection.l2))
  
  
  ### random choose question
  numbers.l2 <- reactiveValues( quesanswer.l2=c())
  
  observe({
    numbers.l2$quesanswer.l2=sample(6:10,1)
  })
  
  output$question.l2<-renderText(bank[numbers.l2$quesanswer.l2,4])
  
  
  output$answer.l2 <- renderPrint({
    if(input$calculate.l2>0){
      
      if((probability.l2$prob.c1.l2==bank[numbers.l2$quesanswer.l2,5])&(probability.l2$prob.c2.l2==bank[numbers.l2$quesanswer.l2,6])& (probability.l2$prob.intersection.l2 == bank[numbers.l2$quesanswer.l2,8])) 
      {cat("Great! You are right!")}
      else if((probability.l2$prob.c1.l2==bank[numbers.l2$quesanswer.l2,5])&(probability.l2$prob.c2.l2==bank[numbers.l2$quesanswer.l2,6])&(probability.l2$d.l2 >= bank[numbers.l2$quesanswer.l2,8])) {
        cat("Probability is right, but the relationship is Wrong. Try again.")
      }
      else if(((probability.l2$prob.c1.l2!=bank[numbers.l2$quesanswer.l2,5]) | (probability.l2$prob.c2.l2!=bank[numbers.l2$quesanswer.l2,6]) )&(probability.l2$d.l2 == bank[numbers.l2$quesanswer.l2,8])){
        cat("The relationship is right, but the probability is Wrong. Try again.")
      }
      else{
        cat("Change the size and placement of your circles to match the probabilities desired.")
      }
    }
    else{
      cat("\n")
    }
  })
  
  
  
  # level 3
  
  var.l3 <- reactiveValues(x = NULL, y = NULL)
  
  observe({
    # Initially will be empty
    if (is.null(input$Click.l3)){
      return()
    }
    
    isolate({
      var.l3$x <- c(var.l3$x, input$Click.l3$x)
      var.l3$y <- c(var.l3$y, input$Click.l3$y)
    })
  })
  
  #fresh the whole page
  observeEvent(
    input$reset_button.l3,{js$reset()}
  )
  
  
  
  output$distPlot.l3 <- renderPlot({
    
    
    
    isolate({
      plot(c(0,1),c(0,1), type = 'n',xaxt='n', yaxt='n',ann=FALSE)
      
    })
    
    col1.l3 <- rgb(red = .0, green = 0, blue = 1, alpha = 0.3)
    col2.l3 <- rgb(red = 0, green = 1, blue = 0, alpha = 0.3)
    col3.l3 <- rgb(red = 1, green = 0, blue = 0, alpha = 0.3)
    draw.circle(var.l3$x[1],var.l3$y[1],input$radius.l3,col=col1.l3)
    
    if (input$reset1l3 == 0){
      draw.circle(var.l3$x[2],var.l3$y[2],input$radius2.l3,col=col2.l3)
    }else{
      draw.circle(input$move.l3,var.l3$y[2],input$radius2.l3,col=col2.l3)
    }
    
    if (input$reset2l3 == 0){
      draw.circle(var.l3$x[3],var.l3$y[3],input$radius3.l3,col=col3.l3)
    }else{
      draw.circle(input$move2.l3,input$move3.l3,input$radius3.l3,col=col3.l3)
    }
    
    
    
    
    
    
    
  },width=500,height = 500)
  
  
  probability.l3 <- reactiveValues(
    
    prob.c1.l3= NULL,
    prob.c2.l3= NULL,
    prob.c3.l3= NULL,
    intersection.c12.l3= NULL,
    intersection.c23.l3= NULL,
    intersection.c13.l3= NULL,
    intersection.c123.l3= NULL,
    d12.l3= NULL,
    d23.l3= NULL,
    d13.l3= NULL
    
    
  )
  
  observe({
    if(input$calculatel3>0){
      
      if (input$reset1l3 == 0){
        probability.l3$d12.l3 <- ((var.l3$x[1]-var.l3$x[2])^2+(var.l3$y[1]-var.l3$y[2])^2)^0.5
        probability.l3$d23.l3 <- ((var.l3$x[3]-var.l3$x[2])^2+(var.l3$y[3]-var.l3$y[2])^2)^0.5
        probability.l3$d13.l3 <- ((var.l3$x[1]-var.l3$x[3])^2+(var.l3$y[1]-var.l3$y[3])^2)^0.5
      }else{
        probability.l3$d12.l3 <- ((var.l3$x[1]-input$move)^2+(var.l3$y[1]-var.l3$y[2])^2)^0.5
        probability.l3$d23.l3 <- ((var.l3$x[3]-input$move)^2+(var.l3$y[3]-var.l3$y[2])^2)^0.5
        probability.l3$d13.l3 <- ((var.l3$x[1]-var.l3$x[3])^2+(var.l3$y[1]-var.l3$y[3])^2)^0.5
      }
      
      if (input$reset2l3 == 0){
        probability.l3$d12.l3 <- ((var.l3$x[1]-var.l3$x[2])^2+(var.l3$y[1]-var.l3$y[2])^2)^0.5
        probability.l3$d23.l3 <- ((var.l3$x[3]-var.l3$x[2])^2+(var.l3$y[3]-var.l3$y[2])^2)^0.5
        probability.l3$d13.l3 <- ((var.l3$x[1]-var.l3$x[3])^2+(var.l3$y[1]-var.l3$y[3])^2)^0.5
      }else{
        probability.l3$d12.l3 <- ((var.l3$x[1]-var.l3$x[2])^2+(var.l3$y[1]-var.l3$y[2])^2)^0.5
        probability.l3$d23.l3 <- ((input$move2.l3-var.l3$x[2])^2+(input$move3.l3-var.l3$y[2])^2)^0.5
        probability.l3$d13.l3 <- ((var.l3$x[1]-input$move2.l3)^2+(var.l3$y[1]-input$move3.l3)^2)^0.5
      }
      
      
      
      subdivide.l3 <- 400
      
      
      x.coord.l3 <- NULL
      y.coord.l3 <- NULL
      
      for(i in 1:subdivide.l3){
        
        x.coord.l3 <- c(x.coord.l3,seq(from = 1/subdivide.l3, to=1, by = 1/subdivide.l3))
        y.coord.l3 <- c(y.coord.l3,rep(i/subdivide.l3,subdivide.l3))
        
      }
      
      sample.space.l3 <-data.frame(x.coord.l3,y.coord.l3)
      
      
      sample.space.l3$radius.c1.l3 <- input$radius.l3
      sample.space.l3$radius.c2.l3 <- input$radius2.l3
      sample.space.l3$radius.c3.l3 <- input$radius3.l3
      sample.space.l3$x.center.c1.l3 <- var.l3$x[1]
      sample.space.l3$y.center.c1.l3 <- var.l3$y[1]
      
      if (input$reset1l3 == 0){
        sample.space.l3$x.center.c2.l3 <- var.l3$x[2]
      }else{
        sample.space.l3$x.center.c2.l3 <- input$move.l3
      }
      sample.space.l3$y.center.c2.l3 <- var.l3$y[2]
      
      if (input$reset2l3 == 0){
        sample.space.l3$x.center.c3.l3 <- var.l3$x[3]
        sample.space.l3$y.center.c3.l3 <- var.l3$y[3]
      }else{
        sample.space.l3$x.center.c3.l3 <- input$move2.l3
        sample.space.l3$y.center.c3.l3 <- input$move3.l3
      }
      
      
      
      sample.space.l3$diff.c1.l3 <- sqrt((sample.space.l3$x.center.c1.l3-sample.space.l3$x.coord.l3)^2+(sample.space.l3$y.center.c1.l3-sample.space.l3$y.coord.l3)^2)
      sample.space.l3$in.c1.l3<- sample.space.l3$diff.c1.l3 <= sample.space.l3$radius.c1.l3
      
      sample.space.l3$diff.c2.l3 <- sqrt((sample.space.l3$x.center.c2.l3-sample.space.l3$x.coord.l3)^2+(sample.space.l3$y.center.c2.l3-sample.space.l3$y.coord.l3)^2)
      sample.space.l3$in.c2.l3<- sample.space.l3$diff.c2.l3 <= sample.space.l3$radius.c2.l3
      
      sample.space.l3$diff.c3.l3 <- sqrt((sample.space.l3$x.center.c3.l3-sample.space.l3$x.coord.l3)^2+(sample.space.l3$y.center.c3.l3-sample.space.l3$y.coord.l3)^2)
      sample.space.l3$in.c3.l3<- sample.space.l3$diff.c3.l3 <= sample.space.l3$radius.c3.l3
      
      sample.space.l3$intersection.c12.l3 <- (sample.space.l3$diff.c1.l3 <= sample.space.l3$radius.c1.l3) & (sample.space.l3$diff.c2.l3 <= sample.space.l3$radius.c2.l3)
      sample.space.l3$intersection.c23.l3 <- (sample.space.l3$diff.c3.l3 <= sample.space.l3$radius.c3.l3) & (sample.space.l3$diff.c2.l3 <= sample.space.l3$radius.c2.l3)
      sample.space.l3$intersection.c13.l3 <- (sample.space.l3$diff.c1.l3 <= sample.space.l3$radius.c1.l3) & (sample.space.l3$diff.c3.l3 <= sample.space.l3$radius.c3.l3)
      sample.space.l3$intersection.c123.l3 <- (sample.space.l3$diff.c1.l3 <= sample.space.l3$radius.c1.l3) & (sample.space.l3$diff.c2.l3 <= sample.space.l3$radius.c2.l3) & (sample.space.l3$diff.c3.l3 <= sample.space.l3$radius.c3.l3)
      
      p1.l3<- mean(sample.space.l3$in.c1.l3)
      probability.l3$prob.c1.l3<-signif(p1.l3,2)
      
      p2.l3<- mean(sample.space.l3$in.c2.l3)
      probability.l3$prob.c2.l3<-signif(p2.l3,2)
      
      p3.l3 <- mean(sample.space.l3$in.c3.l3)
      probability.l3$prob.c3.l3<-signif(p3.l3,2)
      
      p12.l3 <- mean(sample.space.l3$intersection.c12.l3)
      probability.l3$intersection.c12.l3<-signif(p12.l3,2)
      
      p23.l3 <- mean(sample.space.l3$intersection.c23.l3)
      probability.l3$intersection.c23.l3<-signif(p23.l3,2)
      
      p13.l3 <- mean(sample.space.l3$intersection.c13.l3)
      probability.l3$intersection.c13.l3<-signif(p13.l3,2)
      
      p123.l3 <- mean(sample.space.l3$intersection.c123.l3)
      probability.l3$intersection.c123.l3<-signif(p123.l3,2)
    }
  })
  
  
  observe({
    if(input$labell3==0){
      output$labeldoBl3 <- renderUI({
        actionButton("dol3"," ", class = "btn-group")
      })
      
      output$labeldoGl3 <- renderUI({
        actionButton("do2l3"," ", class = "btn-group")
      })
      
      output$labeldoRl3 <- renderUI({
        actionButton("do3l3"," ", class = "btn-group")
      })
      
      output$labeldoBGl3 <- renderUI({
        actionButton("do4l3"," ", class = "btn-group")
      })
      
      output$labeldoBRl3 <- renderUI({
        actionButton("do5l3"," ", class = "btn-group")
      })
      
      output$labeldoGRl3 <- renderUI({
        actionButton("do6l3"," ", class = "btn-group")
      })
      
      output$labeldoBGRl3 <- renderUI({
        actionButton("do7l3"," ", class = "btn-group")
      })
    }
    else{
      output$labeldoBl3 <- renderUI({
        actionButton("dol3","A", class = "btn-group")
      })
      
      output$labeldoGl3 <- renderUI({
        actionButton("do2l3","B", class = "btn-group")
      })
      
      output$labeldoRl3 <- renderUI({
        actionButton("do3l3","C", class = "btn-group")
      })
      
      output$labeldoBGl3 <- renderUI({
        actionButton("do4l3","AandB ", class = "btn-group")
      })
      
      output$labeldoBRl3 <- renderUI({
        actionButton("do5l3","AandC", class = "btn-group")
      })
      
      output$labeldoGRl3 <- renderUI({
        actionButton("do6l3","BandC", class = "btn-group")
      })
      
      output$labeldoBGRl3 <- renderUI({
        actionButton("do7l3","ABC", class = "btn-group")
      })
    }
  })
  
  #blue button
  output$PA.l3<- renderPrint(cat(probability.l3$prob.c1.l3))
  #green button
  output$PB.l3<- renderPrint(cat(probability.l3$prob.c2.l3))
  #red button
  output$PC.l3<- renderPrint(cat(probability.l3$prob.c3.l3))
  #cyan button
  output$AB.l3<- renderPrint(cat(probability.l3$intersection.c12.l3))
  #purple button
  output$AC.l3<- renderPrint(cat(probability.l3$intersection.c13.l3))
  #darkolivegreen button
  output$BC.l3<- renderPrint(cat(probability.l3$intersection.c23.l3))
  #center button
  output$ABC.l3<- renderPrint(cat(probability.l3$intersection.c123.l3))
  
  
  ### random choose question
  numbers.l3 <- reactiveValues( quesanswer.l3=c())
  
  observe({
    numbers.l3$quesanswer.l3=sample(11:15,1)
  })
  
  output$question.l3<-renderText(bank[numbers.l3$quesanswer.l3,4])
  
  output$answer.l3 <- renderPrint({
    if(input$calculatel3>0){
    
      if(any(bank[numbers.l3$quesanswer.l3,3]==c(11,12,13,14))){
        
      if((probability.l3$prob.c1.l3==bank[numbers.l3$quesanswer.l3,5])&(probability.l3$prob.c2.l3==bank[numbers.l3$quesanswer.l3,6])&(probability.l3$prob.c3.l3==bank[numbers.l3$quesanswer.l3,7])&(probability.l3$intersection.c12.l3 ==bank[numbers.l3$quesanswer.l3,8])&(probability.l3$intersection.c23.l3==bank[numbers.l3$quesanswer.l3,9])&(probability.l3$intersection.c13.l3==bank[numbers.l3$quesanswer.l3,10]))
      {cat("Great! You are right!")}
      else if((probability.l3$prob.c1.l3==bank[numbers.l3$quesanswer.l3,5])&(probability.l3$prob.c2.l3==bank[numbers.l3$quesanswer.l3,6])&(probability.l3$prob.c3.l3==bank[numbers.l3$quesanswer.l3,7])&((probability.l3$intersection.c12.l3!=bank[numbers.l3$quesanswer.l3,8])|(probability.l3$intersection.c23.l3!=bank[numbers.l3$quesanswer.l3,9])|(probability.l3$intersection.c13.l3!=bank[numbers.l3$quesanswer.l3,10])))
      {cat("Probability is right, but the relationship is Wrong. Try again.")}
      else if(((probability.l3$prob.c1.l3!=bank[numbers.l3$quesanswer.l3,5])|(probability.l3$prob.c2.l3!=bank[numbers.l3$quesanswer.l3,6])|(probability.l3$prob.c3.l3!=bank[numbers.l3$quesanswer.l3,7]))&(probability.l3$intersection.c12.l3==bank[numbers.l3$quesanswer.l3,8])&(probability.l3$intersection.c23.l3== bank[numbers.l3$quesanswer.l3,9])&(probability.l3$intersection.c13.l3==bank[numbers.l3$quesanswer.l3,10]))
      {cat("The relationship is right, but the probability is Wrong. Try again.")}
      else{
       cat("Change the size and placement of your circles to match the probabilities desired.")
      }
      }
      
      else if(any(bank[numbers.l3$quesanswer.l3,3]==c(15))){
        if((probability.l3$prob.c1.l3==bank[numbers.l3$quesanswer.l3,5])&(probability.l3$prob.c2.l3==bank[numbers.l3$quesanswer.l3,6])&(probability.l3$prob.c3.l3==bank[numbers.l3$quesanswer.l3,7])&(probability.l3$intersection.c12.l3 ==bank[numbers.l3$quesanswer.l3,8])&(probability.l3$intersection.c23.l3==bank[numbers.l3$quesanswer.l3,9])&(probability.l3$intersection.c13.l3==bank[numbers.l3$quesanswer.l3,10])&(probability.l3$intersection.c123.l3==bank[numbers.l3$quesanswer.l3,11]))
        {cat("Great! You are right!")}
        else if((probability.l3$prob.c1.l3==bank[numbers.l3$quesanswer.l3,5])&(probability.l3$prob.c2.l3==bank[numbers.l3$quesanswer.l3,6])&(probability.l3$prob.c3.l3==bank[numbers.l3$quesanswer.l3,7])&((probability.l3$intersection.c12.l3!=bank[numbers.l3$quesanswer.l3,8])|(probability.l3$intersection.c23.l3!=bank[numbers.l3$quesanswer.l3,9])|(probability.l3$intersection.c13.l3!=bank[numbers.l3$quesanswer.l3,10])|(probability.l3$intersection.c123.l3!=bank[numbers.l3$quesanswer.l3,11])))
        {cat("Probability is right, but the relationship is Wrong. Try again.")}
        else if(((probability.l3$prob.c1.l3!=bank[numbers.l3$quesanswer.l3,5])|(probability.l3$prob.c2.l3!=bank[numbers.l3$quesanswer.l3,6])|(probability.l3$prob.c3.l3!=bank[numbers.l3$quesanswer.l3,7]))&(probability.l3$intersection.c12.l3==bank[numbers.l3$quesanswer.l3,8])&(probability.l3$intersection.c23.l3== bank[numbers.l3$quesanswer.l3,9])&(probability.l3$intersection.c13.l3==bank[numbers.l3$quesanswer.l3,10])&(probability.l3$intersection.c123.l3==bank[numbers.l3$quesanswer.l3,11]))
        {cat("The relationship is right, but the probability is Wrong. Try again.")}
        else{
          cat("Change the size and placement of your circles to match the probabilities desired.")
        }
      }
    }
    else{
      cat("\n")
    }
    
  })
  
 
  
})