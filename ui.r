library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)



#fresh the whole page
jsResetCode <- "shinyjs.reset= function() {history.go(0)}"

shinyUI(
  dashboardPage(
    dashboardHeader(title="Probability"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Before you start", tabName = "start", icon = icon("dashboard")),
        menuItem("One Event", tabName = "level1", icon = icon("th")),
        menuItem("Two Events", tabName = "level2", icon = icon("th")),
        menuItem("Three Events", tabName = "level3", icon = icon("th"))
      )
    ),
    dashboardBody(
      
      tags$head( 
        tags$link(rel = "stylesheet", type = "text/css", href = "color.css") 
      ), 
      
      
      
      tabItems(
        
        #before you start
        tabItem(tabName = "start",
                h2(p("About:")),
                h3(p("The goals of this game are to introduce probability, to help you think about the relationships 
                     between events, and how to learn to model real problems using Venn diagrams.")),
                hr(),
                h2(p("Instruction:")),
                h3(p("1. Click points in the plot to create circles and change size of circles by sliderbar.")), 
                h3(p("2. Click 'Check' button and color button to see the probability.")),
                h3(p("3. If you want to move the circle, please click 'Move' button. ")),
                h3(p("4. If you want to do again, please click 'Reset' button.")),
                
                 textOutput('text2'),
                tags$head(tags$style("#text2{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
                         )
                ),
                
                hr(),
                textOutput("text1"),
                tags$head(tags$style("#text1{color: black;
                                     font-size: 25px;
                                     font-style: italic;
                                     }"
                ))
                
                
                                      
               
                    
        ),
        
        
        # Level 1
        tabItem(tabName = "level1",
                
                fluidPage(
                  
                  # Application title
                  titlePanel("One event probability models"),
                  
                  #fresh the whole page
                  useShinyjs(),
                  extendShinyjs(text = jsResetCode),
                  
                  # Sidebar with a slider input for number of bins 
                  sidebarLayout(
                    sidebarPanel(
                      
                      #hide value of silder bar
                      tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
                                                visibility: hidden !important;
                                                }'))),
      
                      
                        h4(p("Blue circle")),br(),
                        div(style = "position: absolute; left: 3em; top: 4.5em", h5("Small")),
                        div(style = "position: absolute; right: 3em; top: 4.5em", h5("Large")),
                        sliderInput("radius.l1",
                                    label = NULL,
                                    min = 0,
                                    max = 1.5,
                                    step =0.005,
                                    value = 0.05,
                                    ticks = FALSE),
                        # #Probability of blue circle
                        actionButton("labell1","Show Label"),br(),br(),
                        
                        fluidRow(
                        column(5,uiOutput("labeldol1")),
                        conditionalPanel(condition = "input.dol1 > 0 ",{
                          column(6, verbatimTextOutput("PA.l1"))
                          
                        })
                        )
                      
                      ),
                    
                    
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                      
                      
                      plotOutput("distPlot.l1",click="Click.l1"),
                      
                      div(style = "position: absolute; right:5em ; top: 23em",
                      
                      column(1,offset = 10, actionButton("calculate.l1","Check")),br(),br(),
                      column(1,offset = 10, actionButton("reset_button.l1","Reset"))),br(),
                      
                      h3(p("Please create a diagram to model the following situation:")),
                      
                      h3(textOutput("question.l1")),
                      h4(textOutput("answer.l1"))
                      
                    )
                ),
               
                  tags$head(
                  
                  #Probability of blue circle 
                  tags$style(HTML("
                                  #dol1 {
                                  background-color:#B2B2FF;
                                  display:block;
                                  height: 60px;
                                  width: 60px;
                                  border-radius: 50%;
                                  border: 1px solid #B2B2FF;
                                  color:#000000;
                                  }
                                  "))
                  
                  
                  )
          
                  )
                  ),
        
        
        # Level 2
        tabItem(tabName = "level2",
                fluidPage(
                  
                  # Application title
                  titlePanel("Two events probability models"),
                  
                  #fresh the whole page
                  useShinyjs(),
                  extendShinyjs(text = jsResetCode),
                  
                  # Sidebar with a slider input for number of bins 
                  sidebarLayout(
                    sidebarPanel(
                      
                      
                      
                      #hide value of silder bar
                      tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
                                                visibility: hidden !important;
                                                }'))),
                      fluidRow(
                      h4(p("Blue circle")),
                      div(style = "position: absolute; left: 3em; top: 4.5em", h5("Small")),
                      div(style = "position: absolute; right: 3em; top: 4.5em", h5("Large")),br(),
                      sliderInput("radius.l2",
                                  label = NULL,
                                  min = 0,
                                  max = 1.5,
                                  step=0.005,
                                  ticks = FALSE,
                                  value = 0.05),
                      
                      h4(p("Green circle")),
                      div(style = "position: absolute; left: 3em; top: 11em", h5("Small")),
                      div(style = "position: absolute; right: 3em; top: 11em", h5("Large")),br(),
                      sliderInput("radius2.l2",
                                  label = NULL,
                                  min = 0,
                                  max = 1.5,
                                  step=0.005,
                                  ticks = FALSE,
                                  value = 0.05),
                      
                      conditionalPanel("input.reset2l2 != 0", 
                                       h4(p("Move the center of Green circle")),
                                       div(style = "position: absolute; left: 3em; top: 19.5em", h5("Left")),
                                       div(style = "position: absolute; right: 3em; top: 19.5em", h5("Right")),br(),
                                       sliderInput("move.l2",
                                                   label = NULL,
                                                   min=0,
                                                   max=1,
                                                   step=0.01,
                                                   ticks = FALSE,
                                                   value=0.5)),br(),
                      
                      conditionalPanel("input.reset2l2 != 0", 
                                       div(style = "position: absolute; left: 3em; top: 26.5em", h5("Down")),
                                       div(style = "position: absolute; right: 3em; top: 26.5em", h5("Up")),br(),
                                       sliderInput("move2.l2",
                                                   label = NULL,
                                                   min=0,
                                                   max=1,
                                                   step=0.01,
                                                   ticks = FALSE,
                                                   value=0.5)),
                      
                      actionButton("labell2","Show Label"),br(),br(),
                      # #Probability of blue circle
                      fluidRow(
                      column(5,uiOutput("labeldoBl2")),
                      conditionalPanel(condition = "input.dol2 > 0 ",{
                       column(6, verbatimTextOutput("PA.l2"))
                      })
                      ),
                      
                      #Probability of green circle
                      fluidRow(
                      column(5,uiOutput("labeldoGl2")),
                      conditionalPanel(condition = "input.do2l2 > 0 ",{
                        column(6,verbatimTextOutput("PB.l2"))
                      })
                      ),
                      
                      #Probability of cyan
                      fluidRow(
                      column(5,uiOutput("labeldoBGl2")),
                      conditionalPanel(condition = "input.do4l2 > 0 ",{
                        column(6,verbatimTextOutput("AB.l2"))
                      })
                      )
                    )
                      
                      ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                      plotOutput("distPlot.l2",click="Click.l2"),
                      
                      div(style = "position: absolute; right : 6em; top: 20em",
                      column(1,offset = 10, actionButton("calculate.l2","Check")),br(),br(),
                      column(1,offset = 10, actionButton("reset2l2","MoveGreen")),br(),br(),
                      column(1,offset = 10, actionButton("reset_button.l2","Reset"))),br(),
                      
                      h3(p("Please create a diagram to model the following situation:")),
                      
                      
                      h3(textOutput("question.l2")),
                      h4(textOutput("answer.l2"))
                    )
                  ),
                  
                  tags$head(
                    
                    #Probability of blue circle 
                    tags$style(HTML("
                                    #dol2 {
                                    background-color:#B2B2FF;
                                    display:block;
                                    height: 60px;
                                    width: 60px;
                                    border-radius: 50%;
                                    border: 1px solid #B2B2FF;
                                    color:#000000;
                                    }
                                    ")),
                    
                    #Probability of green circle
                    tags$style(HTML("
                                    #do2l2 {
                                    background-color:#B2FFB2;
                                    display:block;
                                    height: 60px;
                                    width: 60px;
                                    border-radius: 50%;
                                    border: 1px solid #B2FFB2;
                                    color:#000000;
                                    
                                    }
                                    
                                    ")),
                    
                    
                    #Probability of cyan
                    tags$style(HTML("
                                    #do4l2 {
                                    background-color:#7CC9B2;
                                    display:block;
                                    height: 60px;
                                    width: 60px;
                                    border-radius: 50%;
                                    border: 1px solid #7CC9B2;
                                    color:#000000;
                                    
                                    }
                                    
                                    "))

                    )
                    )
                    ),
        
        # level 3
        tabItem(tabName = "level3",
                fluidPage(
                  
                  # Application title
                  titlePanel("Three events probability models"),
                  
                  #fresh the whole page
                  useShinyjs(),
                  extendShinyjs(text = jsResetCode),
                  
                  # Sidebar with a slider input for number of bins 
                  sidebarLayout(
                    sidebarPanel(
                      
                    
                      #hide value of silder bar
                      tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max, .irs-single {
                                                visibility: hidden !important;
                                                }'))),
      
                      h4(p("Blue circle")),
                      div(style = "position: absolute; left: 3em; top: 4.5em", h5("Small")),
                      div(style = "position: absolute; right: 3em; top: 4.5em", h5("Large")),br(),
                      sliderInput("radius.l3",
                                  label = NULL,
                                  min = 0,
                                  max = 1.5,
                                  step=0.005,
                                  ticks = FALSE,
                                  value = 0.05),
                      h4(p("Green circle")),
                      div(style = "position: absolute; left: 3em; top: 11em", h5("Small")),
                      div(style = "position: absolute; right: 3em; top: 11em", h5("Large")),br(),
                      sliderInput("radius2.l3",
                                  label =NULL,
                                  min = 0,
                                  max = 1.5,
                                  step=0.005,
                                  ticks = FALSE,
                                  value = 0.05),
                      h4(p("Red circle")),
                      div(style = "position: absolute; left: 3em; top: 18.3em", h5("Small")),
                      div(style = "position: absolute; right: 3em; top: 18.3em", h5("Large")),br(),
                      sliderInput("radius3.l3",
                                  label = NULL,
                                  min = 0,
                                  max = 1.5,
                                  step=0.005,
                                  ticks = FALSE,
                                  value = 0.05),
                      
                      
                      conditionalPanel("input.reset1l3 != 0",h4(p("Move the center of Green circle")), 
                                       div(style = "position: absolute; left: 3em; top: 26.5em", h5("Left")),
                                       div(style = "position: absolute; right: 3em; top: 26.5em", h5("Right")),br(),
                                                                                                                   sliderInput("move.l3",
                                                                                                                   label = NULL,
                                                                                                                   min=0,
                                                                                                                   max=1,
                                                                                                                   step=0.01,
                                                                                                                   ticks = FALSE,
                                                                                                                   value=0)),
                      
                      conditionalPanel("input.reset2l3 != 0",h4(p("Move the center of Red circle")), 
                                       div(style = "position: absolute; left: 3em; top: 34em", h5("Left")),
                                       div(style = "position: absolute; right: 3em; top: 34em", h5("Right")),br(),
                                                                                                                 sliderInput("move2.l3",
                                                                                                                 label = NULL,
                                                                                                                 min=0,
                                                                                                                 max=1,
                                                                                                                 step=0.01,
                                                                                                                 ticks = FALSE,
                                                                                                                 value=0)),
                      
                      conditionalPanel("input.reset2l3 != 0", 
                                       div(style = "position: absolute; left: 3em; top: 38.5em", h5("Down")),
                                       div(style = "position: absolute; right: 3em; top: 38.5em", h5("Up")),br(),
                                                                          sliderInput("move3.l3",
                                                                          label = NULL,
                                                                          min=0,
                                                                          max=1,
                                                                          step=0.01,
                                                                          ticks = FALSE,
                                                                          value=0)),
                      
                      
                      actionButton("labell3","Show Label"),br(),br(),
                      # #Probability of blue circle
                      fluidRow(
                      column(5,uiOutput("labeldoBl3")),
                      conditionalPanel(condition = "input.dol3 > 0 ",{
                      column(6, verbatimTextOutput("PA.l3"))
                      })
                      ),
                      
                      #Probability of green circle
                      fluidRow(
                      column(5,uiOutput("labeldoGl3")),
                      conditionalPanel(condition = "input.do2l3 > 0 ",{
                      column(6, verbatimTextOutput("PB.l3"))
                      })
                      ),
                      
                      #Probability of red circle
                      fluidRow(
                      column(5,uiOutput("labeldoRl3")),
                      conditionalPanel(condition = "input.do3l3 > 0 ",{
                      column(6,verbatimTextOutput("PC.l3"))
                      })
                      ),
                      
                      #Probability of cyan
                      fluidRow(
                      column(5,uiOutput("labeldoBGl3")),
                      conditionalPanel(condition = "input.do4l3 > 0 ",{
                      column(6,verbatimTextOutput("AB.l3"))
                      })
                      ),
                      
                      #Probability of purple
                      fluidRow(
                      column(5,uiOutput("labeldoBRl3")),
                      conditionalPanel(condition = "input.do5l3 > 0 ",{
                      column(6,verbatimTextOutput("AC.l3"))
                      })
                      ),
                      
                      #Probability of darkolivegreen
                      fluidRow(
                      column(5,uiOutput("labeldoGRl3")),
                      conditionalPanel(condition = "input.do6l3 > 0 ",{
                      column(6,verbatimTextOutput("BC.l3"))
                      })
                      ),
                      
                      #Probability of central part
                      fluidRow(
                        column(5,uiOutput("labeldoBGRl3")),
                        conditionalPanel(condition = "input.do7l3 > 0 ",{
                          column(6,verbatimTextOutput("ABC.l3"))
                        })
                      )
                      
                      ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                      plotOutput("distPlot.l3",click="Click.l3"),
                      div(style = "position: absolute; right : 6em; top: 18em",
                      column(1,offset = 10, actionButton("calculatel3","Check")),br(),br(),
                      column(1,offset = 10, actionButton("reset1l3","MoveGreen")),br(),br(),
                      column(1,offset = 10, actionButton("reset2l3","MoveRed")),br(),br(),
                      column(1,offset = 10, actionButton("reset_button.l3","Reset"))),br(),
                      
                      h3(p("Please create a diagram to model the following situation:")),
                      
                      h3(textOutput("question.l3")),
                      h4(textOutput("answer.l3"))
                      )
                      ),
                  
                  tags$head(
                    
                    #Probability of blue circle 
                    tags$style(HTML("
                                    #dol3 {
                                    background-color:#B2B2FF;
                                    display:block;
                                    height: 60px;
                                    width: 60px;
                                    border-radius: 50%;
                                    border: 1px solid #B2B2FF;
                                    color:#000000;
                                    
                                    }
                                    
                                    ")),
                    #Probability of green circle
                    tags$style(HTML("
                                    #do2l3 {
                                    background-color:#B2FFB2;
                                    display:block;
                                    height: 60px;
                                    width: 60px;
                                    border-radius: 50%;
                                    border: 1px solid #B2FFB2;
                                    color:#000000;
                                    
                                    }
                                    
                                    ")),
                    
                    #Probability of red circle
                    tags$style(HTML("
                                    #do3l3 {
                                    background-color:#FFB2B2;
                                    display:block;
                                    height: 60px;
                                    width: 60px;
                                    border-radius: 50%;
                                    border: 1px solid #FFB2B2;
                                    color:#000000;
                                    }
                                    
                                    ")),
                    #Probability of cyan
                    tags$style(HTML("
                                    #do4l3 {
                                    background-color:#7CC9B2;
                                    display:block;
                                    height: 60px;
                                    width: 60px;
                                    border-radius: 50%;
                                    border: 1px solid #7CC9B2;
                                    color:#000000;
                                    
                                    }
                                    
                                    ")),
                    
                    #Probability of purple
                    tags$style(HTML("
                                    #do5l3 {
                                    background-color:#C97CB2;
                                    display:block;
                                    height: 60px;
                                    width: 60px;
                                    border-radius: 50%;
                                    border: 1px solid #C97CB2;
                                    color:#000000;
                                    
                                    }
                                    
                                    ")),
                    
                    #Probability of darkolivegreen
                    tags$style(HTML("
                                    #do6l3 {
                                    background-color:#C9B27C;
                                    display:block;
                                    height: 60px;
                                    width: 60px;
                                    border-radius: 50%;
                                    border: 1px solid #C9B27C;
                                    color:#000000;
                                    
                                    }
                                    
                                    ")),
                    
                    tags$style(HTML("
                                    #do7l3 {
                                    background-color:#A48C7C;
                                    display:block;
                                    height: 60px;
                                    width: 60px;
                                    border-radius: 50%;
                                    border: 1px solid #A48C7C;
                                    color:#000000;
                                    
                                    }
                                    
                                    "))
                  
                    )
                  
                  
                    )
                
                )
        ############
        
      )
    )
  )
)