##############PACKAGES##############

library(comprehenr)
library(dplyr)
library(ggplot2)
library(shiny)
library(tidyr)



#No Scientific Notation.
options(scipen = 999)


##############FUNCTIONS##############





##############UI##############
ui <- fluidPage(
  
  #Remove warnings.
  tags$style(type = "text/css",
             ".shiny-output-error {visibility: hidden;}",
             ".shiny-output-error:before {visibility: hidden;}"),
  
  titlePanel("Can You Create a Chain Drawing?"),
  sidebarLayout(
    
    
    
    ##############SIDEBAR PANEL##############
    sidebarPanel(width = 3,
                 #Used in all tabs.
                 h6("Rohan Lewis, 2021.08.02"),
                 br(),
                 radioButtons("ar",
                              "Choose Aspect Ratio",
                              choices = c(1, "Auto"),
                              selected = 1),
                 br(),
                 sliderInput("f",
                             "Choose f",
                             min = 0.001,
                             max = 0.999,
                             step = 0.001,
                             value = 0.8),
                 br(),
                 sliderInput("angle",
                             HTML("Choose &theta;"),
                             min = 0.1,
                             max = 179.9,
                             step = 0.1,
                             value = 45)),
    
    
    
    ##############MAIN PANEL##############
    mainPanel(plotOutput("plot", height = 600),
              verbatimTextOutput("summary"))))



##############SERVER##############
server <- function(input, output, session) {

  ##############Reactive##############

  #Paperclip xs.
  xs <- reactive ({
    
    xs <- c(0, 1)
    for (i in 2:100) {
      x <- xs[i] + (input$f^(i-1))*cos((i-1)*input$angle*pi/180)
      xs <- c(xs, x)
    }
    xs
  })

  
  
  #Paperclip ys.
  ys <- reactive ({
    
    ys <- c(0, 0)
    for (i in 2:100) {
      y <- ys[i] + (input$f^(i-1))*sin((i-1)*input$angle*pi/180)
      ys <- c(ys, y)
    }
    ys
  })

  
  
  #Center of ink arc.
  center <- reactive({ 1 / (1 - input$f^2) })
  
  
  
  #Radius of ink arc.
  radius <- reactive({ input$f * center() })
  
  
  
  #Angle traced by ink arc.
  angle <- reactive({
    
    Arg(1 / (1 - input$f * complex(real = cos(pi*input$angle/180),
                                   imaginary = sin(pi*input$angle/180))) - center())
  })
    
   
   
  #Length of ink arc. 
  arc_length <- reactive({ angle() * radius() })
  
  
  
  #Arc xs.
  arc_xs <- reactive ({
    
    xs <- NULL
    for (i in 1:1000) {
      
      theta <- i * angle() / 1000
      x <- center() + radius() * cos(theta)
      xs <- c(xs, x)
    }
    xs
  })
  
  
  
  #Arc ys.
  arc_ys <- reactive ({
    
    ys <- NULL
    for (i in 1:1000) {
      
      theta <- i * angle() / 1000
      y <- radius() * sin(theta)
      ys <- c(ys, y)
    }
    ys
  })

  
    
  ##############States to Sum##############

  output$plot <- renderPlot({
    
    #Paperclips.
    g <- ggplot() + geom_path(mapping = aes(xs(), ys()),
                              colour = '#1F968B',
                              size = 0.5)
    
    #Ink arc.
    g <- g + geom_path(mapping = aes(arc_xs(), arc_ys()), #geom_path(mapping = aes(Re(drawing()), Im(drawing())),
                       colour = '#481567',
                       size = 1.2)
    
    #Axes.    
    g <- g + scale_x_continuous(expand =  c(0, 0), breaks = NULL)
    g <- g + scale_y_continuous(expand =  c(0, 0), breaks = NULL)

    #Format.
    g <- g + theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
                   axis.text.x = element_blank(),
                   axis.title.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.title.y = element_blank())
    
    #User defined aspect ratio.
    if (input$ar == 1){
     g + coord_fixed(ratio = 1)
    }
    else {
      g
    }
  })
  
  
  
  output$summary <- renderText({
    paste("Degrees of Arc: ", 180 * angle() / pi,
          "\n\nLength of Trace: ", arc_length())
    })
}



# Run the application 
shinyApp(ui = ui, server = server)