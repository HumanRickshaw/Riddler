##############PACKAGES##############

library(comprehenr)
library(dplyr)
library(plotly)
library(readxl)
library(shiny)
library(tidyr)



##############CONSTANTS##############
#No Scientific Notation.
options(scipen = 999)

states = state.name[!(state.name %in% c("Alaska","Hawaii"))]
print(states)
#Founding Members
y <- states[c(4, 5, 29, 30)]
p <- states[c(8, 41, 42, 48)]

#DF
df <- data.frame(state = states,
                 abb = state.abb[!(state.abb %in% c("AK", "HI"))],
                 fill = NA) %>%
  mutate(hover = ifelse(state %in% y,
                        paste(state, "is one of the founding members of the USY."),
                        ifelse(state %in% p,
                               paste(state, "is one of the founding members of the USP."),
                               NA)))

ch_y <- states[!(states %in% p)]
ch_p <- states[!(states %in% y)]
##############FUNCTIONS##############



#Summary text for Yellow and Purple.
summary_text <- function(region, num) {
  
  paste("There are currently",
        num,
        "states in the United States of",
        region)
}



##############UI##############
ui <- fluidPage(
  
  #Remove warnings.
  tags$style(type = "text/css",
             ".shiny-output-error {visibility: hidden;}",
             ".shiny-output-error:before {visibility: hidden;}"),
  
  titlePanel("Dividing the Country"),
  sidebarLayout(
    
    
    
    ##############SIDEBAR PANEL##############
    sidebarPanel(width = 3,
                 #Used in all tabs.
                 h6("Rohan Lewis, 2022.07.11"),
                 br(),
                 h6("Add states by typing or dropdown."),
                 h6("Remove states by delete or backspace."),
                 br(),
                 selectInput("st_y",
                             "Yellow States:",
                             choices = ch_y,
                             selected = c("California", "Colorado", "New Mexico", "New York"),
                             multiple = TRUE),
                 br(),
                 selectInput("st_p",
                             "Purple States:",
                             choices = ch_p,
                             selected = c("Florida", "Texas", "Utah", "Wyoming"),
                             multiple = TRUE)),
    
    ##############MAIN PANEL##############
    mainPanel(plotlyOutput("plot", height = 600),
              verbatimTextOutput("summary"))))



##############SERVER##############
server <- function(input, output, session) {

  ##############Reactive##############
  
  #If a state is added to Yellow, remove it from Purple, if applicable.
  observeEvent(input$st_y,
               {
                 for (state in input$st_y){
                   if (state %in% input$st_p) {
                     
                     new_states <- input$st_p[input$st_p != state]
                     
                     updateSelectInput(session,
                                       "st_p",
                                       choices = ch_p,
                                       selected = new_states)
                   }
                 }
                })
  
  
  
  #If a state is added to Purple, remove it from Yellow, if applicable.
  observeEvent(input$st_p,
               {
                 for (state in input$st_p){
                   if (state %in% input$st_y) {
                     
                     new_states <- input$st_y[input$st_y != state]
                     
                     updateSelectInput(session,
                                       "st_y",
                                       choices = ch_y,
                                       selected = new_states)
                   }
                 }
               })
  
  
  
  #Add a column to color the region and for hovertext.
  fill_df <- reactive({
    
    df %>%
      mutate(fill = ifelse(state %in% input$st_y,
                           1,
                           ifelse(state %in% input$st_p,
                                  2, 0)))
  })
  
    
  ##############States to Sum##############

  output$plot <- renderPlotly({
    
    g <- list(scope = 'usa',
              projection = list(type = 'albers usa'),
              showlakes = TRUE,
              lakecolor = toRGB('white'))
    
    #Initialize
    map <- plot_geo(fill_df(), locationmode = 'USA-states')
    
    map <- map %>% add_trace(z = ~fill,
                             text = ~hover,
                             hoverinfo = 'text',
                             locations = ~abb,
                             color = ~fill,
                             colors = c("grey", "#FFCF20FF", "#541352FF"))
    
    title_text <- "USY and USP"
    
    map <- map %>% hide_colorbar()
    
    map %>% layout(title = list(text = title_text, y = 0.965),
                   font = list(size = 22),
                   geo = g)
  })
  
  output$summary <- renderText({
    
    paste(summary_text("Yellow", length(input$st_y)),
          "\n\n",
          summary_text("Purple", length(input$st_p)))
    })
}



# Run the application 
shinyApp(ui = ui, server = server)