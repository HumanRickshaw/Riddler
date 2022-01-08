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



#48 States and District of Columbia in alphabetical order.
state_list <- c(state.name[1:8],
                "District of Columbia",
                state.name[9:50]) 

state_list <- state_list[!(state_list %in% c("Alaska", "Hawaii"))]



#48 States and DC in alphabetical order. 
state_abb <- c(state.abb[1:8],
               "DC",
               state.abb[9:50])

state_abb<- state_abb[!(state_abb %in% c("AK", "HI"))]

##############READ FILES##############

#Read data.
#Total voting population is sum of Red, Blue, and Other votes.
df <- data.frame(read_excel("./States Area.xlsx")) %>%
  filter(!(state %in% c("Alaska", "Hawaii", "United States"))) %>%
  mutate(area = as.integer(area))

#DF of only USA total data.
df_usa <- df %>%
  filter(state == "United States")

#DF of 48 states and DC.
df <- df %>%
  mutate(abb = state_abb)

area_48 <- sum(df$area)
##############FUNCTIONS##############

#Converts to %.
perc <- function(num, den) {
  paste(round(100*num / den, 3), "%", sep = "")
  }



#Summary text for Border, West, and East.
summary_text <- function(region, num, den) {
  
  paste("The", region, "is",
        prettyNum(num, big.mark = ","),
        "square miles, or",
        perc(num, area_48),
        "of the contiguous US.")
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
                 h6("Rohan Lewis, 2021.06.14"),
                 br(),
                 h6("Add states by typing or dropdown."),
                 h6("Remove states by delete or backspace."),
                 h6("The Eastern States are chosen automatically."),
                 br(),
                 selectInput("st_b",
                             "Border States:",
                             choices = state_list,
                             selected = c("Colorado", "Illinois", "Missouri", 'Nebraska', "New Mexico"),
                             multiple = TRUE),
                 br(),
                 selectInput("st_w",
                             "Western States:",
                             choices = state_list,
                             selected = c("Arizona", "California", "Idaho", "Iowa",
                                          "Minnesota", "Montana", "Oregon",
                                           "Nevada", "North Dakota", "South Dakota", 
                                          "Utah", "Washington", "Wisconsin", "Wyoming"),
                             multiple = TRUE),
                 checkboxInput("yoop",
                               "Include Upper Peninsula in West",
                               value = TRUE)),
    
    
    
    ##############MAIN PANEL##############
    mainPanel(plotlyOutput("plot", height = 600),
              verbatimTextOutput("summary"))))



##############SERVER##############
server <- function(input, output, session) {

  ##############Reactive##############
  
  #If a state is added to the Border, remove it from the West, if applicable.
  observeEvent(input$st_b,
               {
                 for (state in input$st_b){
                   if (state %in% input$st_w) {
                     
                     new_states <- input$st_w[input$st_w != state]
                     
                     updateSelectInput(session,
                                       "st_w",
                                       choices = state_list,
                                       selected = new_states)
                   }
                 }
                })
  
  
  
  #If a state is added to the West, remove it from the Border, if applicable.
  observeEvent(input$st_w,
               {
                 for (state in input$st_w){
                   if (state %in% input$st_b) {
                     
                     new_states <- input$st_b[input$st_b != state]
                     
                     updateSelectInput(session,
                                       "st_b",
                                       choices = state_list,
                                       selected = new_states)
                   }
                 }
               })
  
  
  
  #Eastern States are those not in the Border or West.
  st_e <- reactive({
    
    state_list[!(state_list %in% c(input$st_b, input$st_w))]

  })
  
  
  
  #Add a column to color the region and for hovertext.
  fill_df <- reactive({
    df %>%
      mutate(fill = ifelse(state %in% input$st_b,
                           0,
                           ifelse(state %in% input$st_w,
                                  1, 2)),
             hover = paste(state,
                           ":  ",
                           prettyNum(area, big.mark = ","),
                           " square miles",
                           sep =""))
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
                             colors = c("grey", "#481567FF", "#1F968BFF"))
    
    title_text <- "Sum of Areas of Selected States"
    
    map <- map %>% hide_colorbar()
    
    map %>% layout(title = list(text = title_text, y = 0.965),
                   font = list(size = 22),
                   geo = g)
  })
  
  output$summary <- renderText({
    
    area_border = sum(fill_df()[fill_df()$fill == 0, "area"])
    area_west = sum(fill_df()[fill_df()$fill == 1, "area"]) 
    area_east = sum(fill_df()[fill_df()$fill == 2, "area"])
    
    #Add and subtract the Upper Peninsula from the West and East, respectively.
    if (input$yoop) {
      
      area_yoop = 16377
      area_west = area_west + area_yoop
      area_east = area_east - area_yoop
    }
    
    paste(summary_text("Border", area_border), "\n\n",
          summary_text("West", area_west), "\n\n",
          summary_text("East", area_east), sep = "")
    })
}



# Run the application 
shinyApp(ui = ui, server = server)