library(data.table)
library(plyr)
library(dplyr)
library(pROC)
library(caret)
library(utils)
library(lubridate)
library(doParallel)
library(ggplot2)
library(grid)
library(jpeg)
library(RCurl)


#create short chart for kobe
court_img <- "http://robslink.com/SAS/democd54/nba_court_dimensions.jpg"
court <- rasterGrob(readJPEG(getURLContent(court_img)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

ui <- fluidPage(
  
  titlePanel("Kobe Bryant's Short Chart"),
  
  sidebarLayout(
    
    sidebarPanel( 
      
      selectInput("year", 
                  
                  label = h3("Select Year"), 
                  
                  choices = unique(raw$year)),
      
      sliderInput("slider",
                  
                  label = "Choose slider",
                  
                  min = unique(min(raw$year)),
                  
                  max = unique(max(raw$year)),
                  
                  value = 1996,
                  
                  animate = TRUE
                  )),
      
    
    mainPanel("Kobe's Short Chart based on selected year", 
              
              plotOutput("shortchart"))
  )
)


server <- function(input, output){
  #react to changes in input
  
  react <- reactive({
    sub <- subset(raw, year == input$slider)
    ggplot(sub, aes(x=loc_x, y=loc_y)) + 
      annotation_custom(court, -250, 250, -50, 420) +
      geom_point(aes(colour = shot_zone_basic, shape = shot_made_flag)) +
      xlim(-250, 250) +
      ylim(-50, 420)
    })
  
  output$shortchart <-renderPlot({
    react()
  })
}

shinyApp(server = server, ui = ui)
