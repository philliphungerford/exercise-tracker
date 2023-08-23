##############################################################################
# Purpose: Dashboard Template
# Author: Phillip Hungerford
# Date: 2021-01-14
##############################################################################
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Dashboard help:
# https://rstudio.github.io/shinydashboard/structure.html
#
# Adding text to your shiny app
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
#
# Icons:
# https://fontawesome.com/v4.7.0/icons/
##############################################################################
# Import libraries
library(shiny) # for dashboard
library(shinydashboard) # for tabs
library(dplyr) # filtering
library(RColorBrewer)
library(ggplot2) # for figures
library(tools)
library(tidyverse) # adds to title case
library(scales) # add comma to output
library(DT)
##############################################################################
# SETTINGS
github_link <- 'https://github.com/philliphungerford/exercise-tracker'
website_link <- 'https://philliphungerford.github.io'

# Tab Names
TabNames <- c("Data",
              "Plates")

##############################################################################
# TAB 1: USER INTERFACE
##############################################################################
# Define UI for application that draws a histogram
ui <- dashboardPage(
  #=========================================================================
  # START DASHBOARD
  #=========================================================================
  # MAIN TITLE
  dashboardHeader(
    title =  "Health Tracker",
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$a(
        href = github_link,
        icon('github'),
        "Source Code",
        target = "_blank"
      )
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        href = website_link,
        icon('globe'),
        "Learn More",
        target = "_blank"
      )
    )
  ),
  #=========================================================================
  ## Sidebar content
  dashboardSidebar(sidebarMenu(
    # icons from (https://fontawesome.com/v4.7.0/icons/)
    menuItem(TabNames[1], tabName = TabNames[1], icon = icon("desktop")),
    menuItem(TabNames[2], tabName = TabNames[2], icon = icon("chart-bar"))
    
  )),
  #=========================================================================
  ## Body content
  dashboardBody(tabItems(
    tabItem(
      tabName = TabNames[1],
      
      h1("Summary"),
      
      fluidRow(
        column(width = 3,
               uiOutput("BoxDeadliftMax")),
        
        column(width = 3,
               uiOutput("BoxSquatMax")),
        
        column(width = 3,
               uiOutput("BoxBenchMax")),
        column(width = 3,
               uiOutput("BoxPressMax")),
      ),
      
      fluidRow(column(width = 12,
                      plotOutput("PlotMaxLifts"))),
      
      h1("Data"),
      
      fluidRow(
        column(width = 2,
               dateInput("Date", "Date", value = Sys.Date()),),
        column(width = 2,
               selectInput(
                 "Exercise",
                 "Exercise",
                 choices = c("Deadlift", "Squat", "Bench", "Press", "Row", "Abs")
               ),),
        column(width = 2,
               numericInput("Load", "Load", value = 0),),
        column(width = 2,
               numericInput("RepTarget", "Rep Target", value = 0),),
        column(width = 2,
               numericInput("RepActual", "Rep Actual", value = 0),),
        column(width = 2,
               
               textInput("Note", "Note"),),
      ),
      
      
      fluidRow(column(
        width = 2,
        
        actionButton("AddSet", "Add Set"),
        
      )),
      
      fluidRow(column(
        width = 12,
        numericInput("DeleteSetId", "Row to Delete", value = 1),
        actionButton("DeleteSet", "Delete Set"),
        downloadButton("DownloadData", "Download Data"),
        
      )),
      
      fluidRow(column(
        width = 12,
        h2("Today"),
        DTOutput("FactSetsToday")
      )),
      
      fluidRow(column(
        width = 12,
        h2("Historical"),
        DTOutput("FactSetsAll")
      ))
    ),
    
    tabItem(tabName = TabNames[2],
            
            fluidRow(column(
              width = 12,
              h1("Plate Reference Guide"),
              downloadButton("DownloadDimData", "Download Data"),
              h1(""),
              DTOutput("DimPlates")
            )))
    
    #-----------------------------------------------------------------
  ))
)

#=========================================================================
# END DASHBOARD
#=========================================================================

##############################################################################
# TAB 2: SERVER
##############################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  DimPlates <- read.csv("DimPlates.csv")
  FactSets <- reactiveVal(read.csv("FactSets.csv"))
  
  output$BoxDeadliftMax <- renderValueBox({
    valueBox(
      value = round((max(
        FactSets()$Load[FactSets()$Exercise == "Deadlift"], na.rm = T
      )) / 2.2, 0),
      subtitle = "Deadlift Max",
      color = "blue",
      icon = icon("arrow-up")
      
    )
  })
  
  output$BoxSquatMax <- renderValueBox({
    valueBox(
      value = round((max(
        FactSets()$Load[FactSets()$Exercise == "Squat"], na.rm = T
      )) / 2.2, 0),
      subtitle = "Squat Max",
      color = "red",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxBenchMax <- renderValueBox({
    valueBox(
      value = round((max(
        FactSets()$Load[FactSets()$Exercise == "Bench"], na.rm = T
      )) / 2.2, 0),
      subtitle = "Bench Max",
      color = "orange",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxPressMax <- renderValueBox({
    valueBox(
      value = round((max(
        FactSets()$Load[FactSets()$Exercise == "Press"], na.rm = T
      )) / 2.2, 0),
      subtitle = "Press Max",
      color = "green",
      icon = icon("arrow-up")
    )
  })
  
  observeEvent(input$AddSet, {
    new_id <- max(FactSets()$Id) + 1
    new_entry <-
      data.frame(
        Id = new_id,
        Day = weekdays(input$Date),
        Date = as.character(input$Date),
        Exercise = input$Exercise,
        Load = input$Load,
        RepTarget = input$RepTarget,
        RepActual = input$RepActual,
        Note = input$Note
      )
    updated_data <- rbind(FactSets(), new_entry)
    FactSets(updated_data)
    write.csv(FactSets(), "FactSets.csv", row.names = FALSE)
  })
  
  observeEvent(input$DeleteSet, {
    updated_data <- FactSets()[!FactSets()$Id == input$DeleteSetId,]
    FactSets(updated_data)
    write.csv(FactSets(), "FactSets.csv", row.names = FALSE)
  })
  
  output$FactSetsToday <- renderDT({
    Results <-
      FactSets() %>% arrange(desc(Id))
    
    datatable(
      Results[Results$Date == Sys.Date(),],
      editable = TRUE,
      options = list(pageLength = -1),
      rownames = FALSE
    )
  })
  
  output$FactSetsAll <- renderDT({
    Results <-
      FactSets() %>% arrange(desc(Id)) %>% filter(Date != Sys.Date())
    datatable(
      Results,
      editable = TRUE,
      options = list(pageLength = 20),
      rownames = FALSE
    )
  })
  
  output$DimPlates <- renderDT({
    datatable(DimPlates,
              options = list(pageLength = -1),
              rownames = FALSE)
  })
  
  output$PlotMaxLifts <- renderPlot({
    FactSets <- FactSets()
    
    FactSets$Date <- as.Date(FactSets$Date, format = "%Y-%m-%d")
    
    FactSets$Load <- FactSets$Load / 2.2
    
    filtered_data <-
      FactSets %>% select(Date, Exercise, Load) %>% filter(Exercise %in% c("Deadlift", "Squat", "Bench", "Press"))
    
    filtered_data$Month <- format(filtered_data$Date, "%Y-%m")
    
    max_load_data <- filtered_data %>%
      group_by(Month, Exercise) %>%
      summarize(Max_Load = max(Load), .groups = "drop")
    
    ggplot(max_load_data,
           aes(
             x = Month,
             y = Max_Load,
             color = Exercise,
             group = Exercise
           )) +
      geom_line() +
      geom_point() +
      labs(title = "Max Load by Exercise and Month", x = "Month", y = "Max Load") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 270, hjust = 1)) + 
      scale_color_manual(values = c("Deadlift" = "blue", "Squat" = "red", "Bench" = "orange", "Press" = "green"))
    
  })
  
  output$DownloadData <- downloadHandler(
    filename = function() {
      paste("FactSets", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(FactSets(), file)
    }
  )
  
  output$DownloadDimData <- downloadHandler(
    filename = function() {
      paste("DimPlates", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(DimPlates, file)
    }
  )
  
}
##############################################################################
# RUN APPLICATION
##############################################################################
shinyApp(ui = ui, server = server)
##############################################################################
################################### END ######################################
##############################################################################
