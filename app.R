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
library(fresh)
##############################################################################
# SETTINGS
github_link <-
  'https://github.com/philliphungerford/exercise-tracker'
website_link <- 'https://philliphungerford.github.io'

# Tab Names
TabNames <- c("Lifts",
              "Plates",
              "Exercise",
              "Metrics")

round_to_nearest_5 <- function(x) {
  rounded <- round(x / 5) * 5
  return(rounded)
}

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)
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
    menuItem(TabNames[4], tabName = TabNames[4], icon = icon("area-chart")),
    menuItem(TabNames[3], tabName = TabNames[3], icon = icon("chart-bar")),
    menuItem(TabNames[2], tabName = TabNames[2], icon = icon("search"))
    
  )),
  #=========================================================================
  ## Body content
  
  dashboardBody(
    use_theme(mytheme),
    tabItems(
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
                        plotOutput("PlotMaxLifts"),
                        selectInput("SelectFrequency", "Frequency", choices=c("Monthly", "Weekly", "Daily")))
        ),
        
        h1("Data"),
        p("Note that Load is in pounds (lbs) and is converted to kg."),
        
        fluidRow(
          column(width = 2,
                 dateInput("Date", "Date", value = Sys.Date()),),
          column(width = 2,
                 selectInput(
                   "Exercise",
                   "Exercise",
                   choices = c(
                     "Deadlift",
                     "Squat",
                     "Bench",
                     "Press",
                     "Row",
                     "Abs",
                     "Bicep Curl",
                     "Tricep Extension"
                   )
                 )),
          column(width = 2,
                 numericInput("Load", "Load", value = 0), ),
          column(width = 2,
                 numericInput("RepTarget", "Rep Target", value = 0), ),
          column(width = 2,
                 numericInput("RepActual", "Rep Actual", value = 0), ),
          column(width = 2,
                 
                 textInput("Note", "Note"), ),
        ),
        
        
        fluidRow(
          column(
            width = 2,
            numericInput("SelectSetId", "Select Id to Edit or Delete", value = -1),
          ),
          column(
            width = 2,
            actionButton("AddSet", "Add Set", style = 'margin-top:25px; color: white; background-color: #28A745; border-color: #28A745;'),
          ),
          column(
            width = 2,
            actionButton("EditSet", "Edit Set", style = 'margin-top:25px; color: white; background-color: #6C757D; border-color: #6C757D;'),
          ),
          column(
            width = 2,
            actionButton("DeleteSet", "Delete Set", style = 'margin-top:25px; color: white; background-color: #DC3545; border-color: #DC3545;'),
          ),
        ),
        
        fluidRow(
          column(
            width = 12,
            downloadButton("DownloadData", "Download Data"),
          )
        ),
        
        fluidRow(column(
          width = 12,
          h2("Today"),
          DTOutput("FactSetsToday")
        )),
        
        fluidRow(column(
          width = 12,
          h2("Historical"),
          DTOutput("FactSetAll")
        ))
      ),
      
      tabItem(tabName = TabNames[2],
              
              fluidRow(
                column(
                  width = 12,
                  h1("Plate Reference Guide"),
                  downloadButton("DownloadDimData", "Download Data"),
                  h1(""),
                  DTOutput("DimPlate")
                )
              )),
      
      # exercise
      tabItem(tabName = TabNames[3],
              
              fluidRow(column(
                width = 12,
                h1("Exercise Breakdown"),
                
              ),
              column(width = 12,
                     selectInput(
                       "ExerciseBreakdown",
                       "Select Exercise",
                       choices = c(
                         "Deadlift",
                         "Squat",
                         "Bench",
                         "Press",
                         "Row",
                         "Abs",
                         "Bicep Curl",
                         "Tricep Extension"
                       )
                     ))),
              fluidRow(column(
                width = 12,
                plotOutput("PlotSelectedExercise"),
                DTOutput("DataSelectedExercise")
              )
              )
      ),
      
      # measures
      tabItem(
        tabName = TabNames[4],
        
        fluidRow(column(width = 2,
                        h1("Measures"))
        ),
        
        fluidRow(
          column(width = 2,
                 uiOutput("BoxWeight")),
          column(width = 2,
                 uiOutput("BoxBodyFat")),
          column(width = 2,
                 uiOutput("BoxMuscleMass")),
          column(width = 2,
                 uiOutput("BoxAverageSleep")),
          column(width = 2,
                 uiOutput("BoxAverageSteps")),
        ),
        
        fluidRow(
          column(width = 2,
                 selectInput(
                   "SelectMetric",
                   "Metric",
                   choices = c(
                     "WeightKG",
                     "BodyFat",
                     "MuscleMass",
                     "Sleep_hrs_min",
                     "Nap_hrs_min",
                     "Steps"
                   )
                 ))),
        
        fluidRow(column(width = 12,
                        plotOutput("PlotMetric"))),
        
        fluidRow(column(width = 2,
                        dateInput("MDate", "Date", value = Sys.Date())),
        ),
        
        fluidRow(
          column(width = 2,
                 numericInput("WeightKG", "WeightKG", value = 0)),
          column(width = 2,
                 numericInput("BodyFat", "BodyFat", value = 0)),
          column(width = 2,
                 numericInput("MuscleMass", "MuscleMass", value = 0)),
          column(width = 2,
                 numericInput("Sleep_hrs_min", "Sleep_hrs_min", value = 0)),
          column(width = 2,
                 numericInput("Nap_hrs_min", "Nap_hrs_min", value = 0)),
          column(width = 2,
                 numericInput("Steps", "Steps", value = 0)),
        ),
        fluidRow(
          
          column(
            width = 2,
            numericInput("SelectMeasureId", "Select Id to Edit or Delete", value = -1),
          ),
          column(
            width = 2,
            actionButton("AddMeasure", "Add Measure", style = 'margin-top:25px; color: white; background-color: #28A745; border-color: #28A745;'),
          ),
          column(
            width = 2,
            actionButton("EditMeasure", "Edit Measure", style = 'margin-top:25px; color: white; background-color: #6C757D; border-color: #6C757D;')
          ),
          column(
            width = 2,
            actionButton("DeleteMeasure", "Delete Measure", style = 'margin-top:25px; color: white; background-color: #DC3545; border-color: #DC3545;')
          )
        ),
        
        fluidRow(column(
          width = 12,
          downloadButton("DownloadMeasureData", "Download Data")
        )),
        
        fluidRow(column(width = 12,
                        DTOutput("FactMeasureAll")))
      )
      #-----------------------------------------------------------------
    )
  )
)

#=========================================================================
# END DASHBOARD
#=========================================================================

##############################################################################
# TAB 2: SERVER
##############################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
  DimPlate <- read.csv("data/DimPlate.csv")
  FactSet <- reactiveVal(read.csv("data/FactSet.csv"))
  FactMeasure <- reactiveVal(read.csv("data/FactMeasure.csv"))
  
  output$BoxDeadliftMax <- renderValueBox({
    valueBox(
      value = round_to_nearest_5(round((max(
        FactSet()$Load[FactSet()$Exercise == "Deadlift"], na.rm = T
      )) / 2.2, 0)),
      subtitle = "Deadlift Max (KG)",
      color = "blue",
      icon = icon("arrow-up")
      
    )
  })
  
  output$BoxSquatMax <- renderValueBox({
    valueBox(
      value = round_to_nearest_5(round((max(
        FactSet()$Load[FactSet()$Exercise == "Squat"], na.rm = T
      )) / 2.2, 0)),
      subtitle = "Squat Max (KG)",
      color = "red",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxBenchMax <- renderValueBox({
    valueBox(
      value = round_to_nearest_5(round((max(
        FactSet()$Load[FactSet()$Exercise == "Bench"], na.rm = T
      )) / 2.2, 0)),
      subtitle = "Bench Max (KG)",
      color = "orange",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxPressMax <- renderValueBox({
    valueBox(
      value = round_to_nearest_5(round((max(
        FactSet()$Load[FactSet()$Exercise == "Press"], na.rm = T
      )) / 2.2, 0)),
      subtitle = "Press Max (KG)",
      color = "green",
      icon = icon("arrow-up")
    )
  })
  
  observeEvent(input$AddSet, {
    new_id <- max(FactSet()$Id) + 1
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
    updated_data <- rbind(FactSet(), new_entry)
    FactSet(updated_data)
    write.csv(FactSet(), "data/FactSet.csv", row.names = FALSE)
  })
  
  observeEvent(input$DeleteSet, {
    updated_data <- FactSet()[!FactSet()$Id == input$SelectSetId, ]
    FactSet(updated_data)
    write.csv(FactSet(), "data/FactSet.csv", row.names = FALSE)
  })
  
  output$FactSetsToday <- renderDT({
    Results <-
      FactSet() %>% arrange(desc(Id))
    
    datatable(
      Results[Results$Date == Sys.Date(), ],
      editable = TRUE,
      options = list(pageLength = -1),
      rownames = FALSE
    )
  })
  
  output$FactSetAll <- renderDT({
    Results <-
      FactSet() %>% arrange(desc(Id)) %>% filter(Date != Sys.Date())
    datatable(
      Results,
      editable = TRUE,
      options = list(pageLength = 20),
      rownames = FALSE
    )
  })
  
  output$FactMeasureAll <- renderDT({
    Results <-
      FactMeasure() %>% arrange(desc(Id))
    datatable(
      Results,
      editable = TRUE,
      options = list(pageLength = -1),
      rownames = FALSE
    )
  })
  
  
  output$DimPlate <- renderDT({
    datatable(DimPlate,
              options = list(pageLength = -1),
              rownames = FALSE)
  })
  
  output$PlotMaxLifts <- renderPlot({
    FactSet <- FactSet()
    
    FactSet$Date <- as.Date(FactSet$Date, format = "%Y-%m-%d")
    
    FactSet$Load <- FactSet$Load / 2.2
    
    filtered_data <-
      FactSet %>% select(Date, Exercise, Load) %>% filter(Exercise %in% c("Deadlift", "Squat", "Bench", "Press"))
    
    filtered_data$Year <- format(filtered_data$Date, "%Y")
    filtered_data$Month <- format(filtered_data$Date, "%Y-%m")
    filtered_data$Week <- format(filtered_data$Date, "%Y-%U")
    filtered_data$Day <- format(filtered_data$Date, "%Y-%m-%d")
    
    if(input$SelectFrequency == "Monthly"){
      Results <- filtered_data %>%
        group_by(Month, Exercise) %>%
        summarize(Max_Load = max(Load), .groups = "drop") %>% 
        rename("Time" = "Month")
      
    } else if(input$SelectFrequency == "Weekly") {
      Results <- filtered_data %>%
        group_by(Week, Exercise) %>%
        summarize(Max_Load = max(Load), .groups = "drop") %>% 
        rename("Time" = "Week")
    } else {
      Results <- filtered_data %>%
        group_by(Day, Exercise) %>%
        summarize(Max_Load = max(Load), .groups = "drop") %>% 
        rename("Time" = "Day")
    }
    
    ggplot(Results,
           aes(
             x = Time,
             y = Max_Load,
             color = Exercise,
             group = Exercise
           )) +
      geom_line(linewidth=2) +
      geom_point(size=3) +
      labs(title = "Max Load by Exercise", x = as.character(input$SelectFrequency), y = "Max Load") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
      scale_color_manual(values = c(
        "Deadlift" = "blue",
        "Squat" = "red",
        "Bench" = "orange",
        "Press" = "green"
      )) + geom_text(aes(label = round_to_nearest_5(Max_Load)), vjust = -1)
    
  })
  
  output$DownloadData <- downloadHandler(
    filename = function() {
      paste("FactSet", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(FactSet(), file)
    }
  )
  
  output$DownloadDimData <- downloadHandler(
    filename = function() {
      paste("DimPlate", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(DimPlate, file)
    }
  )
  
  output$DownloadMeasureData <- downloadHandler(
    filename = function() {
      paste("FactMeasure", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(FactMeasure(), file)
    }
  )
  
  observeEvent(input$AddMeasure, {
    new_id <- max(FactMeasure()$Id) + 1
    Date = as.character(input$MDate)
    new_entry <-
      data.frame(
        Id = new_id,
        Date = as.character(input$MDate),
        WeightKG = input$WeightKG,
        BodyFat = input$BodyFat,
        MuscleMass = input$MuscleMass,
        Sleep_hrs_min = input$Sleep_hrs_min,
        Nap_hrs_min = input$Nap_hrs_min,
        Steps = input$Steps
      )
    
    updated_data <- rbind(FactMeasure(), new_entry)
    FactMeasure(updated_data)
    write.csv(FactMeasure(), "data/FactMeasure.csv", row.names = FALSE)
  })
  
  observeEvent(input$DeleteMeasure, {
    updated_data <-
      FactMeasure()[!FactMeasure()$Id == input$SelectMeasureId, ]
    FactMeasure(updated_data)
    write.csv(FactMeasure(), "data/FactMeasure.csv", row.names = FALSE)
  })
  
  output$BoxWeight <- renderValueBox({
    valueBox(
      value = FactMeasure()$WeightKG[FactMeasure()$Date == max(FactMeasure()$Date, na.rm=T)],
      subtitle = "Weight KG",
      color = "blue",
      icon = icon("arrow-up")
      
    )
  })
  
  output$BoxBodyFat <- renderValueBox({
    valueBox(
      value = FactMeasure()$BodyFat[FactMeasure()$Date == max(FactMeasure()$Date, na.rm=T)],
      subtitle = "Current Body fat %",
      color = "red",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxMuscleMass <- renderValueBox({
    valueBox(
      value = FactMeasure()$MuscleMass[FactMeasure()$Date == max(FactMeasure()$Date, na.rm=T)],
      subtitle = "Current Muscle Mass",
      color = "orange",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxAverageSleep <- renderValueBox({
    valueBox(
      value = round(mean(FactMeasure()$Sleep_hrs_min, na.rm=T),2),
      subtitle = "Sleep Average",
      color = "green",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxAverageSteps <- renderValueBox({
    valueBox(
      value = round(mean(FactMeasure()$Steps, na.rm=T),2),
      subtitle = "Steps Average",
      color = "green",
      icon = icon("arrow-up")
    )
  })
  
  output$PlotSelectedExercise <- renderPlot({
    
    SelectedExercise <- input$ExerciseBreakdown
    
    FactSet <- FactSet()
    
    df <- FactSet %>% filter(Exercise %in% SelectedExercise)
    
    df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
    df$Month <- format(df$Date, "%Y-%m")
    df$Value <- as.numeric(df$Load)
    
    ggplot(df,
           aes(
             x = Date,
             y = Load,
             group=Exercise
           )) +
      geom_line(linewidth=2) +
      geom_point(size=3) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
      geom_smooth(method = "lm", se = FALSE) #+
    #geom_text(aes(label = Load), vjust = -1)
    
  })
  
  output$PlotMetric <- renderPlot({
    
    FactMeasure <- FactMeasure()
    
    SelectedCol <- c("Date", input$SelectMetric)
    
    df <- FactMeasure %>% select(all_of(SelectedCol))
    
    names(df)[2] <- "Value"
    
    df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
    df$Month <- format(df$Date, "%Y-%m")
    df$Value <- as.numeric(df$Value)
    
    df$Group = 1
    
    ggplot(df,
           aes(
             x = Date,
             y = Value,
             group=Group
           )) +
      geom_line(linewidth=2) +
      geom_point(size=3) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
      geom_smooth(method = "lm", se = FALSE)# +
    #geom_text(aes(label = Value), vjust = -1)
    
    
  })
  
  observeEvent(input$EditMeasure, {
    edited_data <- FactMeasure()
    edited_data[input$SelectMeasureId, "Date"] <- as.character(input$MDate)
    edited_data[input$SelectMeasureId, "WeightKG"] <- input$WeightKG
    edited_data[input$SelectMeasureId, "BodyFat"] <- input$BodyFat
    edited_data[input$SelectMeasureId, "MuscleMass"] <- input$MuscleMass
    edited_data[input$SelectMeasureId, "Sleep_hrs_min"] <- input$Sleep_hrs_min
    edited_data[input$SelectMeasureId, "Nap_hrs_min"] <- input$Nap_hrs_min
    edited_data[input$SelectMeasureId, "Steps"] <- input$Steps
    FactMeasure(edited_data)
    write.csv(FactMeasure(), "data/FactMeasure.csv", row.names = FALSE)
  })
  
  
  observeEvent(input$EditSet, {
    edited_data <- FactSet()
    edited_data[input$SelectSetId, "Date"]      <- as.character(input$Date)
    edited_data[input$SelectSetId, "Exercise"]  <- input$Exercise
    edited_data[input$SelectSetId, "Load"]      <- input$Load
    edited_data[input$SelectSetId, "RepTarget"] <- input$RepTarget
    edited_data[input$SelectSetId, "RepActual"] <- input$RepActual
    edited_data[input$SelectSetId, "Note"]      <- input$Note
    FactSet(edited_data)
    write.csv(FactSet(), "data/FactSet.csv", row.names = FALSE)
  })
  
  output$DataSelectedExercise <- renderDT({
    
    SelectedExercise <- input$ExerciseBreakdown
    
    FactSet <- FactSet()
    
    df <- FactSet %>% filter(Exercise %in% SelectedExercise) %>% arrange(desc(Id))
    
    datatable(df,
              options = list(pageLength = -1),
              rownames = FALSE)
  })
}
##############################################################################
# RUN APPLICATION
##############################################################################
shinyApp(ui = ui, server = server)
##############################################################################
################################### END ######################################
##############################################################################

# work in progress
# SleepMins = (as.integer(Sleep_hrs_min) * 60 ) + (Sleep_hrs_min - as.integer(Sleep_hrs_min))*100,
# NapMins = (as.integer(Nap_hrs_min) * 60 ) + (Nap_hrs_min - as.integer(Nap_hrs_min))*100,
# TotalSleepMins = SleepMins + NapMins,
# SleepHrsAdj = 0,
# NapHrsAdj = 0,
# TotalSleepHrsAdj = TotalSleepMins/60
