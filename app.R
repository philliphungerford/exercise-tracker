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
library(shinyWidgets)
library(dplyr) # filtering
library(RColorBrewer)
library(ggplot2) # for figures
library(tools)
library(tidyverse) # adds to title case
library(scales) # add comma to output
library(DT)
library(fresh)
library(plotly) # convert ggplot to plotly and outputs ggplotly(p)
##############################################################################
# SETTINGS
github_link  <-
  'https://github.com/philliphungerford/exercise-tracker'
website_link <- 'https://philliphungerford.github.io'

# Tab Names
TabNames <- c("WeightLifting")

round_to_nearest_5 <- function(x) {
  rounded <- round(x / 5) * 5
  return(rounded)
}

WendlerCalc <- function(MaxWeight) {
  PercentageMatrix <- data.frame(
    Week1 = c(0.65, 0.75, 0.85),
    Week2 = c(0.70, 0.80, 0.90),
    Week3 = c(0.75, 0.85, 0.95),
    Week4 = c(0.40, 0.50, 0.60)
  )
  
  Max90 = MaxWeight * 1.06
  #Max100 = Max90 * 1.10
  
  return(ceiling(Max90 * (PercentageMatrix / 5)) * 5)
}

# Function to get the current week number from the CSV file
getWeek <- function() {
  week_data <- read.csv("data/ParameterWeek.csv")
  return(week_data$week)
}

# Function to set the current week number in the CSV file
setWeek <- function(week) {
  write.csv(data.frame(week = week),
            "data/ParameterWeek.csv",
            row.names = FALSE)
}

getExercise <- function() {
  exercise_data <- read.csv("data/ParameterExercise.csv")
  return(exercise_data$exercise)
}

setExercise <- function(exercise) {
  write.csv(data.frame(exercise = exercise),
            "data/ParameterExercise.csv",
            row.names = FALSE)
}

ChoicesExercise <- read.csv("data/DimExercises.csv")[,2]

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
    titleWidth = 150,
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
  dashboardSidebar(
    width=150,
    collapsed = TRUE,
   includeCSS("./www/mytheme.css"),
    sidebarMenu(# icons from (https://fontawesome.com/v4.7.0/icons/)
      menuItem(
        TabNames[1],
        tabName = TabNames[1],
        icon = icon("universal-access")
      ))
  ),
  #=========================================================================
  ## Body content
  
  dashboardBody(tabItems(
    tabItem(
      tabName = TabNames[1],
      
      fluidRow(column(
        7,
        h2(TabNames[1]),
        p("Note that Load is in pounds (lbs) and is converted to kg.")
      ),
      column(
        5,
        div(
          class = "text-right",
          br(),
          br(),
          actionButton("btn_ref_plate", "Plate Reference", style = "background-color: grey; color: white;"),
          actionButton("btn_1rm", "Rep Calculator")
        ),
        p()
      )),
      
      fluidRow(
        box(width=12, collapsible = TRUE, collapsed = FALSE, title = "Summary",class = "custom-box",
            
            #--
            fluidRow(
              column(width = 3, uiOutput("BoxDeadliftMax")),
              column(width = 3, uiOutput("BoxSquatMax")),
              column(width = 3, uiOutput("BoxBenchMax")),
              column(width = 3, uiOutput("BoxPressMax"))
            ),
            
            fluidRow(column(
              width = 12,
              plotlyOutput("PlotMaxLifts"),
              selectInput(
                "SelectFrequency",
                "Frequency",
                choices = c("Monthly", "Weekly", "Daily")
              )
            ))
            #--
      )),
      
      #-------------------------------------------------------------------------
      # Wendler Panel
      fluidRow(
        box(width=12, collapsible = TRUE, collapsed = TRUE, title = "Wendler 531 Reference Table",class = "custom-box",
            
            #--
            fluidRow(
              column(
                8,
                p(
                  "Reference Lifts for Wendler Proportions (Updated automatically based on max lifts). Select the week you are in and the lift you are doing for the day. This tracks main lifts/movements and does not include accessory lifts."
                ),
              ),
              column(
                2,
                selectInput(
                  "WendlerWeek",
                  "Current Week:",
                  choices = 1:4,
                  selected = getWeek()
                ),
              ),
              column(
                2,
                selectInput(
                  "WendlerExercise",
                  "Exercise:",
                  choices = c('Deadlift', 'Squat', 'Bench', 'Press'),
                  selected = getExercise()
                ),
              )
            ),
            
            fluidRow(column(width = 12,
                            DTOutput("WendlerTable"),)),
            #--
            )
      ),

      #-------------------------------------------------------------------------
      fluidRow(
        box(width=12, collapsible = TRUE, collapsed = FALSE, title = "Input Set", class = "custom-box", 
            #--
            fluidRow(
              column(width = 2,
                     dateInput("Date", "Date", value = Sys.Date()),),
              column(
                width = 2,
                selectInput(
                  "Exercise",
                  "Exercise",
                  choices = ChoicesExercise,
                  selected = getExercise()
                )
              ),
              column(width = 1,
                     numericInput("Load", "Load", value = 0), ),
              column(width = 1,
                     numericInput("RepTarget", "Rep Target", value = 0), ),
              column(width = 1,
                     numericInput("RepActual", "Rep Actual", value = 0), ),
              column(width = 3,
                     textInput("Note", "Note"), ),
              
              column(2,
                     
                     div(
                       class = "text-right",
                       actionButton("AddSet", "Add Set", style = 'margin-top:25px; color: white; background-color: #28A745; border-color: #28A745;'),
                       actionButton("EditSet", "Edit Set", style = 'margin-top:25px; color: white; background-color: #FFA500; border-color: #FFA500;'),
                     ),),
            ),
            #--
        )
      ),
      #-------------------------------------------------------------------------
      # data table
      fluidRow(
        box(width=12, collapsible = TRUE, collapsed = FALSE, title = "Sets",class = "custom-box",
            #--
            fluidRow(column(width = 12,
                            DTOutput("FactSetAll"))),
            fluidRow(column(
              width = 12,
              div(
                class = "text-right",
                downloadButton("DownloadData", "Download", style = 'margin-top:25px; color: #000000; background-color: #00FF00; border-color: #00FF00;'),
              ),
            ), ),
            #--
            )
      )
    )
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
  DimPlate <- read.csv("data/DimPlate.csv")
  FactSet <- reactiveVal(read.csv("data/FactSet.csv"))
  FactMeasure <- reactiveVal(read.csv("data/FactMeasure.csv"))
  
  observeEvent(input$WendlerWeek, {
    setWeek(input$WendlerWeek)
  })
  
  observeEvent(input$WendlerExercise, {
    setExercise(input$WendlerExercise)
  })
  
  output$BoxDeadliftMax <- renderValueBox({
    valueBox(
      value = ("Deadlift"),
      subtitle =         paste0((max(
        FactSet()$Load[FactSet()$Exercise == "Deadlift"], na.rm = T
      )),
      " lbs (", round((
        max(FactSet()$Load[FactSet()$Exercise == "Deadlift"], na.rm = T)
      ) / 2.2, 2), " kg)"),
      color = "blue",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxSquatMax <- renderValueBox({
    valueBox(
      value = ("Squat"),
      subtitle =         paste0((max(
        FactSet()$Load[FactSet()$Exercise == "Squat"], na.rm = T
      )),
      " lbs (", round((
        max(FactSet()$Load[FactSet()$Exercise == "Squat"], na.rm = T)
      ) / 2.2, 2), " kg)"),
      color = "red",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxBenchMax <- renderValueBox({
    valueBox(
      value = ("Bench"),
      subtitle = paste0((max(
        FactSet()$Load[FactSet()$Exercise == "Bench"], na.rm = T
      )),
      " lbs (", round((
        max(FactSet()$Load[FactSet()$Exercise == "Bench"], na.rm = T)
      ) / 2.2, 2), " kg)"),
      color = "orange",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxPressMax <- renderValueBox({
    valueBox(
      value = ("Press"),
      subtitle = paste0((max(
        FactSet()$Load[FactSet()$Exercise == "Press"], na.rm = T
      )),
      " lbs (", round((
        max(FactSet()$Load[FactSet()$Exercise == "Press"], na.rm = T)
      ) / 2.2, 2), " kg)"),
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
  
  # observeEvent(input$DeleteSet, {
  #   updated_data <- FactSet()[!FactSet()$Id == input$SelectSetId, ]
  #   FactSet(updated_data)
  #   write.csv(FactSet(), "data/FactSet.csv", row.names = FALSE)
  # })
  
  observeEvent(input$DeleteSet, {
    sel <- input$FactSetAll_rows_selected  # Get selected row
    
    if (length(sel) > 0) {
      curr_data <- FactSet()
      curr_data <- curr_data[-sel, ]  # Remove the selected row
      FactSet(curr_data)  # Update the reactive data
      
      # Write the updated data back to the CSV file
      write.csv(curr_data, "data/FactSet.csv", row.names = FALSE)
    }
    removeModal()
    
  })
  
  output$FactSetAll <- renderDT({
    Results <-
      FactSet()
    
    datatable(
      Results,
      rownames = FALSE,
      selection = 'single',
      editable = FALSE,
      options = list(
        scrollY = '500px',
        scrollX = FALSE,
        paging = FALSE,
        columnDefs = list(list(
          className = "nowrap", targets = "_all"
        )),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().node()).closest('.dataTables_scrollBody').scrollTop($(this.api().table().node()).height());",
          "}"
        )
      ),
    )
    
  })
  
  ## Edit Publication Modal
  
  observeEvent(input$EditSet, {
    SelectedRow <- input$FactSetAll_rows_selected
    
    if (length(SelectedRow) == 0) {
      showNotification("No row selected.")
      return()
    }
    
    EditData <- FactSet()[SelectedRow, , drop = FALSE]
    
    showModal(
      modalDialog(
        title = "Edit Set",
        h4(paste0("ID: "), EditData$Id),
        dateInput("editDate", "Set Date:", value = as.character(EditData$Date)),
        selectInput(
          "editExercise",
          "Exercise",
          choices = ChoicesExercise,
          selected = EditData$Exercise
        ),
        numericInput("editLoad", "Load", value = EditData$Load),
        numericInput("editRepTarget", "RepTarget", value = EditData$RepTarget),
        numericInput("editRepActual", "RepActual", value = EditData$RepActual),
        textInput("editNote", "Note", value = EditData$Note),
        footer = tagList(
          actionButton(
            "btn_update",
            "Update",
            class = "btn-success",
            style = "color: white;"
          ),
          actionButton(
            "DeleteSet",
            "Delete Set",
            class = "btn-danger" ,
            style =  "color: white;"
          ),
          modalButton("Cancel")
        )
      )
    )
  })
  
  
  observeEvent(input$btn_update, {
    SelectedRow <- input$FactSetAll_rows_selected
    
    if (length(SelectedRow) == 0) {
      showNotification("No publication selected.")
      return()
    }
    
    tmp <- FactSet()
    
    tmp[SelectedRow, "Date"] <-  as.character(input$editDate)
    tmp[SelectedRow, "Day"] <-  weekdays(input$editDate)
    tmp[SelectedRow, "Exercise"] <-  input$editExercise
    tmp[SelectedRow, "Load"] <-  input$editLoad
    tmp[SelectedRow, "RepTarget"] <-  input$editRepTarget
    tmp[SelectedRow, "RepActual"] <-  input$editRepActual
    tmp[SelectedRow, "Note"] <-  input$editNote
    
    # write csv
    FactSet(tmp)
    write.csv(tmp, "data/FactSet.csv", row.names = FALSE)
    removeModal()
    
  })
  
  output$TBLOneRepMax <- renderDT({
    
    # get max lifts
    MaxDeadlift <- max(FactSet()$Load[FactSet()$Exercise == "Deadlift"], na.rm = T)
    MaxSquat    <- max(FactSet()$Load[FactSet()$Exercise == "Squat"], na.rm = T)
    MaxBench    <- max(FactSet()$Load[FactSet()$Exercise == "Bench"], na.rm = T)
    MaxPress    <- max(FactSet()$Load[FactSet()$Exercise == "Press"], na.rm = T)
    
    # Create matrix for percentages 
    RepRanges      <- c(1,2,3,4,5,8,10,15)
    RepPercentages <- c(1, 0.97, 0.94, 0.92, 0.89, 0.80, 0.75, 0.67)
  
    # Calculate Max
    RepDead  <- ceiling((MaxDeadlift * RepPercentages)/5)*5
    RepSquat <- ceiling((MaxSquat * RepPercentages)/5)*5
    RepBench <- ceiling((MaxBench * RepPercentages)/5)*5
    RepPress <- ceiling((MaxPress * RepPercentages)/5)*5
    
    # Create table 
    Results <- data.frame(
      Reps = RepRanges,
      RepPercentages =  RepPercentages*100,
      Deadlift = RepDead,
      Squat = RepSquat,
      Bench = RepBench,
      Press = RepPress
    )
    
    datatable(
      Results,
      rownames = FALSE,
      selection = 'single',
      editable = FALSE,
      options = list(
        scrollY = '300px',
        scrollX = FALSE,
        paging = FALSE,
        searching=FALSE,
        ordering=FALSE,
        columnDefs = list(list(
          className = "nowrap", targets = "_all"
        ))
      ),
    )
    
  })
  
  output$WendlerTable <- renderDT({
    ResultsReps <-
      data.frame(
        Set = c(1, 2, 3),
        Week1 = c(5, 5, 5),
        Week2 = c(3, 3, 3),
        Week3 = c(5, 3, 1),
        Week4 = c(5, 5, 5)
      )
    
    ResultsDeadlift <-
      WendlerCalc(max(FactSet()$Load[FactSet()$Exercise == "Deadlift"], na.rm = T)) %>% mutate(Exercise = "Deadlift", Set = c(1, 2, 3))
    ResultsSquat    <-
      WendlerCalc(max(FactSet()$Load[FactSet()$Exercise == "Squat"], na.rm = T)) %>% mutate(Exercise = "Squat", Set = c(1, 2, 3))
    ResultsBench    <-
      WendlerCalc(max(FactSet()$Load[FactSet()$Exercise == "Bench"], na.rm = T)) %>% mutate(Exercise = "Bench", Set = c(1, 2, 3))
    ResultsPress    <-
      WendlerCalc(max(FactSet()$Load[FactSet()$Exercise == "Press"], na.rm = T)) %>% mutate(Exercise = "Press", Set = c(1, 2, 3))
    
    Results <-
      rbind(ResultsDeadlift, ResultsSquat, ResultsBench, ResultsPress)
    
    Results <- Results %>%
      pivot_longer(cols = starts_with("Week"),
                   names_to = "Week",
                   values_to = "Load")
    
    ResultsReps <- ResultsReps %>%
      pivot_longer(cols = starts_with("Week"),
                   names_to = "Week",
                   values_to = "Repetitions")
    
    Results <- Results %>% left_join(ResultsReps)
    
    DimPlateMin <- DimPlate %>% select(Load, starts_with('Plate'))
    
    Results <- Results %>% left_join(DimPlateMin) %>%
      mutate(Week = as.numeric(str_replace(Week, "Week", ""))) %>%
      select(Exercise, Week, Set, everything())
    
    Results <-
      Results %>% filter(Week == input$WendlerWeek &
                           Exercise == input$WendlerExercise)
    
    datatable(
      Results,
      rownames = FALSE,
      selection = 'single',
      editable = FALSE,
      options = list(
        scrollY = '110px',
        scrollX = FALSE,
        paging = FALSE,
        searching=FALSE,
        ordering=FALSE,
        columnDefs = list(list(
          className = "nowrap", targets = "_all"
        ))
      ),
    ) %>%
      formatStyle(
        columns = c(
          'Plate_2_5',
          'Plate_5',
          'Plate_10',
          'Plate_25',
          'Plate_35',
          'Plate_45'
        ),
        backgroundColor = 'lightgrey'
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
    datatable(
      DimPlate,
      rownames = FALSE,
      selection = 'single',
      editable = FALSE,
      options = list(
        scrollY = '600px',
        scrollX = FALSE,
        paging = FALSE,
        columnDefs = list(list(
          className = "nowrap", targets = "_all"
        ))
      ),
    )
    
  })
  
  output$PlotMaxLifts <- renderPlotly({
    FactSet <- FactSet()
    
    FactSet$Date <- as.Date(FactSet$Date, format = "%Y-%m-%d")
    
    FactSet$Load <- FactSet$Load / 2.2
    
    filtered_data <-
      FactSet %>% select(Date, Exercise, Load) %>% filter(Exercise %in% c("Deadlift", "Squat", "Bench", "Press"))
    
    filtered_data$Year <- format(filtered_data$Date, "%Y")
    filtered_data$Month <- format(filtered_data$Date, "%Y-%m")
    filtered_data$Week <- format(filtered_data$Date, "%Y-%U")
    filtered_data$Day <- format(filtered_data$Date, "%Y-%m-%d")
    
    if (input$SelectFrequency == "Monthly") {
      Results <- filtered_data %>%
        group_by(Month, Exercise) %>%
        summarize(Max_Load = max(Load), .groups = "drop") %>%
        rename("Time" = "Month")
      
    } else if (input$SelectFrequency == "Weekly") {
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
    
    p <- ggplot(Results,
                aes(
                  x = Time,
                  y = Max_Load,
                  color = Exercise,
                  group = Exercise
                )) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      labs(
        title = "Max Load by Exercise",
        x = as.character(input$SelectFrequency),
        y = "Max Load"
      ) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
      scale_color_manual(values = c(
        "Deadlift" = "blue",
        "Squat" = "red",
        "Bench" = "orange",
        "Press" = "green"
      ))
    
    ggplotly(p)
    
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
    new_id <- (max(FactMeasure()$Id, na.rm = T) + 1)
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
      value = FactMeasure()$WeightKG[FactMeasure()$Date == max(FactMeasure()$Date, na.rm =
                                                                 T)],
      subtitle = "Weight KG",
      color = "blue",
      icon = icon("arrow-up")
      
    )
  })
  
  output$BoxBodyFat <- renderValueBox({
    valueBox(
      value = FactMeasure()$BodyFat[FactMeasure()$Date == max(FactMeasure()$Date, na.rm =
                                                                T)],
      subtitle = "Current Body fat %",
      color = "red",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxMuscleMass <- renderValueBox({
    valueBox(
      value = FactMeasure()$MuscleMass[FactMeasure()$Date == max(FactMeasure()$Date, na.rm =
                                                                   T)],
      subtitle = "Current Muscle Mass",
      color = "orange",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxAverageSleep <- renderValueBox({
    valueBox(
      value = round(mean(FactMeasure()$Sleep_hrs_min, na.rm = T), 2),
      subtitle = "Sleep Average",
      color = "green",
      icon = icon("arrow-up")
    )
  })
  
  output$BoxAverageSteps <- renderValueBox({
    valueBox(
      value = round(mean(FactMeasure()$Steps, na.rm = T), 2),
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
           aes(x = Date,
               y = Load,
               group = Exercise)) +
      geom_line(linewidth = 2) +
      geom_point(size = 3) +
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
           aes(x = Date,
               y = Value,
               group = Group)) +
      geom_line(linewidth = 2) +
      geom_point(size = 3) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
      geom_smooth(method = "lm", se = FALSE)# +
    #geom_text(aes(label = Value), vjust = -1)
    
    
  })
  
  observeEvent(input$EditMeasure, {
    edited_data <- FactMeasure()
    edited_data[input$SelectMeasureId, "Date"] <-
      as.character(input$MDate)
    edited_data[input$SelectMeasureId, "WeightKG"] <- input$WeightKG
    edited_data[input$SelectMeasureId, "BodyFat"] <- input$BodyFat
    edited_data[input$SelectMeasureId, "MuscleMass"] <-
      input$MuscleMass
    edited_data[input$SelectMeasureId, "Sleep_hrs_min"] <-
      input$Sleep_hrs_min
    edited_data[input$SelectMeasureId, "Nap_hrs_min"] <-
      input$Nap_hrs_min
    edited_data[input$SelectMeasureId, "Steps"] <- input$Steps
    FactMeasure(edited_data)
    write.csv(FactMeasure(), "data/FactMeasure.csv", row.names = FALSE)
  })
  
  #
  # observeEvent(input$EditSet, {
  #   edited_data <- FactSet()
  #   edited_data[input$SelectSetId, "Date"]      <- as.character(input$Date)
  #   edited_data[input$SelectSetId, "Exercise"]  <- input$Exercise
  #   edited_data[input$SelectSetId, "Load"]      <- input$Load
  #   edited_data[input$SelectSetId, "RepTarget"] <- input$RepTarget
  #   edited_data[input$SelectSetId, "RepActual"] <- input$RepActual
  #   edited_data[input$SelectSetId, "Note"]      <- input$Note
  #   FactSet(edited_data)
  #   write.csv(FactSet(), "data/FactSet.csv", row.names = FALSE)
  # })
  #
  output$DataSelectedExercise <- renderDT({
    SelectedExercise <- input$ExerciseBreakdown
    
    FactSet <- FactSet()
    
    df <-
      FactSet %>% filter(Exercise %in% SelectedExercise) %>% arrange(desc(Id))
    
    datatable(df,
              options = list(pageLength = -1),
              rownames = FALSE)
  })
  
  observeEvent(input$btn_1rm, {
    showModal(
      modalDialog(
        title = "Reps",
        size = "l",
        
        fluidRow(column(width = 12,
                        DTOutput("TBLOneRepMax"),)),
        easyClose = TRUE,
        # if you want to allow closing by clicking outside the modal or pressing Escape
        
      )
    )
  })
  
  
  observeEvent(input$btn_ref_plate, {
    showModal(
      modalDialog(
        title = "Plate Reference Guide Modal",
        size = "l",
        
        fluidRow(column(width = 12,
                        DTOutput("DimPlate"),)),
        fluidRow(column(
          12,
          div(
            class = "text-right",
            downloadButton("DownloadDimData", "Download Data", style = 'margin-top:25px; color: #000000; background-color: #00FF00; border-color: #00FF00;')
          ),
        )),
        
        easyClose = TRUE,
        # if you want to allow closing by clicking outside the modal or pressing Escape
        
      )
    )
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
