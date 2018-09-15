# ----------------------------
# HW2 - enshapir
# ----------------------------

library(shiny)
library(tidyverse)
library(plotly)
library(DT)

#quarries the system for the directory of this file and then sets the directory of the file. 
current_file_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_file_path)

#data from Western PA 
#https://data.wprdc.org/dataset/allegheny-county-jail-daily-census

#file path names of the data used
JailFile1 <- file.path('open_data_files', '8fb43d9a-9fb9-431d-93d2-9be7330dc846.csv', fsep = .Platform$file.sep)
JailFile2 <- file.path('open_data_files', '35b19857-e653-4801-a4ba-022365341ce9.csv', fsep = .Platform$file.sep)
JailFile3 <- file.path('open_data_files', '85e0c576-41af-403d-a994-a27834b15157.csv', fsep = .Platform$file.sep)
JailFile4 <- file.path('open_data_files', 'aa4cae88-0831-46ec-8853-7707202f88f4.csv', fsep = .Platform$file.sep)
JailFile5 <- file.path('open_data_files', 'fd3b3cae-355c-46ca-a505-b1f0672153c7-5.csv', fsep = .Platform$file.sep)

#read in the data
Jail_Raw1 <- read.csv(file = JailFile1, header = TRUE, sep = ",")
Jail_Raw2 <- read.csv(file = JailFile2, header = TRUE, sep = ",")
Jail_Raw3 <- read.csv(file = JailFile3, header = TRUE, sep = ",")
Jail_Raw4 <- read.csv(file = JailFile4, header = TRUE, sep = ",")
Jail_Raw5 <- read.csv(file = JailFile5, header = TRUE, sep = ",")

#combine all the files
JailTotal <- bind_rows(Jail_Raw1, Jail_Raw2, Jail_Raw3, Jail_Raw4, Jail_Raw5)

#removing row with age value of -1 which is not possible
JailTotal <- JailTotal %>% filter(Current.Age != -1)

JailTotal$Race <- plyr::revalue(JailTotal$Race, replace = c("A"="ASIAN", 
                                                      "B"="BLACK",
                                                      "H"="HISPANIC",
                                                      "I"="NATIVE AMERICAN",
                                                      "U"="UNKNOWN",
                                                      "W"="WHITE"))


#count by Date, Race, Current Age grouping
AgeRaceByDay <- JailTotal %>% 
  filter(!is.na(Current.Age)) %>%  
  count(Date, Race, Current.Age) %>% 
  as.data.frame()
#date conversion 
AgeRaceByDay$Date <- as.Date(AgeRaceByDay$Date, "%m/%d/%Y")

#Count by grouping Race, Current Age for under 18 populaton
RaceByAge<- JailTotal %>% 
  filter(Current.Age>10&Current.Age<18) %>%  
  count(Race, Current.Age) %>% as.data.frame()


#grouping by Race, Gender and getting mean age
RaceAgeGender<- JailTotal %>% 
  filter(!is.na(Current.Age)) %>%  
  group_by(Race, Gender) %>% 
  summarise(MeanAge = mean(Current.Age))

# Define UI
ui <- fluidPage(
  
  navlistPanel("State of Allegheny County Jail", 
             tabPanel("Average Age of Detainees by Race",
                      sidebarLayout(
                        sidebarPanel(
                          # Race Select
                          selectInput("SelectedRace",
                                      "Race:",
                                      choices = sort(unique(RaceAgeGender$Race)),
                                      multiple = T,
                                      selectize = T,
                                      selected = c("ASIAN", "BLACK")),
                          actionButton("reset", "Reset Filters", icon = icon("refresh"))
                        ),
                        # Output plot
                        mainPanel(
                          plotlyOutput("Raceplot")
                        )
                      )
             ),
             # figure 2
             tabPanel("Under 18 by Race",
                      sidebarLayout(
                        sidebarPanel(
                          # Date Selection
                          sliderInput("SelectedAge",
                                      "Age:",
                                      min = min(RaceByAge$Current.Age, na.rm = TRUE),
                                      max = max(RaceByAge$Current.Age, na.rm = TRUE),
                                      value = c(min(RaceByAge$Current.Age, na.rm = TRUE), 
                                                max(RaceByAge$Current.Age, na.rm = TRUE)),
                                      step = 1)
                        ),
                        # Output plot
                        mainPanel(
                          plotlyOutput("Under18plot")
                        )
                      )
             ),
             #figure 3
             tabPanel("Race Makeup by Age",
                      sidebarLayout(
                        sidebarPanel(
                          dateInput(inputId = "date", 
                                    label ="Choose a Date", 
                                    value = min(AgeRaceByDay$Date, na.rm = TRUE),
                                    min = min(AgeRaceByDay$Date, na.rm = TRUE),
                                    max = max(AgeRaceByDay$Date, na.rm = TRUE)
                          )
                        ),
                        # Output plot
                        mainPanel(
                          plotlyOutput(outputId = "DateRacePlot")
                        )
                      )
             ),
             # Data Table
             tabPanel("Jail Data",
                      inputPanel(
                        downloadButton(outputId = "DataForDownload",
                                       label = "Download Jail Data")
                      ),
                      fluidPage(DT::dataTableOutput("table"))
             )
  )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered Race Age Data
  swInput <- reactive({
    # Race Filter
    if (length(input$SelectedRace) > 0 ) {
      RaceAgeGender.server <- subset(RaceAgeGender, Race %in% input$SelectedRace)
    }
    
    return( RaceAgeGender.server)
  })

  # Filtered under 18 Date
  swInput2 <- reactive({
    under18 <- RaceByAge %>%
      # Slider Filter
      filter(Current.Age >= input$SelectedAge[1] & Current.Age <= input$SelectedAge[2]) %>% 
      as.data.frame()
  })
  
  # Filtered by date
  swInput3 <- reactive({
    AgeRaceByDay.server <- AgeRaceByDay %>%
      # Slider Filter
      filter(Date == input$date) %>% 
      as.data.frame()
  })
  
  swInput4 <- reactive({
    JailTotal.server <- JailTotal
  })
#outplut for plot 1 showing the average age for each gender by race
  output$Raceplot <- renderPlotly({
    dat <- swInput()
    ggplotly(
      ggplot(data = dat, aes(x = Gender, y = MeanAge, color = Race, text = paste0("<br>Race: ", Race,
                                                                                  "<br>Gender: ", Gender,
                                                                                  "<br>Mean Age: ", round(MeanAge, 2)))) + 
        geom_point()+
        guides(color = FALSE)+
        labs(x="Gender", y="Average Age", title = "Average Age of Gender by Race"),
        tooltip = "text")
  })  
  #output for plot 2 showing the number of individuals by race between specific ages
    output$Under18plot <- renderPlotly({
      dat2 <- swInput2()
      ggplotly(
      ggplot(data = dat2, aes(x = Race, y = n, fill = Race, text = paste0("<br>Race: ", Race,
                                                                         "<br>Count: ", n))) + 
        geom_bar(stat = "identity")+
        labs(x="Race", y="Count of Individuals", title = "Count of Individuals by Race between Specified Ages"),
      tooltip = "text")
  })  
  #output of plot 3 showing the distributoin of of each race by age on a specific day
    output$DateRacePlot <- renderPlotly({
      dat3 <- swInput3()
      ggplotly(
        ggplot(dat3, aes(x=Current.Age, y= n, fill=Race)) +
                 geom_bar(stat="identity")+
                 facet_wrap(~Race)+
          labs(x="Age", y="Count of Individuals", title = "Distribution of Age by Race on a Specific Day")
        )
    })  
    
    #reset for plot 1
  observeEvent(input$reset, {
    updateSelectInput(session, "SelectedRace", selected = c("ASIAN", "BLACK"))
    showNotification("You have successfully reset to show all races", type = "message")
  })
  
  #Output the Data Table
  output$table <- DT::renderDataTable({
    JailData <- swInput4()

  })
  
  # Download the Jail Data
  output$DataForDownload <- downloadHandler(
    filename = function() {
      paste("Jail_Data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(swInput4(), file)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)