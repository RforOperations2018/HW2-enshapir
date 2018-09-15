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

#read in the survey data and data dicitonary 
Jail_Raw1 <- read.csv(file = JailFile1, header = TRUE, sep = ",")
Jail_Raw2 <- read.csv(file = JailFile2, header = TRUE, sep = ",")
Jail_Raw3 <- read.csv(file = JailFile3, header = TRUE, sep = ",")
Jail_Raw4 <- read.csv(file = JailFile4, header = TRUE, sep = ",")
Jail_Raw5 <- read.csv(file = JailFile5, header = TRUE, sep = ",")

#combine all the files
JailTotal <- bind_rows(Jail_Raw1, Jail_Raw2, Jail_Raw3, Jail_Raw4, Jail_Raw5)

#removing row with age value of -1 which is not possible
JailTotal <- JailTotal %>% filter(Current.Age != -1)


#creating counts for graphing of number of indviduals by age, all indivduals without an age are removed. 
AgeByDate <- JailTotal %>% 
  filter(!is.na(Current.Age)) %>%  
  count(Date, Current.Age)

#looing at average age by gender
AvgAgeByDayGender <- JailTotal %>% 
  filter(!is.na(Current.Age)) %>%  
  group_by(Date, Gender) %>% 
  summarise(MeanAge = mean(Current.Age))

#for looking at race/age brekdown of under 18 population
RaceByAge<- JailTotal %>% 
  filter(Current.Age>10&Current.Age<18) %>%  
  count(Race, Current.Age) %>% as.data.frame()


#for looking at race/age brekdown of under 18 population
RaceAgeGender<- JailTotal %>% 
  filter(!is.na(Current.Age)) %>%  
  group_by(Race, Current.Age, Gender) %>% 
  summarise(MeanAge = mean(Current.Age))

# Define UI for application that draws a histogram
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
                                      selected = c("A", "B")),
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
             tabPanel("Race/Gender Makeup by Age",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput(inputId = "DaysIncluded", 
                                      label = "How Many Day to Include",
                                      value = 22, 
                                      min = 0, 
                                      max =22,
                                      step = 2
                          )
                        ),
                        # Output plot
                        mainPanel(
                          plotOutput(outputId = "DietAverages")
                        )
                      )
             ),
             # Data Table
             tabPanel("The Data",
                      inputPanel(
                        downloadButton("downloadData","Download Jail Data")
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

  output$Raceplot <- renderPlotly({
    dat <- swInput()
    ggplotly(
      ggplot(data = dat, aes(x = Gender, y = MeanAge, color = Race, text = paste0("<br>Race: ", Race,
                                                                                  "<br>Gender: ", Gender,
                                                                                  "<br>Mean Age: ", MeanAge))) + 
        geom_jitter()+
        guides(color = FALSE)
      , tooltip = "text")
  })  
  
    output$Under18plot <- renderPlotly({
      dat <- swInput2()
      ggplot(data = dat, aes(x = Race, y = n, fill = Race)) + geom_bar(stat = "identity")
  })  
  
  
  observeEvent(input$reset, {
    updateSelectInput(session, "SelectedRace", selected = c("A", "B"))
    showNotification("You have successfully reset to show all races", type = "message")
  })
  
  output$Chickenplot <- renderPlot({
    #chickData <- filter(.data = Chicken.Weight, Chick == input$ChickToDisplay) Broken or just didn't want to over do it??
    ggplot(data = Chicken.Weight, aes(x = Time, y = weight, color = Diet)) + 
      geom_point() + 
      labs(x="Days Since Birth", y="Chick's Weight (gm)", title = "Chicken's Weight Since Birth")
  })
  output$DietAverages <- renderPlot({
    #chickData <- filter(.data = Chicken.Weight, Chick == input$ChickToDisplay)
    ggplot(data = DietAverages, aes(x = Diet, y = Avg_Weight, fill = Diet)) + 
      geom_bar(stat = "identity")+
      labs(x="Experimental Diet Type", y="Average Weight (gm) of Chicks", title = "Average Weight of Chicks by Diet Type")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)