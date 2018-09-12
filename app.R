# ----------------------------
# HW2 - enshapir
# ----------------------------

library(shiny)
library(tidyverse)

#quarries the system for the directory of this file and then sets the directory of the file. 
current_file_path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_file_path)

#data from Western PA 

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


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

