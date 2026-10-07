#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
################################################################################
##################################TEACHER SIDE##################################
################################################################################
library(shiny)
library(shinyalert)
library(stringr)
library(googlesheets4)
library(dplyr)

library(googledrive)
source("google-auth.R", local = TRUE)



ui <- fluidPage(
  
  h1("Teacher Template Creator"),
  
  h3(""),
  
  h4("Enter your questions and answers here. Must include at least one question."),
  
  h3(""),
  
  
  textInput("title", "New Spreadsheet Title"),
  textInput("email", "Enter Email: "),
  textInput("c", "Enter Contest Title: "),
  
  
  sidebarLayout(
    sidebarPanel(textInput("q1", "Question 1:")),
    mainPanel(textInput("a1", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q2", "Question 2:")),
    mainPanel(textInput("a2", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q3", "Question 3:")),
    mainPanel(textInput("a3", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q4", "Question 4:")),
    mainPanel(textInput("a4", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q5", "Question 5:")),
    mainPanel(textInput("a5", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q6", "Question 6:")),
    mainPanel(textInput("a6", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q7", "Question7:")),
    mainPanel(textInput("a7", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q8", "Question 8:")),
    mainPanel(textInput("a8", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q9", "Question 9:")),
    mainPanel(textInput("a9", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q1", "Question 1:")),
    mainPanel(textInput("a1", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q1", "Question 1:")),
    mainPanel(textInput("a1", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q10", "Question 10:")),
    mainPanel(textInput("a10", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q11", "Question 11:")),
    mainPanel(textInput("a11", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q12", "Question 12:")),
    mainPanel(textInput("a12", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q13", "Question 13:")),
    mainPanel(textInput("a13", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q14", "Question 14:")),
    mainPanel(textInput("a14", "Answer: "))
  ),
  
  sidebarLayout(
    sidebarPanel(textInput("q15", "Question 15:")),
    mainPanel(textInput("a15", "Answer: "))
  ),
  
  h3(""),
  
  actionButton("submit", "Submit"),
  
  h3("")
  
)

server <- function(input, output) {
  
  text <- reactiveVal("")
  
  observeEvent(input$submit == TRUE,
               
               if (input$title != "")    
               {df <- data.frame(Question = 1:15,
                                 Question_Text =  c(input$q1, input$q2, input$q3, input$q4, input$q5, input$q6, input$q7, input$q8, input$q9, input$q10, input$q11, input$q12, input$q13, input$q14, input$q15),
                                 A = c(input$a1, input$a2, input$a3, input$a4, input$a5, input$a6, input$a7, input$a8, input$a9, input$a10, input$a11, input$a12, input$a13, input$a14, input$a15))
               submissions <- data.frame("Team" = "", "Question" = "", "Lower" = "", "Upper" = "", "Score" = "")
               sheet <- gs4_create(name = input$title, sheets = list(answers = df, submissions = submissions)) %>%
                 drive_share(role = "writer", type = "user", emailAddress = as.character(input$email))
               sheet_append("https://docs.google.com/spreadsheets/d/1xNZXuFVriHnLpwBilEH5BulwEw4b566Tv4IK_9JzsU4/edit#gid=0", data.frame(as_sheets_id(sheet), as.character(input$c)))
               }
  )
  observeEvent(input$submit, {
    # Show a modal when the button is pressed
    shinyalert("Thank you for submitting.", 
               "Check your email for the document!",
               type = "success")
  })
}



# Run the application
shinyApp(ui = ui, server = server)