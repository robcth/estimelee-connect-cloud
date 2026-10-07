# Loads a bunch of packages
library(shiny)
library(shiny.router)
library(shinyalert)
library(stringr)
library(googlesheets4)
library(googledrive)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(shinycssloaders)
library(bit64)
library(data.table)
library(tableHTML)
library(formattable)
library(memoise)
library(gargle)
library(httr)

source("google-auth.R", local = TRUE)

# Sets contest list and custom colors
contest_list <- read_sheet("https://docs.google.com/spreadsheets/d/1xNZXuFVriHnLpwBilEH5BulwEw4b566Tv4IK_9JzsU4/edit#gid=0")
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"
customPurple = "#ea78f5"
#
#
# est1 <- function(l, u, x) {
#   est <- (u + l) / 2
#   score <- log(abs(x - est) + 1) + log(abs(x - l) + 1) + log(abs(x - u) + 1)
#   return(score)
# }
#
# est2 <- function(l, u, x) {
#   est <- (u + l) / 2
#   scale <- 10;
#   score <- log(abs(1 - est/x) + 1) + log(abs(1 - l/x) + 1) + log(abs(1 - u/x) + 1);
#   score <- score * scale;
#   return(score)
# }
#
# est3 <- function(l, u, x) {
#   est <- (l + u)/2
#   scale <- 5
#   score <- abs(log(x) - log(est)) +
#     abs(log(x) - log(l + .Machine$double.eps)) +
#     abs(log(x) - log(u))
#   score <- scale * score
#   return(score)
# }
#
# est4 <- function(l, u, x) {
#   est = (u + l) / 2
#   scale = 10
#   if ((x/3 <= est) & (est <= 3*x)){
#     score <- log(abs(1 - est/x) + 1) + log(abs(1 - l/x) + 1) + log(abs(1 - u/x) + 1)
#   } else {
#     score <- abs(log(x) - log(est)) + abs(log(x) - log(l + .Machine$double.eps)) + abs(log(x) - log(u))
#   }
#   score = score * scale
#   return(score)
# }
#
# est5 <- function(l, u, x) {
#   est <- (l + u)/2
#   r <- abs((u - l)/2)
#   bonus <- 1.5^(log(abs(x - est)/r))
#   penalty <- 1
#   score <- log(abs(1 - est/x) + 1)
#   score <- score * bonus
#   return(score)
# }
#
# est6 <- function(l, u, x) {
#   est = (u + l) / 2
#   scale = 5
#   if (est < x * 10^(-4)) {
#     score <- 100
#   } else if ((x * 10^(-4) <= est) & (est < x/10)){
#     score <- abs(log(x) - log(est)) + abs(log(x) - log(l + .Machine$double.eps)) + abs(log(x) - log(u))
#   } else if ((x/10 <= est) & (est < 10*x)) {
#     score <- log(abs(1 - est/x) + 1) + log(abs(1 - l/x) + 1) + log(abs(1 - u/x) + 1)
#   } else if ((10 * x <= est) & (est < x * 10^(4))){
#     score <- abs(log(x) - log(est)) + abs(log(x) - log(l + .Machine$double.eps)) + abs(log(x) - log(u))
#   } else {
#     score <- 100
#   }
#   return(score)
# }
#
# est7 <- function(l, u, x) {
#   est <- (u + l) / 2
#   r <- abs(u - l)/2
#   scale <- 5
#   dist <- abs(est - x) - r
#   if (dist <= 0) {
#     dist <- 0
#     score <- 2*log(dist + 1) + log(r/x + 1)
#   } else {
#     score <- 2*log(dist + 1) + log(abs(est - x)/r + 1)/3
#   }
#
#   if (score >= 100){
#     score <- 100
#   }
#   return(score)
# }
#
# est8 <- function(l, u, x) {
#   score <- log(u/l)
#   if (x > u) {
#     score <- score + 6 * log(x/u)
#   }
#   if (x < l) {
#     score <- score + log(l/u)
#   }
#   if (score >= 13) {
#     score <- 13
#   }
#   return(score)
# }

est <- function(l, u, x) {
  r <- abs(u - l)/2
  est <- abs(u + l)/2
  if (r / x > 1) q <- 1/3
  else q <- 1/1.6
  s <- 3.8
  if ((l <= x) & (x <= u)) {
    scale <- 17.316
    score <- (r/x)^(q) * ((abs(est - x) + r)/(r+.Machine$double.eps))^(1/s)
    score <- scale * score
  } else {
    scale <- 1
    f <- 18.18
    g <- 0.7
    d <- 1.818
    h <- 0.6
    if (est >= x) {
      score <- d * (abs(est - x)/(r+.Machine$double.eps))^(h) + f * (est/x)^(g)
    } else {
      score <- d * (abs(est - x)/(r+.Machine$double.eps))^(h) + f * (x/est)^(g)
    }
    score <- score * scale
  }
}

est9 <- memoise(est)

ui <- function(request) {

  dashboardPage(
    header = dashboardHeader(title = "Estimelee Contest"
                             , dropdownMenuOutput("logoutbtn")
    ),
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "sidebarID",
        menuItemOutput("Join"),
        menuItemOutput("Play"),
        # tabName = "Play",
        # icon = icon("play")),
        menuItem(
          "Contact",
          icon = icon("calendar"),
          tabName = "contact",
          badgeLabel = "DON'T",
          badgeColor = "red", selected = FALSE
        ),
        menuItemOutput("Leaderboard"),
        menuItem(
          "How To Play",
          icon = icon("scale-balanced"),
          tabName = "rules",
          badgeLabel = "READ!",
          badgeColor = "purple", selected = TRUE
        )
      )
    ),
    body = dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(tabName = "Join", class = "active",
                div(
                  titlePanel("Join a Contest!"),
                  # Input: Type in your answer ----
                  textInput("contest", "Contest: "),
                  textInput("team", "Team: "),
                  # textInput("email", "Email: "),

                  br(),

                  actionButton("lock", "Submit"),

                  br(),

                  textOutput("error")

                )
        ),
        tabItem(tabName = "Play",
                div(

                  # App title ----
                  titlePanel("Score Board"),

                  br(),
                  # selectInput(inputId = "estAlg", label = "Estimation Algorithm",
                  #             choices = c("est1" = 1, "est2" = 2,
                  #                         "est3" = 3, "est4" = 4,
                  #                         "est5" = 5, "est6" = 6,
                  #                         "est7" = 7, "est8" = 8,
                  #                         "est9" = 9), selected = 9),

                  textOutput("stop"),

                  tags$head(tags$style("#stop{color: red;
                                 font-size: 20px;
                                 }"
                  )
                  ),

                  br(),

                  # Sidebar layout with input and output definitions ----
                  sidebarLayout(

                    # Sidebar panel for inputs ----
                    sidebarPanel(

                      # Input: Select question
                      # selectInput("question", h3("Question: "),
                      #             choices = list("1" = 1, "2" = 2,
                      #                            "3" = 3), selected = 1),

                      textInput("questions", "Question"),

                      # br() element to introduce extra vertical spacing ----
                      br(),

                      textOutput("q_text"),

                      tags$head(tags$style("#q_text{color: black;
                                 font-size: 25px;
                                 font-style: bold;
                                 }"
                      )
                      ),

                      br(),

                      numericInput("lower", "Lower: ", value = 1),

                      numericInput("upper", "Upper: ", value = 1),


                      # br() element to introduce extra vertical spacing ----
                      br(),

                      # Input: Submit answer ----
                      actionButton("try", "Submit"),

                      actionButton("update", "Update")

                    ),

                    # Main panel for displaying outputs ----
                    mainPanel(

                      h3("Your Past Submissions"),
                      shinycssloaders::withSpinner(formattableOutput("formatted_table"))
                    )
                  ),
                  # infoBoxes with fill=FALSE
                  fluidRow(
                    infoBoxOutput("toDoBox"),
                    infoBoxOutput("progressBox"),
                    infoBoxOutput("rankBox")
                  )
                )),
        tabItem(tabName = "Leaderboard",
                br(),
                br(),
                br(),
                actionButton("updateLeaderboard", "Update Leaderboard"),
                br(),
                br(),
                br(),
                fluidRow(column(12, align = "center", shinycssloaders::withSpinner(tableHTML_output("leaderboard"))))
        ),
        tabItem(tabName = "contact",
                h2("Don't contact us!"),
                br(),
                box(width = 10,
                    title = "But if you really want to contact us", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, collapsed = TRUE,
                    "You can try either one or both of our software development team:", br(), br(),
                    fluidRow(box(
                      title = "Jacob Aronson", status = "primary",
                      "aronsonj2@carleton.edu"),
                      box(
                        title = "Kunwu Lyu", status = "primary",
                        "lyuk@carleton.edu")
                    ), br(),
                    "or the person who pays both of us to do this:",
                    fluidRow(box(title = "Rob Thompson", status = "primary", "rthompson@carleton.edu")), br(),

                    "and we'll get back to you when we're done with finals.")


        ),
        tabItem(tabName = "rules",
                div(
                  h1("Welcome to an Estimelee Contest!", align = "center", style = "font-weight:bold"),
                  br(),
                  h2("This is an estimation contest that will push your estimating abilities to their limits.
                      This page should provide you with everything you need to know to participate in the contest.
                      If you have further questions, ask your contest host, or reach us using the Contact Page.", align = "center"),
                  br(),
                  h3("Joining:", style = "font-weight:bold"),  h3("Your contest host has already created a contest. To join, enter their contest title (must be identical) and a team name of your choice on the Join page. From here, go to the Play page to play."),
                  h3("Guessing:", style = "font-weight:bold"),  h3("The host has created a series of estimation questions for your team to answer.
                                                                   Your team will answer in the form of intervals, or ranges, consisting of a lower bound and an upper bound.
                                                                   In order to guess, type the number of the question you would like to answer into the Question field, type your lower and upper bounds, and click submit.
                                                                   Your team has three guesses for each question, and only the best guess will be kept."),
                  h3("Scoring:", style = "font-weight:bold"),  h3("Each of your guesses will receive a score based on its accuracy and precision, with lower scores being better than higher scores.
                                                                   Our scoring system encourages you to try to make guesses that contain the number you are trying to estimate.
                                                                   Among multiple guesses that do contain the true number, narrower intervals will score better.
                                                                   Be careful though, as overconfident guesses that do not contain the true numbers receive higher scores.
                                                                   The maximum score any guess can receive is 100 points."),
                  h3("Following Along:", style = "font-weight:bold"),  h3("After each submission, in order to see how you did, your team must click update, which will refresh a useful table showing your past guesses and how you did.
                                                                          The Leaderboard page has an overall scoreboard showing how every team is doing. Each cell in this table represents the current best guess a team has for a question.
                                                                          If a cell is green, that means the team's best guess contained the target number.
                                                                          The cells will also include check marks to show how many times the team has attempted a guess at that question.
                                                                          The last column of the leaderboard will show a ranking of the total scores of every team."),
                  br(),
                  h2("Enjoy and good luck!", align = "center", style = "font-weight:bold"),

                ))
      ))
  )
}


server <- function(input, output, session) {
  contest_list <- read_sheet("https://docs.google.com/spreadsheets/d/1xNZXuFVriHnLpwBilEH5BulwEw4b566Tv4IK_9JzsU4/edit#gid=0")

  timer <- reactiveTimer(30000)

  # Starts with join visible and logged out
  output$Join <- renderMenu({
    menuItem(text = "Join", icon = icon("th"), tabName = "Join", badgeLabel = "Logged Out", badgeColor = "red")
  })


  # Runs UI
  observeEvent(getQueryString(session)$tab, {
    currentQueryString <- getQueryString(session)$tab # alternative: parseQueryString(session$clientData$url_search)$tab
    if(is.null(input$sidebarID) || !is.null(currentQueryString) && currentQueryString != input$sidebarID){
      freezeReactiveValue(input, "sidebarID")
      updateTabItems(session, "sidebarID", selected = currentQueryString)
    }
  }, priority = 1)

  observeEvent(input$sidebarID, {
    currentQueryString <- getQueryString(session)$tab # alternative: parseQueryString(session$clientData$url_search)$tab
    pushQueryString <- paste0("?tab=", input$sidebarID)
    if(is.null(currentQueryString) || currentQueryString != input$sidebarID){
      freezeReactiveValue(input, "sidebarID")
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 0)

  output$stop <- renderText({"YOU CANNOT ENTER SUBMISSIONS UNTIL YOU HAVE CREATED A TEAM"})
  output$error <- renderText({"Contest Doesn't Exist"})

  shinyjs::disable("lower")
  shinyjs::disable("upper")
  shinyjs::disable("question")

  shinyjs::hide("error")

  locked <- TRUE

  observeEvent(input$lock, {


    if (!(input$contest %in% contest_list[[2]])) {
      shinyjs::show("error")
    }
    else if (input$team != ""){
      # Show a modal when the button is pressed
      shinyalert(title = "Thank you for submitting.", text = str_c("You are now team ", as.character(input$team),
                                                                   " in contest ", as.character(input$contest), "!"),
                 type = "success", closeOnEsc = TRUE,
                 closeOnClickOutside = TRUE, timer = 10000)
      shinyjs::disable("contest")
      shinyjs::disable("team")
      shinyjs::disable("lock")
      shinyjs::hide("stop")
      shinyjs::enable("lower")
      shinyjs::enable("upper")
      shinyjs::enable("question")
      shinyjs::hide("error")
      output$Play <- renderMenu({
        menuItem(text = "Play", tabName = "Play", icon = icon("play"))
      })
      output$Join <- renderMenu({
        menuItem(text = "Join", icon = icon("th"), tabName = "Join", badgeLabel = "Logged In!", badgeColor = "green")
      })
      output$Leaderboard <- renderMenu({
        menuItem(text = "Leaderboard", tabName = "Leaderboard", icon = icon("ranking-star"))
      })
      output$logoutbtn <- renderUI({
        tags$li(a(icon("right-from-bracket"), "Logout",
                  href="javascript:window.location.reload(true)"),
                class = "dropdown",
                style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
      })
      if (input$questions != "") {
        output$toDoBox <- renderInfoBox({
          infoBox(
            "You have", as.character(length(questions)), " questions in total", icon = icon("list"),
            color = "blue"
          )
        })
      }
    }
  }
  )



  c_id <- reactive({filter(contest_list, Title == as.character(input$contest))[[1,1]]})

  observeEvent(input$try == TRUE, {

    shinyjs::disable("update")
    shinyjs::disable("try")
    if (input$team != "")
    {Answer_sheet <- read_sheet(c_id(), sheet = "answers")
    num_questions <- length(1:sum(!is.na(Answer_sheet[[2]])))
    if (!is.na(input$lower) & !is.na(input$upper)){
      if ((input$questions == "") | (as.integer(input$questions) > 15) |
          (as.integer(input$questions) > num_questions) | (is.na(as.integer(input$questions)))) {
        shinyalert(title = "Question does not exist", text = "Make sure you enter the right question number", type = "error", closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE, timer = 10000)
      } else if (((is.na(as.numeric(input$lower)))) | (is.na(as.numeric(input$upper)))) {
        shinyalert(title = "Invalid Guess", text = "Your answer must be a whole number", type = "error", closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE, timer = 10000)
      } else if ((length(as.character(input$lower)) > 18) | (length(as.character(input$upper)) > 18)) {
        shinyalert(title = "Invalid Guess", text = "Your interval is too large, try again", type = "error", closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE, timer = 10000)
      } else if (as.numeric(input$lower) > as.numeric(input$upper)) {
        shinyalert(title = "Invalid Guess", text = "Make sure your lower bound is less than or equal to your upper bound", type = "error", closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE, timer = 10000)
      } else if ((as.numeric(input$lower) <= 0) |
                 (as.numeric(input$upper) <= 0)) {
        shinyalert(title = "Invalid Guess", text = "Please make sure you have a positive interval", type = "error", closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE, timer = 10000)
      } else {
        # Sys.sleep(1.5)
        currentResultsData <- read_sheet(c_id(), sheet = "submissions")
        submissionsCount <- nrow(filter(currentResultsData, Team == input$team, Question == input$questions))
        if (submissionsCount == 3){
          shinyalert(title = "Maximum Submissions Reached", text = "You are out of attempts.", type = "error", closeOnEsc = TRUE,
                     closeOnClickOutside = TRUE, timer = 10000)
        }
        else {
          # Sys.sleep(1.5)
          answer <- as.numeric(Answer_sheet[[as.integer(input$questions), 3]])
          # if (as.character(input$estAlg) == 1) {
          #   score <- est1(as.integer64.character(input$lower), as.integer64.character(input$upper), answer)
          # } else if (as.character(input$estAlg) == 2) {
          #   score <- est2(as.integer64.character(input$lower), as.integer64.character(input$upper), answer)
          # } else if (as.character(input$estAlg) == 3) {
          #   score <- est3(as.integer64.character(input$lower), as.integer64.character(input$upper), answer)
          # } else if (as.character(input$estAlg) == 4) {
          #   score <- est4(as.integer64.character(input$lower), as.integer64.character(input$upper), answer)
          # } else if (as.character(input$estAlg) == 5) {
          #   score <- est5(as.integer64.character(input$lower), as.integer64.character(input$upper), answer)
          # } else if (as.character(input$estAlg) == 6) {
          #   score <- est6(as.integer64.character(input$lower), as.integer64.character(input$upper), answer)
          # } else if (as.character(input$estAlg) == 7) {
          #   score <- est7(as.integer64.character(input$lower), as.integer64.character(input$upper), answer)
          # } else if (as.character(input$estAlg) == 8) {
          #   score <- est8(as.integer64.character(input$lower), as.integer64.character(input$upper), answer)
          # } else if (as.character(input$estAlg) == 9) {
          score <- est9(as.numeric(input$lower), as.numeric(input$upper), answer)
          # } else {
          #   score <- 200
          # }
          score <- round(score, digits = 2)
          if ((as.numeric(input$lower) <= answer) &
              (as.numeric(input$upper) >= answer)) {
            contains <- TRUE
          } else {
            contains <- FALSE
          }
          sheet_append(
            c_id(),
            data = data.frame(input$team, input$questions, input$lower, input$upper, score, contains),
            sheet = 2)
          shinyalert(title = "Submission Success", text = "Exit and click update to see your results", type = "success", closeOnEsc = TRUE,
                     closeOnClickOutside = TRUE, timer = 10000)}

      }

    }}
    shinyjs::enable("try")
    shinyjs::enable("update")}
  )


  resultsData <- reactive({
    input$update
    # Sys.sleep(1.5)
    read_sheet(c_id(), sheet = "submissions")})


  answersData <- reactive({
    input$update
    # Sys.sleep(1.5)
    read_sheet(c_id(), sheet = "answers")
  })

  output$q_text <- renderText({as.character(answersData()[as.integer(input$questions), 2])})

  yourData <- reactive({
    filter(resultsData(), Team == input$team, Question == input$questions)
  })

  yourTotalData <- reactive({
    filter(resultsData(), Team == input$team)
  })

  output$pastSubs <- renderDataTable({yourData()})

  # Filter the data for the specific team and question
  filtered_data <- reactive({
    resultsData() %>%
      filter(Team == input$team, Question == input$questions) %>%
      arrange(desc(row_number()))  # Sort in descending order for the latest submission first
  })

  observeEvent(input$update, {
    shinyjs::disable("update")
    shinyjs::disable("try")
    currentResultsData <- resultsData()
    submissionsCount <- nrow(filter(currentResultsData, Team == input$team, Question == input$questions))
    if (nrow(resultsData()) > 1){
      questions <- 1:sum(!is.na(answersData()[[2]]))
      bestScores <- rep(100, length(questions))
      for (i in 1:length(questions)){
        # Make i character to allow comparisons to characters
        if (as.character(i) %in% unique(yourTotalData()$Question)){
          if ((!is.na(min(filter(yourTotalData(), Question == as.character(i))$Score)))
              & (min(filter(yourTotalData(), Question == as.character(i))$Score) < 100)) {
            bestScores[i] <- min(filter(yourTotalData(), Question == as.character(i))$Score)
          }
        }
      }
      scoreTable <- data.frame(Question = questions, Best = bestScores)

      output$yourResults <- renderDataTable({scoreTable})

      output$yourPlot <- renderPlot({
        ggplot(scoreTable, aes(y = Best, x = Question)) + geom_line(color = "red") + geom_point() + theme_minimal()
      })



      colList <- list(questions)
      containsList <- list(questions)
      guessesList <- list(questions)

      for (t in unique(resultsData()$Team)[-1]) {
        col <- rep(100, length(questions))
        containsCol <- rep(FALSE, length(questions))
        guessesCol <- rep(3, length(questions))
        for (i in 1:length(questions)){
          # Make i character to allow comparisons to characters
          if (as.character(i) %in% unique(filter(resultsData(), Team == t)$Question)){
            guessesCol[i] <- 3 - nrow(filter(resultsData(), Team == t, Question == as.character(i)))
            done <- length(unique(filter(resultsData(), Team == t)$Question))
            toDo <- length(questions) - length(unique(filter(resultsData(), Team == t)$Question))
            guessLeft <- 3 - submissionsCount
            # Check if NA before trying to filter
            if(!is.na(min(filter(currentResultsData, Team == t, Question == as.character(i))$Score)) & (min(filter(currentResultsData, Team == t, Question == as.character(i))$Score)) < 100){
              col[i] <- min(filter(currentResultsData, Team == t, Question == as.character(i))$Score)
              if (TRUE %in% filter(currentResultsData, Team == t, Question == as.character(i), Score == min(filter(currentResultsData, Team == t, Question == as.character(i))$Score))$Contains) {containsCol[i] <- TRUE}
            }
            else {
              if (TRUE %in% filter(currentResultsData, Team == t, Question == as.character(i))$Contains) {containsCol[i] <- TRUE}
            }
          }
        }
        colList <- c(colList, list(col))
        containsList <- c(containsList, list(containsCol))
        guessesList <- c(guessesList, list(guessesCol))
      }



      totalLeaderboard <- data.frame(colList)

      colnames(totalLeaderboard) <- c("Question", unique(resultsData()$Team)[-1])

      totalLeaderboardRotated <- transpose(totalLeaderboard)

      rownames(totalLeaderboardRotated) <- colnames(totalLeaderboard)
      colnames(totalLeaderboardRotated) <- str_c("Q", as.character(1:ncol(totalLeaderboardRotated)))

      totalLeaderboardRotated <- totalLeaderboardRotated %>% mutate(Total = rowSums(across(everything())))


      #############################################################################################################

      containsdf <- data.frame(containsList)
      colnames(containsdf) <- c("Question", unique(resultsData()$Team)[-1])
      containsdfRotated <- transpose(containsdf)
      rownames(containsdfRotated) <- colnames(containsdf)
      colnames(containsdfRotated) <- str_c("Q", as.character(1:ncol(containsdfRotated)))
      containsdfRotated$Total <- totalLeaderboardRotated$Total
      containsdfRotated <- containsdfRotated %>% arrange(Total)

      guessesdf <- data.frame(guessesList)
      colnames(guessesdf) <- c("Question", unique(resultsData()$Team)[-1])
      guessesdfRotated <- transpose(guessesdf)
      rownames(guessesdfRotated) <- colnames(guessesdf)
      colnames(guessesdfRotated) <- str_c("Q", as.character(1:ncol(guessesdfRotated)))
      guessesdfRotated$Total <- totalLeaderboardRotated$Total
      guessesdfRotated <- guessesdfRotated %>% arrange(Total)

      output$colors <- renderDataTable({guessesdfRotated})


      totalLeaderboardRotated <- totalLeaderboardRotated %>% arrange(Total)

      totalLeaderboardRotated <- totalLeaderboardRotated %>% slice(-1)
      containsdfRotated <- containsdfRotated %>% slice(-1)
      guessesdfRotated <- guessesdfRotated %>% slice(-1)


      for (i in 1:nrow(totalLeaderboardRotated)){
        for (j in 2:ncol(totalLeaderboardRotated) - 1){
          totalLeaderboardRotated[i,j] <- str_c(as.character(totalLeaderboardRotated[i,j]), " ", strrep("✓", 3 - guessesdfRotated[i,j]))
        }
      }



      finalLeaderboard <- tableHTML(totalLeaderboardRotated, widths = c(250, rep(120, ncol(totalLeaderboardRotated))), rownames = TRUE)
      # for (i in 1:ncol(totalLeaderboardRotated)){
      for (i in 1:length(questions)){
        finalLeaderboard <- finalLeaderboard %>% add_css_conditional_column(conditional = 'logical',
                                                                            value = min(totalLeaderboardRotated[[i]]),
                                                                            css = list(c('background-color', 'font-weight'),
                                                                                       c('#90EE90', 'bold')),
                                                                            columns = i,
                                                                            logical_conditions = list(containsdfRotated[[i]] == TRUE))
      }

      finalLeaderboard <- finalLeaderboard %>% add_css_column(css = list("text-align", "center"),
                                                              columns = 0:ncol(totalLeaderboardRotated))

      finalLeaderboard <- finalLeaderboard %>% add_css_header(css = list("text-align", "center"),
                                                              headers = 0:ncol(totalLeaderboardRotated)+1)

      finalLeaderboard <- finalLeaderboard %>% add_css_header(css = list("font-size", "25px"),
                                                              headers = 0:ncol(totalLeaderboardRotated)+1)

      # finalLeaderboard <- finalLeaderboard %>% add_css_column(css = list("font-size", "20px"),
      #                                                         columns = 0)
      #
      # finalLeaderboard <- finalLeaderboard %>% add_css_column(css = list("font-size", "20"),
      #                                                         columns = 1:ncol(totalLeaderboardRotated)+1)

      finalLeaderboard <- finalLeaderboard %>% add_css_row(css = list(c("height", "font-size"),
                                                                      c("50px", "20px")),
                                                           rows = 1:nrow(totalLeaderboardRotated) + 1)



      output$leaderboard <- render_tableHTML({finalLeaderboard})

      # output$leaderboard <- renderDataTable({datatable(totalLeaderboardRotated)%>%formatStyle("1",backgroundColor=styleEqual(0, "red"))},
      #                                   bordered = TRUE,
      #                                   align = 'c',
      #                                   rownames = TRUE)

      # output$leaderboard <- DT::renderDataTable({totalLeaderboardRotated})

      plotData <- pivot_longer(totalLeaderboard, 2:ncol(totalLeaderboard), names_to = "Team", values_to = "Score")

      totals <- plotData %>% group_by(Team) %>% summarize(Score = sum(Score)) %>% arrange(Score)

      output$totalScoreGraphic <- renderPlot({
        ggplot(totals, aes(x = Team, y = Score)) + geom_bar(stat = "identity") + theme_minimal()
      })

      output$totalScore <- renderTable({totals})


      output$leaderboardPlot <- renderPlot({
        ggplot(plotData, aes(y = Score, x = Question, color = Team)) + geom_line() + geom_point() + theme_minimal()
      })

      # Calculate the improvement
      output$formatted_table <- renderFormattable({
        if (nrow(resultsData()) > 1) {
          data <- filtered_data()
          # Add a column for submission attempts
          data$Attempt <- nrow(data) - seq_along(data$Score) + 1

          if (nrow(data) > 0) {
            for (i in 1:(nrow(data))) {
              if (!is.na(data$Score[i]) & ((as.numeric(data$Score[i])) >= 100)) {
                data$Score[i] <- 100
              }
            }}

          for (i in 1:(nrow(data) - 1)) {
            data$Improvement[i] <- -(data$Score[i] - data$Score[i+1])/data$Score[i+1] * 100
            data$Improvement[nrow(data)] <- NA
          }



          # Round the 'Score' column to 2 decimal places
          data$Improvement <- sprintf("%.1f%%", round(data$Improvement, 1))
          data$Improvement[nrow(data)] <- NA
          data$Score <- round(data$Score, 2)

          # Find the row index with the lowest score
          lowest_score_row <- which.min(data$Score)

          # Get the corresponding "Attempt" value for the row with the lowest score
          lowest_score_attempt <- data$Attempt[lowest_score_row]

          # Improvement Formatter
          improvement_formatter <- formatter("span",
                                             style = x ~ style(font.weight = "bold",
                                                               color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))),
                                             x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
          )

          # Specify which columns to display
          display_data <- data[, c("Attempt", "Lower", "Upper", "Score", "Contains", "Improvement")]


          # Use formattable to add formatting
          formattable(display_data,
                      align = c("c", "c", "c", "c", "c", "c"),
                      list(
                        Attempt = formatter("span",
                                            style = x ~ style(color = "gray"),
                                            x ~ icontext(ifelse(x == as.character(lowest_score_attempt), "star", ""), x)),
                        Lower = color_tile(customGreen0, customGreen),
                        Upper = color_tile(customGreen0, customGreen),
                        Score = color_bar(customPurple),
                        Contains = color_tile(customRed, customGreen),
                        Improvement = improvement_formatter
                      )
          )
        }
        # # Check if data is not empty
        # if (nrow(data) == 0) {
        #   return(NULL)
        # }
      })

      output$rankBox <- renderInfoBox({
        infoBox(
          "You have", as.character(guessLeft), " guesses left for this question", icon = icon("hashtag"),
          color = "yellow"
        )
      })

      output$toDoBox <- renderInfoBox({
        infoBox(
          "You answered", as.character(done), " questions", icon = icon("list"),
          color = "blue"
        )
      })
      output$progressBox <- renderInfoBox({
        infoBox(
          "You have", as.character(toDo), " questions to do", icon = icon("bars-progress"),
          color = "purple"
        )
      })
    }
    shinyjs::enable("update")
    shinyjs::enable("try")


  })

  observeEvent(input$updateLeaderboard, {
    currentResultsData <- read_sheet(c_id(), sheet = "submissions")
    currentYourTotalData <- filter(currentResultsData, Team == input$team)
    submissionsCount <- nrow(filter(currentResultsData, Team == input$team, Question == input$questions))
    if (nrow(currentResultsData) > 1){
      questions <- 1:sum(!is.na(answersData()[[2]]))
      bestScores <- rep(100, length(questions))
      for (i in 1:length(questions)){
        # Make i character to allow comparisons to characters
        if (as.character(i) %in% unique(currentYourTotalData$Question)){
          if ((!is.na(min(filter(currentYourTotalData, Question == as.character(i))$Score)))
              & (min(filter(currentYourTotalData, Question == as.character(i))$Score) < 100)) {
            bestScores[i] <- min(filter(currentYourTotalData, Question == as.character(i))$Score)
          }
        }
      }
      scoreTable <- data.frame(Question = questions, Best = bestScores)

      output$yourResults <- renderDataTable({scoreTable})

      output$yourPlot <- renderPlot({
        ggplot(scoreTable, aes(y = Best, x = Question)) + geom_line(color = "red") + geom_point() + theme_minimal()
      })


      index <- 1


      colList <- list(questions)
      containsList <- list(questions)
      guessesList <- list(questions)

      for (t in unique(currentResultsData$Team)[-1]) {
        col <- rep(100, length(questions))
        containsCol <- rep(FALSE, length(questions))
        guessesCol <- rep(3, length(questions))
        for (i in 1:length(questions)){
          # Make i character to allow comparisons to characters
          if (as.character(i) %in% unique(filter(currentResultsData, Team == t)$Question)){
            guessesCol[i] <- 3 - nrow(filter(currentResultsData, Team == t, Question == as.character(i)))
            done <- length(unique(filter(currentResultsData, Team == t)$Question))
            toDo <- length(questions) - length(unique(filter(currentResultsData, Team == t)$Question))
            guessLeft <- 3 - submissionsCount
            # Check if NA before trying to filter
            if(!is.na(min(filter(currentResultsData, Team == t, Question == as.character(i))$Score)) & (min(filter(currentResultsData, Team == t, Question == as.character(i))$Score)) < 100){
              col[i] <- min(filter(currentResultsData, Team == t, Question == as.character(i))$Score)
              if (TRUE %in% filter(currentResultsData, Team == t, Question == as.character(i), Score == min(filter(currentResultsData, Team == t, Question == as.character(i))$Score))$Contains) {containsCol[i] <- TRUE}
            }
            else {
              if (TRUE %in% filter(currentResultsData, Team == t, Question == as.character(i))$Contains) {containsCol[i] <- TRUE}
            }
          }
        }
        colList <- c(colList, list(col))
        containsList <- c(containsList, list(containsCol))
        guessesList <- c(guessesList, list(guessesCol))
      }



      totalLeaderboard <- data.frame(colList)

      colnames(totalLeaderboard) <- c("Question", unique(currentResultsData$Team)[-1])

      totalLeaderboardRotated <- transpose(totalLeaderboard)

      rownames(totalLeaderboardRotated) <- colnames(totalLeaderboard)
      colnames(totalLeaderboardRotated) <- str_c("Q", as.character(1:ncol(totalLeaderboardRotated)))

      totalLeaderboardRotated <- totalLeaderboardRotated %>% mutate(Total = rowSums(across(everything())))


      #############################################################################################################

      containsdf <- data.frame(containsList)
      colnames(containsdf) <- c("Question", unique(currentResultsData$Team)[-1])
      containsdfRotated <- transpose(containsdf)
      rownames(containsdfRotated) <- colnames(containsdf)
      colnames(containsdfRotated) <- str_c("Q", as.character(1:ncol(containsdfRotated)))
      containsdfRotated$Total <- totalLeaderboardRotated$Total
      containsdfRotated <- containsdfRotated %>% arrange(Total)

      guessesdf <- data.frame(guessesList)
      colnames(guessesdf) <- c("Question", unique(currentResultsData$Team)[-1])
      guessesdfRotated <- transpose(guessesdf)
      rownames(guessesdfRotated) <- colnames(guessesdf)
      colnames(guessesdfRotated) <- str_c("Q", as.character(1:ncol(guessesdfRotated)))
      guessesdfRotated$Total <- totalLeaderboardRotated$Total
      guessesdfRotated <- guessesdfRotated %>% arrange(Total)

      output$colors <- renderDataTable({guessesdfRotated})


      totalLeaderboardRotated <- totalLeaderboardRotated %>% arrange(Total)

      totalLeaderboardRotated <- totalLeaderboardRotated %>% slice(-1)
      containsdfRotated <- containsdfRotated %>% slice(-1)
      guessesdfRotated <- guessesdfRotated %>% slice(-1)


      for (i in 1:nrow(totalLeaderboardRotated)){
        for (j in 2:ncol(totalLeaderboardRotated) - 1){
          totalLeaderboardRotated[i,j] <- str_c(as.character(totalLeaderboardRotated[i,j]), " ", strrep("✓", 3 - guessesdfRotated[i,j]))
        }
      }



      finalLeaderboard <- tableHTML(totalLeaderboardRotated, widths = c(250, rep(120, ncol(totalLeaderboardRotated))), rownames = TRUE)
      # for (i in 1:ncol(totalLeaderboardRotated)){
      for (i in 1:length(questions)){
        finalLeaderboard <- finalLeaderboard %>% add_css_conditional_column(conditional = 'logical',
                                                                            value = min(totalLeaderboardRotated[[i]]),
                                                                            css = list(c('background-color', 'font-weight'),
                                                                                       c('#90EE90', 'bold')),
                                                                            columns = i,
                                                                            logical_conditions = list(containsdfRotated[[i]] == TRUE))
      }

      finalLeaderboard <- finalLeaderboard %>% add_css_column(css = list("text-align", "center"),
                                                              columns = 0:ncol(totalLeaderboardRotated))

      finalLeaderboard <- finalLeaderboard %>% add_css_header(css = list("text-align", "center"),
                                                              headers = 0:ncol(totalLeaderboardRotated)+1)

      finalLeaderboard <- finalLeaderboard %>% add_css_header(css = list("font-size", "25px"),
                                                              headers = 0:ncol(totalLeaderboardRotated)+1)

      # finalLeaderboard <- finalLeaderboard %>% add_css_column(css = list("font-size", "20px"),
      #                                                         columns = 0)
      #
      # finalLeaderboard <- finalLeaderboard %>% add_css_column(css = list("font-size", "20"),
      #                                                         columns = 1:ncol(totalLeaderboardRotated)+1)

      finalLeaderboard <- finalLeaderboard %>% add_css_row(css = list(c("height", "font-size"),
                                                                      c("50px", "20px")),
                                                           rows = 1:nrow(totalLeaderboardRotated) + 1)



      output$leaderboard <- render_tableHTML({finalLeaderboard})

      # output$leaderboard <- renderDataTable({datatable(totalLeaderboardRotated)%>%formatStyle("1",backgroundColor=styleEqual(0, "red"))},
      #                                   bordered = TRUE,
      #                                   align = 'c',
      #                                   rownames = TRUE)

      # output$leaderboard <- DT::renderDataTable({totalLeaderboardRotated})

      plotData <- pivot_longer(totalLeaderboard, 2:ncol(totalLeaderboard), names_to = "Team", values_to = "Score")

      totals <- plotData %>% group_by(Team) %>% summarize(Score = sum(Score)) %>% arrange(Score)

      output$totalScoreGraphic <- renderPlot({
        ggplot(totals, aes(x = Team, y = Score)) + geom_bar(stat = "identity") + theme_minimal()
      })

      output$totalScore <- renderTable({totals})


      output$leaderboardPlot <- renderPlot({
        ggplot(plotData, aes(y = Score, x = Question, color = Team)) + geom_line() + geom_point() + theme_minimal()
      })

      output$rankBox <- renderInfoBox({
        infoBox(
          "You have", as.character(guessLeft), " guesses left for this question", icon = icon("hashtag"),
          color = "yellow"
        )
      })

      output$toDoBox <- renderInfoBox({
        infoBox(
          "You answered", as.character(done), " questions", icon = icon("list"),
          color = "blue"
        )
      })
      output$progressBox <- renderInfoBox({
        infoBox(
          "You have", as.character(toDo), " questions to do", icon = icon("bars-progress"),
          color = "purple"
        )
      })
    }



  })




}




enableBookmarking(store = "url")
shinyApp(ui, server)
