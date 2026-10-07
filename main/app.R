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
library(dplyr)
library(shinydashboard)
library(shinycssloaders)

ui <- function(request) {
  dashboardPage(
    header = dashboardHeader(title = "Estimelee Contest"),
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "sidebarID",
        menuItem(
          "Go",
          tabName = "go",
          icon = icon("earth-americas")
        ),
        menuItem(
          "Contact",
          icon = icon("calendar"),
          tabName = "contact",
          badgeLabel = "DON'T",
          badgeColor = "red"
        )
      ),
      disable = TRUE),
    body = dashboardBody(
      shinyjs::useShinyjs(),
      tabItems(
        tabItem(tabName = "go",
                div(
                  # App title ----
                  titlePanel("Welcome to Estimelee!"),

                  br(),
                  br(),
                  br(),

                  fluidRow(column(6, align = "center", offset = 3,
                                  actionButton(inputId = "creatorClick",
                                                                                label = "I want to host", icon = icon("chalkboard-user"),
                                                                                onclick ="window.open('https://01a117a2-bb04-4c71-60d2-510f8c696f3a.share.connect.posit.cloud/?tab=create', '_blank')",
                                                                                class="btn-lg",
                                                                                style="color: #ffffff; background-color: #72d0fc; border-color: #c34113;
>                                border-radius: 20px;
>                                border-width: 4px;
                               font-size: 45pt; display:center-align"))),
                  br(),
                  br(),
                  br(),
                  fluidRow(column(6, align = "center", offset = 3,
                                              actionButton(inputId = "playerClick",
                                               label = "I want to play", icon = icon("gamepad"),
                                               onclick ="window.open('https://01a117a4-3827-d946-d67a-1ec3184544a5.share.connect.posit.cloud/?tab=rules', '_blank')",
                                               class="btn-lg",style="color: #ffffff; background-color: #fc72e3; border-color: #c34113;
>                                border-radius: 20px;
>                                border-width: 4px;
                               font-size: 45pt; display:center-align")))



                )),
        tabItem(tabName = "contact",
                h2("Don't contact us!")))))
}

server <- function(input, output, session) {

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




}

shinyApp(ui, server, enableBookmarking = "disable")
