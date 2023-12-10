library(shiny)


ui <- fluidPage(

  dashboardHeader(title = "Basic dashboard"),
  ## ui.R ##

  sidebarPanel(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),

  mainPanel(

    tags$head(tags$style(HTML('
      .modal.in .modal-dialog{
        width:100%;
        height:100%;
        margin:0px;
      }

      .modal-content{
        width:100%;
        height:100%;
      }
    '))),

    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),

                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),

      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)


library(shiny)
library(shinydashboard)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, {
    # event will be called when histdata changes, which only happens once, when it is initially calculated
    showModal(modalDialog(
      title = "Landing Page",
      h1('Landing Page'),
      p('Theoretically you can put whatever content you want in here')
    ))
  })

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)
