library(shiny)
library(shinyWidgets)
library(OCSRiskCalculator)
library(svglite)
library(stringr)
library(markdown)
library(bslib)
library(bsicons)

source("summary_page.R")
source("specific_outcome_page.R")

get_outcomes <- function()
{
  outcomes <- OCSRiskCalculator:::get_outcomes()
  outcomes[-which(outcomes %in% c("avn","tbs","mbs"))]
}

theme_selector <- F


 custom_theme <- bs_theme(
   version = 5,
#   bg = "#e0f3db",
#   fg = "#000000",
#   primary = "#0199F8",
#   secondary = "#FF374B",
#   base_font = "Calibri"
)

custom_theme <- bs_theme_update(custom_theme, preset = "shiny")

time_range_map <- c(
  "<1 year"=0,
  "1-2 years"=1.5,
  "2-3 years"=2.5,
  "3-4 years"=3.5,
  "4-5 years"=4.5,
  "5-6 years"=5.5,
  "6-7 years"=6.5,
  "7-8 years"=7.5,
  "8-9 years"=8.5,
  "9-10 years"=9.5,
  "10 years or more"=10.5
)


start_here_innerHTML <- '<h1 style="color:tomato">← Start Here</h1>
                        <p>Enter your data in the left panel and press “Calculate”</p>'


ui <- fluidPage(
  theme = custom_theme,

  tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css")),

  tags$script('$(document).on("shiny:sessioninitialized",function(){$.get("https://api.ipify.org", function(response) {Shiny.setInputValue("getIP", response);});})'),
  verbatimTextOutput('ip'),

  shinyjs::useShinyjs(),

  tags$head(
    HTML("<title>Oral corticosteroid risk calculator for patients with asthma</title>"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  titlePanel(h1(id="title_panel", "Oral corticosteroid risk calculator for patients with asthma")),

  tabsetPanel(id="master_panel", type="hidden",
    tabPanel("welcome_panel",
      tags$iframe(src=("Welcome.html?hgf"), style="width:100%; height:70vh"),
      fluidRow(
        column(3),
        column(3,
          span(style="font-weight:bold;", checkboxInput("consent0","I understand the purpose of this tool", width="100%"))
          ),
          column(3,
            fluidRow(actionButton("make_app_visible","Take me to the app!"))
          ),
          column(3,
                 textOutput("need_to_consent0"),
                 tags$head(tags$style("#need_to_consent0{color:red;}")))
        )
    ),
    tabPanel("app_panel",
      sidebarLayout(
        sidebarPanel(id="input_panel",
          sliderTextInput(
            inputId = "ocs_years",
            label = "Number of years taking oral corticosteroids (past or present use):",
            choices = names(time_range_map),
            selected = names(time_range_map)[5]
          )
          ,radioButtons("cur_ocs","Are you currently taking oral corticosteroids:", choices=c("No"=0, "Yes"=1))
          ,radioButtons("ocs_intensity","Generally, your oral corticosteroids use has been:*", choices=c("Low"=0, "High"=1))
          ,p(id="high_dose_desc", style="color:gray", "*Oral corticosteroids use is considered high if you have received", strong("4 or more"), " prescriptions for oral corticosteroids during a year")
          ,div("This tool should be used in discussion with your care provider")
          ,actionButton("calculate","Calculate!")
          ,actionButton("reset","Restart")
          ,textOutput("need_to_consent")
          ,tags$head(tags$style("#need_to_consent{color: red;
                                   font-style: bold;
                                   }"
            )
          )
        )

        ,mainPanel(
          tabsetPanel(id="main_panel",
                      tabPanel("Summary", htmlOutput("start_here"),  uiOutput("summary_plot", inline=F, width="500px"), uiOutput("summary_desc")),
                      tabPanel("Specific outcome",
                               h5(paste0("In this page, we evaluate the risk of specific outcomes")),
                               selectInput("specific_outcome_selector","Please select the outcome:", c("PLEASE SELECT", names(get_outcomes()))),
                               uiOutput("specific_outcome_content")),
                      tabPanel("About", tags$iframe(src=("About.html"), style='width:100%; height:70vh;')),
                      tabPanel("Terms", div(style="white-space: pre-wrap;width:100%; height:100vh;  padding: 15px; overflow: auto;",
                                            OCSRiskCalculator::terms_of_use()
                                        )
                              )
                      )
        )))),
  tags$script(src="app.js"),
  hr(),
  div(id="footer",
    div("By NAPTIA Consultation - App version: 1 (2024.01.28)")
  )
)
















server <- function(input, output, session)
{
  outcomes <- get_outcomes()

  #output$ip <- reactive(input$getIP)

  shinyjs::disable("specific_outcome_selector") #when first started this should be disabled

  # observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = c(), {
  #   showModal(modalDialog(
  #     title = "Welcome!",
  #     h1("Salam")
  #   ))
  # })



  output$start_here <- renderUI(HTML(start_here_innerHTML))

  pfl <- reactive({
    c(female=as.integer(input$female),
      age= as.integer(input$age),
      cur_ocs=as.integer(input$cur_ocs),
      ocs_year=(time_range_map[input$ocs_years]),
      ocs_intensity=(as.integer(input$ocs_intensity)))
  })

  observeEvent(input$make_app_visible,
   {
     if(isolate(input$consent0))
     {
       updateTabsetPanel(session, inputId="master_panel", selected="app_panel")
     }
     else
     {
       output$need_to_consent0 <- renderText("Please consent to use this tool")
     }

   })

  observeEvent(input$know_my_bg_risk, {
    if(input$know_my_bg_risk)
    {
      shinyjs::show("div_know_my_bg_risk", anim=T)
      shinyjs::runjs("document.getElementById('div_know_my_bg_risk').style.visibility='visible'")

      #UPdate the progress bars as sometimes (always?) the stacked progress bar does not show the right value initially?
      outcome <- get_outcomes()[input$specific_outcome_selector]
      risk_before <- input$specific_outcome_before/100
      risk_after <- min(risk_before*calculate_risk(pfl(),outcome),1)
      updateProgressBar(id="pb_specific_outcome_before",value=risk_before*100)
      updateProgressBar(id="pb_specific_outcome_after",value=(risk_after-risk_before)*100)
    }
    else
    {
      shinyjs::hide("div_know_my_bg_risk", anim=F)
    }


    })

  observeEvent(input$calculate, {
    #if(isolate(input$consent))
    if(T)
    {
      plt <- create_bar_plot(profile=pfl(), outcomes=outcomes)
      output$summary_plot <- renderUI(HTML(plt))
      output$summary_desc <- renderUI(generate_summary_text())

      for(nm in names(input))
        shinyjs::disable(nm)
      shinyjs::enable("reset")
      output$need_to_consent <- renderText("")
      output$start_here <- renderUI(HTML(""))

      shinyjs::enable("specific_outcome_selector")
      if(input$specific_outcome_selector!="PLEASE SELECT")
        output$specific_outcome_content <- renderUI(create_specific_coutcome_content(pfl(), input$specific_outcome_selector))

      updateCheckboxInput(inputId='know_my_bg_risk', value=F)

    }
    else
    {
      output$need_to_consent <- renderText("Please consent to use this tool")
    }
  })

  observeEvent(input$reset, {
    for(i in 1:length(outcomes))
    {
      output[[paste0("summary_",outcomes[i])]] <- renderUI(HTML(""))
    }
    output$start_here <- renderUI(HTML(start_here_innerHTML))
    output$summary_plot <- NULL
    output$summary_desc <- NULL

    updateCheckboxInput(inputId="consent",value = F)
    for(nm in names(input))
      shinyjs::enable(nm)
    output$need_to_consent <- renderText("")

    output$specific_outcome_content <- renderUI("")
    shinyjs::disable("specific_outcome_selector")
    updateTabsetPanel(session, inputId="main_panel", selected="Summary")
  })


  observeEvent(input$outcome_clicked,
  {
    updateSelectInput(
      inputId="specific_outcome_selector",
      selected = input$outcome_clicked
    )

    updateTabsetPanel(session, inputId="main_panel", selected="Specific outcome")

  })


  observeEvent(input$specific_outcome_selector,
  {
    if(input$specific_outcome_selector!="PLEASE SELECT")
    {
      output$specific_outcome_content <- renderUI(create_specific_coutcome_content(pfl(), input$specific_outcome_selector)) #Just in case the content might be different per outcome, we are 'creating things' on the fly
      updateCheckboxInput(inputId="know_my_bg_risk",value=F)
      #shinyjs::hide("div_know_my_bg_risk")
    }
    else
    {
      output$specific_outcome_content <- renderUI("")
    }
  })


  observeEvent(input$specific_outcome_before,
  {
    outcome <- get_outcomes()[input$specific_outcome_selector]
    risk_before <- input$specific_outcome_before/100
    risk_after <- min(risk_before*calculate_risk(pfl(),outcome),1)
    updateProgressBar(id="pb_specific_outcome_before",value=risk_before*100)
    updateProgressBar(id="pb_specific_outcome_after",value=(risk_after-risk_before)*100)

    # yellow <- round(risk_before*100)
    # red <- round(risk_after*100) - yellow
    # green <- 100 - red - yellow
    # output$specific_outcome_icon_array <- renderUI(HTML(generate_icon_array(counts=c(yellow, red, green))))
    # output$specific_outcome_icon_array_legend <- renderUI(HTML(generate_icon_array_legend()))
  })

}



if(theme_selector==FALSE)
{
  shinyApp(ui, server)
}else
{
  bslib::run_with_themer(shinyApp(ui, server))
}
