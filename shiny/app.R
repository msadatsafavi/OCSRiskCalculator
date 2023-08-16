library(shiny)
library(shinyWidgets)
library(OCSRiskCalculator)
library(svglite)
library(stringr)
library(markdown)

source("summary_page.R")
source("specific_outcome_page.R")



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



start_here_innerHTML <- '<h1>← Start Here</h1>
                        <p>Enter your data in the left panel and press “Calculate” after agreeing with the terms</p>
                        <p><span style="color:Tomato">Warning: This tool SHOULD NOT BE USED to replace a diagnostic or treatment decision made by a physician. None of the variables on the left panel have a causal interpretation in the model. Changing them for a single patient to estimate the effect of an intervention would be misleading and should be avoided.  </span>
                        <br></p>
                        <p>For a detailed description of the tool, please refer to the <a href="https://doi.org/10.1016/S2213-2600%2819%2930397-2">About page</a>.</p>'


shinyApp(
  ui = fluidPage(
    tags$style(
    "
    #summary_desc {
      border-width:1px;
      border-style:solid;
      border-color:#959595;
      background-color: #0e406a;
      font-size: 20px;
      color: white;
    }

    #summary_plot {
      border-width:1px;
      background-color: #d8eaf5;
    }

    #high_dose_desc {
      border-width:1px;
      border-style:solid;
      border-color:#959595;
      font-style: italic;
    }

    .container-fluid {
      background-color: #d8eaf5;
    }

    .nav-tabs {
      font-size: 20px;
    }

    body {
      background-color: #d8eaf5;
    }

    #input_panel {
      background-color: white;
      font-size: 20px;
    }

    #title_panel {
      background-color: #8b3972;
      color: white;
      height: 80px;
      padding: 20px;
      border: 3px solid #eea342;
    }

    #about_button {
      background-color: white;
      color: #0e406a;
      border: 3px solid #0e406a;
      padding: 15px;
      font-size: 20px;
    }

    #footer {
      text-align: center;
    }

    "),
    shinyjs::useShinyjs(),
    tags$head(
      HTML("<title>Oral corticosteroid risk calculator</title>"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    titlePanel(h1(id="title_panel", "Oral corticosteroid risk calculator",align="center")),
    sidebarLayout(
      sidebarPanel(id="input_panel",
         radioButtons("cur_ocs","Currently taking oral corticosteroids:", choices=c("No"=0, "Yes"=1))
        #,sliderInput("ocs_years", label="Number of years taking oral corticosteroids:", min=1, max=10, value=1, width="50%")
        ,hr()
        ,sliderTextInput(
          inputId = "ocs_years",
          label = "Number of years taking oral corticosteroids:",
          choices = names(time_range_map),
          selected = names(time_range_map)[5]
        )
        ,hr()
        ,radioButtons("ocs_intensity","Generally, your oral corticosteroids dose has been:", choices=c("Low"=0, "High"=1))
        ,p(id="high_dose_desc", "Oral corticosteroids use is considered high if you have received", strong("4 or more"), " prescriptions for oral corticosteroids during a year")
        ,hr()
        ,checkboxInput("consent","I understand the risks of using this tool")
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
        textOutput("profile"),
        tabsetPanel(id="main_panel",
                    tabPanel("Summary", htmlOutput("start_here"),  uiOutput("summary_plot", inline=F, width="500px"), uiOutput("summary_desc")),
                    tabPanel("Specific outcome",
                             h3(paste0("In this page, we evaluate the risk of specific outcomes")),
                             selectInput("specific_outcome_selector","Please select the outcome:", c("PLEASE SELECT", names(get_outcomes()))),
                             uiOutput("specific_outcome_content")),
                    tabPanel("About", includeMarkdown("About.html"))),
      )),
    tags$script(src="app.js"),
    hr(),
    div(id="footer",
        div("By NAPTIA Consultation"),
        div(print(paste("Server version: 0.3 (2023.08.15)")))
        )
  ),
















  server = function(input, output, session)
  {
    outcomes <- get_outcomes()

    # panels <- list(tabPanel("Summary", htmlOutput("start_here"),  uiOutput("summary_plot", inline=F, width="500px"), uiOutput("summary_desc")))
    # for(i in 1:2)
    # {
    #   panels[[i+1]] <- tabPanel(names(outcomes)[i], htmlOutput(paste0("details_", outcomes[i]), inline=T, fill=F))
    #
    # }
    # output$main <- renderUI(do.call(tabsetPanel,args=panels))

    output$start_here <- renderUI(HTML(start_here_innerHTML))

    pfl <- reactive({
      c(female=as.integer(input$female),
        age= as.integer(input$age),
        cur_ocs=as.integer(input$cur_ocs),
        ocs_year=(time_range_map[input$ocs_years]),
        ocs_intensity=(as.integer(input$ocs_intensity)))
    })

    output$profile <- renderText(pfl())

    observeEvent(input$calculate, {
      if(isolate(input$consent))
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
      }
    })


    observeEvent(input$specific_outcome_before,
    {
      outcome <- get_outcomes()[input$specific_outcome_selector]
      risk_before <- input$specific_outcome_before/100
      risk_after <- min(risk_before*calculate_risk(pfl(),outcome),1)
      updateProgressBar(id="specific_outcome_after",value=risk_after*100)
      yellow <- round(risk_before*100)
      red <- round(risk_after*100) - yellow
      green <- 100 - red - yellow
      output$specific_outcome_icon_array <- renderUI(HTML(generate_icon_array(counts=c(yellow, red, green))))
      output$specific_outcome_icon_array_legend <- renderUI(HTML(generate_icon_array_legend()))
    })

  }
)




