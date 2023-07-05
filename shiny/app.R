library(shiny)
library(shinyWidgets)
library(OCSRiskCalculator)

faces <- c(red='<svg class="red-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.red-face .cls-1{fill:#a53a47;}.red-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><path class="cls-2" d="M8.3,6a2,2,0,1,1-2-2A2,2,0,0,1,8.3,6Z"></path><circle class="cls-2" cx="14.7" cy="5.98" r="1.97"></circle><path class="cls-2" d="M16.61,15.38a6.29,6.29,0,0,0-12.18,0,.33.33,0,0,0,.24.41.34.34,0,0,0,.42-.24,5.6,5.6,0,0,1,10.86,0,.36.36,0,0,0,.15.21.39.39,0,0,0,.18,0h.08A.34.34,0,0,0,16.61,15.38Z"></path></g></g></svg>',
          green='<svg class="green-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.green-face .cls-1{fill:#2eb49a;}.green-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><circle class="cls-2" cx="6.64" cy="6.95" r="1.83"></circle><circle class="cls-2" cx="14.42" cy="6.95" r="1.83"></circle><path class="cls-2" d="M16.67,11.29a.36.36,0,0,0-.27,0,.41.41,0,0,0-.17.22,5.89,5.89,0,0,1-11.41,0,.36.36,0,0,0-.16-.22.36.36,0,0,0-.27,0,.37.37,0,0,0-.22.17.36.36,0,0,0,0,.27,6.61,6.61,0,0,0,12.8,0A.37.37,0,0,0,16.67,11.29Z"></path></g></g></svg>',
          yellow='<svg class="yellow-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.yellow-face .cls-1{fill:#eea342;}.yellow-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><circle class="cls-2" cx="6.67" cy="7.92" r="1.81"></circle><circle class="cls-2" cx="14.36" cy="7.92" r="1.81"></circle><path class="cls-2" d="M17.12,14.29A11.46,11.46,0,0,1,4,14.3a.45.45,0,1,0-.53.73,12.42,12.42,0,0,0,7.17,2.23,11.66,11.66,0,0,0,7-2.25.45.45,0,0,0,.09-.63A.46.46,0,0,0,17.12,14.29Z"></path></g></g></svg>')


time_range_map <- c(
  "<1 year"=0.5,
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
                        <p>Enter patient data in the left panel and press “Calculate” after agreeing with the terms</p>
                        <p><span style="color:Tomato">Warning: This tool SHOULD NOT BE USED to replace a diagnostic or treatment decision made by a physician. None of the variables on the left panel have a causal interpretation in the model. Changing them for a single patient to estimate the effect of an intervention would be misleading and should be avoided.  </span>
                        <br></p>
                        <p>For a detailed description of the tool, please refer to the <a href="https://doi.org/10.1016/S2213-2600%2819%2930397-2">publication</a>.</p>'


shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    print(paste("Server version: 0.1")),
    titlePanel("OCS risk calculator"),
    sidebarLayout(
      sidebarPanel(
         radioButtons("cur_ocs","Currently taking oral corticosteroids:", choices=c("No"=0, "Yes"=1))
        #,sliderInput("ocs_years", label="Number of years taking oral corticosteroids", min=1, max=10, value=1, width="50%")
        ,sliderTextInput(
          inputId = "ocs_years",
          label = "Number of years taking oral corticosteroids:",
          choices = names(time_range_map),
          selected = names(time_range_map)[5]
        )
        ,radioButtons("ocs_intensity","Generally, your oral corticosteroids dose has been", choices=c("Low"=0, "High"=1))
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
        uiOutput("main"),
      )),
    tags$script(src="app.js")
  ),

  server = function(input, output, session)
  {
    outcomes <- get_outcomes()

    panels <- list(tabPanel("Summary", htmlOutput("start_here"),  plotOutput("summary", inline = T)))
    for(i in 1:length(outcomes))
    {
      panels[[i+1]] <- tabPanel(names(outcomes)[i], htmlOutput(paste0("details_", outcomes[i]), inline=T, fill=F))

    }
    output$main <- renderUI(do.call(tabsetPanel,args=panels))

    for(i in 1:length(outcomes))
    {
      local(
      {
        my_i <- i
        output[[paste0("details_",outcomes[my_i])]] <- renderUI(create_single_page(outcomes[my_i]))
      })
    }

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
        create_bar_plot(profile=pfl(), outcomes=outcomes)

        for(nm in names(input))
          shinyjs::disable(nm)
        shinyjs::enable("reset")
        output$need_to_consent <- renderText("")
        output$start_here <- renderUI(HTML(""))
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
      output$summary <- NULL

      updateCheckboxInput(inputId="consent",value = F)
      for(nm in names(input))
        shinyjs::enable(nm)
      output$need_to_consent <- renderText("")
    })

    create_bar_plot <- function(profile, outcomes)
    {
      #input$calculate
      rrs <- rep(NA,length(outcomes))
      for(i in 1:length(outcomes))
      {
        rrs[i] <- calculate_risk(profile,outcomes[i])
      }

      labels <- sapply(rrs, function(x)
        {
          if(x<1) "No increase"
          else
            if(x>2) ">100%"
            else
              paste0("+",round(x*100-100),"%")
        })

        rrs <- sapply(rrs, function(x)
        {
          if(x<1) 1
          else
            if(x>2) 2
          else
            x
        })


      df <- data.frame(outcome=names(outcomes),rr=rrs,label=labels)

      require(ggplot2)
      plt <- ggplot(data=df,aes(x=outcome, y=rr))+
        xlab("")+ylab("")+
        #ylim(0,2.5)+
        geom_bar(stat="identity")+
        geom_text(aes(label=label), hjust=1, vjust=0.5, color="yellow", size=7)+
        theme(axis.text=element_text(size=20))+
        geom_hline(yintercept=1, linetype="dashed", color = "orange", size=0.5)+
        coord_flip()

      output$summary <- renderPlot(plt, width=1000, height=500)
    }


    create_single_page <- function(outcome)
    {
      list(
        sliderInput(paste0(outcome,"_before"),label="Your risk without using OCS",min=0,max=100, value=50),
        progressBar(paste0(outcome,"_after"), value = 0, title = "Your risk with using OCS", display_pct = TRUE, status = "danger", striped = TRUE)
      )
    }

    observeEvent(input$ost_before, {
      updateProgressBar(id="ost_after",value=input$ost_before/2)
      #shinyjs::disable('ost_after')
    })

    #output$icon_array <- renderText(HTML(generate_icon_array()))
  }
)




