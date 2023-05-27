library(shiny)
library(OCSRiskCalculator)

faces <- c(red='<svg class="red-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.red-face .cls-1{fill:#a53a47;}.red-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><path class="cls-2" d="M8.3,6a2,2,0,1,1-2-2A2,2,0,0,1,8.3,6Z"></path><circle class="cls-2" cx="14.7" cy="5.98" r="1.97"></circle><path class="cls-2" d="M16.61,15.38a6.29,6.29,0,0,0-12.18,0,.33.33,0,0,0,.24.41.34.34,0,0,0,.42-.24,5.6,5.6,0,0,1,10.86,0,.36.36,0,0,0,.15.21.39.39,0,0,0,.18,0h.08A.34.34,0,0,0,16.61,15.38Z"></path></g></g></svg>',
          green='<svg class="green-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.green-face .cls-1{fill:#2eb49a;}.green-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><circle class="cls-2" cx="6.64" cy="6.95" r="1.83"></circle><circle class="cls-2" cx="14.42" cy="6.95" r="1.83"></circle><path class="cls-2" d="M16.67,11.29a.36.36,0,0,0-.27,0,.41.41,0,0,0-.17.22,5.89,5.89,0,0,1-11.41,0,.36.36,0,0,0-.16-.22.36.36,0,0,0-.27,0,.37.37,0,0,0-.22.17.36.36,0,0,0,0,.27,6.61,6.61,0,0,0,12.8,0A.37.37,0,0,0,16.67,11.29Z"></path></g></g></svg>',
          yellow='<svg class="yellow-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.yellow-face .cls-1{fill:#eea342;}.yellow-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><circle class="cls-2" cx="6.67" cy="7.92" r="1.81"></circle><circle class="cls-2" cx="14.36" cy="7.92" r="1.81"></circle><path class="cls-2" d="M17.12,14.29A11.46,11.46,0,0,1,4,14.3a.45.45,0,1,0-.53.73,12.42,12.42,0,0,0,7.17,2.23,11.66,11.66,0,0,0,7-2.25.45.45,0,0,0,.09-.63A.46.46,0,0,0,17.12,14.29Z"></path></g></g></svg>')


shinyApp(
  ui = fluidPage(
    #print(paste("Server version: 0.1")),
    titlePanel("OCS risk calculator")
    ,inputPanel(
       numericInput("age", label="age", min=18, max=80, value=40, width="50%")
      ,radioButtons("female","Gender:", choices=c("Female"=1, "Male"=0))
      ,radioButtons("cur_ocs","Currently taking OCS:", choices=c("No"=0, "Yes"=1))
      ,numericInput("ocs_years", label="Number of years taking corticosteroids", min=1, max=30, value=10, width="50%")
      ,radioButtons("hist_ocs","Generally, your OCS dose has been", choices=c("Low"=0, "High"=1))
      ,submitButton("Calculate!")
    )
    ,mainPanel(
      textOutput("profile")
      ,uiOutput("main", inline=TRUE, fill=TRUE)
    )
  ),


  server = function(input, output, session)
  {

    generate_icon_array <- function(order=c('yellow','red','green'),counts=c(50,30,20))
    {
      x <- c(rep(faces[order[1]],counts[1]),
             rep(faces[order[2]],counts[2]),
             rep(faces[order[3]],counts[3]))
      out <- "<TABLE style='width:100pt;height=100pt;margin:0;padding:0;float:left'>"
      for(i in 1:10)
      {
        out <- paste(out,"<TR>")
        for(j in 1:10)
        {
          out <- paste(out,"<TD style='width:10%;height=10%'>",x[10*(i-1)+j],"</TD>")
        }
        out <- paste(out,"</TR>")
      }
      out <- paste(out,"</TABLE>")

      out
    }

    create_res_panel <- function(profile, outcome)
    {
      tmp <- round(calculate_risk(profile,outcome)*1000)/10
      tmp_i <- round(tmp)
      paste0(names(outcomes)[which(outcomes==outcome)],":",tmp[1],"% (from:", tmp[2],"% to:", tmp[3],"%)",generate_icon_array(counts=c(tmp_i[2],tmp_i[1],100-tmp_i[2]-tmp_i[1])))
      #generate_icon_array()
    }

    outcomes <- get_outcomes()

    output$test1 <- renderText(input$female)

    get_profile <- reactive(
      {
        profiile <- c(female=as.integer(input$female),
                      age= as.integer(input$age),
                      cur_ocs=as.integer(input$cur_ocs),
                      hist_ocs_low_years=(as.integer(input$ocs_years)*(as.integer(input$hist_ocs)==0)),
                      hist_ocs_high_years=(as.integer(input$ocs_years)*(as.integer(input$hist_ocs)==1)))
      })

    output$profile <- renderText(deparse(get_profile()))

    output$main <- renderUI(
      {
        pf <- get_profile()

        res_panels <- list()
        for(i in 1:length(outcomes))
        {
          res_panels[[i]] <- HTML(create_res_panel(pf, outcomes[i]))
        }

        res_panels
      }
    )
    #output$icon_array <- renderText(HTML(generate_icon_array()))
  }
)




