library(shiny)
library(OCSRiskCalculator)

faces <- c(red='<svg class="red-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.red-face .cls-1{fill:#a53a47;}.red-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><path class="cls-2" d="M8.3,6a2,2,0,1,1-2-2A2,2,0,0,1,8.3,6Z"></path><circle class="cls-2" cx="14.7" cy="5.98" r="1.97"></circle><path class="cls-2" d="M16.61,15.38a6.29,6.29,0,0,0-12.18,0,.33.33,0,0,0,.24.41.34.34,0,0,0,.42-.24,5.6,5.6,0,0,1,10.86,0,.36.36,0,0,0,.15.21.39.39,0,0,0,.18,0h.08A.34.34,0,0,0,16.61,15.38Z"></path></g></g></svg>',
          green='<svg class="green-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.green-face .cls-1{fill:#2eb49a;}.green-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><circle class="cls-2" cx="6.64" cy="6.95" r="1.83"></circle><circle class="cls-2" cx="14.42" cy="6.95" r="1.83"></circle><path class="cls-2" d="M16.67,11.29a.36.36,0,0,0-.27,0,.41.41,0,0,0-.17.22,5.89,5.89,0,0,1-11.41,0,.36.36,0,0,0-.16-.22.36.36,0,0,0-.27,0,.37.37,0,0,0-.22.17.36.36,0,0,0,0,.27,6.61,6.61,0,0,0,12.8,0A.37.37,0,0,0,16.67,11.29Z"></path></g></g></svg>',
          yellow='<svg class="yellow-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.yellow-face .cls-1{fill:#eea342;}.yellow-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><circle class="cls-2" cx="6.67" cy="7.92" r="1.81"></circle><circle class="cls-2" cx="14.36" cy="7.92" r="1.81"></circle><path class="cls-2" d="M17.12,14.29A11.46,11.46,0,0,1,4,14.3a.45.45,0,1,0-.53.73,12.42,12.42,0,0,0,7.17,2.23,11.66,11.66,0,0,0,7-2.25.45.45,0,0,0,.09-.63A.46.46,0,0,0,17.12,14.29Z"></path></g></g></svg>')


info_div_innerHTML <- "<TABLE style='background-color:gray;width:100%;height:100%'>
                        <TBODY>
                          <TR>
                            <TD></TD>
                            <TD align='center'>
                              <SPAN style='border:5px red solid; background-color:orange;'>
                                More Info
                              </SPAN>
                            </TD>
                            <TD></TD>
                          </TR>
                        </TBODY>
                      </TABLE>"

start_here_innerHTML <- '<h1>← Start Here</h1>
                        <p>Enter patient data in the left panel and press “Run the prediction model.”</p>
                        <p>Inputs marked with <span style="color:red">*</span> are required.</p>
                        <p><span style="color:Tomato">Warning: This tool SHOULD NOT BE USED to replace a diagnostic or treatment decision made by a physician. None of the predictors on the left panel have a causal interpretation in the model. Changing them for a single patient to estimate the effect of an intervention would be misleading and should be avoided.  </span>
                        <br></p>
                        <p>For a detailed description of the model, please refer to the <a href="https://doi.org/10.1016/S2213-2600%2819%2930397-2">publication</a>.</p>
                        <p>ACCEPT is also available as an Excel spreadsheet, R package, and API. Please refer to project <a href="http://resp.core.ubc.ca/research/Specific_Projects/accept">homepage</a> for more information.</p>'


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
         numericInput("age", label="age", min=18, max=80, value=40, width="50%")
        ,radioButtons("female","Gender:", choices=c("Female"=1, "Male"=0))
        ,radioButtons("cur_ocs","Currently taking OCS:", choices=c("No"=0, "Yes"=1))
        ,numericInput("ocs_years", label="Number of years taking corticosteroids", min=1, max=30, value=10, width="50%")
        ,radioButtons("hist_ocs","Generally, your OCS dose has been", choices=c("Low"=0, "High"=1))
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
    generate_icon_array <- function(order=c('yellow','red','green'),counts=c(50,30,20))
    {
      x <- c(rep(faces[order[1]],counts[1]),
             rep(faces[order[2]],counts[2]),
             rep(faces[order[3]],counts[3]))
      out <- "<TABLE style='width:100%;margin:0;padding:0'><TBODY>"
      for(i in 1:10)
      {
        out <- paste(out,"<TR>")
        for(j in 1:10)
        {
          out <- paste(out,"<TD style='width:10%;height=10%'>",x[10*(i-1)+j],"</TD>")
        }
        out <- paste(out,"</TR>")
      }
      out <- paste(out,"</TBODY></TABLE>")

      out
    }


    observeEvent(input$calculate, {
      if(isolate(input$consent))
      {
        for(i in 1:length(outcomes))
        {
          local({
            my_i <- i
            output[[paste0("summary_",outcomes[my_i])]] <- renderUI(HTML(create_summary_panel(profile=pfl(), outcome=outcomes[my_i])))
          })
        }
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

      updateCheckboxInput(inputId="consent",value = F)
      for(nm in names(input))
        shinyjs::enable(nm)
      output$need_to_consent <- renderText("")
    })

    create_summary_panel <- function(profile, outcome)
    {
      #input$calculate
      tmp <- round(calculate_risk(profile,outcome)*1000)/10
      tmp_i <- round(tmp)
      paste0("<DIV id='",outcome,"' onmouseover='mouseOverOutcome(this.id)' onmouseout='mouseExitOutcome(this.id)'>
                <A href='http://www.sony.com'>
                  <TABLE style='float:left;border:1px black solid;width:250pt;margin-top: 20px;  margin-bottom: 20px;  margin-right: 20px;  margin-left: 20px;'>
                    <TBODY>
                      <TR>
                        <TD>",names(outcomes)[which(outcomes==outcome)],":",tmp[1],"% (from:", tmp[2],"% to:", tmp[3],"%)
                        </TD>
                      </TR>
                      <TR>
                        <TD>
                          <DIV style='position:relative;z-index:1'>",
                            generate_icon_array(counts=c(tmp_i[2],tmp_i[1],100-tmp_i[2]-tmp_i[1])),"
                            <DIV id='more_info_",outcome,"' style='position:absolute;width:100%;height:100%;top:0;left:0;z-index:10;opacity:0.5;visibility:hidden'>",info_div_innerHTML,"
                            </DIV>
                          </DIV>
                        </TD>
                      </TR>
                    </TBODY>
                  </TABLE>
                </A>
             </DIV>")
    }

    outcomes <- get_outcomes()

    panels <- list(tabPanel("Summary", uiOutput("summary")))
    for(i in 1:length(outcomes))
    {
      panels[[i+1]] <- tabPanel(names(outcomes)[i])
    }
    output$main <- renderUI(do.call(tabsetPanel,args=panels))

    output$summary <- renderUI({
      panel_output_list <- list(htmlOutput("start_here", inline = T))
      for(i in 1:length(outcomes))
      {
        panel_output_list[[i+1]] <- htmlOutput(paste0("summary_", outcomes[i]), inline=T, fill=F)
      }

      panel_output_list
    })

    output$start_here <- renderUI(HTML(start_here_innerHTML))

    pfl <- reactive({
                 c(female=as.integer(input$female),
                 age= as.integer(input$age),
                 cur_ocs=as.integer(input$cur_ocs),
                 hist_ocs_low_years=(as.integer(input$ocs_years)*(as.integer(input$hist_ocs)==0)),
                 hist_ocs_high_years=(as.integer(input$ocs_years)*(as.integer(input$hist_ocs)==1)))
              })

    output$profile <- renderText(pfl())


    #output$icon_array <- renderText(HTML(generate_icon_array()))
  }
)




