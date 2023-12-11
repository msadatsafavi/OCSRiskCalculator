outcome_descs <-

  list(
    ost="Osteoporosis is a systemic skeletal disorder characterized by low bone mass, micro-architectural deterioration of bone tissue leading to bone sterility, and consequent increase in fracture risk. It is the most common reason for a broken bone among the elderly"
  )



create_specific_coutcome_content <- function(profile, outcome_name)
{
  outcome <- get_outcomes()[outcome_name]
  outcome_desc <- outcome_descs[outcome]
  bg_risk <- calculate_baseline_risk(NULL, outcome)
  rr <- calculate_risk(profile ,outcome)

  preamble <- list(
    h4("You have selected the following outcome:", span(class="text-success", outcome_name)),
    h5(style="border:1px;  font-style:italic", ifelse(is.null(outcome_desc),"",outcome_desc)))

  if(rr > 1)
  {
    df <- data.frame(outcome=outcome_name, rr=rr, label=ifelse(rr<1,"No increase", ifelse(rr>2, ">100%", paste0("+",round(rr*100-100),"%"))))
    content <- list(
      h4("The risk ratio for this outcome is ",
         span(class="text-success", round(rr,2))),
      div(style="width:90%",
        renderPlot(
          ggplot(data=df,aes(x=outcome, y=(rr-1)*100))+
            xlab("")+ylab("Percent Increase")+
            geom_bar(stat="identity", fill="#43a2ca")+
            geom_text(aes(label=label), hjust=-0.1, vjust=0.5, color="#636363", size=7)+
            theme(axis.text=element_text(size=20))+
            geom_hline(yintercept=1, linetype="dashed", color = "orange", size=0.5)+
            coord_flip(ylim=c(0,115))+
            theme(axis.title=element_text(size=20,face="bold"),  plot.background = element_rect(fill = "#ffffff"),
                  panel.background = element_rect(fill = "#ffffff", colour="#0e406a")),
             height=150)),

      hr(),
      h5("If, after consulting with your care provider, you know your risk of this outcome, you can calculate the absolute increase in your risk."),
      h5(checkboxInput("know_my_bg_risk","I know my background risk", value=T)),
      div(id="div_know_my_bg_risk", style="visibility:hidden",
        h5("In the study by Sullivan et al, the 10-year risk of this outcome in individuals who are not taking oral corticosteroids was approximately",
           span(class="text-success", round(bg_risk*100),"%")),
        div(
          span(style="float:left;", sliderInput("specific_outcome_before",label="Risk without using oral corticosteroids*",min=0,max=100, value=bg_risk*100)),
          br(),br(),span(class="text-danger-emphasis", style="margin-top:100px", "  *Your background risk should be estimated after consulting with your care provider.")),
        br(),
        hr(),


        #progressBar("specific_outcome_after", value = 0, title = "Risk with using oral corticosteroids", display_pct = TRUE, status = "danger", striped = TRUE),

        HTML("<div class='progress' style='height:50px;'>
                <div id='pb_specific_outcome_before' class='progress-bar progress-bar-striped' role='progressbar' style='width: 15%; aria-valuenow='15' aria-valuemin='0' aria-valuemax='100'>15</div>
                <div id='pb_specific_outcome_after' class='progress-bar progress-bar-striped progress-bar-animated bg-dark' role='progressbar' style='width: 30%' aria-valuenow='30' aria-valuemin='0' aria-valuemax='100'>30</div>
              </div>"),
        HTML("Legend:<BR/>"),
        HTML("<div class='progress' style='width:250px; float:left;'>
                <div class='progress-bar progress-bar-striped' role='progressbar' style='width: 100%; aria-valuenow='100' aria-valuemin='0' aria-valuemax='100'>Risk without oral corticosteroids</div>
              </div>
             <div class='progress'  style='width:250px; margin:left; '>
                <div class='progress-bar progress-bar-striped progress-bar-animated bg-dark' role='progressbar' style='width: 100%; aria-valuenow='100' aria-valuemin='0' aria-valuemax='100'>Added risk with oral corticosteroids</div>
              </div>
             "),
        hr()
        #,
        # div(style="border:solid; color:gray; float:left; width:100%;",
        #   span(style='float:left; width:250px;', uiOutput("specific_outcome_icon_array", inline=T)),
        #   span(style="margin-left:10px;margin-top:100px;float:left;", uiOutput("specific_outcome_icon_array_legend",inline=T)),
        #   span(style="margin-left:10px;float:left; height:250px;", "") #TODO
        #   )
      )
    )
  }
  else
  {
    content <- list(
      h2(style="border-style:solid;border-color:#d8eaf5", "The risk of this outcome is not signifiucantly increased in patients with the selected pattern of oral corticosteroid use.")
    )
  }

  c(preamble, content)
}

# generate_icon_array_legend <- function()
# {
#   out <- paste("","<TABLE><TR><TD style='width:20pt;'>",faces['yellow'],"</TD><TD>Risk without taking oral corticosteroi</TD></TR>")
#   out <- paste(out,"<TABLE><TR><TD style='width:20pt;'>",faces['red'],"</TD><TD>Extra risk due to taking oral corticosteroid</TD></TR>")
#   out <- paste(out,"<TABLE><TR><TD style='width:20pt;'>",faces['green'],"</TD><TD>Not at risk</TD></TR></TABLE>")
#
#   out
#
# }
#
# faces <- c(red='<svg class="red-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.red-face .cls-1{fill:#a53a47;}.red-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><path class="cls-2" d="M8.3,6a2,2,0,1,1-2-2A2,2,0,0,1,8.3,6Z"></path><circle class="cls-2" cx="14.7" cy="5.98" r="1.97"></circle><path class="cls-2" d="M16.61,15.38a6.29,6.29,0,0,0-12.18,0,.33.33,0,0,0,.24.41.34.34,0,0,0,.42-.24,5.6,5.6,0,0,1,10.86,0,.36.36,0,0,0,.15.21.39.39,0,0,0,.18,0h.08A.34.34,0,0,0,16.61,15.38Z"></path></g></g></svg>',
#            green='<svg class="green-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.green-face .cls-1{fill:#2eb49a;}.green-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><circle class="cls-2" cx="6.64" cy="6.95" r="1.83"></circle><circle class="cls-2" cx="14.42" cy="6.95" r="1.83"></circle><path class="cls-2" d="M16.67,11.29a.36.36,0,0,0-.27,0,.41.41,0,0,0-.17.22,5.89,5.89,0,0,1-11.41,0,.36.36,0,0,0-.16-.22.36.36,0,0,0-.27,0,.37.37,0,0,0-.22.17.36.36,0,0,0,0,.27,6.61,6.61,0,0,0,12.8,0A.37.37,0,0,0,16.67,11.29Z"></path></g></g></svg>',
#            yellow='<svg class="yellow-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.yellow-face .cls-1{fill:#eea342;}.yellow-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><circle class="cls-2" cx="6.67" cy="7.92" r="1.81"></circle><circle class="cls-2" cx="14.36" cy="7.92" r="1.81"></circle><path class="cls-2" d="M17.12,14.29A11.46,11.46,0,0,1,4,14.3a.45.45,0,1,0-.53.73,12.42,12.42,0,0,0,7.17,2.23,11.66,11.66,0,0,0,7-2.25.45.45,0,0,0,.09-.63A.46.46,0,0,0,17.12,14.29Z"></path></g></g></svg>')
#
#
#
# generate_icon_array <- function(order=c('yellow','red','green'),counts=c(50,30,20))
# {
#   x <- c(rep(faces[order[1]],counts[1]),
#          rep(faces[order[2]],counts[2]),
#          rep(faces[order[3]],counts[3]))
#   out <- "<TABLE style='width:100%; margin:0;padding:0;display:inline-table'><TBODY>"
#   for(i in 1:10)
#   {
#     out <- paste(out,"<TR>")
#     for(j in 1:10)
#     {
#       out <- paste(out,"<TD style='width:10%;height=10%'>",x[10*(i-1)+j],"</TD>")
#     }
#     out <- paste(out,"</TR>")
#   }
#   out <- paste(out,"</TBODY></TABLE>")
#
#   out
# }
