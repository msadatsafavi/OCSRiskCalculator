outcome_descs <-

  list(
    ost="Osteoporosis is a systemic skeletal disorder characterized by low bone mass, micro-architectural deterioration of bone tissue leading to bone sterility, and consequent increase in fracture risk. It is the most common reason for a broken bone among the elderly"
  )


create_single_page <- function(outcome)
{
  outcomes <- get_outcomes()
  outcome_name <- names(which(outcomes==outcome))
  outcome_desc <- outcome_descs[outcome]
  list(
    h3(paste0("In this page, we evaluate the risk of specific outcome (",outcome_name,")")),
    h4(ifelse(is.null(outcome_desc),"",outcome_desc)),
    hr(),
    sliderInput(paste0(outcome,"_before"),label="Risk without using oral corticosteroids*",min=0,max=100, value=50),
    span("*Your background risk should be estimated after consulting with your care provider.", style="font-style:italic;"),
    hr(),
    progressBar(paste0(outcome,"_after"), value = 0, title = "Risk with using oral corticosteroids", display_pct = TRUE, status = "danger", striped = TRUE),
    uiOutput(paste0(outcome,"_icon_array"), inline=T),
    uiOutput(paste0(outcome,"_icon_array_legend"),inline=T)
  )
}

xxx<-function()
{
  out <- paste("","<TABLE><TR><TD style='width:20pt;'>",faces['yellow'],"</TD><TD>General (background) risk</TD></TR>")
  out <- paste(out,"<TABLE><TR><TD style='width:20pt;'>",faces['red'],"</TD><TD>Risk due to oral corticosteroid use</TD></TR>")
  out <- paste(out,"<TABLE><TR><TD style='width:20pt;'>",faces['green'],"</TD><TD>Not at risk</TD></TR></TABLE>")

  out

}

faces <- c(red='<svg class="red-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.red-face .cls-1{fill:#a53a47;}.red-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><path class="cls-2" d="M8.3,6a2,2,0,1,1-2-2A2,2,0,0,1,8.3,6Z"></path><circle class="cls-2" cx="14.7" cy="5.98" r="1.97"></circle><path class="cls-2" d="M16.61,15.38a6.29,6.29,0,0,0-12.18,0,.33.33,0,0,0,.24.41.34.34,0,0,0,.42-.24,5.6,5.6,0,0,1,10.86,0,.36.36,0,0,0,.15.21.39.39,0,0,0,.18,0h.08A.34.34,0,0,0,16.61,15.38Z"></path></g></g></svg>',
           green='<svg class="green-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.green-face .cls-1{fill:#2eb49a;}.green-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><circle class="cls-2" cx="6.64" cy="6.95" r="1.83"></circle><circle class="cls-2" cx="14.42" cy="6.95" r="1.83"></circle><path class="cls-2" d="M16.67,11.29a.36.36,0,0,0-.27,0,.41.41,0,0,0-.17.22,5.89,5.89,0,0,1-11.41,0,.36.36,0,0,0-.16-.22.36.36,0,0,0-.27,0,.37.37,0,0,0-.22.17.36.36,0,0,0,0,.27,6.61,6.61,0,0,0,12.8,0A.37.37,0,0,0,16.67,11.29Z"></path></g></g></svg>',
           yellow='<svg class="yellow-face" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 21.35 21.35"><defs><style>.yellow-face .cls-1{fill:#eea342;}.yellow-face .cls-2{fill:#fff;}</style></defs><g id="Layer_2" data-name="Layer 2"><g id="Layer_1-2" data-name="Layer 1"><circle class="cls-1" cx="10.67" cy="10.67" r="10.67"></circle><circle class="cls-2" cx="6.67" cy="7.92" r="1.81"></circle><circle class="cls-2" cx="14.36" cy="7.92" r="1.81"></circle><path class="cls-2" d="M17.12,14.29A11.46,11.46,0,0,1,4,14.3a.45.45,0,1,0-.53.73,12.42,12.42,0,0,0,7.17,2.23,11.66,11.66,0,0,0,7-2.25.45.45,0,0,0,.09-.63A.46.46,0,0,0,17.12,14.29Z"></path></g></g></svg>')



generate_icon_array <- function(order=c('yellow','red','green'),counts=c(50,30,20))
{
  x <- c(rep(faces[order[1]],counts[1]),
         rep(faces[order[2]],counts[2]),
         rep(faces[order[3]],counts[3]))
  out <- "<TABLE style='width:300px;margin:0;padding:0;display:inline-table'><TBODY>"
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
