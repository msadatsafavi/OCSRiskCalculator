generate_summary_text <- function()
{
  HTML("
  <P><B>About this bar chart</B></P>
      The bar chart above demonstrates the <B>Risk Ratio</B>:
       the ratio of risk of outcomes between someone with exposure to oral corticosteroids to the same person,
       had they not taken any oral corticosteroids.
       For example, a risk ratio of 50% for diabetes indicates that
       a person with this history of oral corticosteroid use has 50% higher risk of developing diabetes compared with a person of similar age and sex
       but without having used any oral corticosteroids.

       ")
}



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

  plt <- ggplot(data=df,aes(x=outcome, y=(rr-1)*100))+
    xlab("")+ylab("Percent Increase")+
    geom_bar(stat="identity")+
    geom_text(aes(label=label), hjust=-0.1, vjust=0.5, color="orange", size=7)+
    theme(axis.text=element_text(size=20))+
    geom_hline(yintercept=1, linetype="dashed", color = "orange", size=0.5)+
    coord_flip(ylim=c(0,115))+
    theme(axis.title=element_text(size=20,face="bold"),  plot.background = element_rect(fill = "#d8eaf5"),
          panel.background = element_rect(fill = "#d8eaf5", colour="#0e406a"))

    ggsave(tf1<-tempfile(fileext = ".svg"), plt, width=10, height=5)
    str <- paste(readLines(tf1), collapse="\n")
    for(outcome_name in names(outcomes))
    {
      str <- str_replace(str, outcome_name, paste("<a onclick='Shiny.setInputValue(\"outcome_clicked\", \"",outcome_name,"\"); return(false);' href='http://", outcome_name, ".com'>", outcome_name,"</a>"))
    }
    writeLines(str, tf1)

  str
}
