

generate_summary_text <- function()
{
  HTML("
    <DIV><P><B>About this bar chart</B></P>
       <P>The bar chart above demonstrates the <B>Risk Ratio</B></P>

       <P><B>What is Risk Ratio?</B><BR/>
       The ratio of risk of outcomes between someone with exposure to oral corticosteroids to the same person, had they not taken any oral corticosteroids.</P>

      <P><B>Example:</B><BR/>
      A risk ratio of 50% for Diabetes indicates that a person with a history of oral corticosteroid exposure has a 50% higher risk of developing diabetes compared with a similar person without having used any oral corticosteroids.</P>

    </DIV>
  ")
}



create_bar_plot <- function(profile, outcomes)
{
  #input$calculate
  rrs <- rep(NA,length(outcomes))
  for(i in 1:length(outcomes))
  {
    rrs[i] <- OCSRiskCalculator::calculate_risk(profile,outcomes[i])
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
  df2 <- df[order(df$rr,decreasing=F),]
  df2$outcome <- factor(df2$outcome, levels=df2$outcome)

  require(ggplot2)

  plt <- ggplot(data=df2,aes(x=outcome, y=(rr-1)*100))+
    xlab("")+ylab("Percent Increase")+
    #aes(x=reorder(label,rr,))+
    geom_bar(stat="identity", fill="#43a2ca")+
    geom_text(aes(label=label), hjust=-0.1, vjust=0.5, color="#636363", size=7, family="open sans")+
    theme(axis.text=element_text(size=20))+
    theme(text=element_text(size=16,  family="open sans"))+
    geom_hline(yintercept=1, linetype="dashed", color = "orange", size=0.5)+
    coord_flip(ylim=c(0,115))+
    theme(axis.title=element_text(size=20, face="bold"),  plot.background = element_rect(fill = "#ffffff"),
          panel.background = element_rect(fill = "#ffffff", colour="#0e406a"))

    ggsave(tf1<-tempfile(fileext = ".svg"), plt, width=10, height=5)
    str <- paste(readLines(tf1), collapse="\n")
    for(outcome_name in names(outcomes))
    {
      str <- str_replace(str, outcome_name, paste0("<a style='font-weight:bold'  onclick='Shiny.setInputValue(\"outcome_clicked\", \"",outcome_name,"\"); return(false);' href='http://", outcome_name, ".com'>", outcome_name,"</a>"))
    }
    writeLines(str, tf1)

    paste("
          <p>The specific outcomes shown in the below graph are not a comprehensive list, rather comprise the more common outcomes associated with oral corticosteroid exposure.
          <span style='font-weight:bold; color:black'>You can click on each outcome to learn more about them individually</span></p>"
          ,str)
}
