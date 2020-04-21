
plot.cases <- function(dat, measure, ylabel, group, facet){
  ylab <- ylabel
  if(is.null(facet)){
    dat %>%
      filter(metric==measure) %>%
      ggplot(aes(x=time)) +
      geom_ribbon(aes(ymin = low_95, ymax = up_95), alpha =1, fill="#efedf5") +
      geom_ribbon(aes(ymin=low_50 , ymax=up_50) , alpha = 1, fill="#bcbddc") +
      geom_line(aes(y=median), lwd=1, color="#756bb1") +
      facet_grid(.~get(group)) +
      theme_minimal_grid() +
      theme(text = element_text(size=10), 
            axis.text = element_text(size=8)) +
      labs(x = "\nTime (days)", y=paste(ylab, "\n", sep=""))
  } else {
    dat %>%
      filter(metric==measure) %>%
      ggplot(aes(x=time)) +
      geom_ribbon(aes(ymin = low_95, ymax = up_95), alpha =1, fill="#efedf5") +
      geom_ribbon(aes(ymin=low_50 , ymax=up_50) , alpha = 1, fill="#bcbddc") +
      geom_line(aes(y=median), lwd=1, color="#756bb1") +
      facet_grid(get(facet)~get(group)) +
      theme_minimal_grid() +
      theme(text = element_text(size=10), 
            axis.text = element_text(size=8)) +
      labs(x = "\nTime (days)", y=paste(ylab, "\n", sep=""))
  }

}


plot.rates <- function(dat, measure, ylabel, group, facet){
  ylab <- ylabel
  if(is.null(facet)){
    dat %>%
      filter(metric==measure) %>% 
      ggplot(aes(x=time)) +
      geom_ribbon(aes(ymin = 10^3*low_95/pop, ymax = 10^3*up_95/pop), alpha =1, fill="#efedf5") +
      geom_ribbon(aes(ymin=10^3*low_50/pop , ymax=10^3*up_50/pop) , alpha = 1, fill="#bcbddc") +
      geom_line(aes(y=10^3*median/pop), lwd=1, color="#756bb1") +
      facet_grid(.~get(group)) +
      theme_minimal_grid() +
      theme(text = element_text(size=10), 
            axis.text = element_text(size=8)) +
      labs(x = "\nTime (days)", y=paste(ylab, "\nper 1000 population\n", sep=" "))
  } else {
    dat %>%
      filter(metric==measure) %>% 
      ggplot(aes(x=time)) +
      geom_ribbon(aes(ymin = 10^3*low_95/pop, ymax = 10^3*up_95/pop), alpha =1, fill="#efedf5") +
      geom_ribbon(aes(ymin=10^3*low_50/pop , ymax=10^3*up_50/pop) , alpha = 1, fill="#bcbddc") +
      geom_line(aes(y=10^3*median/pop), lwd=1, color="#756bb1") +
      facet_grid(get(facet)~get(group)) +
      theme_minimal_grid() +
      theme(text = element_text(size=10), 
            axis.text = element_text(size=8)) +
      labs(x = "\nTime (days)", y=paste(ylab, "\nper 1000 population\n", sep=" "))
  }

}
  


