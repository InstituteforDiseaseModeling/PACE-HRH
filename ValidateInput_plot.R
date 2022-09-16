options(install.packages.check.source = "no")
packages = c("tidyverse", "showtext")
for(i in packages){
  if(!require(i, character.only = T)){
    install.packages(i)
    library(i, character.only = T)
  }
}

plot_result <- function(result_file, outfile, fancy = TRUE){
  
  final_result <- result_file %>%
    mutate(
      warning = if_else(severity=="warning", fails, 0L),
      info = if_else(severity=="info", fails, 0L),
      fails = if_else(severity=="error", fails, 0L)
    ) %>%
    select(c("name","items","passes","warning","info","fails")) %>%
    pivot_longer(cols=c("passes","warning","info","fails"), names_to="result", values_to="total")
  
  ylevel_order <- rev(final_result %>% select(name) %>% unique() %>% arrange(name) %>% .$name)
  xlevel_order <- c("info", "fails", "warning", "passes")
  
  # a normal plot...
  if (!fancy){
    p <- ggplot(final_result, aes(x=total/items, y=factor(name, level=ylevel_order), fill=factor(result, level=xlevel_order))) +
      geom_bar(stat = "identity") +
      xlab(NULL) + ylab(NULL) +
      scale_fill_manual(values = c("passes"="limegreen", "fails"="orangered", "warning"="darkgoldenrod1", "info"="dodgerblue")) +
      scale_x_continuous(labels=scales::percent) +
      scale_y_discrete(position = "right") +
      geom_text(data=subset(final_result,total> 0), aes(label=total, y=name), size=3, position=position_fill(), hjust=1.4, color="white") +
      ggtitle("Input Spreadsheet Validation Results") +
      coord_cartesian(xlim=c(0,1)) +
      theme(
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.margin=margin(3,1,3,3,"lines"),
        legend.direction ="horizontal",
        legend.position = "bottom",
        legend.title=element_blank(),
        panel.background=element_rect(fill = "white"),
        text=element_text(size=11))
    ggsave(outfile, p, width = 160, height = 160, units="mm")
  }
  else{
    # a plot using fancy font...
    font_add_google(name="Barlow Condensed", family="barlow-cond")
    showtext_auto()
    
    p <- ggplot(final_result, aes(x=total/items, y=factor(name, level=ylevel_order), fill=factor(result, level=xlevel_order))) +
      geom_bar(stat = "identity") +
      xlab(NULL) + ylab(NULL) +
      scale_fill_manual(values = c("passes"="limegreen", "fails"="orangered", "warning"="darkgoldenrod1", "info"="dodgerblue")) +
      scale_x_continuous(labels=scales::percent) +
      scale_y_discrete(position = "right") +
      geom_text(data=subset(final_result,total> 0), aes(label=total, y=name), size=9, position=position_fill(), hjust=1.4, color="white") +
      ggtitle("Input Spreadsheet Validation Results") +
      coord_cartesian(xlim=c(0,1)) +
      theme(
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.margin=margin(3,1,3,3,"lines"),
        legend.direction ="horizontal",
        legend.position = "bottom",
        legend.title=element_blank(),
        panel.background=element_rect(fill = "white"), 
        text=element_text(size=40, family="barlow-cond"))
    
    ggsave(outfile, p, width = 160, height = 160, units="mm")
    showtext_auto(FALSE)
  }
 
}

