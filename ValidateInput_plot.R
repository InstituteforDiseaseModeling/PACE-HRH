library(tidyverse)

outfile <- file.path("log", "input_validation_results.png")
result_file <- read_csv("log/input_validation_results.csv")


# simplified transformation for demo only...

final_result <- result_file %>%
  mutate(
    warning = if_else(endsWith(severity,"g"),fails,0),
    info = if_else(endsWith(severity,"o"),fails,0),
    fails = fails - warning - info
  ) %>%
  select(c("name","items","passes","warning","info","fails")) %>%
  pivot_longer(cols=c("passes","warning","info","fails"), names_to="result", values_to="total")


# a normal plot...

p <- ggplot(final_result, aes(x=total/items, y=name, fill=result)) +
  geom_bar(stat = "identity") +
  xlab(NULL) + ylab(NULL) +
  scale_fill_manual(values = c("passes"="limegreen", "fails"="orangered", "warning"="plum", "info"="dodgerblue")) +
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



# a plot using fancy font...

#install.packages("showtext")
library(showtext)
font_add_google(name="Barlow Condensed", family="barlow-cond")
showtext_auto()

p <- ggplot(final_result, aes(x=total/items, y=name, fill=result)) +
  geom_bar(stat = "identity") +
  xlab(NULL) + ylab(NULL) +
  scale_fill_manual(values = c("passes"="limegreen", "fails"="orangered", "warning"="plum", "info"="dodgerblue")) +
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
