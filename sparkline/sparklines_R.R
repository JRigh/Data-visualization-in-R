#----------------
# Sparklies plots
#----------------

library(tidyverse)
library(reshape2)

# creating dataset
set.seed(2023)
year = 1986:2023
v1 = rpois(n = length(year), lambda = 4) * seq(0.1, 1, length = 38)
v2 = rpois(n = length(year), lambda = 6) 
v3 = rpois(n = length(year), lambda = 3.6) / seq(0.1, 1, length = 38)

# data reshaping
dataset <- data.frame('year' = year, 'v1' = v1, 'v2' = v2, 'v3' = v3)
dataset <- melt(dataset, id="year")

# ploting
ggplot(dataset, aes(x=year, y = value)) + 
  facet_grid(variable ~ ., scales = "free_y") + 
  geom_line(linewidth=0.3) + 
  geom_point() + 
  geom_point(data = dataset[which.max(dataset$value), ], color="red") +
  geom_point(data = dataset[which.min(dataset$value), ], color="red") +
  scale_color_manual(values = c(NA, "red"), guide=F) +
  labs(title = 'Sparklines plot with facet_grid',
       subtitle = 'artificial dataset',
       y="value", x="year") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----