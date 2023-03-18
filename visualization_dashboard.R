#------------------------
# Visualization dashboard
#------------------------

library(tidyverse)
library(gridExtra)

data(diamonds)

sample <- sample(1:nrow(diamonds), size = 5000, replace = FALSE)
diamonds <- diamonds[sample, ]

# 1. scatterplot
diamond.plot <- ggplot(data=diamonds, aes(x=carat, y=price, colour = clarity))+
  geom_point(aes(size = cut))+ 
  labs(title = 'Example of Scatterplot',
       subtitle = 'diamonds dataset',
       y="price", x="carat") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. boxplot
diamond.bxplot <- ggplot(diamonds, aes(x = cut, y=price)) +
  geom_boxplot(aes(fill = color))+ 
  labs(title = 'Example of Boxplots',
       subtitle = 'diamonds dataset',
       y="price", x="cut") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 3. violin plot
diamond.violinplot <- ggplot(diamonds, aes(x = cut, y=price)) +
  geom_violin(aes(fill = color)) + 
  labs(title = 'Example of Violin plots',
       subtitle = 'diamonds dataset',
       y="price", x="cut") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 4. bar plot
diamond.barpot <- ggplot(diamonds, aes(x = clarity, fill=cut)) +
  geom_bar() +
  coord_flip() +
  labs(title = 'Example of Bar plots',
       subtitle = 'diamonds dataset',
       y="count", x="cut") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

grid.arrange(diamond.plot, diamond.bxplot,diamond.violinplot, diamond.barpot,
             nrow = 2)

#----
# end
#----
