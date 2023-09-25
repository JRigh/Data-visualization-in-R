#---------------------------
# Parallel coordinates plots
#---------------------------

library(GGally)
data(iris)

ggparcoord(iris, columns = 1:4, groupColumn = 5, order = "anyClass",
           showPoints = TRUE, alphaLines = 0.4) + 
  labs(title = 'Parallel coordinates plot',
       subtitle = 'iris dataset',
       y="value", x="Species") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----