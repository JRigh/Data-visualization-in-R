#-------------------------------------------------
# Ridgeplots:
# Different distribution density on the same graph
# on different levels
#-------------------------------------------------

library(ggplot2)
library(ggjoy)

# example 1 using iris data
data(iris)

# plot
ggplot(iris, aes(x = Sepal.Width, y = Species, fill = Species)) +
  geom_joy(scale = 2, alpha = 0.4) +                                          # create ridges
  scale_y_discrete(expand=c(0.05, 0)) +
  scale_x_continuous(expand=c(0.05, 0.05)) +
  labs(title = 'Multiple densities on different levels',
       subtitle = 'Ridge plot with ggjoy on iris dataset',
       y="Species", x="density Sepal.Width") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# example 2: simpler

library(ggridges)
library(ggplot2)

# plot
ggplot(iris, aes(x = Sepal.Length, y = Species, fill = Species)) +
  geom_density_ridges(alpha = 0.4) +                                          # create ridges
  labs(title = 'Multiple densities on different levels',
       subtitle = 'Ridge plot with ggridges on iris dataset',
       y="Species", x="density Sepal Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----
