#--------------------------------
# Scatterplot with densities in R
#--------------------------------

library(tidyverse)           # for ggplot2, dplyr
library(cowplot)             # for marginal densities
library(gridExtra)           # for multiple plots

data(iris)

# 1. Create initial scatterplot
p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species))+
  geom_point() + 
  labs(title = 'Scatterplot with marginal densities',
       subtitle = 'Sepal.Length x Sepal.Width from Iris dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. Create marginal densities
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = iris, aes(x = Sepal.Length, fill = Species),
               alpha = 0.4, size = 0.2)
ydens <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data = iris, aes(x = Sepal.Width, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

# 3. Create complete plot
ggdraw(p2)

#--------------------------------------------
# Scatterplot with different regression lines
#--------------------------------------------

# linear model
mod1 <- lm(Petal.Length ~ Sepal.Length, data = iris)
iris$predictions <- predict(mod1, type = 'response')

# plot
p <- ggplot(data = iris, aes(x = Sepal.Length,y = Petal.Length, colour=Species)) + 
  geom_smooth(method=lm) + 
  geom_line(color='black', size = 1.2, aes(x=Sepal.Length, y = predictions)) +
  geom_point() +
  labs(title = 'Scatterplot with different regression lines',
       subtitle = 'Sepal.Length x Petal.Width from Iris dataset, see if we should use mixed models',
       y="Petal Length", x="Sepal Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. Create marginal densities
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = iris, aes(x = Sepal.Length, fill = Species),
               alpha = 0.4, size = 0.2)
ydens <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data = iris, aes(x = Petal.Length, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

# 3. Create complete plot
ggdraw(p2)

#----
# end
#----

#-----------------------------------------------------------
# Scatterplot with nonparametric lines and marginal boxplots
#-----------------------------------------------------------

# Nonparametric regression model
mod1 <- ksmooth(x = iris$Sepal.Length, y = iris$Petal.Length,
                kernel = "normal", bandwidth = 1)

# plot
p <- ggplot(data = iris, aes(x = Sepal.Length,y = Petal.Length, colour=Species)) + 
  geom_smooth(method='loess') + 
  geom_line(color='black', size = 1.2, data = iris, aes(x=mod1$x, y = mod1$y)) +
  geom_point() +
  labs(title = 'Scatterplot with different Nonparametric regression lines and marginal boxplots plots',
       subtitle = 'Sepal.Length x Petal.Length from Iris dataset',
       y="Petal Length", x="Sepal Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. Create marginal Boxplots
xbp <- axis_canvas(p, axis = "x") +
  geom_boxplot(data = iris, aes(x = Sepal.Length, fill = Species),
               alpha = 0.4, size = 0.2)
ybp <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_boxplot(data = iris, aes(x = Petal.Length, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xbp, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ybp, grid::unit(.2, "null"), position = "right")

# 3. Create complete plot
ggdraw(p2)

#----
# end
#----