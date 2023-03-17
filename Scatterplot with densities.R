#--------------------------------
# Scatterplot with densities in R
#--------------------------------

library(ggplot2)
library(cowplot)

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

# Multiple Scatterplots with densities

library(gridExtra)

# first plot -------------------------------------------------------------------
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
plot1 <- ggdraw(p2)

# second plot ------------------------------------------------------------------
# 1. Create initial scatterplot
p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species))+
  geom_point() + 
  labs(title = 'Scatterplot with marginal densities',
       subtitle = 'Sepal.Length x Petal.Length from Iris dataset',
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
  geom_density(data = iris, aes(x = Petal.Length, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

# 3. Create complete plot
plot2 <- ggdraw(p2)

# third plot -------------------------------------------------------------------
# 1. Create initial scatterplot
p <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Width, color = Species))+
  geom_point() + 
  labs(title = 'Scatterplot with marginal densities',
       subtitle = 'Sepal.Length x Petal.Width from Iris dataset',
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
  geom_density(data = iris, aes(x = Petal.Width, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

# 3. Create complete plot
plot3 <- ggdraw(p2)

# fourth plot ------------------------------------------------------------------
# 1. Create initial scatterplot
p <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, color = Species))+
  geom_point() + 
  labs(title = 'Scatterplot with marginal densities',
       subtitle = 'Sepal.Width x Petal.Width from Iris dataset',
       y="Sepal.Width", x="Sepal.Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. Create marginal densities
xdens <- axis_canvas(p, axis = "x") +
  geom_density(data = iris, aes(x = Sepal.Width, fill = Species),
               alpha = 0.4, size = 0.2)
ydens <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data = iris, aes(x = Petal.Width, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xdens, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.2, "null"), position = "right")

# 3. Create complete plot
plot4 <- ggdraw(p2)

# final plot -------------------------------------------------------------------
final.plot <- grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)

