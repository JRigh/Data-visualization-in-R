#-----------------------------------
# Histograms with overlaying density
#-----------------------------------

data(iris)

# 1. Create first plot
p1 <- ggplot(iris, aes(x = Petal.Length)) +
  geom_histogram( color = 'black', fill = 'darkred', binwidth = 0.3) +
  labs(title = 'Histogram of Petal Length',
       subtitle = 'iris dataset',
       y="count", x="Sepal Width") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 1. Create first plot
p2 <- ggplot(iris, aes(x = Petal.Length, color = Species, fill = Species)) +
  geom_histogram( color = 'black', binwidth = 0.3, alpha = 0.4) +
  labs(title = 'Histogram by Species',
       subtitle = 'iris dataset',
       y="count", x="Sepal Width") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. Create second plot
p3 <-  ggplot(iris) +
  geom_histogram(aes(x = Petal.Length, y = stat(density)), fill = "black", binwidth = 0.1) +
  geom_density(aes(x = Petal.Length, fill = Species, colour = Species), alpha = 0.4) +
  labs(title = 'Histogram with overlying densities by Species',
       subtitle = 'iris dataset',
       y="count", x="Sepal Width") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 3. Create third plot
p4 <- ggplot(iris) +
  geom_density(aes(x = Petal.Length, fill = Species), alpha = 0.4) +
  labs(title = 'Densities by Species',
       subtitle = 'iris dataset',
       y="count", x="Sepal Width") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 3. Create final plot
final.plot <- grid.arrange(p1, p2, p3, p4, nrow = 2)

#----
# end
#----