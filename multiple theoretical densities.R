#------------------------------------------
# Plotting multiple densities on same graph
#------------------------------------------

# 1. Create artificial data
set.seed(2023)
mu = seq(from = -10, to = 10, by = 1000)
density1 = rnorm(n = 10000, mean = 0, sd = 1)
density2 = rnorm(n = 10000, mean = -4, sd = 2)
density3 = rnorm(n = 10000, mean = 5, sd = 1.5)

# 2. Create data frame
data_frame <- data.frame('mu' = mu, 'density1' = density1, 
                         'density2' = density2, 'density3' = density3)
data_frame = na.omit(data_frame)

# 3. Create plot
ggplot(data=data_frame) +
  stat_function(fun=dnorm, args=list(mean = 0, sd = 1), 
                aes(linetype = "Density 1"), lwd = 1.2) +
  stat_function(fun=dnorm, args=list(mean = -4, sd = 2), 
                aes(linetype = "Density 2"), lwd = 1.2) +
  stat_function(fun=dnorm, args=list(mean = 5, sd = 1.5), 
                aes(linetype = "Density 3"), lwd = 1.2) +
  xlim(-10, 10) + ylim(0, 0.4) +
  labs(title = 'Multiple theoretical densities on same graph',
       subtitle = 'artificial dataset',
       y="density", x="mu") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----