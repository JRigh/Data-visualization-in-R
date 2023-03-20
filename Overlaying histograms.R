#----------------------
# Overlaying histograms
#----------------------

library(tidyverse)
library(gridExtra)

# 1. Create dataset
x1 <- rnorm(n = 100, mean = 5, sd = 1)
x2 <- runif(n = 100, min = 0, max = 10) + rgamma(n = 100, shape = 10, rate = 1)
category <- sample(c('yes', 'no'), size = 100, replace = TRUE, prob = c(0.2, 0.8))
dataset <- data.frame(x1, x2, category)

# 1. Create first plot
p1 <- ggplot(dataset, aes(x = x1, fill = category)) +                              # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.3, bins = 20) +
  labs(title = 'Overlaying histogram 2',
       subtitle = 'artificial dataset',
       y="count of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. Create second plot
p2 <- ggplot(dataset, aes(x = x2, fill = category)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.3, bins = 20) +
  labs(title = 'Overlaying histogram 1',
       subtitle = 'artificial dataset',
       y="count of customers", x="churn") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 3. Create final plot
final.plot <- grid.arrange(p1, p2, nrow = 1)

#----
# end
#----