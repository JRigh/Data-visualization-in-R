#----------------------------------------------
# Plot multiple scatterplots and densities in R
#----------------------------------------------

library(GGally)

data(iris)

## Visualization
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4))

#----
# end
#----
