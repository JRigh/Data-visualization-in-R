#----------------------------------------------
# Plot multiple scatterplots and densities in R
#----------------------------------------------

library(GGally)

data(iris)

# summary of distributions
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4))

#----
# end
#----
