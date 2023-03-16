#-----------------
# 3D barplots in R
#-----------------

library(lattice)
library(latticeExtra)
library(RColorBrewer)

# create matrix to be plotted
Matrix <- matrix(seq(1,1000, by=round(1000/20)),
                 nrow=5,byrow=TRUE)
rownames(Matrix)<-LETTERS[1:5]
colnames(Matrix)<-letters[1:4]

# color
redcol = colorRampPalette(brewer.pal(9,'Reds'))(150)

# 3D barplot 
cloud(Matrix, panel.3d.cloud = panel.3dbars, zoom = 0.96,
      xbase = 0.4, ybase = 0.4, zlim = c(0, max(Matrix)),
      scales = list(arrows = FALSE, just = "right"), 
      xlab = NULL, ylab = NULL, zlab = NULL,
      par.settings = list(axis.line = list(col = "transparent")),
      col.facet = level.colors(Matrix, 
                               at = do.breaks(range(Matrix), 30),
                               col.regions = redcol,
                               colors = TRUE),
      main='3D Barplot',
      colorkey = list(col = redcol, at = do.breaks(range(Matrix), 30)),
      screen = list(z = 20, x = -60))


#----
# end
#----
