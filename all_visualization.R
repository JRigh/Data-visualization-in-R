#-------------------------------
# Elements of visualization in R
#-------------------------------

library(tidyverse)
library(quantmod)
library(reshape2)
library(GGally)
library(cowplot)
library(gridExtra)           # for multiple plots
library(ggridges)
library(fmsb)
library(sjPlot)
library(ggfx)                # for shaded areas

data(iris)
data(diamonds)

#--------------------------------
# Scatterplot with densities in R
#--------------------------------

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
ggplot(data = iris, aes(x = Sepal.Length,y = Petal.Length, colour=Species)) + 
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

#----
# end
#----


#---------------------------------------------------------
# Scatterplot with regression lines and marginal densities
#---------------------------------------------------------

# Nonparametric regression model
mod1 <- lm(Sepal.Width ~ Sepal.Length, data = iris)
iris$predictions <- predict(mod1, type = 'response')

# plot
p <- ggplot(data = iris, aes(x = Sepal.Length,y = Sepal.Width, colour=Species)) + 
  geom_smooth(method='lm', se = FALSE) + 
  geom_line(color='black', size = 1.2, data = iris, aes(x=Sepal.Length, y = predictions)) +
  geom_point() +
  labs(title = 'Scatterplot with different  regression lines and marginal densities',
       subtitle = 'Sepal Width x Sepal Length from Iris dataset',
       y="Petal Length", x="Sepal Width") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. Create marginal Boxplots
xbp <- axis_canvas(p, axis = "x") +
  geom_density(data = iris, aes(x = Sepal.Length, fill = Species),
               alpha = 0.4, size = 0.2)
ybp <- axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data = iris, aes(x = Sepal.Width, fill = Species),
               alpha = 0.4, size = 0.2) + coord_flip()

p1 <- insert_xaxis_grob(p, xbp, grid::unit(.2, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ybp, grid::unit(.2, "null"), position = "right")

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
  geom_smooth(method='loess', se = FALSE) + 
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




# subset diamonds
set.seed(2023)
indexes <- sample(diamonds$color, size = 10000, replace = FALSE)
result <- subset(diamonds, colors = indexes)
diamonds1000 <- result[sample(nrow(result), 1000), ]
diamonds1000 <- dplyr::slice_sample(result, n = 1000)

# create scatterplot
ggplot(data=diamonds1000, aes(x=carat, y=price, colour = clarity))+
  geom_point(aes(size = cut)) + 
  viridis::scale_color_viridis(discrete=TRUE,option="magma") +
  labs(title = 'Scatterplot with point sizes and colors by group',
       subtitle = 'subset of diamonds dataset',
       y="price", x="carat") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----

#---------------
# time data
#---------------
# 1. retrieve stock prices from Yahoo finance 

# Tesla, Inc.
TSLA <- getSymbols("TSLA", src = "yahoo", from = "2020-01-01", to = "2020-12-31", auto.assign = FALSE)
# Apple Inc.
AAPL <- getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = "2020-12-31", auto.assign = FALSE)

# 2. Create dataset

dates <- index(TSLA)
dataset <- data.frame('dates' = dates, TSLA[, 6], AAPL[, 6])

dataset_long <- melt(dataset, id.vars = "dates")
head(dataset_long)  

# 3. Time series plot of one stock

ggplot(dataset, aes(x = dates, y = TSLA.Adjusted)) +
  geom_line(color = 'darkblue') + 
  geom_point(size = 0.6) +
  labs(title = 'Time series plot',
       subtitle = 'Tesla stock',
       y="Adjusted closing price", x="time") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 4. Time series plot of the different stocks

ggplot(dataset_long, aes(x = dates, y = value, col = variable)) +
  geom_line() + 
  geom_point(size = 0.6) +
  labs(title = 'Multiple time series plot',
       subtitle = 'Tesla and Apple stocks',
       y="Adjusted closing price", x="time") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 5. Diverging ar chart of comparing to average

# compute the mean of the variable 'var3' for each 'subcategory' group
dataset_2 <- dataset %>% 
  mutate(Mean_TSLA = mean(TSLA.Adjusted)) %>%
  mutate(Diff_TSLA = TSLA.Adjusted - Mean_TSLA) %>%
  mutate(Mean_AAPL = mean(AAPL.Adjusted)) %>%
  mutate(Diff_AAPL = AAPL.Adjusted - Mean_AAPL)

p1 <- ggplot(dataset_2, aes(x = dates, y = Diff_TSLA)) +
  geom_bar(stat='identity', width=.5, aes(fill=Diff_TSLA),
           show.legend = TRUE) +
  scale_fill_continuous(name="Difference") +
  labs(title = 'Diverging time series bar plot',
       subtitle = 'Tesla',
       y="difference", x="time") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

p2 <- ggplot(dataset_2, aes(x = dates, y = Diff_AAPL)) +
  geom_bar(stat='identity', width=.5, aes(fill=Diff_AAPL),
           show.legend = TRUE) +
  scale_fill_continuous(name="Difference") +
  labs(title = 'Diverging time series bar plot',
       subtitle = 'Apple',
       y="difference", x="time") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

final.plot <- grid.arrange(p1, p2, nrow = 2)


#----
# end
#----

#---------------------------------------------
# Plot time data with labels above a threshold
#---------------------------------------------

# 1. create variables

set.seed(2023)
x <- seq(from = as.Date("2011-12-30"), to = as.Date("2011-12-30") + 99, by="days")
y <- abs(rt(n = 100, df = 1, ncp = 4))
group <- rep(c('a', 'b', 'c', 'd', 'e'), 20)
ID <- 1:100

# 2. Create dataset in the form of a data frame

dataset <- data.frame(x, y, group, ID)

# 3. Create plot

ggplot(dataset, aes(x = x, y = y, color = group))+
  geom_point() + 
  scale_colour_discrete(l = 50) +                                                           # change the color tone
  geom_hline(yintercept = mean(y), linetype="dashed", color = 'black') +                    # add horizontal line
  geom_text(aes(label = ID), dataset %>% filter(y>mean(y)), 
            show_guide  = FALSE, vjust = -0.6, nudge_y = 1.2) +                             # add ID if point > criterion                                                       # fixed legend label
  scale_x_date(date_labels = "%Y %b %d", date_breaks = "7 day") +                           # fix x-axis labels
  labs(title = 'Time data with labels above average',
       subtitle = 'variable y by month colored by group label, on artificial dataset',
       y="value", x="day") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        axis.text.x=element_text(angle=40, hjust=1),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----

#----------------
# Sparklies plots
#----------------

set.seed(2023)
year = 1986:2023
variable1 = rpois(n = length(year), lambda = 4)
variable2 = rpois(n = length(year), lambda = 6)
variable3 = rpois(n = length(year), lambda = 3.6)

dataset <- data.frame('year' = year, 'v1' = variable1,
                      'v2' = variable2, 'v3' = variable3)

dataset <- melt(dataset, id="year")

ggplot(dataset, aes(x=year, y = value)) + 
  facet_grid(variable ~ ., scales = "free_y") + 
  geom_line(size=0.3) + 
  geom_point() + 
  scale_color_manual(values = c(NA, "red"), guide=F) +
  labs(title = 'Sparklines plot with facet_grid',
       subtitle = 'artificial dataset',
       y="value", x="year") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----

#----------------------
# Circular stacked plot
#----------------------

set.seed(2023) # for reproducibility
var1 <- c(rpois(300,2), rpois(300,4), rpois(300,10))
var2 <- c(rpois(300,4), rpois(300,2), rpois(300,1))
var3 <- c(rpois(300,2), rpois(300,5), rpois(300,10))
dates <- seq(as.Date("2019-01-01"), as.Date("2021-06-18"), by="days")
length(dates)

dataset <- data.frame(var1, var2, var3, dates)

# group by months
dataset2 <- dataset %>% 
  mutate(month = format(dates, "%m"), year = format(dates, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total1 = sum(var1), total2 = sum(var2), total3 = sum(var3))

tail(dataset2)

dataset22019 <- dataset2 %>% subset(year == 2019)
dataset22020 <- dataset2 %>% subset(year == 2020)

# transform data from wide to long using melt() from reshape2
data_long_2019 <- melt(dataset22019, id.vars=c("month", "year"))
data_long_2019$month <- data_long_2019[,1]

data_long_2020 <- melt(dataset22020, id.vars=c("month", "year"))
data_long_2020$month <- data_long_2020[,1]

# stacked bar plot
p1 <- ggplot(data_long_2019, aes(fill=variable, y=value, x=month)) + 
  geom_bar(position="stack", stat="identity") +
  labs(caption = "Artificial dataset 2019") +
  scale_fill_brewer() +
  coord_polar() +
  labs(title = 'Circular stacked bar plot 1',
       subtitle = 'Variable x Month on artificial dataset',
       y="Variable", x="Month",
       caption = "Artificial dataset 2019") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

p2 <- ggplot(data_long_2020, aes(fill=variable, y=value, x=month)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer() +
  coord_polar() +
  labs(title = 'Circular stacked bar plot 2',
       subtitle = 'Variable x Month on artificial dataset',
       y="Variable", x="Month",
       caption = "Artificial dataset 2020") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

grid.arrange(p1, p2, nrow = 1)

#---------------------------------------------
# Plot time data with labels above a threshold
#---------------------------------------------

# 1. create variables

set.seed(2023)
x <- seq(from = as.Date("2011-12-30"), to = as.Date("2011-12-30") + 99, by="days")
y <- abs(rt(n = 100, df = 1, ncp = 4))
group <- rep(c('a', 'b', 'c', 'd', 'e'), 20)
ID <- 1:100

# 2. Create dataset in the form of a data frame

dataset <- data.frame(x, y, group, ID)

# 3. Create plot

ggplot(dataset, aes(x = x, y = y, color = group))+
  geom_point() + 
  scale_colour_discrete(l = 50) +                                                           # change the color tone
  geom_hline(yintercept = mean(y), linetype="dashed", color = 'black') +                    # add horizontal line
  geom_text(aes(label = ID), dataset %>% filter(y>mean(y)), 
            show_guide  = FALSE, vjust = -0.6, nudge_y = 1.2) +                             # add ID if point > criterion                                                       # fixed legend label
  scale_x_date(date_labels = "%Y %b %d", date_breaks = "7 day") +                           # fix x-axis labels
  labs(title = 'Time data with labels above average',
       subtitle = 'variable y by month colored by group label, on artificial dataset',
       y="value", x="day") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        axis.text.x=element_text(angle=40, hjust=1),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----

#-----------------------------------
# Histograms with overlaying density
#-----------------------------------

data(iris)

# 1. Create first plot
p1 <- ggplot(iris, aes(x = Petal.Length)) +
  geom_histogram( color = 'black', fill = 'darkred', binwidth = 0.3) +
  labs(title = 'Histogram of Sepal Width',
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

# 2. Create thrid plot
p3 <-  ggplot(iris) +
  geom_histogram(aes(x = Petal.Length, y = stat(density)), fill = "black", binwidth = 0.1) +
  geom_density(aes(x = Petal.Length, fill = Species, colour = Species), alpha = 0.4) +
  labs(title = 'Histogram with overlying densities by Species',
       subtitle = 'iris dataset',
       y="density", x="Sepal Width") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 4. Create fourth plot
p4 <- ggplot(iris) +
  geom_density(aes(x = Petal.Length, fill = Species), alpha = 0.4) +
  labs(title = 'Densities by Species',
       subtitle = 'iris dataset',
       y="density", x="Sepal Width") +
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

#----------------------
# Overlaying histograms
#----------------------

# 1. Create first plot
p1 <- ggplot(iris, aes(x = Sepal.Length, fill = Species)) +                              # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.3, bins = 20) +
  labs(title = 'Overlaying histogram 2',
       subtitle = 'iris dataset',
       y="count", x="Sepal Length") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. Create second plot
p2 <- ggplot(iris, aes(x = Sepal.Width, fill = Species)) +                       # Draw overlaying histogram
  geom_histogram(position = "identity", alpha = 0.3, bins = 20) +
  labs(title = 'Overlaying histogram 1',
       subtitle = 'iris dataset',
       y="count", x="Sepal Width") +
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

#-------------------------------------------------
# Ridgeplots:
# Different distribution density on the same graph
# on different levels
#-------------------------------------------------

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

#------------------
# Stacked bar plots
#------------------

data(diamonds)

diamonds2 <- diamonds %>%
  group_by(cut) %>% 
  count(color) %>% 
  mutate(percentage = n/nrow(diamonds) * 100) %>% 
  rename(nobservations = n)

p1 <- ggplot(diamonds2, aes(x = cut, y = percentage, fill = color)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Stacked bar plot with percentages',
       subtitle = 'Diamonds dataset',
       y="percentage of color", x="cut") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

p2 <- ggplot(diamonds2, aes(x = cut, y = nobservations, fill = color)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(title = 'Stacked bar plot with counts',
       subtitle = 'Diamonds dataset',
       y="count of color", x="cut") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

grid.arrange(p1, p2, nrow = 2)

#----
# end
#----

#----------------------------------------------------
# Box plots and violin plots with mean color gradient
#----------------------------------------------------

iris2 <- iris %>% 
  group_by(Species) %>%
  mutate(Mean_SL = mean(Sepal.Length))

# multiple box plots
p1 <- ggplot(iris2, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(fill=Mean_SL)) +
  geom_point(aes(x = Species, y = Sepal.Length), position = 'jitter', size = 0.4) +
  scale_fill_gradient2('mean(Sepal Length)', low = "blue4",
                       mid = "white", high = "firebrick4",
                       midpoint = mean(iris2$Sepal.Length)) +
  facet_wrap(~Species, scales="free") +
  labs(title = 'Box plots for Sepal Length x Species, for each Species group',
       subtitle = "Color gradient indicate the mean of the variable 'Sepal Length'",
       caption = "irir dataset") +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=6, face="italic", color="darkred"))

# multiple violin plots
p2 <-ggplot(iris2, aes(x = Species, y = Sepal.Length)) +
  geom_violin(aes(fill=Mean_SL)) +
  geom_point(aes(x = Species, y = Sepal.Length), position = 'jitter', size = 0.4) +
  scale_fill_gradient2('mean(Sepal Length)', low = "blue4",
                       mid = "white", high = "firebrick4",
                       midpoint = mean(iris2$Sepal.Length)) +
  facet_wrap(~Species, scales="free") +
  labs(title = 'Violin plots for Sepal Length x Species, for each Species group',
       subtitle = "Color gradient indicate the mean of the variable 'Sepal Length'",
       caption = "irir dataset") +
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=6, face="italic", color="darkred"))

final.plot <- grid.arrange(p1, p2, nrow = 2)

#----
# end
#----

#-------------------------
# Summary of distributions
#-------------------------

# 1 . Summary of distributions
ggpairs(iris, ggplot2::aes(colour = Species, alpha = 0.4)) +
  labs(title = 'Summary of distributions with ggpairs',
       subtitle = 'all variables x all variables, by group, iris dataset',
       y="", x="") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----

#------------------------
# Visualization dashboard
#------------------------

data(diamonds)

# 1. scatterplot
diamond.plot <- ggplot(data=diamonds, aes(x=carat, y=price, colour = clarity))+
  geom_point(aes(size = cut))+ 
  labs(title = 'Example of Scatterplot',
       subtitle = 'diamonds dataset',
       y="price", x="carat") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 2. boxplot
diamond.bxplot <- ggplot(diamonds, aes(x = cut, y=price)) +
  geom_boxplot(aes(fill = color))+ 
  labs(title = 'Example of Boxplots',
       subtitle = 'diamonds dataset',
       y="price", x="cut") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 3. violin plot
diamond.violinplot <- ggplot(diamonds, aes(x = cut, y=price)) +
  geom_violin(aes(fill = color)) + 
  labs(title = 'Example of Violin plots',
       subtitle = 'diamonds dataset',
       y="price", x="cut") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# 4. bar plot
diamond.barpot <- ggplot(diamonds, aes(x = clarity, fill=cut)) +
  geom_bar() +
  coord_flip() +
  labs(title = 'Example of Bar plots',
       subtitle = 'diamonds dataset',
       y="count", x="cut") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

grid.arrange(diamond.plot, diamond.bxplot, diamond.violinplot, diamond.barpot,
             nrow = 2)

#----
# end
#----

#---------------------------
# Parallel coordinates plots
#---------------------------

ggparcoord(iris, columns = 1:4, groupColumn = 5, order = "anyClass",
           showPoints = TRUE, alphaLines = 0.4) + 
  labs(title = 'Parallel coordinates plots',
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

#--------------
# Pareto charts
#--------------

# taken from:https://rpubs.com/dav1d00/ggpareto

# creating a factor variable:
dataset <- rep(c(LETTERS[1:5]), c(30, 66, 6, 42, 21))

# implementing the function:
ggpareto <- function(x) {
  title <- deparse(substitute(x))
  x <- data.frame(modality = na.omit(x))
  Df <- x %>% group_by(modality) %>% summarise(frequency=n()) %>% 
    arrange(desc(frequency))
  Df$modality <- ordered(Df$modality, levels = unlist(Df$modality, use.names = F))
  Df <- Df %>% mutate(modality_int = as.integer(modality), 
                      cumfreq = cumsum(frequency), cumperc = cumfreq/nrow(x) * 100)
  nr <- nrow(Df)
  N <- sum(Df$frequency)
  Df_ticks <- data.frame(xtick0 = rep(nr +.55, 11), xtick1 = rep(nr +.59, 11), 
                         ytick = seq(0, N, N/10))
  y2 <- c("  0%", " 10%", " 20%", " 30%", " 40%", " 50%", " 60%", " 70%", " 80%", " 90%", "100%")
  
  library(ggplot2)
  
  g <- ggplot(Df, aes(x=modality, y=frequency)) + 
    geom_bar(stat="identity", aes(fill = modality_int)) +
    geom_line(aes(x=modality_int, y = cumfreq, color = modality_int)) +
    geom_point(aes(x=modality_int, y = cumfreq, color = modality_int), pch = 19) +
    scale_y_continuous(breaks=seq(0, N, N/10), limits=c(-.02 * N, N * 1.02)) + 
    scale_x_discrete(breaks = Df$modality) +
    guides(fill = FALSE, color = FALSE) + 
    annotate("rect", xmin = nr + .55, xmax = nr + 1, 
             ymin = -.02 * N, ymax = N * 1.02, fill = "white") +
    annotate("text", x = nr + .8, y = seq(0, N, N/10), label = y2, size = 3.5) +
    geom_segment(x = nr + .55, xend = nr + .55, y = -.02 * N, yend = N * 1.02, color = "grey50") +
    geom_segment(data = Df_ticks, aes(x = xtick0, y = ytick, xend = xtick1, yend = ytick)) +
    labs(title = 'Pareto plot (frequency and percentage by group)',
         subtitle = 'artificial dataset',
         y="Frequency", x="Group") +
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8),
          plot.subtitle=element_text(size=10, face="italic", color="darkred"),
          panel.background = element_rect(fill = "white", colour = "grey50"),
          panel.grid.major = element_line(colour = "grey90"))
  return(list(graph = g, Df = Df[, c(3, 1, 2, 4, 5)]))
}

# applying the function to the factor variable:
ggpareto(dataset)


#----
# end
#----

#-------------
# Mosaic plots
#-------------

# using diamonds dataset for illustration
df <- diamonds %>%
  group_by(cut, clarity) %>%
  summarise(count = n()) %>%
  mutate(cut.count = sum(count),
         prop = count/sum(count)) %>%
  ungroup()

ggplot(df, aes(x = cut, y = prop, width = cut.count, fill = clarity)) +
  geom_bar(stat = "identity", position = "fill", colour = "black") +
  facet_grid(~cut, scales = "free_x", space = "free_x") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(title = 'Mosaic plot of frequency for two nomial or categorical variables',
       subtitle = 'Diamonds dataset',
       y="features", x="correlation") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#----
# end
#----

#----------------------
# Plotting correlations
#----------------------

# 1. Download dataset
dataset <- read.csv('C:/Users/julia/OneDrive/Desktop/github/11. Customer_churn_analysis/Telco-Customer-Churn.csv', na.strings = c('','?'))
dataset$ChurnDummy <- as.factor(ifelse(dataset$Churn == 'Yes', 1, 0))
dataset = na.omit(dataset) # removing missing values

# recapitulation of training set with separation between predictors and outcomes
rec_obj <- dataset %>%
  recipe(Churn ~ .) %>%
  step_rm(customerID) %>%
  step_naomit(all_outcomes(), all_predictors()) %>%
  step_discretize(tenure, options = list(cuts = 6)) %>%
  step_log(TotalCharges) %>%
  step_mutate(Churn = ifelse(Churn == "Yes", 1, 0)) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()

summary(rec_obj)
print(summary(rec_obj), n = 36)

# design matrix of predictors and vector of outcomes as numeric
features_train_tbl <- juice(rec_obj, all_predictors(), composition = "matrix") 
response_train_vec <- juice(rec_obj, all_outcomes()) %>% pull()

# analysis of correlations
corrr_analysis <- features_train_tbl %>%
  as_tibble() %>%
  mutate(Churn = response_train_vec) %>%
  correlate() %>%
  focus(Churn) 

# positive and negative correlated predictors
pos <- corrr_analysis %>%
  filter(Churn > 0)

neg <- corrr_analysis %>%
  filter(Churn < 0)

# plot
ggplot(corrr_analysis, aes(x = Churn, y = fct_reorder(term, desc(Churn)))) +
  geom_point() +
  geom_segment(aes(xend = 0, yend = term), data = under, color = 'darkred') +
  geom_point(data = neg, color = 'darkred') +
  geom_segment(aes(xend = 0, yend = term), data = over, color = "darkblue") +
  geom_point(data = pos, color = "darkblue") +
  labs(title = 'Plotting correlations of features to a response (even nominal)',
       subtitle = 'Telco dataset',
       y="features", x="correlation") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# corelogram

library(corrplot)

par(mfrow = c(1,2))
corrplot(cor(iris[,1:4]), method="pie",
         main = '')
corrplot(cor(iris[,1:4]), method="number",
         main = '')

#----
# end
#----

#-----------------------------------------------------
# Plotting odds ratios from binary logistic regression
#-----------------------------------------------------

# 1. Download dataset
dataset <- read.csv('C:/Users/julia/OneDrive/Desktop/github/11. Customer_churn_analysis/Telco-Customer-Churn.csv', na.strings = c('','?'))
dataset$ChurnDummy <- as.factor(ifelse(dataset$Churn == 'Yes', 1, 0))
dataset = na.omit(dataset) # removing missing values

model.1.lr <- glm(formula = ChurnDummy  ~ gender + SeniorCitizen + Partner + Dependents + 
                    tenure + PhoneService +  MultipleLines +
                    InternetService +OnlineSecurity + OnlineBackup + 
                    DeviceProtection + TechSupport + StreamingTV +
                    StreamingMovies + Contract + PaperlessBilling + 
                    PaymentMethod + MonthlyCharges + TotalCharges,               
                  data = dataset, family = "binomial")

summary <- summary(model.1.lr)

# export the results in LaTex document
print(xtable(summary$coefficients, type = "latex"), file = "Customer_churn_analysis_tables.tex")

# Confidence intervals
exp(confint(model.1.lr))

# plot coefficients odds ratio 
plot_model(model.1.lr, vline.color = "red",
           sort.est = TRUE, show.values = TRUE) +
  labs(title = 'Plotting odds ratios - Binary Logistic regression',
       subtitle = 'Telco dataset',
       y="features", x="Odds ratio") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90")) 

#----
# end
#----


#----------------------
# Ploting likert scales
#----------------------

library(likert)
data("pisaitems")

# capture Likert data by group
data <- likert(pisaitems[,2:6], grouping=pisaitems$CNT) 

# plot
plot(data) +
  labs(title = 'Plotting likert scales',
       subtitle = 'pisaitems dataset',
       y="group", x="percentage") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90")) 


#----
# end
#----

#-------------------------------------
# Plotting Markowitz tangent portfolio 
#-------------------------------------

library(quantmod)
library(PerformanceAnalytics)

retrieve stock prices from Yahoo finance
# Tesla, Inc.
TSLA <- getSymbols("TSLA", src = "yahoo", from = "2020-01-01", to = "2022-06-01", 
                   auto.assign = FALSE)
# Apple Inc.
AAPL <- getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = "2022-06-01",
                   auto.assign = FALSE)
# Meta Platforms, Inc.
META <- getSymbols("META", src = "yahoo", from = "2020-01-01", to = "2022-06-01", 
                   auto.assign = FALSE)
# Amazon.com, Inc.
AMZN <- getSymbols("AMZN", src = "yahoo", from = "2020-01-01", to = "2022-06-01",
                   auto.assign = FALSE)
# time series plot of the different stocks
plot(TSLA$TSLA.Adjusted, main = 'Stock prices ...')
lines(AAPL$AAPL.Adjusted, col = 'darkred')
lines(META$META.Adjusted, col = 'darkblue')
lines(AMZN$AMZN.Adjusted, col = 'darkgreen')

# daily log returns
TSLAreturns = Return.calculate(TSLA[,6],method="log")
TSLAreturns = TSLAreturns[(-1)]
AAPLreturns = Return.calculate(AAPL[,6],method="log")
AAPLreturns = AAPLreturns[(-1)]
METAreturns = Return.calculate(META[,6],method="log")
METAreturns = METAreturns[(-1)]
AMZNreturns = Return.calculate(AMZN[,6],method="log")
AMZNreturns = AMZNreturns[(-1)]

returns<- data.frame(cbind(TSLAreturns, AAPLreturns, METAreturns, AMZNreturns))

# mean returns
mean.returns <- as.numeric(colMeans(returns))
# anualized risk (stadard deviation) of returns
cov.returns.anualized <- cov(returns) * 252
# simulations
nsim <- 10000
# storage objects
Aweights <- matrix(rep(0, nsim*4), nrow = nsim, ncol = 4)
Returns.Port <- numeric(nsim)
Risk.Port <- numeric(nsim)
weights <- numeric(4)
Weights <-matrix(rep(0, nsim*4), nrow = nsim, ncol = 4)

set.seed(2023)
for(i in 1:nsim) {
  weights <- runif(4)
  sweights <- sum(weights)
  Weights[i, ] <- weights/sweights
  # Portfolio return
  returns.Port <- sum((weights/sweights) * mean.returns)
  Returns.Port[i] <- ((returns.Port + 1)^252) - 1
  # Rortfolio risk
  Risk.Port[i] <- sqrt(t((weights/sweights)) %*% (cov.returns.anualized  
                                                  %*% (weights/sweights)))
}
Portfolios <- matrix(cbind(Weights,Returns.Port,Risk.Port), 
                     byrow = FALSE, ncol = 6)
colnames(Portfolios) <- c("TESLA", "APPLE", "META", "AMZN", "Return", "Risk")
head(round(Portfolios,4))

# tangent portfolio (maximizing mean return over risk)
Portfolios <- data.frame(Portfolios)

p1<-  ggplot(Portfolios, aes(x = Risk, y = Return, color = Risk)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  scale_colour_gradient2() +
  geom_point(aes(x = Risk,
                 y = Return), data = Portfolios[which.max(mean(Portfolios$Return)/Portfolios$Risk), ], 
             color = 'black', size = 3) +
  annotate('text', x = 0.39, y = 0.7, label = "Portfolio selection (black)") +
  labs(title = 'Tangent Portfolio - Maximizing the Sharpe ratio',
       subtitle = 'Portfolio of 4 stocks retrieved from Yahoo finance',
       y="Annualized Returns", x="Annualized Risk (standard deviation)") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#----
# end
#----

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

#------------------------------------------
# Plotting multiple densities on same graph
# with different colors
#------------------------------------------

# priors
alpha1 = 2; beta1 = 2
lambda <- seq(0,10,length=1000)
prior1 = lambda^(alpha1 - 1) * exp(- lambda * beta1) ; prior1 = prior1 / sum(prior1)

# data
n = 20; lambda1 = 1 # Poisson likelihood parameters

# likelihood
likelihood1 = dpois(lambda1, lambda)

# posterior
lp1 = likelihood1 * prior1 ; posterior1 = lp1 / sum(lp1)

set.seed(2023)
data1 = rpois(n = n, lambda = lambda1)
meandata1 = mean(data1) # [1] 0.85
alpha_posterior = round(alpha1 + n*mean(data1), 2) # 19
beta_posterior = n + beta1 # 22

# dataframe
data_frame <- data.frame('lambda' = lambda, 'prior' = prior1, 'likelihood' = likelihood1, 'posterior' = posterior1)

ggplot(data=data_frame) +
  geom_line(aes(x = lambda, y = prior1, color = 'Ga(2,2) prior'), lwd = 1.2) +
  geom_line(aes(x = lambda, y = likelihood1/sum(likelihood1), 
                color = 'Scaled likelihood P(1)'), lwd = 1.2) +
  geom_line(aes(x = lambda, y = posterior1, color = 'Ga(19,22) posterior'), lwd = 1.2) +
  xlim(0, 10) + ylim(0, max(c(prior1, posterior1, likelihood1 / sum(likelihood1)))) +
  scale_color_manual(name = "Distributions", values = c("Ga(2,2) prior" = "darkred", 
                                                        "Scaled likelihood P(1)" = "black",
                                                        'Ga(19,22) posterior' = 'darkblue')) +
  labs(title = 'Multiple densities with different colors',
       subtitle = 'artificial data',
       y="density", x="parameter") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----

#---------------------------------------------
# Distribution with colored areas and segments
#---------------------------------------------

# 1. Create artificial data
set.seed(2023)

tibble(dataset = rnorm(100000, 0, 1)) |> 
  ggplot(aes(dataset)) +
  as_reference(geom_density(adjust = 2, fill = "white"), id = "density") +
  with_blend(annotate("rect", xmin = c(-1.96,1.96), xmax = c(-Inf, Inf), ymin = -Inf, ymax = Inf,
                      fill = "black"), bg_layer = "density", blend_type = "atop") +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 0.389)) +
  labs(title = 'Distribution with colored or shaded areas and segments',
       subtitle = 'Standard Normal artificial dataset',
       y="Mean value", x="n") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----

#----------------------------------
# plot convergence of MC estimators
#----------------------------------

set.seed(2023)
n <- 10000
theta.hat <- se <- numeric(2)
h <- function(x) {exp(-x)/(1+x^2)}  # function to integrate

# crude MC estimation
set.seed(1)
x1 <- runif(n)
set.seed(2)
x2 <- runif(n)
set.seed(3)
x3 <- runif(n)
set.seed(4)
x4 <- runif(n)
set.seed(5)
x5 <- runif(n)

fg_1 <- h(x1)
fg_2 <- h(x2)
fg_3 <- h(x3)
fg_4 <- h(x4)
fg_5 <- h(x5)

# dataframe
dataset <- data.frame('n' = 1:length(fg_1), 'x1' = fg_1, 'x2' = fg_2, 
                       'x3' = fg_3, 'x4' = fg_4, 'x5' = fg_5)

# plot 
ggplot(dataset, aes( x = n)) +
  geom_line( aes(y = cumsum(x1)/(1:length(x1))), color = 'black') +
  geom_line(aes(y = cumsum(x2)/(1:length(x2))), color = 'black') +
  geom_line(aes(y = cumsum(x3)/(1:length(x3))), color = 'black') +
  geom_line(aes(y = cumsum(x4)/(1:length(x4))), color = 'black') +
  geom_line(aes(y = cumsum(x5)/(1:length(x5))), color = 'black') +
  ylim(0.45 , 0.6) +
  labs(title = 'Plot of the convergence of MC estimates',
       subtitle = 'artificial dataset',
       y="Mean value", x="n") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

#----
# end
#----

