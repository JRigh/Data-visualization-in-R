#-------------------------------
# Time series visualization in R
#-------------------------------

library(quantmod)
library(reshape2)
library(tidyverse)

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

# 1. Time series plot of one stock

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

# 2. Time series plot of the different stocks

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

# 3. Bar chart of comparing to average

library(gridExtra)

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


# 4. Plot time data with labels above a threshold

set.seed(2023)
x <- seq(from = as.Date("2011-12-30"), to = as.Date("2011-12-30") + 99, by="days")
y <- abs(rt(n = 100, df = 1, ncp = 4))
group <- rep(c('a', 'b', 'c', 'd', 'e'), 20)
ID <- 1:100

# Create dataset in the form of a data frame

dataset <- data.frame(x, y, group, ID)

# Create plot

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
