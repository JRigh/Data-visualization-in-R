#-------------------------------
# Time series visualization in R
#-------------------------------

library(quantmod)
library(reshape2)

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

# 5. Bar chart of comparing to average

library(tidyverse)

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
