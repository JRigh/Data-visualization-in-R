#-----------------------------------------------
# 1. create artificial dataset and data cleaning
#-----------------------------------------------

library(tidyverse)
library(lubridate)
library(reshape2)

set.seed(1986) # for reproducibility
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
data_long_2019$month <- month.abb[as.numeric(data_long_2019[,1])]

data_long_2020 <- melt(dataset22020, id.vars=c("month", "year"))
data_long_2020$month <- month.abb[as.numeric(data_long_2020[,1])]

#----------------------------------------
# 2. visualization using stacked bar plot
#----------------------------------------

library(gridExtra)

# stacked bar plot
p1 <- ggplot(data_long_2019, aes(fill=variable, y=value, x=month)) + 
  geom_bar(position="stack", stat="identity") +
  labs(caption = "Artificial dataset 2019") +
  scale_fill_brewer() +
  coord_polar() +
  labs(title = 'Circular stacked bar plot',
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
  labs(title = 'Circular stacked bar plot',
       subtitle = 'Variable x Month on artificial dataset',
       y="Variable", x="Month",
       caption = "Artificial dataset 2020") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=9, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

grid.arrange(p1, p2, nrow = 1)

#----
# end
#----
