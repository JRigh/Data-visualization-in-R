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