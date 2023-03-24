# Data-visualization-in-R
Some elements of data visualization with R 'ggplot2' mostly

Examples include scatterplots, histograms, kernel density estimators (KDE), time series plots, bar plots, diverging bar plots, pareto plots...
It also includes code for multiple plots on the same window. Here is an excerpt of some of the visualizations taken as examples.

## Examples

1. Plots of time data

Time series plot (numerical x time variable)

```r
ggplot(dataset, aes(x = dates, y = value)) +
  geom_line() + 
  customization
```

![timeseries1](/assets/timeseries1.png)

Diverging bar plot (eg. numerical x time variable)

```r
ggplot(dataset, aes(x = dates, y = value)) +
  geom_bar() + 
  customization
```

![divergingbarplot](/assets/divergingbarplot.png)

Plot with values above a threshold (eg. numerical x time variable)

```r
ggplot(dataset, aes(x = dates, y = value)) +
  geom_point() +                                                                         
  geom_text(aes(label = ID), dataset %>% filter(y>mean(y)), show_guide  = FALSE) +                           
  customization
```

![timeseries4](/assets/timeseries4.png)

2. Multiple plots

Scatterplot with marginal densities and regression lines by group (eg. numerical x numerical, by group)

```r
library(cowplot)
```

![composite1](/assets/composite1.png)

Scatterplot with different pointsize

```r
ggplot(data=dataset, aes(x=variable1, y=variable2, colour = group))+
  geom_point(aes(size = variable3)) +
  customization
```

![scatterplotdiamonds1000](/assets/scatterplotdiamonds1000.png)

Plot multiples subplots (eg. mixed of numerical and categorical/nominal variables

```r
p1 = ggplot(data=dataset) +
  customization
  
p2 = ggplot(data=dataset) +
  customization

grid.arrange(p1, p2)
```

![dashboard1](/assets/dashboard1.png)

Plot of histogram with kernel density estimator (eg. numerical by group)

```r
ggplot(dataset) +
  geom_histogram() +
  geom_density() +
  customization
```

![histdensities](/assets/histdensities.png)

Ridge plots (eg. numerical by group)

```r
ggplot(iris, aes(x = value, y = group, fill = group)) +
  geom_density_ridges()
```

![ridgeplot](/assets/ridgeplot.png)

Plot joint distribution of two categorical or nominal variages (eg. nominal x categorical)

```r
ggplot(dataset, aes(x = variable1, y = prop, width = cut.count, fill = variable2)) +
  geom_bar() +
  facet_grid(~ variable1, scales = "free_x", space = "free_x") +
  customization
```

![mosaicplot](/assets/mosaicplot.png)

3. Statistics 

Plot a distribution with colored or shaded area

```r
ggplot(aes(dataset)) +
  as_reference(geom_density(), id = "density") +
  with_blend(annotate("), bg_layer = "density", blend_type = "atop") +
  customization
```

![distribution3](/assets/distribution3.png)

Plot multiple theoretical distributions on the same plot (eg. three Normal densities)

```r
ggplot(data=dataset) +
  stat_function(fun=dnorm, args=list(mean = 0, sd = 1)) +
  customization
```

![multipletheoreticaldensities](/assets/multipletheoreticaldensities.png)


Convergence of MC estimators (eg. numerical x integer sequence)

```r
ggplot(dataset, aes( x = seq)) +
  geom_line( aes(y = cumsum(x1)/(1:length(x1)))) +
  customization
```

![convergence](/assets/convergence.png)




And much more.
