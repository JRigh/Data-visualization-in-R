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

![composite1](/assets/composite1.png)

Plot multiples subplots (eg. mixed of numerical and categorical/nominal variables

![dashboard1](/assets/dashboard1.png)

Plot of histogram with kernel density estimator (eg. numerical by group)

![histdensities](/assets/histdensities.png)

3. Statistics

Convergence of MC estimators (eg. numerical x integer sequence)

![convergence](/assets/convergence.png)

![histdensities](/assets/histdensities.png)

 histdensities.png
Add files via upload
March 24, 2023 17:17
histdensity.png
Add files via upload
March 24, 2023 17:17
likertplot.png
Add files via upload
March 24, 2023 17:17
likertplots.png
Add files via upload
March 24, 2023 17:17
mosaicplot.png
Add files via upload
March 24, 2023 17:17
mtdcolors.png
Add files via upload
March 24, 2023 17:17
multipletheoreticaldensities.png
Add files via upload
March 24, 2023 17:17
oddsratios.png
Add files via upload
March 24, 2023 17:17
overlayinghistograms.png
Add files via upload
March 24, 2023 17:17
paracoord.png
Add files via upload
March 24, 2023 17:17
pareto.png
Add files via upload
March 24, 2023 17:17
ridgeplot.png
Add files via upload
March 24, 2023 17:17
scatterplotdiamonds1000.png
Add files via upload
March 24, 2023 17:17
scatterplotdifreglines.png
Add files via upload
March 24, 2023 17:17
scatterplotwithdensities.png 

And much more.
