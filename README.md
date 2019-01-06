# PrettyPlot

_Prettier base plots in R_.

## Examples

Load the package.

```R
source("prettyplot.R")
```

### Scatter plots

```R
plot(cars)
```

|       PrettyPlot        |           Base            |
| :---------------------: | :-----------------------: |
| ![](examples/sp-pp.png) | ![](examples/sp-base.png) |

### Histograms

```R
hist(rnorm(1000))
```

|        PrettyPlot         |            Base             |
| :-----------------------: | :-------------------------: |
| ![](examples/hist-pp.png) | ![](examples/hist-base.png) |

### Boxplots

```R
with(ChickWeight,
     boxplot(weight ~ Time)
)
```

|       PrettyPlot        |           Base            |
| :---------------------: | :-----------------------: |
| ![](examples/bp-pp.png) | ![](examples/bp-base.png) |

### Labelling axes

```R
load("ozone.RData")
df = ozone[, c("O3", "T12", "Ne12", "Vx")]
m = apply(df, 2, mean)
with(df, 
     plot(O3 ~ T12, xmark=m["T12"], ymark=m["O3"])
)
axelines(m["T12"], m["O3"], col=2)
```

<img src="examples/labels.png" width="400">

### Correlation matrices

```R
load("ozone.RData")
df = ozone[, c("O3", "T12", "Ne12", "Vx")]
cor.im(df)
```

<img src="./examples/cor.png" width="400">
                                                    
