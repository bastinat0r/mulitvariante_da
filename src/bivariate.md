# Bivariate Analysen

## Univariate
### When travelling within cities, how often do you encounter problems that limit your access to activities, goods or services?
```{r, echo=TRUE, warnings=TRUE}
freq(cities$qd3, plot=T, main="When travelling within cities, how often do you encounter problems that limit your access to activities, goods or services?")
```
### Road congestion
```{r, echo=TRUE, warnings=TRUE}
freq(cities$qd4_1, plot=T)
```
### Noise pollution
```{r, echo=TRUE, warnings=TRUE}
freq(cities$qd4_2, plot=T)
```
### Air pollution
```{r, echo=TRUE, warnings=TRUE}
freq(cities$qd4_3, plot=T)
```
### Accidents
```{r, echo=TRUE, warnings=TRUE}
freq(cities$qd4_4, plot=T)
```
### Travelling costs
```{r, echo=TRUE, warnings=TRUE}
freq(cities$qd4_5, plot=T)
```

## Demographie vs. Mobilität

### Geschlecht ...
```{r, echo=TRUE, warnings=TRUE}
comp <- cities$d10
# Probleme beim Reisen
bivariateComp(cities$qd3, comp)
# Stau
bivariateComp(cities$qd4_1, comp)
# Lärm
bivariateComp(cities$qd4_2, comp)
# Luft
bivariateComp(cities$qd4_3, comp)
# Unfälle
bivariateComp(cities$qd4_4, comp)
# Kosten
bivariateComp(cities$qd4_5, comp)
```

### Alter ...
```{r, echo=TRUE, warnings=TRUE}
comp <- cities$d11
# Probleme beim Reisen
bivariateComp(cities$qd3, comp)
# Stau
bivariateComp(cities$qd4_1, comp)
# Lärm
bivariateComp(cities$qd4_2, comp)
# Luft
bivariateComp(cities$qd4_3, comp)
# Unfälle
bivariateComp(cities$qd4_4, comp)
# Kosten
bivariateComp(cities$qd4_5, comp)
```

### Geselschaftl. Stellung ...
```{r, echo=TRUE, warnings=TRUE}
comp <- cities$d61
# Probleme beim Reisen
bivariateComp(cities$qd3, comp)
# Stau
bivariateComp(cities$qd4_1, comp)
# Lärm
bivariateComp(cities$qd4_2, comp)
# Luft
bivariateComp(cities$qd4_3, comp)
# Unfälle
bivariateComp(cities$qd4_4, comp)
# Kosten
bivariateComp(cities$qd4_5, comp)
```

### Mobilitätsverhalten ...


# Multivariates Foo
## CFA-Mobilitätsverhalten

## Clusteranalyse
Mobilitätsverhalten, Alter, Selbsteinschätzung

## 
