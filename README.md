# spain-forecast
Spain's main economic aggregates forecasted using ARIMA




```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(eurostat)
library(forecast)
library(GGally)
```

```{r message=FALSE, warning=FALSE}
# Download the dataset & label the data
#data <- get_eurostat("namq_10_gdp", time_format = "num")
#data <- label_eurostat(data)
```

```{r}
# Save the data
#saveRDS(data, file = "/Users/Gabriel Pierobon/Documents/Publicaciones/Eurostat/data.RDS")
```

```{r}
# Load the dataset
data <- readRDS("/Users/Gabriel Pierobon/Documents/Publicaciones/Eurostat/data.RDS")
```


```{r}
# Filter the quarterly EU19 GDP dataset
data <- data %>% 
        mutate(geo = as.character(geo)) %>% 
          filter(unit == "Current prices, million euro",
                 s_adj == "Unadjusted data (i.e. neither seasonally adjusted nor calendar adjusted data)",
                 geo == "Spain") %>% 
                    arrange(time) %>% 
                        spread(na_item, values)

data$unit <- NULL
data$s_adj <- NULL
data$geo <- NULL
```

```{r}
data <- data[, c(1, 3, 7, 18, 19, 22, 31)]

names(data)[2]<-paste("GDP")
names(data)[3]<-paste("Consumption")
names(data)[4]<-paste("Gross Capital")
names(data)[5]<-paste("Exports")
names(data)[6]<-paste("Imports")
names(data)[7]<-paste("Salaries")

names(data)
```

```{r}
data <- ts(data[, -1], start = c(1995, 1), frequency = 4)
head(data, 20)
```

```{r}
autoplot(data, facets = TRUE) +
  labs(title = "Time Series plot of Spain economic indicators",
       subtitle = "1995-2018 / In millions of euros (€)",
       y = "millions of euros (€)",
       x = "Period")
```

```{r message=FALSE, warning=FALSE}
data %>% as.data.frame() %>% ggpairs(title = "Pairs Plot")
```

```{r}
autoplot(data[, "GDP"]) +
 labs(title = "Spain's GDP evolution", 
      subtitle = "1995-2018 / In millions of euros (€)", 
      y ="GDP (€) in millions",
      x = "Period")A
```

```{r}
ggsubseriesplot(data[, "GDP"]) +
  labs(title = "Subseries Plot: observe average seasonality for all years",
       subtitle = "Spain's quarterly GDP",
       y = "GDP (€) in millions")
```

```{r}
ggseasonplot(data[, "GDP"], year.labels = TRUE, year.labels.left = TRUE) +
  labs(title = "Subseries Plot: observe seasonality in each year",
       subtitle = "Spain's quarterly GDP",
       y = "GDP (€) in millions")+
  theme_classic()
```

```{r}
ggAcf(data[, "GDP"]) +
  ggtitle("Autocorrlation function: GDP")
```

```{r}
train_set <- window(data, end = c(2016, 4))
```

```{r}
arima_train <- auto.arima(train_set[, "GDP"], 
                          trace = FALSE, 
                          ic = "aicc", 
                          approximation = FALSE,
                          stepwise = FALSE,
                          lambda = "auto")
arima_train
```

```{r message=FALSE, warning=FALSE}
checkresiduals(arima_train)
```

```{r}
round(accuracy(forecast(arima_train, h = 5), data[, "GDP"]), 3)
```

```{r}
arima_full <- auto.arima(data[, "GDP"],
                         trace = FALSE, 
                         ic = "aicc", 
                         approximation = FALSE,
                         stepwise = FALSE,
                         lambda = "auto")
arima_full
```

```{r message=FALSE, warning=FALSE}
options(scipen = 999)

arima_full %>% forecast(h = 8) %>% autoplot() +
  labs(title = "Spain's quarterly GDP forecast: ARIMA modelling",
       subtitle = "In millions of euros (€), 2018-19",
       y = "GDP (€)")+
  scale_x_continuous(breaks = seq(1995, 2020, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

```{r}
gdp_fcst <- as.data.frame(forecast(arima_full, h = 8))
gdp_fcst$Indicator <- "GDP"
gdp_fcst$Period <- rownames(gdp_fcst)
gdp_fcst
```

```{r message=FALSE, warning=FALSE}
# Consumption
consumption_model <- auto.arima(data[, "Consumption"], 
                                trace = FALSE, 
                                ic = "aicc", 
                                approximation = FALSE,
                                stepwise = FALSE,
                                lambda = "auto")

consumption_model %>% 
  forecast(h = 8) %>% 
  autoplot() +
  labs(title = "Spain's quarterly Consumption forecast: ARIMA modelling",
       subtitle = "In million euro (€), for years 2018-19",
       y = "GDP (€)")+
  scale_x_continuous(breaks = seq(1995, 2020, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


con_fcst <- as.data.frame(forecast(consumption_model, h = 8))
con_fcst$Indicator <- "Consumption"
con_fcst$Period <- rownames(con_fcst)
```

```{r message=FALSE, warning=FALSE}
# Gross Capital
Gross_Capital_model <- auto.arima(data[, "Gross Capital"],
                                  trace = FALSE, 
                                  ic = "aicc", 
                                  approximation = FALSE,
                                  stepwise = FALSE,
                                  lambda = "auto")

Gross_Capital_model %>% 
  forecast(h = 8) %>% 
  autoplot() +
  labs(title = "Spain's quarterly Gross Capital forecast: ARIMA modelling",
       subtitle = "In million euro (€), for years 2018-19",
       y = "GDP (€)")+
  scale_x_continuous(breaks = seq(1995, 2020, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cap_fcst <- as.data.frame(forecast(Gross_Capital_model, h = 8))
cap_fcst$Indicator <- "Gross Capital"
cap_fcst$Period <- rownames(cap_fcst)
```

```{r message=FALSE, warning=FALSE}
# Exports
exports_model <- auto.arima(data[, "Exports"],
                            trace = FALSE, 
                            ic = "aicc", 
                            approximation = FALSE,
                            stepwise = FALSE,
                            lambda = "auto")

exports_model %>% 
  forecast(h = 8) %>% 
  autoplot() +
  labs(title = "Spain's quarterly Exports forecast: ARIMA modelling",
       subtitle = "In million euro (€), for years 2018-19",
       y = "GDP (€)")+
  scale_x_continuous(breaks = seq(1995, 2020, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

exp_fcst <- as.data.frame(forecast(exports_model, h = 8))
exp_fcst$Indicator <- "Exports"
exp_fcst$Period <- rownames(exp_fcst)
```

```{r message=FALSE, warning=FALSE}
# Imports
imports_model <- auto.arima(data[, "Imports"],
                            trace = FALSE, 
                            ic = "aicc", 
                            approximation = FALSE,
                            stepwise = FALSE,
                            lambda = "auto")

imports_model %>% 
  forecast(h = 8) %>% 
  autoplot() +
  labs(title = "Spain's quarterly Imports forecast: ARIMA modelling",
       subtitle = "In million euro (€), for years 2018-19",
       y = "GDP (€)")+
  scale_x_continuous(breaks = seq(1995, 2020, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

imp_fcst <- as.data.frame(forecast(imports_model, h = 8))
imp_fcst$Indicator <- "Imports"
imp_fcst$Period <- rownames(imp_fcst)
```

```{r message=FALSE, warning=FALSE}
# Salaries
salaries_model <- auto.arima(data[, "Salaries"], 
                             trace = FALSE, 
                             ic = "aicc", 
                             approximation = FALSE,
                             stepwise = FALSE,
                             lambda = "auto")

salaries_model %>% 
  forecast(h = 8) %>% 
  autoplot() +
  labs(title = "Spain's quarterly Salaries forecast: ARIMA modelling",
       subtitle = "In million euro (€), for years 2018-19",
       y = "GDP (€)")+
  scale_x_continuous(breaks = seq(1995, 2020, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sal_fcst <- as.data.frame(forecast(salaries_model, h = 8))
sal_fcst$Indicator <- "Salaries"
sal_fcst$Period <- rownames(sal_fcst)
```

```{r}
predictions <- rbind(gdp_fcst, con_fcst, cap_fcst, exp_fcst, imp_fcst, sal_fcst) %>% select(7,6,2,4,1,3,5)
rownames(predictions) <- c()
predictions
```




