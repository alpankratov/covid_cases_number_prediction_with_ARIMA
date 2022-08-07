library(tidyverse)
library(ggplot2)
library(GGally)
library(gridExtra)
library(zoo) 
library(ggfortify)
library(forecast)
library(Metrics)

covid <- as_tibble(read_csv("data/covid19_subset.csv"))
covid <- covid %>%
  select(-geoId, -countryterritoryCode) %>%  # redundant variables as they duplicate countriesAndTerritories
  select(-day,-month,-year) %>% # redundant variables as together duplicate daterep
  mutate(countriesAndTerritories = as_factor(countriesAndTerritories),
         continentExp = as_factor(continentExp))
sort(unique(covid$dateRep)) # the data is from March 23 to May 31

# # Seperating the data into test and trining data
# Test data
covid.test <- covid %>% 
  filter(dateRep > "2020-05-15")

# Training data
covid.training <- covid %>% 
  filter(dateRep <= "2020-05-15")


# Splitting the data into information on Luxembourg, Egypt and Serbia for exploration of the data only
covid.training.EGY  <-  covid.training %>% filter(countriesAndTerritories == "Egypt")
covid.training.LUX  <-  covid.training %>% filter(countriesAndTerritories == "Luxembourg")
covid.training.SRB  <-  covid.training %>% filter(countriesAndTerritories == "Serbia")
covid.test.EGY  <-  covid.test %>% filter(countriesAndTerritories == "Egypt")
covid.test.LUX  <-  covid.test %>% filter(countriesAndTerritories == "Luxembourg")
covid.test.SRB  <-  covid.test %>% filter(countriesAndTerritories == "Serbia")
# We will explore and base our model only on the training data as though the test data (from May 15) have not existed at the date of analysis.


# Create a plot for number of deaths
covid.EGY.plot <- covid.training.EGY %>% ggplot(aes(dateRep, deaths)) + 
  geom_line(color = "#3a91bf", size = 2) + 
  ggtitle("Number of COVID-19 deaths per day in Egypt") + xlab ("Date") + ylab ("Number of daily deaths")
covid.LUX.plot <- covid.training.LUX %>% ggplot(aes(dateRep, deaths)) + 
  geom_line(color = "#503abf", size = 2) + 
  ggtitle("Number of COVID-19 deaths per day in Luxembourg") + xlab ("Date") + ylab ("Number of daily deaths")
covid.SRB.plot <- covid.training.SRB %>% ggplot(aes(dateRep, deaths)) + 
  geom_line(color = "#3abf68", size = 2) + 
  ggtitle("Number of COVID-19 deaths per day in Serbia") + xlab ("Date") + ylab ("Number of daily deaths")

# Create a plot for number of total daily cases
covid.EGY.plot.cases <- covid.training.EGY %>% ggplot(aes(dateRep, cases)) + 
  geom_line(color = "#3a91bf", size = 2) + 
  ggtitle("Number of COVID-19 cases per day in Egypt") + xlab ("Date") + ylab ("Number of daily cases")
covid.LUX.plot.cases <- covid.training.LUX %>% ggplot(aes(dateRep, cases)) + 
  geom_line(color = "#503abf", size = 2) + 
  ggtitle("Number of COVID-19 cases per day in Luxembourg") + xlab ("Date") + ylab ("Number of daily cases")
covid.SRB.plot.cases <- covid.training.SRB %>% ggplot(aes(dateRep, cases)) + 
  geom_line(color = "#3abf68", size = 2) + 
  ggtitle("Number of COVID-19 cases per day in Serbia") + xlab ("Date") + ylab ("Number of daily cases")


grid.arrange(covid.LUX.plot, covid.LUX.plot.cases, 
             covid.EGY.plot, covid.EGY.plot.cases,
             covid.SRB.plot, covid.SRB.plot.cases,
             nrow = 3)


# The data in each of the countries does not show seasonality. It is expected as the period of recording of the data is too short. 
# Regarding the trend, there is a strong evidence of the trend in the number of daily cases (plots on the right). 
# In Egypt, the number of cases is on the rise, whereas the trend is opposite for the number of daily covid-19 cases in Luxembourg. 
# In Serbia the daily cases increase up until mid-April and then starting to decrease after.
# At the same time, although plots of daily covid-19 death on the left slightly resemble those on the right, the trend there is not clearly visible. 
# In order to check if the number of deaths plot includes trend or not, we will use as autocorrelation function to see if this is the case.
# ACF plot is more robust technique of trend identification than visual observation of the daily deaths and cases plots. 
#It should be noted that autoplot function for plotting correlogram below omits the first lag that always equals to 1.

# Checking if data is stationary

grid.arrange(autoplot(acf(covid.training.LUX$deaths)) + ggtitle("ACF number of daily deaths from covid-19 in Luxembourg"),
             autoplot(acf(covid.training.LUX$cases)) + ggtitle("ACF number of daily coovid-19 cases in Luxembourg"),
             autoplot(acf(covid.training.EGY$deaths)) + ggtitle("ACF number of daily deaths from covid-19 in Egypt"),
             autoplot(acf(covid.training.EGY$cases)) + ggtitle("ACF number of daily coovid-19 cases in Egypt"),
             autoplot(acf(covid.training.SRB$deaths)) + ggtitle("ACF number of daily deaths from covid-19 in Serbia"),
             autoplot(acf(covid.training.SRB$cases)) + ggtitle("ACF number of daily coovid-19 cases in Serbia"))



# Number of daily deaths appear to be stationary and show no trend and seasonality in Luxembourg and Serbia as no lags exceed 
# the confidence interval of the ACF (blue dashed line). It looks like that the number of deaths in Egypt show patterns of 
# trend as lags on the ACF plot decrease gradually although the trend is not as strong as the trend for the number of covid-19 daily cases. 
# Looking at the plot of number death over the training data period in Egypt, it does not show any inflection points or any other 
# sign that the trend is not linear, so we will use linear trend in the model. 

# In comparison, number of daily cases in almost all lags exceed the confidence and the lags subside gradually, 
# which indicates a presence of strong trend in the number of daily cases. Qualitatively, we can see and conclude 
# from the ACFs that the number of daily deaths are stationary for Luxembourg and Serbia, considering full period 
# of training data (see below additional considerations for Serbia deaths in starting from April 24th) while deaths 
# in Egypt and daily cases in all countries are not stationary since lags exceed the confidence interval and decrease gradually, 
# which is an indication of existence of trend.

# At the same time although correlogram of Serbia time series shows that the daily deaths are stationary if to consider full 
# period of training data, the general plot shows trend downwards trend in daily deaths in Serbia starting from approximately April 24th. 
# Looking at the ACF plot of the data from this date it is clear that there strong trend exists starting from this date. 
# Considering this, we will build exponential smoothing model that would incorporate additive trend and use the smoothing factor 
# alpha closer to 1 in order to give more value to recent observations.


autoplot(acf(covid.training.SRB$deaths[27:48])) + ggtitle("ACF number of daily deaths from covid-19 in Serbia from April 24 until May 15")

# We will finalize exploration of data by checking correlation between number of daily cases and daily death. 
# It is not expected to be strong for Luxembourg and Serbia as we noted above that number of daily deaths is stationary 
# time series whereas number of daily cases present strong trend, but it may be worth it to have a closer look at the figures.

# The correlation is not strong as it was expected. It is stronger (0.676) in Egypt where the number of daily cases increases
# and where we saw trend pattern, and getting lower in Serbia (0.464) where the number of daily covid-19 infections started 
# to subside recently and is even lower in Luxembourg where the decreasing trend was present from the beginning of the period
# under analysis. However, the correlation is not strong enough for number of daily cases to be the main predictor of the number
# of daily deaths from covid-19.

as_tibble(matrix(NA, 1, 0)) %>% 
  mutate(Egypt = cor(covid.training.EGY$cases, covid.training.EGY$deaths),
         Luxembourg = cor(covid.training.LUX$cases, covid.training.LUX$deaths),
         Serbia = cor(covid.training.SRB$cases, covid.training.SRB$deaths))


# After exploring the data, we will perform time series modelling for covid-19 deaths in each country. 
# Considering that the number of daily deaths appears stationary for Luxembourg and Serbia (looking at the whole period of testing data) 
# we can build ARIMA model. We will also build exponential smoothing models without trend and seasonality for Luxembourg and Serbia 
# (based on full period of testing data) and with trend for Serbia (starting from April 24th) and Egypt. And for comparison reason we
# will also build exponential smoothing models using ets() function of forecast package in R without specification of seasonality and 
# trend so that the functions would build the model it considers the most optimal and compare it with our selected model in case they
# are different from the one we selected manually.

# After we compare forecasting accuracy of three models by plotting it and calculating root mean square error.

# We will first select the best ARIMA model for each country based on smallest AIC using the auto.arima() function from library(forecast).
auto.arima(covid.training.LUX$deaths, seasonal = FALSE, stationary = TRUE, trace = TRUE) # best model is ARIMA(0,0,0)
auto.arima(covid.training.SRB$deaths, seasonal = FALSE, stationary = TRUE, trace = TRUE) # best model is ARIMA(0,0,0)

# The data shows no short term correlation for Serbia and Luxembourg so best fit ARIMA model is ARIMA(0,0,0)
# We have already looked at ACF plots that support MA(0) model, now we will look at partial ACF plots to double check if AR(0) is appropriate as well

grid.arrange(autoplot(pacf(covid.training.LUX$deaths)) + ggtitle("PACF number of daily deaths from covid-19 in Luxembourg"),
             autoplot(pacf(covid.training.SRB$deaths)) + ggtitle("PACF number of daily deaths from covid-19 in Serbia"))
# No Lags are above confidence interval in Serbia and Luxembourg, therfore AR(o) is appropriate

# Now we will prepare models for each country:

# First for Luxembourg
# Modelling Luxembourg ----
# Model 1 - ARIMA(0,0,0) model
LUX.arima.model <- auto.arima(covid.training.LUX$deaths, seasonal = FALSE, stationary = TRUE, trace = TRUE)
LUX.arima.predict <- predict(LUX.arima.model, n.ahead = length(covid.test.LUX$dateRep), se.fit = TRUE)
LUX.arima.predict.plot <- data.frame(arima.predict = LUX.arima.predict$pred,
                                     Lo.95 = LUX.arima.predict$pred - 1.96 * LUX.arima.predict$se,
                                     Hi.95 = LUX.arima.predict$pred + 1.96 * LUX.arima.predict$se,
                                     dateRep = covid.test.LUX$dateRep)
LUX.arima.predict.plot

# Model 2 - Exponential smoothing - without trend and seasonality

LUX.exp_smoothing.model <- ets(covid.training.LUX$deaths, model = 'ANN')
LUX.exp_smoothing.predict <- forecast.ets(ets(covid.training.LUX$deaths, model = 'ANN'), h = length(covid.test.LUX$dateRep), level = 95)
LUX.exp_smoothing.predict.plot <- data.frame(LUX.exp_smoothing.predict, dateRep = covid.test.LUX$dateRep)

# Model 3 - Exponential smoothing - optimal model decided by R functions

LUX.exp_smoothing2.model <- ets(covid.training.LUX$deaths)
LUX.exp_smoothing2.predict <- forecast.ets(ets(covid.training.LUX$deaths), h = length(covid.test.LUX$dateRep), level = 95)
LUX.exp_smoothing2.predict.plot <- data.frame(LUX.exp_smoothing2.predict, dateRep = covid.test.LUX$dateRep)
plot(LUX.exp_smoothing2.predict) # the most optimal model in accrodance with ets() function is the model with additive errors and 
# trend but without seasonality

# Now we will prepare model for Egypt
# Modelling Egypt ----
# Model 1 - Exponential smoothing - optimal model decided by R functions

EGY.exp_smoothing.model <- ets(covid.training.EGY$deaths)
EGY.exp_smoothing.predict <- forecast.ets(ets(covid.training.EGY$deaths), h = length(covid.test.EGY$dateRep), level = 95)
EGY.exp_smoothing.predict.plot <- data.frame(EGY.exp_smoothing.predict, dateRep = covid.test.EGY$dateRep)
EGY.exp_smoothing.predict.plot
plot(EGY.exp_smoothing.predict) # the most optimal model in accrodance with ets() function is the model with additive errors and 
# trend but without seasonality same as we planned to build manually

# Model 2 - Linear regression model with linear trend
EGY.trend.model <- lm(deaths ~ dateRep, data=covid.training.EGY)
EGY.trend.predict <- predict(EGY.trend.model, newdata = covid.test.EGY, se.fit = TRUE)
EGY.trend.predict.plot <- data.frame(predict = EGY.trend.predict$fit,
                                     Lo.95 = EGY.trend.predict$fit - 1.96 * EGY.trend.predict$se.fit,
                                     Hi.95 = EGY.trend.predict$fit + 1.96 * EGY.trend.predict$se.fit,
                                     dateRep = covid.test.EGY$dateRep)
EGY.trend.predict.plot


# And finally models for Serbia
# Modelling Serbia ----
# Model 1 - ARIMA(0,0,0) model
SRB.arima.model <- auto.arima(covid.training.SRB$deaths, seasonal = FALSE, stationary = TRUE, trace = TRUE)
SRB.arima.predict <- predict(SRB.arima.model, n.ahead = length(covid.test.SRB$dateRep), se.fit = TRUE)
SRB.arima.predict.plot <- data.frame(arima.predict = SRB.arima.predict$pred,
                                     Lo.95 = SRB.arima.predict$pred - 1.96 * SRB.arima.predict$se,
                                     Hi.95 = SRB.arima.predict$pred + 1.96 * SRB.arima.predict$se,
                                     dateRep = covid.test.SRB$dateRep)
SRB.arima.predict.plot

# Model 2 - Exponential smoothing with trend

# First we will estimate the smoothing parameter that will be the best for the model.
# To do this, we will build multiple models and chose the model that provides the lowest RMSE in the period where the trend is sisible (from April 24) 
k <- seq(0.01, 0.99, 0.01)
m <- rep(NA, times = length(k))
for(i in 1:length(k)) {
  model = ets(covid.training.SRB$deaths, model = 'AAN', alpha = k[i])
  m[i] <- sqrt(sum((covid.training.SRB$deaths[27:48] - model$fitted[27:48])^2)/length(covid.training.SRB$deaths[27:48]))
}
model_check = data.frame(alpha = k, MSE = m)
model_check %>% filter(MSE == min(MSE))
# Accrodingly, alpha = 0.49 is the best option

SRB.exp_smoothing.model <- ets(covid.training.SRB$deaths, model = 'AAN', alpha = 0.49)
SRB.exp_smoothing.predict <- forecast.ets(ets(covid.training.SRB$deaths, model = 'AAN'), h = length(covid.test.SRB$dateRep), level = 95)
SRB.exp_smoothing.predict.plot <- data.frame(SRB.exp_smoothing.predict, dateRep = covid.test.SRB$dateRep)
SRB.exp_smoothing.predict.plot

# Model 3 - Exponential smoothing - optimal model decided by R functions

SRB.exp_smoothing2.model <- ets(covid.training.SRB$deaths)
SRB.exp_smoothing2.predict <- forecast.ets(ets(covid.training.SRB$deaths), h = length(covid.test.SRB$dateRep), level = 95)
SRB.exp_smoothing2.predict.plot <- data.frame(SRB.exp_smoothing2.predict, dateRep = covid.test.SRB$dateRep)
plot(SRB.exp_smoothing2.predict) # the most optimal model in accrodance with ets() function is the model without trend and seasonality


# After building the models, we will show the fitted values and 95% confidence interval on the plot
# and see how each model fits the test data.
colors <- c("Training data" = "#503abf", "Test data" = "#0e4d59", "Fitted values" = "#bf2348", "Confidence interval" = "#5389a6") #will be used in legend to plots

#Calculation of root meas squared error that will be plotted below
sqrt(sum((covid.test.LUX$deaths - LUX.arima.predict.plot$arima.predict)^2)/length(covid.test.LUX$deaths)) == rmse(covid.test.LUX$deaths,LUX.arima.predict.plot$arima.predict) # Checking the rmse() function logic
RMSE.LUX.MODEL_ARIMA <- round(rmse(covid.test.LUX$deaths,LUX.arima.predict.plot$arima.predict), digits = 3)
RMSE.LUX.MODEL_EXPSM1 <- round(rmse(covid.test.LUX$deaths,LUX.exp_smoothing.predict.plot$Point.Forecast), digits = 3)
RMSE.LUX.MODEL_EXPSM2 <- round(rmse(covid.test.LUX$deaths,LUX.exp_smoothing2.predict.plot$Point.Forecast), digits = 3)

RMSE.EGY.MODEL_EXPSM <- round(rmse(covid.test.EGY$deaths,EGY.exp_smoothing.predict.plot$Point.Forecast), digits = 3)
RMSE.EGY.MODEL_TREND <- round(rmse(covid.test.EGY$deaths,EGY.trend.predict.plot$predict), digits = 3)

RMSE.SRB.MODEL_ARIMA <- round(rmse(covid.test.SRB$deaths,SRB.arima.predict.plot$arima.predict), digits = 3)
RMSE.SRB.MODEL_EXPSM1 <- round(rmse(covid.test.SRB$deaths,SRB.exp_smoothing.predict.plot$Point.Forecast), digits = 3)
RMSE.SRB.MODEL_EXPSM2 <- round(rmse(covid.test.SRB$deaths,SRB.exp_smoothing2.predict.plot$Point.Forecast), digits = 3)

# Plotting Luxembourg -----
LUX.arima.ggplot <- ggplot() +
  geom_point(data = covid.training.LUX, aes(x = dateRep, y = deaths, color = "Training data" ), size = 2) +
  geom_point(data = covid.test.LUX, aes(x = dateRep, y = deaths, color = "Test data"), size = 2) +
  geom_point(data = LUX.arima.predict.plot, aes(x = dateRep, y = arima.predict, color = "Fitted values"), size = 2) + 
  geom_ribbon(data=LUX.arima.predict.plot, aes(x = dateRep, ymin=Lo.95, ymax=Hi.95, fill = "Confidence interval"), alpha=0.3) + 
  ggtitle("Covid-19 death in Luxembourg - ARIMA(0,0,0) model", subtitle = str_c("Root mean squared error is ", RMSE.LUX.MODEL_ARIMA)) + 
  labs(x = "Date",
       y = "Deaths per day",
       color = "Legend",
       fill = "") +
  scale_color_manual(values = colors) +
  theme(legend.position="bottom")

LUX.exp_smoothing.ggplot <- ggplot() +
  geom_point(data = covid.training.LUX, aes(x = dateRep, y = deaths, color = "Training data" ), size = 2) +
  geom_point(data = covid.test.LUX, aes(x = dateRep, y = deaths, color = "Test data"), size = 2) +
  geom_point(data = LUX.exp_smoothing.predict.plot, aes(x = dateRep, y = Point.Forecast, color = "Fitted values"), size = 2) + 
  geom_ribbon(data=LUX.exp_smoothing.predict.plot, aes(x = dateRep, ymin=Lo.95, ymax=Hi.95, fill = "Confidence interval"), alpha=0.3) + 
  ggtitle("Covid-19 death in Luxembourg - Exponential smoothing model (w/o trend and seasonality)",
          subtitle = str_c("Root mean squared error is ", RMSE.LUX.MODEL_EXPSM1)) + 
  labs(x = "Date",
       y = "Deaths per day",
       color = "Legend",
       fill = "") +
  scale_color_manual(values = colors) +
  theme(legend.position="bottom")

LUX.exp_smoothing2.ggplot <- ggplot() +
  geom_point(data = covid.training.LUX, aes(x = dateRep, y = deaths, color = "Training data" ), size = 2) +
  geom_point(data = covid.test.LUX, aes(x = dateRep, y = deaths, color = "Test data"), size = 2) +
  geom_point(data = LUX.exp_smoothing2.predict.plot, aes(x = dateRep, y = Point.Forecast, color = "Fitted values"), size = 2) + 
  geom_ribbon(data=LUX.exp_smoothing2.predict.plot, aes(x = dateRep, ymin=Lo.95, ymax=Hi.95, fill = "Confidence interval"), alpha=0.3) + 
  ggtitle("Covid-19 death in Luxembourg - Exponential smoothing model with trend (optimal as per ets() function)",
          subtitle = str_c("Root mean squared error is ", RMSE.LUX.MODEL_EXPSM2)) + 
  labs(x = "Date",
       y = "Deaths per day",
       color = "Legend",
       fill = "") +
  scale_color_manual(values = colors) +
  theme(legend.position="bottom")

grid.arrange(LUX.arima.ggplot, LUX.exp_smoothing.ggplot, LUX.exp_smoothing2.ggplot)

# Plotting Egypt -----
EGY.exp_smoothing.ggplot <- ggplot() +
  geom_point(data = covid.training.EGY, aes(x = dateRep, y = deaths, color = "Training data" ), size = 2) +
  geom_point(data = covid.test.EGY, aes(x = dateRep, y = deaths, color = "Test data"), size = 2) +
  geom_point(data = EGY.exp_smoothing.predict.plot, aes(x = dateRep, y = Point.Forecast, color = "Fitted values"), size = 2) + 
  geom_ribbon(data=EGY.exp_smoothing.predict.plot, aes(x = dateRep, ymin=Lo.95, ymax=Hi.95, fill = "Confidence interval"), alpha=0.3) + 
  ggtitle("Covid-19 death in Egypt - Exponential smoothing model with trend (optimal as per ets() function) and as we planned to build manually",
          subtitle = str_c("Root mean squared error is ", RMSE.EGY.MODEL_EXPSM)) + 
  labs(x = "Date",
       y = "Deaths per day",
       color = "Legend",
       fill = "") +
  scale_color_manual(values = colors) +
  theme(legend.position="bottom")

EGY.trend.ggplot <- ggplot() +
  geom_point(data = covid.training.EGY, aes(x = dateRep, y = deaths, color = "Training data" ), size = 2) +
  geom_point(data = covid.test.EGY, aes(x = dateRep, y = deaths, color = "Test data"), size = 2) +
  geom_point(data = EGY.trend.predict.plot, aes(x = dateRep, y = predict, color = "Fitted values"), size = 2) + 
  geom_ribbon(data=EGY.trend.predict.plot, aes(x = dateRep, ymin=Lo.95, ymax=Hi.95, fill = "Confidence interval"), alpha=0.3) + 
  ggtitle("Covid-19 death in Egypt - Linear regression model with linear trend",
          subtitle = str_c("Root mean squared error is ", RMSE.EGY.MODEL_TREND)) + 
  labs(x = "Date",
       y = "Deaths per day",
       color = "Legend",
       fill = "") +
  scale_color_manual(values = colors) +
  theme(legend.position="bottom")

grid.arrange(EGY.exp_smoothing.ggplot, EGY.trend.ggplot)

# Plotting Serbia -----

SRB.arima.ggplot <- ggplot() +
  geom_point(data = covid.training.SRB, aes(x = dateRep, y = deaths, color = "Training data" ), size = 2) +
  geom_point(data = covid.test.SRB, aes(x = dateRep, y = deaths, color = "Test data"), size = 2) +
  geom_point(data = SRB.arima.predict.plot, aes(x = dateRep, y = arima.predict, color = "Fitted values"), size = 2) + 
  geom_ribbon(data=SRB.arima.predict.plot, aes(x = dateRep, ymin=Lo.95, ymax=Hi.95, fill = "Confidence interval"), alpha=0.3) + 
  ggtitle("Covid-19 death in Serbia - ARIMA(0,0,0) model",
          subtitle = str_c("Root mean squared error is ", RMSE.SRB.MODEL_ARIMA)) + 
  labs(x = "Date",
       y = "Deaths per day",
       color = "Legend",
       fill = "") +
  scale_color_manual(values = colors) +
  theme(legend.position="bottom")

SRB.exp_smoothing.ggplot <- ggplot() +
  geom_point(data = covid.training.SRB, aes(x = dateRep, y = deaths, color = "Training data" ), size = 2) +
  geom_point(data = covid.test.SRB, aes(x = dateRep, y = deaths, color = "Test data"), size = 2) +
  geom_point(data = SRB.exp_smoothing.predict.plot, aes(x = dateRep, y = Point.Forecast, color = "Fitted values"), size = 2) + 
  geom_ribbon(data=SRB.exp_smoothing.predict.plot, aes(x = dateRep, ymin=Lo.95, ymax=Hi.95, fill = "Confidence interval"), alpha=0.3) + 
  ggtitle("Covid-19 death in Serbia - Exponencial smoothing model with trend and alpha = 0.49",
          subtitle = str_c("Root mean squared error is ", RMSE.SRB.MODEL_EXPSM1)) + 
  labs(x = "Date",
       y = "Deaths per day",
       color = "Legend",
       fill = "") +
  scale_color_manual(values = colors) +
  theme(legend.position="bottom")

SRB.exp_smoothing2.ggplot <- ggplot() +
  geom_point(data = covid.training.SRB, aes(x = dateRep, y = deaths, color = "Training data" ), size = 2) +
  geom_point(data = covid.test.SRB, aes(x = dateRep, y = deaths, color = "Test data"), size = 2) +
  geom_point(data = SRB.exp_smoothing2.predict.plot, aes(x = dateRep, y = Point.Forecast, color = "Fitted values"), size = 2) + 
  geom_ribbon(data=SRB.exp_smoothing2.predict.plot, aes(x = dateRep, ymin=Lo.95, ymax=Hi.95, fill = "Confidence interval"), alpha=0.3) + 
  ggtitle("Covid-19 death in Serbia - Exponential smoothing model w/o trend and seasonality (optimal as per ets() function)",
          subtitle = str_c("Root mean squared error is ", RMSE.SRB.MODEL_EXPSM2)) + 
  labs(x = "Date",
       y = "Deaths per day",
       color = "Legend",
       fill = "") +
  scale_color_manual(values = colors) +
  theme(legend.position="bottom")

grid.arrange(SRB.arima.ggplot, SRB.exp_smoothing.ggplot, SRB.exp_smoothing2.ggplot)

