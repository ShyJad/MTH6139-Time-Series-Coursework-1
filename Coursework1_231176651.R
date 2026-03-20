#' ---
#' title: MTH6139 Time Series
#' author: Shyla Jadav -- 231176651
#' date: Spring 2025
#' ---

# 1. Load Libraries -----------------------------------------------------------
library(quantmod)
library(prophet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

# 2. Fetch Data ---------------------------------------------------------------
getSymbols("LIT", src = "yahoo", auto.assign = TRUE)
head(LIT)
tail(LIT)

# 3. Build Monthly Dataframe --------------------------------------------------
LithiumMonthly <- data.frame(
    ds = index(LIT),
    y  = as.numeric(Cl(LIT))
) %>%
    mutate(MonthStart = as.Date(format(ds, "%Y-%m-01"))) %>%
    group_by(MonthStart) %>%
    summarise(y = mean(y, na.rm = TRUE)) %>%
    rename(ds = MonthStart)

head(LithiumMonthly)
nrow(LithiumMonthly)

# 4. Convert to ts Object -----------------------------------------------------
LithiumTS <- ts(LithiumMonthly$y,
                start     = c(as.integer(format(min(LithiumMonthly$ds), "%Y")),
                              as.integer(format(min(LithiumMonthly$ds), "%m"))),
                frequency = 12)
start(LithiumTS)
end(LithiumTS)

# 5. Plot ---------------------------------------------------------------------
plot(LithiumTS, main = "LIT ETF Monthly Price",
     ylab = "Price (USD)", xlab = "Year",
     col = "blue", lwd = 2)

# 6. Decomposition ------------------------------------------------------------
LithiumDecomp <- decompose(LithiumTS, type = "multiplicative")
plot(LithiumDecomp)

# 7. Linear Regression --------------------------------------------------------
TimeIndex   <- 1:length(LithiumTS)
LinearModel <- lm(LithiumTS ~ TimeIndex)
summary(LinearModel)

plot(LithiumTS, main = "LIT ETF with Linear Trend",
     col = "blue", lwd = 2)
abline(LinearModel, col = "red", lty = 2, lwd = 2)
legend("topleft", legend = c("LIT Price", "Linear Trend"),
       col = c("blue", "red"), lty = c(1,2), lwd = 2)

# 8. Fit Prophet --------------------------------------------------------------
ProphetModel    <- prophet(LithiumMonthly)
FutureDates     <- make_future_dataframe(ProphetModel, periods = 24, freq = "month")
ProphetForecast <- predict(ProphetModel, FutureDates)

plot(ProphetModel, ProphetForecast)
prophet_plot_components(ProphetModel, ProphetForecast)

# 9. Model Without Seasonality ------------------------------------------------
ProphetModelNoSeason    <- prophet(LithiumMonthly,
                                   yearly.seasonality = FALSE,
                                   weekly.seasonality = FALSE,
                                   daily.seasonality  = FALSE)
FutureDatesNoSeason     <- make_future_dataframe(ProphetModelNoSeason,
                                                 periods = 24, freq = "month")
ProphetForecastNoSeason <- predict(ProphetModelNoSeason, FutureDatesNoSeason)
plot(ProphetModelNoSeason, ProphetForecastNoSeason)

# 10. Compare Forecasts -------------------------------------------------------
data.frame(
    Date            = tail(ProphetForecast$ds, 6),
    WithSeasonality = round(tail(ProphetForecast$yhat, 6), 2),
    NoSeasonality   = round(tail(ProphetForecastNoSeason$yhat, 6), 2)
)