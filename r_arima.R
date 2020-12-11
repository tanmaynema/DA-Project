new_df <- data.frame(vehicles$comb08, vehicles$co2TailpipeGpm, vehicles$year, vehicles$fuelType1, vehicles$make)
#Consider only IC engine vehicles
IC_veh <- filter(new_df, vehicles.fuelType1 != "Electricity") 
mpg_over_time <- qqplot(IC_veh$vehicles.year, IC_veh$vehicles.comb08)
#Consider Companies with highest vehicle count (Top 5)

#Chevy
chevy <- filter(IC_veh, vehicles.make == "Chevrolet")
chevy_avg_co2 <- aggregate(cbind(chevy$vehicles.co2TailpipeGpm)~chevy$vehicles.year, FUN = mean)
ts_chevy_co2 <- ts(chevy_avg_co2$V1, frequency = 1, start = 1984)
plot(ts_chevy_co2)
autoarima_chevy_co2 <- auto.arima(ts_chevy_co2)
chevy_forecast_co2 <- forecast(autoarima_chevy_co2, h=10)
chevy_co2_forecast <- plot(chevy_forecast_co2, main = "Tailpipe CO2 Emissions Forecast for Chevrolet", xlab = "Years", ylab = "Avg. CO2 Emissions (g/miles)")

#Ford
ford <- filter(IC_veh, vehicles.make == "Ford")
ford_avg_co2 <- aggregate(cbind(ford$vehicles.co2TailpipeGpm)~ford$vehicles.year, FUN = mean)
ts_ford_co2 <- ts(ford_avg_co2$V1, frequency = 1, start = 1984)
plot(ts_ford_co2)
autoarima_ford_co2 <- auto.arima(ts_ford_co2)
ford_forecast_co2 <- forecast(autoarima_ford_co2, h=10)
ford_co2_forecast <- plot(ford_forecast_co2, main = "Tailpipe CO2 Emissions Forecast for Ford", xlab = "Years", ylab = "Avg. CO2 Emissions (g/miles)")

#Dodge
dodge <- filter(IC_veh, vehicles.make == "Dodge")
dodge_avg_co2 <- aggregate(cbind(dodge$vehicles.co2TailpipeGpm)~dodge$vehicles.year, FUN = mean)
ts_dodge_co2 <- ts(dodge_avg_co2$V1, frequency = 1, start = 1984)
plot(ts_dodge_co2)
autoarima_dodge_co2 <- auto.arima(ts_dodge_co2)
dodge_forecast_co2 <- forecast(autoarima_dodge_co2, h=10)
dodge_co2_forecast <- plot(dodge_forecast_co2, main = "Tailpipe CO2 Emissions Forecast for Dodge", xlab = "Years", ylab = "Avg. CO2 Emissions (g/miles)")

#GMC
gmc <- filter(IC_veh, vehicles.make == "GMC")
gmc_avg_co2 <- aggregate(cbind(gmc$vehicles.co2TailpipeGpm)~gmc$vehicles.year, FUN = mean)
ts_gmc_co2 <- ts(gmc_avg_co2$V1, frequency = 1, start = 1984)
plot(ts_gmc_co2)
autoarima_gmc_co2 <- auto.arima(ts_gmc_co2)
gmc_forecast_co2 <- forecast(autoarima_gmc_co2, h=10)
gmc_co2_forecast <- plot(gmc_forecast_co2, main = "Tailpipe CO2 Emissions Forecast for GMC", xlab = "Years", ylab = "Avg. CO2 Emissions (g/miles)")

#Toyota
toyota <- filter(IC_veh, vehicles.make == "Toyota")
toyota_avg_co2 <- aggregate(cbind(toyota$vehicles.co2TailpipeGpm)~toyota$vehicles.year, FUN = mean)
ts_toyota_co2 <- ts(toyota_avg_co2$V1, frequency = 1, start = 1984)
plot(ts_toyota_co2)
autoarima_toyota_co2 <- auto.arima(ts_toyota_co2)
toyota_forecast_co2 <- forecast(autoarima_toyota_co2, h=10)
toyota_co2_forecast <- plot(toyota_forecast_co2, main = "Tailpipe CO2 Emissions Forecast for Toyota", xlab = "Years", ylab = "Avg. CO2 Emissions (g/miles)")
