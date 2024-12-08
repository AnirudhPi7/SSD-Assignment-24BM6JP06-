if (!requireNamespace("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
bike <- read.csv("SeoulBikeData.csv")
#1. 8760 observations, 14 variables
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("car", quietly = TRUE)) install.packages("car")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")

library(factoextra)
library(DT)
library(car)
library(ggplot2)

str(bike)
length(bike)
head(bike)
datatable(bike, options = list(pageLength = 10, autoWidth = TRUE))

bike_mean <- mean(bike$Rented.Bike.Count, na.rm = TRUE)
bike_median <- median(bike$Rented.Bike.Count, na.rm = TRUE)
bike_sd <- sd(bike$Rented.Bike.Count, na.rm = TRUE)
bike_min <- min(bike$Rented.Bike.Count, na.rm = TRUE)
bike_max <- max(bike$Rented.Bike.Count, na.rm = TRUE)
list(
  Mean = bike_mean,
  Median = bike_median,
  Standard_Deviation = bike_sd,
  Minimum = bike_min,
  Maximum = bike_max
)

hist(bike$Rented.Bike.Count
     ,
     main = "Histogram of Count of Rented Bikes",
     xlab = "Count of Rented Bikes",
     col = "lightblue",
     border = "black",)

boxplot(bike$Rented.Bike.Count,
        main = "Boxplot of Count of Rented Bikes",
        ylab = "Total Count of Rented Bikes",
        col = "lightgreen",
        horizontal = TRUE,
        notch = TRUE)

bike_clean <- bike[!is.na(bike$Hour), ]

bike$Temp_Range <- cut(bike$Temperature, 
                       breaks = c(-Inf, 0, 10, 20, 30, Inf), 
                       labels = c("Very Cold", "Cold", "Moderate", "Warm", "Hot"))

barplot(table(bike$Temp_Range),
        main = "Rented Bike Count by Temperature Range",
        xlab = "Temperature Range",
        ylab = "Frequency",
        col = "lightblue",
        border = "black")


correlation <- cor(bike$Temperature, bike$DewPointTemperature, method = "pearson", use = "complete.obs")
correlation

bike_clean <- bike[!is.na(bike$Temperature) & !is.na(bike$DewPointTemperature), ]

model_linear <- lm(DewPointTemperature ~ Temperature, data = bike_clean)

plot(bike_clean$Temperature, bike_clean$DewPointTemperature,
     main = "Scatter Plot of Temperature vs Dew Point Temperature",
     xlab = "Temperature (°C)",
     ylab = "Dew Point Temperature (°C)",
     pch = 19, col = "blue")

abline(model_linear, col = "red", lwd = 2)

legend("topleft", legend = c("Data Points", "Trend Line"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

model <- lm(Rented.Bike.Count ~ Hour+Temperature+Humidity+WindSpeed+SolarRadiation+Rainfall+Snowfall+Seasons+Holiday+Functioning.Day, data = bike)
summary(model)
vif(model)

par(mfrow = c(1, 2))
plot(model, which = 1, main = "Residuals vs Fitted")
plot(model, which = 2, main = "Normal Q-Q Plot")
par(mfrow = c(1, 1))

bike_complete <- na.omit(bike)
bike_complete <- subset(bike_complete, select = -Rented.Bike.Count)
bike_complete <- bike_complete[, !(sapply(bike_complete, function(x) is.character(x) || is.factor(x)))]
str(bike_complete)
scaled_data <- scale(bike_complete)
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
summary(pca_result)

explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
plot(explained_variance, type = "b", 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Scree Plot of Principal Components",
     pch = 19, col = "blue")
cumulative_variance <- cumsum(explained_variance)
lines(cumulative_variance, type = "b", col = "red", pch = 19)
legend("topright", legend = c("Proportion of Variance", "Cumulative Variance"),
       col = c("blue", "red"), pch = 19)

fviz_pca_biplot(pca_result,
                repel = TRUE,           
                col.var = "red",        
                col.ind = "gray",       
                geom.ind = "point",     
                pointsize = 2,          
                alpha.ind = 0.5,
                title = "PCA Biplot of Bike Data")