#1. 153 observations, 6 variables
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")

library(factoextra)
library(DT)
data(airquality)
str(airquality)
length(airquality)
head(airquality)
datatable(airquality, options = list(pageLength = 10, autoWidth = TRUE))

ozone_mean <- mean(airquality$Ozone, na.rm = TRUE)
ozone_median <- median(airquality$Ozone, na.rm = TRUE)
ozone_sd <- sd(airquality$Ozone, na.rm = TRUE)
ozone_min <- min(airquality$Ozone, na.rm = TRUE)
ozone_max <- max(airquality$Ozone, na.rm = TRUE)

list(
  Mean = ozone_mean,
  Median = ozone_median,
  Standard_Deviation = ozone_sd,
  Minimum = ozone_min,
  Maximum = ozone_max
)

hist(airquality$Ozone,
     main = "Histogram of Ozone Levels",
     xlab = "Ozone (ppb)",
     col = "lightblue",
     border = "black",
     breaks = 20)

boxplot(airquality$Ozone,
        main = "Boxplot of Ozone Levels",
        ylab = "Ozone (ppb)",
        col = "lightgreen",
        horizontal = TRUE,
        notch = TRUE)

cleaned_Ozone <- airquality$Ozone[!is.na(airquality$Ozone)]

Q1 <- quantile(cleaned_Ozone, 0.25, na.rm = "TRUE")
Q3 <- quantile(cleaned_Ozone, 0.75, na.rm = "TRUE")
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers <- cleaned_Ozone[cleaned_Ozone < lower_bound | cleaned_Ozone > upper_bound]
number_of_outliers <- length(outliers)
number_of_outliers

barplot(table(airquality$Month),
        main = "Bar Plot of Observations by Month",
        xlab = "Month",
        ylab = "Frequency",
        col = "lightcoral",
        names.arg = c("May", "June", "July", "August", "September"),
        border = "black")


correlation <- cor(airquality$Ozone, airquality$Temp, method = "pearson", use = "complete.obs")
correlation

plot(airquality$Temp, airquality$Ozone,
     main = "Scatter Plot of Ozone vs. Temperature",
     xlab = "Temperature (°F)",
     ylab = "Ozone (ppb)",
     pch = 19,
     col = "blue")

abline(lm(Ozone ~ Temp, data = airquality), col = "red", lwd = 2)

legend("topleft", legend = c("Data Points", "Trend Line"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

model <- lm(Ozone ~ Temp + Wind + Solar.R, data = airquality)
summary(model)

par(mfrow = c(1, 2))
plot(model, which = 1, main = "Residuals vs Fitted")
plot(model, which = 2, main = "Normal Q-Q Plot")
par(mfrow = c(1, 1))

airquality_complete <- na.omit(airquality)
airquality_complete <- subset(airquality_complete, select = -Ozone)
scaled_data <- scale(airquality_complete)
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
                title = "PCA Biplot of Air Quality Data")