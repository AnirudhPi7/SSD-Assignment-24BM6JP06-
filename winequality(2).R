winequality <- read.csv("D://winequality-red.csv")
#1. 1599 observations, 12 variables
str(winequality)
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")

library(factoextra)
library("DT")
length(winequality)
head(winequality)
datatable(winequality, options = list(pageLength = 10, autoWidth = TRUE))

wine_mean <- mean(winequality$total.sulfur.dioxide, na.rm = TRUE)
wine_median <- median(winequality$total.sulfur.dioxide, na.rm = TRUE)
wine_sd <- sd(winequality$total.sulfur.dioxide, na.rm = TRUE)
wine_min <- min(winequality$total.sulfur.dioxide, na.rm = TRUE)
wine_max <- max(winequality$total.sulfur.dioxide, na.rm = TRUE)
list(
  Mean = wine_mean,
  Median = wine_median,
  Standard_Deviation = wine_sd,
  Minimum = wine_min,
  Maximum = wine_max
)

hist(winequality$total.sulfur.dioxide,
     main = "Histogram of Total Sulfur Dioxide Levels",
     xlab = "Total Sulfur Dioxide",
     col = "lightblue",
     border = "black",)

boxplot(winequality$total.sulfur.dioxide,
        main = "Boxplot of Total Sulfur Dioxide Levels",
        ylab = "Total Sulfur Dioxide",
        col = "lightgreen",
        horizontal = TRUE,
        notch = TRUE)

cleaned_SO2 <- winequality$total.sulfur.dioxide[!is.na(winequality$total.sulfur.dioxide)]

Q1 <- quantile(cleaned_SO2, 0.25, na.rm = "TRUE")
Q3 <- quantile(cleaned_SO2, 0.75, na.rm = "TRUE")
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

outliers <- cleaned_SO2[cleaned_SO2 < lower_bound | cleaned_SO2 > upper_bound]
number_of_outliers <- length(outliers)
number_of_outliers

barplot(table(winequality$quality),
        main = "Bar Plot of Observations by Quality",
        xlab = "Quality",
        ylab = "Frequency",
        col = "lightcoral",
        names.arg = c(3, 4, 5, 6, 7, 8),
        border = "black")


correlation <- cor(winequality$total.sulfur.dioxide, winequality$free.sulfur.dioxide, method = "pearson", use = "complete.obs")
correlation

plot(winequality$total.sulfur.dioxide, winequality$free.sulfur.dioxide,
     main = "Scatter Plot of Total Sulfur Dioxide vs Free Sulfur Dioxide",
     xlab = "Total Sulfur Dioxide",
     ylab = "Free Sulfur Dioxide",
     pch = 19,
     col = "blue")

abline(lm(free.sulfur.dioxide ~ total.sulfur.dioxide, data = winequality), col = "red", lwd = 2)

legend("topleft", legend = c("Data Points", "Trend Line"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

model <- lm(total.sulfur.dioxide ~ fixed.acidity+volatile.acidity+citric.acid+free.sulfur.dioxide+pH, data = winequality)
summary(model)

par(mfrow = c(1, 2))
plot(model, which = 1, main = "Residuals vs Fitted")
plot(model, which = 2, main = "Normal Q-Q Plot")
par(mfrow = c(1, 1))

winequality_complete <- na.omit(winequality)
winequality_complete <- subset(winequality_complete, select = -total.sulfur.dioxide)
scaled_data <- scale(winequality_complete)
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
                title = "PCA Biplot of Wine Quality Data")