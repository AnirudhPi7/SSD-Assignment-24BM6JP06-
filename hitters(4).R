# 322 observations, 20 variables
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
if (!requireNamespace("ISLR2", quietly = TRUE)) install.packages("ISLR2")
if (!require(moments)) install.packages("moments")
library(moments)
library(ISLR2)
library(factoextra)
library(DT)
data(Hitters)
str(Hitters)
length(Hitters)
head(Hitters)
datatable(Hitters, options = list(pageLength = 10, autoWidth = TRUE))

runs_mean <- mean(Hitters$Runs, na.rm = TRUE)
runs_median <- median(Hitters$Runs, na.rm = TRUE)
runs_sd <- sd(Hitters$Runs, na.rm = TRUE)
runs_min <- min(Hitters$Runs, na.rm = TRUE)
runs_max <- max(Hitters$Runs, na.rm = TRUE)
runs_skewness <- skewness(Hitters$Runs)
runs_kurtosis <- kurtosis(Hitters$Runs)

list(
  Mean = runs_mean,
  Median = runs_median,
  Standard_Deviation = runs_sd,
  Minimum = runs_min,
  Maximum = runs_max
)

hist(Hitters$Runs,
     main = "Histogram of Runs",
     xlab = "Runs",
     col = "lightblue",
     border = "black",
     breaks = 20)

boxplot(Hitters$Runs,
        main = "Boxplot of Runs",
        ylab = "Runs",
        col = "lightgreen",
        horizontal = TRUE,
        notch = TRUE)

barplot(table(Hitters$League),
        main = "Bar Plot of League",
        xlab = "League",
        ylab = "Number of Players",
        col = "lightcoral",
        border = "black")

correlation <- cor(Hitters$Runs, Hitters$Hits, method = "pearson", use = "complete.obs")
correlation

plot(Hitters$Runs, Hitters$Hits,
     main = "Scatter Plot of Runs vs Hits",
     xlab = "Runs",
     ylab = "Hits",
     pch = 19,
     col = "blue")

abline(lm(Hits ~ Runs, data = Hitters), col = "red", lwd = 2)

legend("topleft", legend = c("Data Points", "Trend Line"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

hitters_complete <- na.omit(Hitters)
model <- lm(Runs ~ ., data = hitters_complete)
model_step <- step(model)
model_summary <- summary(model)

significant_vars <- names(which(model_summary$coefficients[, 4] <= 0.05))

significant_vars <- significant_vars[significant_vars != "(Intercept)"]

formula <- as.formula(paste("Runs ~", paste(significant_vars, collapse = " + ")))
significant_model <- lm(formula, data = hitters_complete)
summary(significant_model)

par(mfrow = c(1, 2))
plot(significant_model, which = 1, main = "Residuals vs Fitted")
plot(significant_model, which = 2, main = "Normal Q-Q Plot")

par(mfrow = c(1, 1))
Hitters_complete <- na.omit(Hitters)
Hitters_complete <- subset(Hitters_complete, select = -Runs)
numeric_data <- Hitters_complete[, sapply(Hitters_complete, is.numeric)]
scaled_data <- scale(numeric_data)
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
                title = "PCA Biplot of Hitters Data")