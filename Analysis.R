# --- Package installs
install.packages("ISLR")
library(ISLR)
install.packages(c("MuMIn"))
library(MuMIn)
Hitters <- na.omit(Hitters)
install.packages("leaps")
library(leaps)


# --- Fitting regression subsets model
regfit <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
reg_summary <- summary(regfit)

# --- Showing the dataset
Hitters

# --- Calculating metrics
n <- nrow(Hitters)
p <- 2:20 # Starts at 2 since smallest model is beta_0, beta_1
bic_values <- reg_summary$bic
aic_values <- bic_values - log(n) * p + 2 * p
aicc_values <- aic_values + (2 * p * (p + 1)) / (n - p - 1)
results <- data.frame(
     num_predictors = 1:19,
     AIC  = aic_values,
     BIC  = bic_values,
     AICc = aicc_values
     )
results


# --- Ordering the metrics
aic_order <- order(results$AIC)
aic_selected <- aic_order[c(1, 2, 3, round(19/2) -1, round(19/2), round(19/2) +1, 17, 18, 19)]
print(results[aic_selected, ])

bic_order <- order(results$BIC)
bic_selected <- bic_order[c(1, 2, 3, round(19/2) -1, round(19/2), round(19/2) +1,  17, 18, 19)]
aicc_order <- order(results$AICc)
aicc_selected <- aicc_order[c(1, 2, 3, round(19/2) -1, round(19/2), round(19/2) +1, 17, 18, 19)]
print(results[bic_selected, ])
print(results[aicc_selected, ])


par(mfrow = c(1, 3))
plot(1:19, aic_values,  type = "b", xlab = "# Predictors", ylab = "AIC",  main = "AIC by Subset Size",  col = "steelblue", pch = 19)
points(which.min(aic_values),  min(aic_values),  col = "red", pch = 19, cex = 1.5)
plot(1:19, bic_values,  type = "b", xlab = "# Predictors", ylab = "BIC",  main = "BIC by Subset Size",  col = "darkgreen", pch = 19)
points(which.min(bic_values),  min(bic_values),  col = "red", pch = 19, cex = 1.5)
plot(1:19, aicc_values, type = "b", xlab = "# Predictors", ylab = "AICc", main = "AICc by Subset Size", col = "purple", pch = 19)
points(which.min(aicc_values), min(aicc_values), col = "red", pch = 19, cex = 1.5)