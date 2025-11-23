# Import necessary libraries
library(MASS) ## for ridge estimation

# Load data
data <- read.csv("credit_data.csv")

# Replace Yes/No with binary numbers
data$Student <- ifelse(data$Student == "Yes", 1, 0)
data$Own <- ifelse(data$Own == "Yes", 1, 0)
data$Married <- ifelse(data$Married == "Yes", 1, 0)

# Create dummy variables for Region (excluding the intercept)
region_dummies <- model.matrix(~ Region - 1, data = data)

# Combine with the rest of the data and remove Region
data <- cbind(data[ , !(names(data) %in% "Region")], region_dummies)

# View cleaned-up data as table
View(data)

# Define predictors and variable of interest
X <- dplyr::select(data, Income, Limit, Rating, Cards, Age, Education, Own, Student, Married, RegionEast, RegionWest, RegionSouth)
Y <- data$Balance

# Standardize X values and convert to dataframe
X_standardized <- scale(X)
X_std <- as.data.frame(X_standardized)

# Define ridge coefficient function
compute_coefficients <- function(lambda, X, y, m = 10) {
  
  ridge_fit <- lm.ridge(y ~ as.matrix(X) - 1, lambda = lambda)
  
  coefficients <- coef(ridge_fit)
  
  return(coefficients)
}

# Define OLS computation function (no intercept so all dummies are estimated)
compute_ols <- function(X, y) {
  df  <- data.frame(y = y, X)
  fit <- lm(y ~ . - 1, data = df)          # <- no intercept
  b   <- coef(fit)
  b[colnames(X)]                            # return in same order as X
}

# calculate OLS prediction coefficients (drops south)
ols_coefficients <- compute_ols(X_std, Y)

# Define possible lambda values
lambdas <- 10^seq(-2, 5, length.out = 100)

# Calculate coefficients for all lambda values
coefficients <- sapply(lambdas, function(lambda) compute_coefficients(lambda, X_std, Y))

# Transpose dataframe (switch columns and rows)
df <- t(coefficients)

View(df)

# Clean up the column names and include lambda values
colnames(df) <- gsub("as\\.matrix\\(X\\)", "", colnames(df))
df <- cbind(lambda = lambdas, df)
df <- as.data.frame(df)

# calculcate l2 norm of Ridge estimators for each lambda
ridge_norms <- apply(df[ , !(names(df) %in% "lambda")], 1, function(row) sqrt(sum(row^2)))
print(ridge_norms)

# calculate l2 norm of OLS estimators (remove N/A from dummy variable to allow for computation)
print(ols_coefficients)
ols_norm <- sqrt(sum(ols_coefficients^2, na.rm = TRUE))
print(ols_norm)

# get norm ratio for x-axis on plot
ridge_norm_ratio <- ridge_norms / ols_norm

# add ratio to dataframe
df$norm_ratio <- ridge_norm_ratio


# Display final dataframe
View(df)

# --- Ensure figures folder exists
if (!dir.exists("figures")) dir.create("figures")


# Code for first plot (lambdas on x-axis)

# Save plot as PNG
png("figures/figure_6.4_1.png", width = 768, height = 889, res = 120)

# Start base plot with the first line
plot(df$lambda, df$Income, type = "l", log = "x", lwd = 2,
     ylim = c(-300, 450), col = "black",
     xlab = expression(lambda),
     ylab = "Standardized Coefficients",
     xaxt = "n",  # suppress default x-axis
     yaxt = "n")  # suppress default y-axis

# Custom x-axis
axis(side = 1, at = c(0.01, 1, 100, 10000),
     labels = c("1e-02", "1e+00", "1e+02", "1e+04"))

# Custom y-axis
axis(side = 2, at = seq(-300, 400, by = 100),
     labels = seq(-300, 400, by = 100), las = 0)


# Add the other special lines
lines(df$lambda, df$Limit, col = "red", lty = 2, lwd = 2)      # dashed
lines(df$lambda, df$Rating, col = "blue", lty = 3, lwd = 2)     # dotted
lines(df$lambda, df$Student, col = "goldenrod2", lty = 4, lwd = 2) # dash-dot

# Add grey lines for all the "other" series
lines(df$lambda, df$Cards, col = "grey70", lwd = 1)
lines(df$lambda, df$Age, col = "grey70", lwd = 1)
lines(df$lambda, df$Education, col = "grey70", lwd = 1)
lines(df$lambda, df$Own, col = "grey70", lwd = 1)
lines(df$lambda, df$Married, col = "grey70", lwd = 1)
lines(df$lambda, df$RegionEast, col = "grey70", lwd = 1)
lines(df$lambda, df$RegionWest, col = "grey70", lwd = 1)
lines(df$lambda, df$RegionSouth, col = "grey70", lwd = 1)

# Add legend
legend("topright",
       legend = c("Income", "Limit", "Rating", "Student"),
       col = c("black", "red", "blue", "goldenrod2"),
       lty = c(1, 2, 3, 4),
       lwd = 2,
       bty = "n",
       cex = 1.4)
dev.off()


# Second plot for l2 norm ratio on x-axis
df$norm_ratio <- ridge_norms / ols_norm

# For the norm-ratio plot, we want x to run from 0 -> 1 left-to-right
df_ratio <- df[order(df$norm_ratio), ]

# --- Save the second figure (norm-ratio on x-axis)
png("figures/figure_6.4_2.png", width = 768, height = 889, res = 120)

plot(df_ratio$norm_ratio, df_ratio$Income, type = "l", lwd = 2,
     xlim = c(0, 1), ylim = c(-300, 450), col = "black",
     xlab = expression( "||" * hat(beta)[lambda]^R * "||"[2] / "||" * hat(beta) * "||"[2]),
     ylab = "Standardized Coefficients",
     xaxt = "s", yaxt = "s")

# Custom y-axis
axis(side = 2, at = seq(-300, 400, by = 100),
     labels = seq(-300, 400, by = 100), las = 0)

# Highlighted series
lines(df_ratio$norm_ratio, df_ratio$Limit,   col = "red",       lty = 2, lwd = 2)
lines(df_ratio$norm_ratio, df_ratio$Rating,  col = "blue",      lty = 3, lwd = 2)
lines(df_ratio$norm_ratio, df_ratio$Student, col = "goldenrod2", lty = 4, lwd = 2)

# Grey lines for the others
lines(df_ratio$norm_ratio, df_ratio$Cards,       col = "grey70", lwd = 1)
lines(df_ratio$norm_ratio, df_ratio$Age,         col = "grey70", lwd = 1)
lines(df_ratio$norm_ratio, df_ratio$Education,   col = "grey70", lwd = 1)
lines(df_ratio$norm_ratio, df_ratio$Own,         col = "grey70", lwd = 1)
lines(df_ratio$norm_ratio, df_ratio$Married,     col = "grey70", lwd = 1)
lines(df_ratio$norm_ratio, df_ratio$RegionEast,  col = "grey70", lwd = 1)
lines(df_ratio$norm_ratio, df_ratio$RegionWest,  col = "grey70", lwd = 1)
lines(df_ratio$norm_ratio, df_ratio$RegionSouth, col = "grey70", lwd = 1)

# Finish writing the file
dev.off()