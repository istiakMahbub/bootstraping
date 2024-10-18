# Initial data 
data <- c(18, 28, 30, 31, 35, 37, 38, 46, 50, 53, 54, 59, 61, 63, 67, 69, 71)

# Calculate and print the mean
mean_value <- mean(data)
print(paste("Mean:", mean_value))

# Calculate and print the standard deviation
stdev_value <- sd(data)
print(paste("Standard Deviation:", stdev_value))

# Calculate and print the median
median_value <- median(data)
print(paste("Median:", median_value))

# Custom function to generate a bootstrap sample using vectorization
generate_bootstrap_sample <- function(sample) {
  n <- length(sample)
  indices <- sample(1:n, n, replace = TRUE)
  bootstrap_sample <- sample[indices]
  return(bootstrap_sample)
}

# Custom bootstrap function using vectorization
bootstrap <- function(sample, statistic, B) {
  n <- length(sample)
  bootstrap_stats <- numeric(B)
  
  for (b in 1:B) {
    bootstrap_sample <- generate_bootstrap_sample(sample)
    bootstrap_stats[b] <- statistic(bootstrap_sample)
  }
  return(bootstrap_stats)
}

# Set the seed for reproducibility
set.seed(12345)

# Number of bootstrap samples
B <- 1000

# Perform bootstrap to estimate the mean
bootstrap_means <- bootstrap(data, mean, B)
bootstrap_means

# Print the first few bootstrap means and the estimated mean
print("First 10 Bootstrap Means:")
print(head(bootstrap_means, 10))

# Calculate the estimated mean from the bootstrap means
estimated_mean <- mean(bootstrap_means)
print(paste("Estimated Mean from Bootstrap Samples:", estimated_mean))

# Calculate the standard error of the bootstrap means
bootstrap_se <- sd(bootstrap_means)
print(paste("Standard Error of the Mean:", bootstrap_se))

# Calculate the normal confidence interval (95%)
z_value <- qnorm(0.975)  # for a 95% confidence level
ci_normal_lower <- estimated_mean - z_value * bootstrap_se
ci_normal_upper <- estimated_mean + z_value * bootstrap_se
print(paste("Normal Confidence Interval (95%):", ci_normal_lower, "to", ci_normal_upper))

# Calculate the percentile interval (95%)
ci_percentile_lower <- quantile(bootstrap_means, 0.025)
ci_percentile_upper <- quantile(bootstrap_means, 0.975)
print(paste("Percentile Interval (95%):", ci_percentile_lower, "to", ci_percentile_upper))

# Calculate Bias Corrected Intervals (BC)
# First, compute the proportion of bootstrap estimates less than the original estimate
p_boot <- mean(bootstrap_means < mean_value)
z_0 <- qnorm(p_boot)

# Adjust the confidence intervals using the bias correction factor
ci_bc_lower <- qnorm(0.025, mean = z_0, sd = 1)
ci_bc_upper <- qnorm(0.975, mean = z_0, sd = 1)

ci_bc_lower <- mean(bootstrap_means) + ci_bc_lower * sd(bootstrap_means)
ci_bc_upper <- mean(bootstrap_means) + ci_bc_upper * sd(bootstrap_means)

print(paste("Bias Corrected Interval (95%):", ci_bc_lower, "to", ci_bc_upper))

# Calculate Bias Corrected and Accelerated Intervals (BCa)
# First, calculate the acceleration constant (a)
jackknife_means <- sapply(1:length(data), function(i) mean(data[-i]))
mean_jackknife <- mean(jackknife_means)
a <- sum((mean_jackknife - jackknife_means)^3) / (6 * (sum((mean_jackknife - jackknife_means)^2))^(3/2))

# Calculate adjusted z0 and acceleration factor
z0_adjusted <- function(alpha) {
  qnorm(alpha) + z_0 + (z_0 + qnorm(alpha)) / (1 - a * (z_0 + qnorm(alpha)))
}

# Adjust the bias-corrected intervals using the acceleration factor
ci_bca_lower <- quantile(bootstrap_means, pnorm(z0_adjusted(0.025)))
ci_bca_upper <- quantile(bootstrap_means, pnorm(z0_adjusted(0.975)))

print(paste("Bias Corrected and Accelerated Interval (95%):", ci_bca_lower, "to", ci_bca_upper))


# Calculate the variance of the bootstrap estimates
bootstrap_variance <- var(bootstrap_means)
print(paste("Variance of the Bootstrap Estimates:", bootstrap_variance))

# Calculate the bias of the bootstrap estimates
bootstrap_bias <- mean(bootstrap_means) - mean_value
print(paste("Bias of the Bootstrap Estimates:", bootstrap_bias))

# Hypothesis Testing
# Hypothesized mean value
mu_0 <- 50  

# Perform two-tailed hypothesis testing
p_value <- mean(abs(bootstrap_means - mean_value) >= abs(mu_0 - mean_value))
print(paste("P-value:", p_value))

# Decision rule (alpha = 0.05 for a 95% confidence level)
alpha <- 0.05

if (p_value < alpha) {
  print("Reject the null hypothesis: The average score is significantly different from the hypothesized value.")
} else {
  print("Fail to reject the null hypothesis: There is no significant difference between the average score and the hypothesized value.")
}


# Function to measure execution time for a given number of bootstrap samples
measure_execution_time <- function(sample, statistic, B) {
  start_time <- Sys.time()
  bootstrap(sample, statistic, B)
  end_time <- Sys.time()
  return(as.numeric(difftime(end_time, start_time, units = "secs")))
}

# Define a range of B values for execution time measurement
B_values <- c(1000, 10000, 100000, 1000000)

# Measure execution time for each B
execution_times <- sapply(B_values, function(B) measure_execution_time(data, mean, B))


# Transform B_values and execution_times using the natural logarithm
log_B_values <- log(B_values)
log_execution_times <- log(execution_times)

# Perform linear regression on the log-transformed data
model <- lm(log_execution_times ~ log_B_values)
summary(model)

# Extract the slope of the regression line
slope <- coef(model)[2]
print(paste("Estimated complexity (slope):", slope))

# Plot the log-transformed data and the regression line
plot(log_B_values, log_execution_times, pch = 16, col = "blue",
     xlab = "Log(Number of Bootstrap Samples (B))",
     ylab = "Log(Execution Time (seconds))",
     main = "Log-Log Plot of Execution Time vs. Number of Bootstrap Samples")
abline(model, col = "red")
legend("topleft", legend = c("Data points", "Regression line"), col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 1))


# Convergence of bootstrap mean
# Function to calculate the cumulative mean of bootstrap means as B increases
cumulative_bootstrap_means <- function(sample, statistic, B_values) {
  cumulative_means <- numeric(length(B_values))
  
  for (i in seq_along(B_values)) {
    B <- B_values[i]
    bootstrap_means <- bootstrap(sample, statistic, B)
    cumulative_means[i] <- mean(bootstrap_means)
  }
  
  return(cumulative_means)
}

# Define a range of B values for convergence plot
B_values_convergence <- seq(100, 10000, by = 100)

# Calculate the cumulative bootstrap means
cumulative_means <- cumulative_bootstrap_means(data, mean, B_values_convergence)

# Plot the convergence of the bootstrap mean
plot(B_values_convergence, cumulative_means, type = "l", col = "blue",
     xlab = "Number of Bootstrap Samples (B)",
     ylab = "Cumulative Bootstrap Mean",
     main = "Convergence of Bootstrap Mean")

# Add a horizontal line representing the true mean
true_mean <- mean(data)
abline(h = true_mean, col = "red", lty = 2)
legend("topright", legend = c("Cumulative Bootstrap Mean", "True Mean"), col = c("blue", "red"), lty = c(1, 2))


# Histogram analysis of original and bootstrap sample data
# Number of bootstrap samples
B <- 10000

# Perform bootstrap to estimate the mean
bootstrap_samples <- replicate(B, generate_bootstrap_sample(data))

# Flatten the bootstrap samples into a single vector
bootstrap_samples_flat <- as.vector(bootstrap_samples)

# Compare the distribution of bootstrap samples to the original data

# Histograms
par(mfrow = c(1, 2))  # Set up a plotting area with 1 row and 2 columns
hist(data, probability = TRUE, main = "Histogram of Original Data", xlab = "Value", col = "lightblue", border = "black")
hist(bootstrap_samples_flat, probability = TRUE, main = "Histogram of Bootstrap Samples", xlab = "Value", col = "lightgreen", border = "black")
par(mfrow = c(1, 1))  # Reset the plotting


# Presidential race between Nixon and McGovern
votes_nixon <- 1240
votes_mcgovern <- 760
total_votes <- 2000

# Create a binary vector representing the votes
votes <- c(rep(1, votes_mcgovern), rep(0, votes_nixon))


# Number of bootstrap samples
B <- 10000

# Perform bootstrap to estimate the proportion
bootstrap_proportions <- bootstrap(votes,mean, B)

# Calculate the standard error of the bootstrap proportions
bootstrap_se <- sd(bootstrap_proportions)
print(paste("Standard Error of the Proportion:", bootstrap_se))

# Calculate the normal confidence interval (95%)
z_value <- qnorm(0.975)  # for a 95% confidence level
ci_normal_lower <- 0.3752 - z_value * bootstrap_se
ci_normal_upper <- 0.3752 + z_value * bootstrap_se
print(paste("Normal Confidence Interval (95%):", ci_normal_lower, "to", ci_normal_upper))

# Calculate the percentile interval (95%)
ci_percentile_lower <- quantile(bootstrap_proportions, 0.025)
ci_percentile_upper <- quantile(bootstrap_proportions, 0.975)
print(paste("Percentile Interval (95%):", ci_percentile_lower, "to", ci_percentile_upper))

# Plot the bootstrap proportions
hist(bootstrap_proportions, probability = TRUE, breaks = 30, main = "Bootstrap Distribution of Proportion of Votes for McGovern",
     xlab = "Proportion", col = "lightblue", border = "black", 
     cex.main = 2, cex.lab = 2, cex.axis = 2) # Increase title, label, and axis sizes
abline(v = ci_percentile_lower, col = "red", lwd = 2, lty = 2)
abline(v = ci_percentile_upper, col = "red", lwd = 2, lty = 2)
abline(v = 0.3752, col = "blue", lwd = 2)
legend(x = 0.401, y = 35, legend = c("Actual Proportion (37.52%)", "95% CI Lower Bound", "95% CI Upper Bound"),
       col = c("blue", "red", "red"), lty = c(1, 1, 1), lwd = 1, cex = 1.1, box.lty = 0, bg = NA)



# Runtime comparison
# Install and load the required packages
if (!require("boot")) {
  install.packages("boot")
  library(boot)
}


# Boot function for built-in bootstrap method
boot_mean <- function(data, indices) {
  return(mean(data[indices]))
}

# Function to measure execution time for a given number of bootstrap samples (custom method)
measure_execution_time_custom <- function(sample, statistic, B) {
  start_time <- Sys.time()
  bootstrap(sample, statistic, B)
  end_time <- Sys.time()
  return(as.numeric(difftime(end_time, start_time, units = "secs")))
}

# Function to measure execution time for a given number of bootstrap samples (built-in method)
measure_execution_time_builtin <- function(data, statistic, B) {
  start_time <- Sys.time()
  boot(data, statistic, R = B)
  end_time <- Sys.time()
  return(as.numeric(difftime(end_time, start_time, units = "secs")))
}

# Define a range of B values for execution time measurement
B_values <- c(1000, 10000, 100000, 1000000, 10000000, 20000000)

# Measure execution time for each B using custom method
execution_times_custom <- sapply(B_values, function(B) measure_execution_time_custom(data, mean, B))

# Measure execution time for each B using built-in method
execution_times_builtin <- sapply(B_values, function(B) measure_execution_time_builtin(data, boot_mean, B))

# Print the measured execution times
execution_times <- data.frame(B_values, execution_times_custom, execution_times_builtin)
print(execution_times)

# Plot the execution time against B for both methods
plot(B_values, execution_times_custom, type = "b", col = "blue", log = "xy",
     xlab = "Number of Bootstrap Samples (B)",
     ylab = "Execution Time (seconds)",
     main = "Runtime Comparison of Bootstrap Methods",
     ylim = range(c(execution_times_custom, execution_times_builtin)))
lines(B_values, execution_times_builtin, type = "b", col = "red")
legend("topleft", legend = c("Custom Method", "Built-in Method"), col = c("blue", "red"), lty = 1)



