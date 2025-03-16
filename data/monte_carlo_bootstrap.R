# Custom function to generate a bootstrap sample
generate_bootstrap_sample <- function(sample) {
  n <- length(sample)
  bootstrap_sample <- numeric(n)
  for (i in 1:n) {
    index <- sample(1:n, 1, replace = TRUE)
    bootstrap_sample[i] <- sample[index]
  }
  return(bootstrap_sample)
}

# Custom bootstrap function
bootstrap <- function(sample, statistic, B) {
  n <- length(sample)
  bootstrap_stats <- numeric(B)
  
  for (b in 1:B) {
    bootstrap_sample <- generate_bootstrap_sample(sample)
    bootstrap_stats[b] <- statistic(bootstrap_sample)
  }
  
  return(bootstrap_stats)
}

# Load necessary libraries
library(utils)

# Load the dataset
data <- read.csv("processed_cleveland.csv")
head(data)
# Select the 'chol' (cholesterol) column, dropping missing values
chol_data <- na.omit(data$chol)

# Parameters
B <- 1000
statistic <- mean

# Perform bootstrap
bootstrap_means <- bootstrap(chol_data, statistic, B)

# Compute the original sample's mean
original_mean <- mean(chol_data)
cat("Original mean cholesterol level:", original_mean, "\n")

# Known mean from literature
known_mean_literature <- 200

# Calculate variance (standard error)
bootstrap_variance <- var(bootstrap_means)
bootstrap_se <- sd(bootstrap_means)
cat("Bootstrap standard error (variance):", bootstrap_se, "\n")


# Calculate bias
bootstrap_mean <- mean(bootstrap_means)
bias <- bootstrap_mean - original_mean
cat("Bootstrap bias:", bias, "\n")

# Calculate confidence intervals
ci_lower <- quantile(bootstrap_means, 0.025)
ci_upper <- quantile(bootstrap_means, 0.975)
cat("95% Confidence interval: [", ci_lower, ", ", ci_upper, "]\n")

# Plot the bootstrap distribution and compare with originial mean
hist(bootstrap_means, breaks = 30, main = "Bootstrap Distribution of Mean Cholesterol Level",
     xlab = "Mean Cholesterol Level", border = "blue", col = "green", xlim = c(min(bootstrap_means), max(bootstrap_means)))
abline(v = ci_lower, col = "red", lty = 2)
abline(v = ci_upper, col = "red", lty = 2)
abline(v = original_mean, col = "purple", lty = 1)
legend("topright", legend=c("95% CI Lower", "95% CI Upper", "Known Mean"),
       col=c("red", "red", "purple"), lty=c(2, 2, 1), cex=0.8)



# Measure execution time for different B values
execution_times <- numeric()
B_values <- c(100, 500, 1000, 2000, 5000)
for (B in B_values) {
  start_time <- Sys.time()
  bootstrap_means <- bootstrap(chol_data, statistic, B)
  end_time <- Sys.time()
  execution_times <- c(execution_times, end_time - start_time)
}

# Display execution times
data.frame(B_values, execution_times)


# Check convergence by incrementally increasing B
convergence_means <- numeric()
B_max <- 5000
step <- 100
for (B in seq(step, B_max, by=step)) {
  bootstrap_means <- bootstrap(chol_data, statistic, B)
  convergence_means <- c(convergence_means, mean(bootstrap_means))
}

# Plot convergence
plot(seq(step, B_max, by=step), convergence_means, type='l', xlab='Number of Bootstrap Samples (B)', ylab='Bootstrap Mean', main='Convergence of Bootstrap Mean')

