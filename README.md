Bootstrapping Analysis: Monte Carlo Simulations
Overview
This script demonstrates a detailed implementation of bootstrapping, a resampling method used to estimate statistics such as the mean, standard error, confidence intervals, and perform hypothesis testing. Additionally, it includes performance comparisons between custom bootstrapping methods and built-in R functions, along with an analysis of execution time and convergence behavior.

Prerequisites
Ensure you have the following software and libraries installed:

R version 4.0 or higher
Required R libraries:
boot (for built-in bootstrap function)
To install the required library:

R
Copy code
install.packages("boot")
Features
1. Basic Statistical Calculations
Calculates the mean, standard deviation, and median for an initial dataset.
2. Bootstrap Sample Generation
A custom function generates bootstrap samples by resampling the original dataset with replacement.
3. Bootstrap Analysis
The code bootstraps the dataset to estimate the mean, calculates bootstrap means, and estimates the standard error.
Normal confidence intervals (95%), percentile confidence intervals (95%), and bias-corrected (BC) confidence intervals are calculated.
The Bias-Corrected and Accelerated (BCa) method is used for more accurate confidence interval estimation, accounting for both bias and acceleration.
4. Hypothesis Testing
Performs a two-tailed hypothesis test to compare the observed mean against a hypothesized value (e.g., µ₀ = 50) using bootstrap sampling.
5. Runtime and Complexity Analysis
Compares the execution time of the custom bootstrap function and the built-in boot function across different numbers of bootstrap samples.
The log-log plot of execution time versus the number of bootstrap samples is used to analyze complexity and performance.
6. Convergence Analysis
The code calculates the cumulative mean of bootstrap samples and plots the convergence of the bootstrapped mean to the true mean as the number of bootstrap samples increases.
7. Histogram Analysis
Produces histograms comparing the distribution of original data and bootstrap samples to visually examine the differences.
8. Application to Real-World Data
Includes an analysis of voting proportions in the 1972 U.S. presidential race between Nixon and McGovern. Bootstrapping is used to estimate the confidence interval for McGovern's vote proportion.
9. Performance Comparison: Custom vs Built-in Method
Measures and plots the execution time of the custom bootstrap implementation and the built-in boot function to compare performance at different sample sizes.
How to Use the Code
1. Run Basic Statistical Analysis:
The code starts by calculating the mean, standard deviation, and median of the sample dataset.

2. Perform Bootstrapping:
Run the bootstrap analysis by using the custom bootstrap function to estimate the mean, standard error, and confidence intervals.
3. Hypothesis Testing:
Hypothesis testing is performed to compare the bootstrapped mean against a specified hypothesized mean value (µ₀ = 50).
4. Convergence and Execution Time Analysis:
The code evaluates the convergence of bootstrap means and measures the execution time for both custom and built-in methods across varying sample sizes.
5. View Results:
Plots are generated to visualize the histograms of the original dataset and bootstrap samples, the convergence of bootstrap means, and the execution time comparison between methods.
Key Functions
generate_bootstrap_sample(): Generates a bootstrap sample from the original data.
bootstrap(): Applies a specified statistic to each bootstrap sample.
measure_execution_time(): Measures the time taken to run the bootstrap analysis for a specified number of samples.
cumulative_bootstrap_means(): Calculates cumulative means for bootstrapped data to analyze convergence.
bootstrap_proportions(): Estimates the proportion of votes using bootstrapping for the real-world example.
Output
The script outputs the following key results:

Bootstrap means, standard error, confidence intervals (Normal, Percentile, Bias-Corrected, BCa).
The p-value from hypothesis testing and whether the null hypothesis is rejected.
Visualizations of histograms, bootstrap convergence, and runtime comparisons.
Example Usage
R
Copy code
# Calculate and print basic statistics
mean_value <- mean(data)
stdev_value <- sd(data)
median_value <- median(data)

# Perform bootstrap to estimate mean and standard error
bootstrap_means <- bootstrap(data, mean, B = 1000)
bootstrap_se <- sd(bootstrap_means)

# Calculate normal and percentile confidence intervals
ci_normal <- c(estimated_mean - z_value * bootstrap_se, estimated_mean + z_value * bootstrap_se)
ci_percentile <- quantile(bootstrap_means, probs = c(0.025, 0.975))
Results Interpretation
Bootstrap Mean: The bootstrapped estimate of the mean provides an accurate approximation of the population mean.
Confidence Intervals: The different confidence intervals (Normal, Percentile, BC, and BCa) reflect the range within which the true population parameter lies.
Hypothesis Test: The p-value indicates whether the observed mean is significantly different from the hypothesized mean (µ₀).
Execution Time Comparison: The plot comparing custom and built-in methods reveals which approach is more efficient for large-scale bootstrapping.
Notes
The script can handle different datasets by changing the initial data vector.
To analyze a different statistic (e.g., median), modify the statistic argument in the bootstrap() function.