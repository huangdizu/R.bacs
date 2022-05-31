#Question 1
#a
pnorm(-3.7)

#b
pnorm(-3.7)*2500000

#Question 2
#a
setwd("C:/Users/eason/Desktop/清大 BACS/資料/")
verizon <- read.csv("verizon.csv")
time <- verizon$Time

#1
#Density Plot
plot(density(time), lwd=2, col="cornflowerblue")
abline(v=mean(time), lty="dashed")

#2
#Hnull: The population mean is 7.6
#Halt: The population mean is significantly greater than or less than 7.6
#The hypothesis
hyp_mean <- 7.6

#Sample descriptive statistics
sample_n <- length(time)
sample_mean <- mean(time)
sample_sd <- sd(time)
sample_se <- sd(time)/sqrt(length(time))

#3
#Confidence Interval
quants_99_2sided <- qt(c(0.005, 0.995), sample_n - 1)
sample_mean + quants_99_2sided * sample_se

#4
t_value <- (mean(time)-hyp_mean) / sample_se
p_value <- pt(t_value, sample_n - 1, lower.tail = FALSE)

#5
#Classical hypothesis testing(t-value, p-value) seems to relate to the Null t-distribution

#6
#The hypothesis cannot be rejected at 99% CI(two-tailed), but we should probably collect more data

#b
sample_boot <- function(sample0, hyp_mean) {
    resample <- sample(sample0, replace = TRUE)
    resample_se <- sd(resample) / sqrt(length(resample))
    
    boot_mean <- mean(resample)
    boot_diff <- mean(resample) - hyp_mean
    boot_t <- (mean(resample) - hyp_mean) / resample_se
    c(boot_mean, boot_diff, boot_t)
}
set.seed(42)
boot_stats <- replicate(2000, sample_boot(time, hyp_mean))

#1 Bootstrapped means
boot_means <- boot_stats[1,]
q_means <- quantile(boot_means, probs = c(0.005, 0.995))
plot(density(boot_means))
abline(v=hyp_mean, lwd=2)
abline(v=q_means, lty="dashed")

#2 Bootstrapped differences
boot_diffs <- boot_stats[2,]
q_diffs <- quantile(boot_diffs, probs = c(0.005, 0.995))
plot(density(boot_diffs))
abline(v=0, lwd=2)
abline(v=q_diffs, lty="dashed")

#3 Bootstrapped t-statistics
boot_t_vals <- boot_stats[3,]
q_boots <- quantile(boot_t_vals, probs = c(0.005, 0.995))
plot(density(boot_t_vals))
abline(v=0, lwd=2)
abline(v=q_boots, lty="dashed")