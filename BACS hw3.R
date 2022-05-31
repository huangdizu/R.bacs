#Question1
#a
#1
set.seed(10)
rnorm_dist <- rnorm(1000, mean=940, sd=150)

standardize <- function(x){
    (x - mean(x)) / sd(x)
}

rnorm_std <- standardize(rnorm_dist)
mean(rnorm_std)
sd(rnorm_std)

#2
plot(density(rnorm_std))

#3

#b
#1
setwd("C:/Users/eason/Desktop/清大 BACS/資料/")
bookings <- read.table("first_bookings_datetime_sample.txt", header=TRUE)
bookings$datetime[1:9]

hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins

standardize <- function(x){
    (x - mean(x)) / sd(x)
}

minday_std <- standardize(minday)
mean(minday_std)
sd(minday_std)

#2
plot(density(minday_std))
plot(density(minday))

#Question2
#a
visualize_sample_ci <- function(num_samples = 100, sample_size = 100, 
                                pop_size=10000, distr_func=rnorm, ...) {
    # Simulate a large population
    population_data <- distr_func(pop_size, ...)
    pop_mean <- mean(population_data)
    pop_sd <- sd(population_data)
    
    # Simulate samples
    samples <- replicate(num_samples, 
                         sample(population_data, sample_size, replace=FALSE))
    
    # Calculate descriptives of samples
    sample_means = apply(samples, 2, FUN=mean)
    sample_stdevs = apply(samples, 2, FUN=sd)
    sample_stderrs <- sample_stdevs/sqrt(sample_size)
    ci95_low  <- sample_means - sample_stderrs*1.96
    ci95_high <- sample_means + sample_stderrs*1.96 
    ci99_low  <- sample_means - sample_stderrs*2.58
    ci99_high <- sample_means + sample_stderrs*2.58
    
    # Visualize confidence intervals of all samples
    plot(NULL, xlim=c(pop_mean-(pop_sd/2), pop_mean+(pop_sd/2)), 
         ylim=c(1,num_samples), ylab="Samples", xlab="Confidence Intervals")
    add_ci_segment(ci95_low, ci95_high, ci99_low, ci99_high,
                   sample_means, 1:num_samples, good=TRUE)
    
    # Visualize samples with CIs that don't include population mean
    bad = which(((ci95_low > pop_mean) | (ci95_high < pop_mean)) |
                    ((ci99_low > pop_mean) | (ci99_high < pop_mean)))
    add_ci_segment(ci95_low[bad], ci95_high[bad], ci99_low[bad], ci99_high[bad],
                   sample_means[bad], bad, good=FALSE)
    
    # Draw true population mean
    abline(v=mean(population_data))
}

add_ci_segment <- function(ci95_low, ci95_high, ci99_low, ci99_high, 
                           sample_means, indices, good=TRUE) {
    segment_colors <- list(c("lightcoral", "coral3", "coral4"),
                           c("lightskyblue", "skyblue3", "skyblue4"))
    color <- segment_colors[[as.integer(good)+1]]
    
    segments(ci99_low, indices, ci99_high, indices, lwd=3, col=color[1])
    segments(ci95_low, indices, ci95_high, indices, lwd=3, col=color[2])
    points(sample_means, indices, pch=18, cex=0.6, col=color[3])
}

visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size = 10000,
                    distr_func = rnorm, mean = 20, sd = 3)

#b
visualize_sample_ci(num_samples = 100, sample_size = 300, pop_size = 10000, 
                    distr_func = rnorm, mean = 20, sd = 3)

#c
visualize_sample_ci(num_samples = 100, sample_size = 100, pop_size = 10000,
                    distr_func = runif)
visualize_sample_ci(num_samples = 100, sample_size = 300, pop_size = 10000,
                    distr_func = runif)

#Question 3
#a
setwd("C:/Users/eason/Desktop/清大 BACS/資料/")
bookings <- read.table("first_bookings_datetime_sample.txt", header=TRUE)
bookings$datetime[1:9]

hours  <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$hour
mins   <- as.POSIXlt(bookings$datetime, format="%m/%d/%Y %H:%M")$min
minday <- hours*60 + mins
plot(density(minday), main="Minute (of the day) of first ever booking", col="blue", lwd=2)

#1
minday_mean <- mean(minday)
minday_sd <- sd(minday)
minday_size <- length(minday)
minday_se <- (minday_sd / sqrt(minday))
minday_ci_95 <- quantile(minday, probs=c(0.025, 0.975))

#2,3
plot_resample <- function(sample0){
    resample <- sample(sample0, length(sample0), replace = TRUE)
    lines(density(resample), col = rgb(0.5, 0.5, 1, 0.1))
    resample_stat <- mean(resample)
    abline(v = mean(resample), col = rgb(0.5, 1, 1, 0.1))
    return(resample_stat)
}

show_resample_width <- function(sample0){
    num_bootstraps = 2000
    plot(density(sample0), lwd = 0, ylab = "", frame.plot = FALSE, yaxt = "n")
    sample_means <- replicate(num_bootstraps, plot_resample(sample0))
    lines(density(sample0), lwd = 1, col = "black")
}

show_resample_width(minday)

#4
quantile(show_resample_width(minday), probs = c(0.025, 0.975))

#b
#1
minday_median <- median(minday)
minday_median

#2
sample0 <- minday
resample_minday_median <- function(sample0) {
        resample <- sample(sample0, length(sample0), replace=TRUE)
        mean(resample)
    }

boots_minday_median <- replicate(2000, resample_minday_median(sample0))

plot(density(boots_minday_median))

#3
quantile(boots_minday_median, probs = c(0.025, 0.975))