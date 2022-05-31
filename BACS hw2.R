#Question 1
#a
# Three normally distributed data sets
d1 <- rnorm(n=100, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=500, mean=45, sd=5)
# Combining them into a composite dataset
d123 <- c(d1, d2, d3)
# Let’s plot the density function of d123
plot(density(d123), col="blue", lwd=2, main = "Distribution 2")
# Add vertical lines showing mean and median
abline(v=mean(d123))
abline(v=median(d123), lty="dashed")

#b
d4 <- rnorm(800, mean=30, sd=5)
plot(density(d4), col="blue", lwd=2, main = "Distribution 3")
abline(v=mean(d4))
abline(v=median(d4), lty="dashed")

#c
#mean


#Question 2
quartiles_vs_sd <- function(distr, title){
    #plot data distribution, mean + standard deviations lines
    plot(density(distr), col="blue", lwd=2, main = title)
    abline(v=mean(distr))
    abline(v=mean(distr)+(-3:3)*sd(distr), lty="dashed")
    
    #return the distance of each quartile from the mean
    q = quantile(distr, c(0.25, 0.5, 0.75))
    return((q-mean(distr))/sd(distr))
}
#a,b
rdata <- rnorm(2000, 0, 1)
quartiles_vs_sd(rdata, "Distribution A")

#C
rdata1 <- rnorm(2000, 35, 3.5)
quartiles_vs_sd(rdata1, "Distribution B")

#d
d1 <- rnorm(n=100, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=500, mean=45, sd=5)
d123 <- c(d1, d2, d3)
quartiles_vs_sd(d123, "Distribution 2")

#Question 3
#a

#b
#Sturges' Rule: Log length for number of bins
sturges_formula <- function(distr, title) {
    k = ceiling(log2(length(distr))) + 1
    h = (max(distr) - min(distr))/k
    hist(distr, breaks = k, main=title)
    return(data.frame(k, h))
}

#Scott’s Rule: Standard deviation for bin size
scotts_rule <- function(distr, title) {
    h <- 3.5*sd(distr) / (length(distr)^(1/3))
    k = ceiling((max(distr) - min(distr))/h)
    hist(distr, breaks = k, main=title)
    return(data.frame(k, h))
}

#Freedman-Diaconis Choice: IQR for bin size
fd_choice <- function(distr, title) {
    h <- 2*IQR(distr) / (length(distr)^(1/3))
    k <- ceiling((max(distr) - min(distr))/h)
    hist(distr, breaks = k, main=title)
    return(data.frame(k, h))
}

rand_data <- rnorm(800, mean=20, sd=5)
sturges_formula(rand_data, "sturges_formula(rand_data)")
scotts_rule(rand_data, "scotts_rule(rand_data)")
fd_choice(rand_data, "fd_choice(rand_data)")

#c
out_data <- c(rand_data, runif(10, min=40, max=60))
sturges_formula(out_data, "sturges_formula(out_data)")
scotts_rule(out_data, "scotts_rule(out_data)")
fd_choice(out_data, "fd_choice(out_data)")