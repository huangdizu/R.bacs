#Question 1
#a, b
setwd("C:/Users/eason/Desktop/清大 BACS/資料/")
verizon_wide <- read.csv("verizon_wide.csv")
library(tidyr)
verizon_long <- gather(verizon_wide, na.rm=TRUE, key = "provider", value = "time")
providers <- split(x = verizon_long$time, f = verizon_long$provider)

#c
head(verizon_long)
tail(verizon_long)

#d
plot(density(providers$ILEC), col="cornflowerblue", lwd=2)
lines(density(providers$CLEC), col="coral3", lwd=2)

#Question 2
#a

#b1,2 不太確定significance0.01是甚麼意思
t.test(providers$CLEC, providers$ILEC, alt="greater", var.equal=TRUE)
t.test(providers$CLEC, providers$ILEC, alt="greater", var.equal=FALSE)

#c1 看一下函式怎麼寫
observed_diff <- mean(providers$CLEC) - mean(providers$ILEC)

permute_diff <- function(values, groups){
    permuted <- sample(values, replace=FALSE)
    grouped <- split(permuted, groups)
    mean(grouped$CLEC) - mean(grouped$ILEC)
}

nperms <- 10000
set.seed(43)
permuted_diffs <- replicate(nperms, permute_diff(verizon_long$time, verizon_long$provider))
hist(permuted_diffs, breaks = "fd", probability=TRUE)
lines(density(permuted_diffs), lwd=2)
abline(v=observed_diff, col="coral3", lwd=3)

#c2
p_1tailed <- sum(permuted_diffs > observed_diff) / nperms
p_2tailed <- sum(abs(permuted_diffs) > observed_diff) / nperms

p_1tailed
p_2tailed

#c3 
#We cannot reject Hnull

#Question 3
#a1
gt_eq <- function(a, b){
    ifelse(a>b, 1, 0) + ifelse(a==b, 0.5, 0)
}
W <- sum(outer(providers$CLEC, providers$ILEC, FUN = gt_eq))

#b
n1 = length(providers$CLEC) 
n2 = length(providers$ILEC) 

n1
n2

wilcox_p_1tail <- 1 - pwilcox(W, n1, n2)
wilcox_p_2tail <- 2 * wilcox_p_1tail

wilcox_p_1tail
wilcox_p_2tail

#c paired=false是甚麼意思
wilcox.test(time ~ provider, data=verizon_long, alternative="greater", paired=FALSE)

#d自己再想想
#Values in the two samples are distinct(values seem shifted)
#Means of samples are not distinguishable, but CLEC vs ILEC customers seem to be treated differently

#Question 4
#a1~5
norm_qq_plot <- function(values){
    probs1000 <- seq(0, 1, 0.001)
    q_vals <- quantile(values, probs1000)
    q_norm <- qnorm(probs1000, mean=mean(values), sd=sd(values))
    plot(q_norm, q_vals, xlab="normal quantiles", ylab="values quantiles")
    abline(a=0, b=1, col="red", lwd=2)
}

#b
set.seed(978234)
d1 <- rnorm(n=500, mean=15, sd=5)
d2 <- rnorm(n=200, mean=30, sd=5)
d3 <- rnorm(n=100, mean=45, sd=5)
d123 <- c(d1, d2, d3)

plot(density(d123))
norm_qq_plot(d123)

#c
norm_qq_plot(providers$CLEC)
norm_qq_plot(providers$ILEC)
