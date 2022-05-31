#Question 1
#自己上網看上課前十分鐘(討論)

#Question 2
setwd("C:/Users/eason/Desktop/清大 BACS/資料/")
verizon <- read.csv("verizon.csv")
time <- verizon$Time

hyp_mean <- 7.6
sample_n <- length(time)
sample_mean <- mean(time)
sample_sd <- sd(time)
sample_se <- sd(time)/sqrt(length(time))

#a
#1
t.test(time, mu=7.6, alternative="greater", conf.level=0.99)

#2 不知道為啥要加sig.level, type=one.sample
power.t.test(n=length(time), delta=sample_mean-hyp_mean, sig.level=0.01,
             sd=sample_sd, alternative="one.sided", type= "one.sample")

#b
#1
t_value <- (mean(time)-hyp_mean) / sample_se

#2
bootstrap_null_alt <- function(sample0, hyp_mean){
    resample <- sample(sample0, length(sample0), replace=TRUE)
    resample_se <- sd(resample) / sqrt(length(resample))
    
    t_stat_alt  <- (mean(resample) - hyp_mean)      / resample_se
    t_stat_null <- (mean(resample) - mean(sample0)) / resample_se
    
    c(t_stat_alt, t_stat_null)
}

set.seed(42)
boot_t_stats <- replicate(10000, bootstrap_null_alt(time, hyp_mean))
t_alt  <- boot_t_stats[1,]
t_null <- boot_t_stats[2,]

#3
cutoff_99 <- quantile(t_null, probs =0.99)

#4
#bootstrapped_pvalue 跟直接用pt算不知道為啥不一樣(?)因為bootstrap?//不太懂ecdf怎麼運作的
null_probs <- ecdf(t_null)
one_tailed_pvalue <- 1 - null_probs(t_value)
#bootstrapped_power 
alt_probs <- ecdf(t_alt)
t_power <-  1 - alt_probs(cutoff_99)





