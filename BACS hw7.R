setwd("C:/Users/eason/Desktop/清大 BACS/資料/")
m1 <- read.csv("pls-media1.csv", header = TRUE)
m2 <- read.csv("pls-media2.csv", header = TRUE)
m3 <- read.csv("pls-media3.csv", header = TRUE)
m4 <- read.csv("pls-media4.csv", header = TRUE)

#Question 1
#a
experiment <- rbind(m1, m2, m3, m4)
exp_groups <- split(experiment$INTEND.0, experiment$media)
media_mean <- sapply(split(experiment$INTEND.0, experiment$media), mean)
round(media_mean, 2)

#b
#Descriptive Statistics
media_length <- sapply(split(experiment$INTEND.0, experiment$media), length)
media_sd <- sapply(split(experiment$INTEND.0, experiment$media), sd)
media_se <- media_sd / sqrt(media_length)

media_length
media_sd
media_se

#Boxplot
boxplot(experiment$INTEND.0 ~ experiment$media,
        xlab="Media", ylab="Response", 
        medcol="gray", medlty="dashed", medlwd=1)
segments( 1:4, media_mean - 1.96*media_se, 
          1:4, media_mean + 1.96*media_se, 
          lwd=6, col="cornflowerblue")

#Density Plots
plot(density(exp_groups$`1`), col="blue", lwd=2)
lines(density(exp_groups$`2`), col="dimgrey", lwd=2)
lines(density(exp_groups$`3`), col="red", lwd=2)
lines(density(exp_groups$`4`), col="darkgreen", lwd=2)
abline(v =media_mean, lty="dashed", col=c("blue", "dimgrey", "red", "darkgreen"), lwd=2)

#c



#Question 2
#a

#b
all_media <- experiment$INTEND.0
media_grand_mean <- mean(all_media)
n_total <- length(all_media) 
k <- length(exp_groups)
n_total
k

sstr <- sum(media_length)*((media_mean - media_grand_mean)^2)
df_mstr <- k - 1
mstr <- sstr/df_mstr

sse <- sum((media_length - 1)*(media_sd^2))
df_mse <- n_total - k
mse <- sse/df_mse

f_value <- mstr/mse
f_value
p_value <- pf(f_value, df_mstr, df_mse, lower.tail = FALSE)
p_value

#c
ftest_aov <- aov(experiment$INTEND.0 ~ factor(experiment$media))
summary(ftest_aov)

#d 要懂怎麼看結果
TukeyHSD(ftest_aov)

#e
qqnorm(experiment$INTEND.0, pch=19, col="gray")
qqline(experiment$INTEND.0)

f_test <- oneway.test(experiment$INTEND.0 ~ factor(experiment$media), var.equal = TRUE)
f_test
qf(df1=4-1, df2=nrow(experiment)-4, p=c(0.95, 0.99))
qf

#Question 3
#a

#b 先把他整理成一個表格, 有點好奇是不是一定要apply才可以多種分開, rank_msq那邊看一下
all_ranks <- rank(experiment$INTEND.0)
group_ranks <- split(all_ranks, experiment$media)

group_ranksums <- sapply(group_ranks, sum)
group_n <- sapply(group_ranks, length)
group_rankmeans <- sapply(group_ranks, mean)

N <- nrow(experiment)
groups <- data.frame(ranksum = group_ranksums,
                     n = group_n,
                     rankmeans = group_rankmeans)
ngroups <- nrow(groups)

rank_msq <- function(strategy){
    strategy['ranksum']^2/ strategy['n']
}

H <- 12/(N*(N+1))*sum(apply(groups, 1, rank_msq)) - 3*(N+1)
kw_p <- 1 - pchisq(H, df=ngroups-1)

#c
kruskal.test(experiment$INTEND.0 ~ experiment$media, data = experiment)

#d 要懂結果有甚麼意思 找不到老師要的FSA套件，於是跳過執行，僅列出code
#install.packages(FSA)
#library(FSA)
#dunnTest(experiment$INTEND.0 ~ experiment$media, data = experiment)






