#Question 1
library(data.table)
setwd("C:/Users/eason/Desktop/清大 BACS/資料")
ac_bundles_dt <- fread("piccollage_accounts_bundles.csv")
ac_bundles_matrix <- as.matrix(ac_bundles_dt[, -1, with=FALSE])

#a1

#a2 這個只是intuitive guess/ grep用法看看好用好用
terms <- "mom|mother|dad|father|baba|mama"
bundle_names <- colnames(ac_bundles_matrix)
grep(terms, bundle_names, ignore.case=TRUE, value=TRUE)

#b1-1,2 整段從老師解答那邊偷 完全弄不出來/老師在課本裡用cosine/在這裡用qlcMatrix因為比較快
install.packages("qlcMatrix")  
library(qlcMatrix)

cosine_recos <- function(items_matrix){
    cos_sim_matrix <- qlcMatrix::cosSparse(items_matrix)
    #Removing self-recommendation: Make diagonals ‘2.00’ 
    #diag(…) – get or set the diagonals of a matrix
    diag(cos_sim_matrix) <- 2
    #Sort recommendations for each row
    #Remove self-recommendations (largest value)
    #Return only five recommendations
    row_recos <- function(cos_sim_row){
        names(sort(cos_sim_row, decreasing=TRUE))
    }
    all_recos <- t(apply(cos_sim_matrix, 2, row_recos))
    #Remove first column from recommendation matrix
    final_recos <- all_recos[, -1]
    return(final_recos[, 1:5])
}

#b1-3 Cosine Similarity Recommendations
recos_cos <- cosine_recos(ac_bundles_matrix)
recos_cos["sweetmothersday", ]

#b2 Correlation-based Recommendations 跟b3比可以發現著重的點不同
#each number becomes relative to the average usage of a given bundle
bundle_means <- apply(ac_bundles_matrix, 2, means)
bundle_means_matrix <- t(replicate(nrow(ac_bundles_matrix), bundle_means))
ac_bundles_mc_b <- ac_bundles_matrix - bundle_means_matrix

recos_cor <- cosine_recos(ac_bundles_mc_b)
recos_cos["sweetmothersday", ]

#b3 Adjusted-Cosine Recommendations
#each number becomes relative to the average usage of a particular account
account_means <- apply(ac_bundles_matrix, 1, mean)
account_means_matrix <- replicate(ncol(ac_bundles_matrix), account_means)
ac_bundles_mc_ac <- ac_bundles_matrix - account_means_matrix

recos_adj_cos <- cosine_recos(ac_bundles_mc_ac)
recos_adj_cos["sweetmothersday", ]

#c

#e

#Question 2 
#a,b,c,d,e,f 就玩玩看/e,f可以想一下/我也不確定ef在甚麼情況下比較好
interactive_regression <- function() {
    cat("Click on the plot to create data points; hit [esc] to stop")
    plot(NA, xlim=c(-5,50), ylim=c(-5,50))
    points = data.frame()
    repeat {
        click_loc <- locator(1)
        if (is.null(click_loc)) break
        if(nrow(points) == 0 ) {
            points <- data.frame(x=click_loc$x, y=click_loc$y)
        } else {
            points <- rbind(points, c(click_loc$x, click_loc$y))
        }
        plot(points, xlim=c(-5,50), ylim=c(-5,50), pch=19, cex=2, col="gray")
        if (nrow(points) < 2) next
        
        model <- lm(points$y ~ points$x)
        abline(model, lwd=2, col="cornflowerblue")
        text(1, 50, paste(c("Raw intercept: ", round(model$coefficients[1], 2)), collapse=" "))
        text(1, 45, paste(c("Raw slope    : ", round(model$coefficients[2], 2)), collapse=" "))
        text(1, 40, paste(c("Correlation  : ", round(cor(points$x, points$y), 2)), collapse=" "))
    }
    
    return(points)
}

interactive_regression()

#g
pts <- interactive_regression() 
summary( lm( pts$y ~ pts$x ))
cor(pts)


