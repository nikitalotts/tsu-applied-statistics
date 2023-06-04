lab_2_1 <-function(){
  dist_func <- function(X1, X2, hist_name){
    cat("#####################################################\n")
    #CONST
    ALPHA = 0.05
    windows.options(width = 20, height = 10, reset = FALSE)
    x11()
    par(mfrow = c(1, 2)) 
    
    ###
    #https://www.geeksforgeeks.org/shapiro-wilk-test-in-r-programming/
    #https://www.statology.org/shapiro-wilk-test-r/ 
    ###
    
    # H0 - has nomal dist if > alpha it's accepted
    #check for normality
    cat("X1 has normal distribution: ", shapiro.test(X1)$p.value > ALPHA, "\n")
    cat("X2 has normal distribution: ", shapiro.test(X2)$p.value > ALPHA, "\n")
    
    ###
    #H0 - the variances are equal
    #if p.val > alpha: var are equal
    ###
    #check for variances equality
    #var.test(X1, X2)
    are_variances_equal = var.test(X1, X2)$p.value > ALPHA
    cat("X1 and X2 has equal variances: ", are_variances_equal, "\n")
    
    ###
    #http://www.sthda.com/english/wiki/one-sample-t-test-in-r
    #Null hypothesis: the data are normally distributed -> p > a: he distribution of the data are not significantly different from normal distribtion
    ###
    #check for mean equality
    are_means_equal = t.test(X1, X2, var.equal = are_variances_equal)$p.value > ALPHA
    cat("X1 and X2 has equal means: ", are_means_equal, "\n")
    
    max_x_range = max(range(X1), range(X2))
    min_x_range = min(range(X1), range(X2))
    max_y_range = max( max( density(X1)$y ), max( density(X2)$y ))
    
    hist(X1, prob = TRUE, xlim=c(min_x_range, max_x_range),
         ylim=c(0, max_y_range+0.1*max_y_range),
         col=rgb(0,0,1,1/4), xlab="", main=hist_name)
    text(x = mean(5),                  
         y = mean(5),
         paste("Mean =", mean(X1)),
         col = "blue",
         cex = 1)
    abline(v = mean(X1),                       
           col = "blue",
           lwd = 2)
    lines(density(X1), lwd = 2, col = "blue")
    
    hist(X2, prob = TRUE, xlim=c(min_x_range, max_x_range),
         col=rgb(1,0,0,1/4), add=TRUE)
    
    legend("topright", c(paste("Mean X1: ", mean(X1)),
                         paste("Mean X2: ", mean(X2))),
           fill=c("blue", "red"), cex=0.5)
    
    abline(v = mean(X2),                       
           col = "red",
           lwd = 2)
    lines(density(X2), lwd = 2, col = "red")
    
    boxplot(X1, 
            X2,
            names = c("First", "Second"), 
            col = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)),
            xlab="Groups")
  }
  
  #3.1.1
  X=rnorm(95, 25, 2) 
  Y=rnorm(95, 25, 2) 
  dist_func(X, Y, 
            "Histogram #1: 
          Дисперсии равны. Нет статистически значимых отличий между средними.")
  
  #3.1.2
  X=rnorm(95, 10, 3) 
  Y=rnorm(95, 15, 3) 
  dist_func(X, Y, 
            "Histogram #2: 
          Дисперсии равны. Есть статистически значимые отличия между средними.")
  
  #3.1.3
  X=rnorm(95, 12, 1) 
  Y=rnorm(95, 12, 3) 
  dist_func(X, Y, "Histogram #3: 
          Дисперсии неравны. Нет статистически значимых отличий между средними.")
  
  #3.1.4
  X=rnorm(95, 10, 5) 
  Y=rnorm(95, 15, 10) 
  dist_func(X, Y, "Histogram #4: 
          Дисперсии неравны. Есть статистически значимые отличия между средними.")
  
}

lab_2_1()
