#IMPORT
library(MASS)

# CONST
OBSERVATIONS = 1000
ALPHA = 0.05

#Functions
create_sample <- function(sample1, sample2, main_func){
  if (main_func == 'ODD'){
    states = rep(1, length(sample1))
    df <- data.frame(sample1, sample2, states)
    
    x_positive = df$sample1 >= 0
    y_positive = df$sample2 >= 0
    
    df[x_positive & y_positive, ]$states = 1
    df[!x_positive & y_positive, ]$states = 2
    df[!x_positive & !y_positive, ]$states = 3
    df[x_positive & !y_positive, ]$states = 4
    return(df)
  } else if(main_func == 'pTDD'){
    states = rep(1, length(sample1))
    df <- data.frame(sample1, sample2, states)
    
    x_positive = df$sample1 >= 0
    y_positive = df$sample2 >= 0
    df[x_positive & y_positive, ]$states = 1
    df[!x_positive & y_positive, ]$states = 2
    df[!x_positive & !y_positive, ]$states = 3
    df[x_positive & !y_positive, ]$states = 4
    return(df)
  } else if (typeof(main_func) == "double") {
    states = rep(main_func, length(sample1))
    df <- data.frame(sample1, sample2, states)
    return(df)
  } else {
    cat("ValueError: Wrong method name", "\n")
  }
}
TDD <- function(group_by){
  # mus
  mu1 <- 0
  mu2 <- 0
  # sigmas
  sigma1 <- 1
  sigma2 <- 3
  # corr. coef.
  rho <- 0.7
  
  # create the variance covariance matrix
  sigmas<-rbind( c( sigma1, rho*sqrt(sigma1)*sqrt(sigma2) ),
                 c( rho*sqrt(sigma1)*sqrt(sigma2), sigma2 ))
  
  # generate the multivariate normal distribution
  Z1 <- mvrnorm(n = OBSERVATIONS, mu = c(mu1, mu2), Sigma = sigmas)
  Z2 <- mvrnorm(n = OBSERVATIONS, mu = c(mu1, mu2), Sigma = sigmas)
  
  cor_test = cor.test(Z1,Z2)
  #cat("Corr. coef: ", cor_test$estimate, "\n")
  corr_not_equals_zero = cor_test$p.value < ALPHA
  cat("True correlation is not equal to 0: ",
      corr_not_equals_zero, "\n")
  
  if (group_by == "samples"){
    df1 <- create_sample(Z1[, 1], Z1[, 2], 1)
    df2 <- create_sample(Z2[, 1], Z2[, 2], 2)
  } else if(group_by == "pTDD"){
    df1 <- create_sample(Z1[, 1], Z1[, 2], "pTDD")
    df2 <- create_sample(Z2[, 1], Z2[, 2], "pTDD")
  } else {
    cat ("ValueError: wrong no such group factor")
  }
  
  library(plotly)
  fig <- plot_ly(data = df1, x = ~df1$sample1, y = ~df1$sample2,
                 type = 'scatter', mode = 'markers',
                 color = ~states, colors=c("blue", "red", "blue", "red"))%>%
    add_trace(data = df2, x = ~df2$sample1, y = ~df2$sample2,
              type = 'scatter', mode = 'markers'
              ,color = ~states, colors=c("blue", "red", "blue", "red"), showlegend = F)%>%
    layout(title= list(text = paste0("Rxy=", rho)), font=t,
           yaxis = list(title = list(text ='Y')),
           xaxis = list(title = list(text ='X')),
           plot_bgcolor='#e5ecf6')%>% 
    hide_colorbar()
  fig
}
ODD <- function(corr_func){
  # mus
  mu1 <- 0
  mu2 <- 0
  # sigmas
  sigma1 <- 1
  sigma2 <- 3
  # corr. coef.
  rho <- 0.1
  
  # create the variance covariance matrix
  sigmas<-rbind( c( sigma1, rho*sqrt(sigma1)*sqrt(sigma2) ),
                 c( rho*sqrt(sigma1)*sqrt(sigma2), sigma2 ))
  
  BND <- mvrnorm(n = OBSERVATIONS, mu = c(mu1, mu2), Sigma = sigmas)
  
  X <- BND[, 1]
  Y <- BND[, 2]
  
  Z <- create_sample(X, Y, "ODD")
  
  ###
  #Article on which calculation based on:
  #http://statistica.ru/theory/znachimost-koeffitsienta-korrelyatsii-doveritelnyy-interval/
  ###
  if (corr_func == 'cor.test'){
    cor_test = cor.test(X,Y)
    cat("Corr. coef: ", cor_test$estimate, "\n")
    corr_not_equals_zero = cor_test$p.value < ALPHA
    cat("True correlation is not equal to 0: ",
        corr_not_equals_zero, "\n")
  } else if(corr_func == 'cor'){
    r <- cor(X, Y, method = "pearson")
    cat("Corr. coef: ", r, "\n")
    t_value = r * sqrt( ( (OBSERVATIONS - 2) / ( 1 - r**2 ) ) )
    t_critical = qt(p = ALPHA/2, df = OBSERVATIONS - 2, lower.tail=FALSE)
    
    # H0 corr. coef == 0
    corr_coef_equals_zero = abs(t_value) < t_critical
    cat("Corr. coef. is statisticly significant: ",
        !corr_coef_equals_zero, "\n")
  }
  else{
    cat("ValueError: Wrong method name to calculate correlation!", "\n")
  }
  
  library(plotly)
  fig <- plot_ly(data = Z, x = ~Z$sample1, y = ~Z$sample2,
                 type = 'scatter', mode = 'markers',
                 color = ~states, colors=c('blue', 'red', 'blue', 'red'))%>%
    hide_colorbar()%>% 
    layout(title= list(text = paste0("Rxy=", rho)), font=t,
           yaxis = list(title = list(text ='Y')),
           xaxis = list(title = list(text ='X')),
           plot_bgcolor='#e5ecf6')
  fig
}
#TDD('pTDD')

lab_2_2 <- function(){
  ODD('cor.test')
}

lab_2_2()

