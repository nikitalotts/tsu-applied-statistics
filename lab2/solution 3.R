#CONST
ALPHA = 0.05

#1.
RENT_DATA <- read.csv("./Rent 22 01 11.csv",
                      header = TRUE, sep = ";")

#2.
sapply(RENT_DATA, typeof)

# rent - numeric
# m2 - numeric
# floor - order
# rooms - order
# walls - categorical(binary)
# district - categorical


transformed_data = transform(RENT_DATA, 
                             m2 = as.numeric(gsub(",", ".", gsub("\\.", "", m2))),
                             district = as.character(district),
                             walls = as.logical(walls)
                             )

str(transformed_data)

#3. numeric features
num_feat_func <- function(data, columns){
  for (column in columns){
    curr_col = column
    x11()
    lab = "monthly payment, rub"
    if (column != "rent") lab = "square, m2"
    hist(data[, c(curr_col)], prob = TRUE,
         col=rgb(0,0,1,1/4), main=paste("Histogram of ", curr_col), xlab = lab)
    lines(density(data[, c(curr_col)]), lwd = 2, col = "red")

    #4.    
    ###
    #https://www.geeksforgeeks.org/shapiro-wilk-test-in-r-programming/
    ###
    # H0 - has nomal dist if p > alpha it's accepted
    cat(curr_col, " has normal distribution: ", shapiro.test(data[, c(curr_col)])$p.value > ALPHA, "\n") 
  }
}

num_feat_func(transformed_data, c("rent", "m2"))


#5. categorical features
cat_feat_func <- function(data, y_col, x_col){
  x11()
  boxplot(
    data[, c(y_col)] ~ data[, c(x_col)],
          xlab = x_col,
          ylab = y_col,
          col = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4))
  )
  
  group_amount = length(unique(transformed_data[, c(x_col)]))
  
  if (group_amount == 2){
    #cat("Mann–Whitney test, the difference between means of " , y_col , " and ", x_col,  " are significant: ", 
    cat("Mann–Whitney test, ranked distribution of " , y_col , " and ", x_col,  " are approximately equal: ", 
        wilcox.test(data[, c(y_col)] ~ data[, c(x_col)])$p.value < ALPHA, "\n")
  } else if (group_amount > 2){
    cat("Kruskal–Wallis ANOVA, mean ranks of the ", y_col , " and ", x_col,  " are the same ", 
        kruskal.test(data[, c(y_col)] ~ data[, c(x_col)])$p.value > ALPHA, "\n")
  } else {
    cat("Error in tests!")
  }
}

call_cat_feat_func <- function(){
  cat_feat_func(transformed_data, "m2", "walls")
  cat_feat_func(transformed_data, "rent", "walls")
  cat_feat_func(transformed_data, "m2", "district")
  cat_feat_func(transformed_data, "rent", "district")
}

call_cat_feat_func()

#6. Remove outliers
remove_outliers_by_groups <- function(data, num_feature, cat_feature){
  source_dim = dim(data)
  #cat(source_dim)
  list_quantiles <- tapply(data[, c(num_feature)], data[, c(cat_feature)], quantile)

  Q1s <- sapply(1:dim(list_quantiles), function(i) list_quantiles[[i]][2])
  Q3s <- sapply(1:dim(list_quantiles), function(i) list_quantiles[[i]][4])
  IQRs <- tapply(data[, c(num_feature)], data[, c(cat_feature)], IQR)
  
  Lowers <- Q1s - 1.5*IQRs
  Uppers <- Q3s + 1.5*IQRs
  
  splited_data <- split(data, data[, c(cat_feature)])
  
  data_no_outlier <- NULL
  for (i in 1:dim(list_quantiles)){
    out <- subset(splited_data[[i]], splited_data[[i]][, c(num_feature)] > Lowers[i] 
                  & splited_data[[i]][, c(num_feature)] < Uppers[i])
    data_no_outlier <- rbind(data_no_outlier, out)
  }
  result_dim = dim(data_no_outlier)
  #cat(result_dim)
  #cat("Removed ", source_dim[[1]] - result_dim[[1]], " rows. \n")
  
  return(data_no_outlier)
}

transformed_data_no_outliers = remove_outliers_by_groups(transformed_data, "rent", "district")

cat_feat_func(transformed_data_no_outliers, "rent", "district")

#7. Numeric and orders
num_order_corrs <- function(source_data, columns){
  #install.packages("Hmisc")
  library("Hmisc")
  
  num_and_order_columns = columns 
  check_corr_data = source_data[, num_and_order_columns]
  
  #install.packages("corrplot")
  library("corrplot")
  rcorr_result = rcorr(as.matrix(check_corr_data))
  corr_matrix = rcorr_result$r
  p_value_matrix = rcorr_result$P
  
  #visualize corr. matrix
  x11()
  corrplot(corr_matrix, method = 'number')
  
  library(dplyr)
  p_value_matrix <- p_value_matrix %>% replace(is.na(.), 0)
  
  #show signicance of corr. coefs
  x11()
  corrplot(p_value_matrix <= ALPHA, diag = FALSE, 
           method = 'number', bg = "lightgrey", title="Are corr. significant")
}

num_order_corrs(transformed_data_no_outliers, c("rent", "m2", "floor", "rooms"))

#8. Categorical features
cat_feat_tab <- function(data, col1, col2){
  library("gt")
  library(grid)
  library(gridExtra)
  x11()
  
  tab = table(transformed_data_no_outliers[, c(col1)], transformed_data_no_outliers[, c(col2)])
  grid.table(tab)
  chisquare_test = chisq.test(tab)$p.value < ALPHA
  #fisher_test = fisher.test(tab)$p.value < ALPHA
  
  cat("Chisq. test, the variables are independent, there is no relationship between ", col1, " and ", col2, "", chisquare_test, "\n")
  #cat("Fisher test, the variables are independent, there is no relationship between ", col1, " and ", col2, "", fisher_test, "\n")
  install.packages("rcompanion")
  library("rcompanion")
  cramerV(tab, ci=TRUE)
}

install.packages("rcompanion")
library("rcompanion")

cat_feat_tab(transformed_data_no_outliers, "walls", "district")


