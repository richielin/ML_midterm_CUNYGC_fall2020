

## synthetic dataset generator, with 3 parameters:
### number of classes of each variale (not appliable to Y)
### probablity of each class in each variable
### total number of observations

set.seed(78901)

ob = 3000000

# gen <- function(p = c(), k = length(p), ob) {
#   if (k <= 0) {
#     print("k = 0")
#     break
#   }
#   
#   for (i in 1:ob) {
#     
#   }
# }



y <- sample(x = c(1:4), size = ob, prob = c(.2, .2, .4, .2), replace = T)
x1 <- sample(x = c(1:2), size = ob, prob = c(.3, .7), replace = T)
x2 <- sample(x = c(1:6), size = ob, prob = c(.1, .1, .3, .2,.1,.2), replace = T)
x3 <- sample(x = c(1:3), size = ob, prob = c(.1, .1 ,.8), replace = T)
x4 <- sample(x = c(1:3), size = ob, prob = c(.6, .2, .2), replace = T)
x5 <- sample(x = c(1:4), size = ob, prob = c(.2, .2, .2, .4), replace = T)
x6 <- sample(x = c(1:2), size = ob, prob = c(.3, .7), replace = T)


df <- data.frame(y, x1, x2, x3, x4, x5, x6)

econ_gain <-  diag(length(unique(y)))


# econ_gain <- matrix(
#     c(2,2,2,2,2,1,
#       0,2,-1,0,0,1,
#       3,0,3,0,0,3,
#       1,1,0,0,1,0,
#       3,0,1,1,0,2,
#       0,0,1,0,1,0), ncol = 6, nrow = 6, byrow = T)





### version 1

## calculate prior prob_d and prob_c


fun_prob <- function(df){
  df$x_index <- apply(df[, grep( "y|index", names(df), invert = T)],MARGIN = 1, paste, collapse = ".")
  
  ob <- nrow(df) 

  df_cnt <- as.data.frame(table( df$y, df$x_index))
  df_cnt <-  data.frame(df_cnt, prob_d_c = rep(NA, nrow(df_cnt)))
  names(df_cnt) <- c("y", "x_index", "cnt", "prob_d_c")


  for (i in unique(df_cnt$y)) {
    df_cnt[df_cnt$y ==i, "prob_c"] <- sum(df_cnt[df_cnt$y ==i, "cnt"]) / ob
    for (j in unique(df_cnt$x_index)){
      # df_cnt[df_cnt$x_index ==j, "prob_d"] <- sum(df_cnt[df_cnt$x_index ==j, "cnt"]) / ob
      df_cnt[df_cnt$x_index ==j, "prob_d_c"] <- sum(df_cnt[df_cnt$x_index ==j, "cnt"]) / sum(df_cnt[df_cnt$y ==i, "cnt"])
    }
  }


  out <- df_cnt[order(df_cnt[,"x_index"], df_cnt[,"y"]),]
  rownames(out) <-  1:nrow(out)

  return(out)
}

## testing set



fun_test <- function(df_test, prior_df = prior_df, show_e = F) {
  
  test_meta <- fun_prob(df_test)
  
  result <- data.frame()
  for (i in test_meta$x_index) {
    tmp_set_denom <- test_meta[test_meta$x_index == i,]
    denom <- sum(tmp_set_denom$prob_d_c * tmp_set_denom$prob_c)
    
    out <- data.frame()
    for (j in unique(test_meta$y)) {
      tmp_set_numer <- prior_df[prior_df$y == j & prior_df$x_index == i, ]
      numer <- tmp_set_numer$prob_d_c * tmp_set_numer$prob_c
      
      PT_C_coma_D <- numer / denom
      
      out <- rbind(out, data.frame(i, j, PT_C_coma_D))
    }
    
    out <- out[order(out$j),]
    e_out<- colSums(out$PT_C_coma_D * econ_gain)
    e <- max(e_out)
    asgn_class <- which(e_out == max(e_out))
    true_class <- which(tmp_set_denom$cnt == max(tmp_set_denom$cnt))
    
    # print(data.frame(i, e,asgn_class))
    
    result <- rbind(result, data.frame(i, e, asgn_class, true_class))
    
    }
   result <- result[!duplicated(result),]
   
   names(result) <- c("x_index", "e", "asgn_class", "true_class")
  
   E = sum(result$e)
   
   
   if (show_e){
     return(E)
   } else {
     return(result) 
   }

}



prior_df = fun_prob(df_train)


# fun_test(df_test, show_result = T)


## Optimization

iter <- 12

for (i in 1:iter) {
  if (i ==  1) {
    prior_df = fun_prob(df_train)
  } else {
    delta <- 0.02
    delta_set <-
      fun_test(df_test = df_test,
               prior_df = prior_df,
               show_e = F)
    
    prior_df <-
      merge(
        prior_df,
        delta_set[delta_set$asgn_class != delta_set$true_class,],
        by.x = c("x_index", "y"),
        by.y = c("x_index", "true_class"),
        all.x = T
      )
    
    prior_df$prob_d_c[!is.na(prior_df$asgn_class)] <-
      prior_df$prob_d_c[!is.na(prior_df$asgn_class)] + delta
    
    prior_df <-
      prior_df[, c('x_index', 'y', "cnt" , "prob_d_c", "prob_c")]
    
    for (j in unique(prior_df$y)) {
      prior_df[prior_df$y == j, "prob_d_c"] <-
        prior_df[prior_df$y == j, "prob_d_c"] / sum(prior_df[prior_df$y == j, "prob_d_c"])
    }
    
  }

  # if (i < n_fold) {
  #   show_E = T
  # } else{
  #   show_E = F
  # }
  
  print(fun_test(
    df_test = df_test,
    prior_df = prior_df,
    show_e = T
  ))
}





## CV
n_fold = 10
for (i in 1:n_fold) {
  test_index <-
    ((i - 1) * 1 / n_fold * nrow(df) + 1):((i) * 1 / n_fold * nrow(df))
  
  df_test <- df[test_index, ]
  df_train <- df[-test_index , ]
  print(fun_test(
    df_test = df_test,
    prior_df = prior_df,
    show_e = T
  ))
}













