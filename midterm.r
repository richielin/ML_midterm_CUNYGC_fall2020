

## synthetic dataset generator, with 3 parameters:
### number of classes of each variale (not appliable to Y)
### probablity of each class in each variable
### total number of observations

set.seed(7890)

P = c(0.5, 0.5)
k = length(p)
ob = 100000

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



y <- sample(x = c(1:4), size = ob, prob = c(.25, .25, .2, .3), replace = T)
x1 <- sample(x = c(1:2), size = ob, prob = c(.3, .7), replace = T)
x2 <- sample(x = c(1:6), size = ob, prob = c(.1, .1, .3, .2,.1,.2), replace = T)
x3 <- sample(x = c(1:3), size = ob, prob = c(.1, .1 ,.8), replace = T)
x4 <- sample(x = c(1:3), size = ob, prob = c(.6, .2, .2), replace = T)
x5 <- sample(x = c(1:4), size = ob, prob = c(.2, .2, .2, .4), replace = T)
x6 <- sample(x = c(1:2), size = ob, prob = c(.3, .7), replace = T)


df <- data.frame(y, x1, x2, x3, x4, x5, x6)

train_index<- sort(sample(nrow(df), nrow(df) * 0.8))

df_train <- df[train_index , ]
df_test <- df[- train_index, ]



## version 2

fun_prior <- function (df) {
  output <- data.frame()
  
  for (i in colnames(df)) {
    for (j in unique(df[, i])) {
      if (i == "y") {
        y_index = paste(i, j, sep = ".")
        x_index = "n.ob"
        cnt = length(df[df[, i] == j, i])
        prob = cnt / nrow(df)
        out = data.frame(y_index, x_index, cnt, prob)
        output = rbind(output, out)
        
      } else {
        y_index = "n.ob"
        x_index = paste(i, j, sep = ".")
        cnt = length(df[df[, i] == j, i])
        prob = cnt / nrow(df)
        out = data.frame(y_index, x_index, cnt, prob)
        output = rbind(output, out)
        
        for (k in unique(df[, "y"])) {
          y_index = paste('y', k, sep = ".")
          x_index = paste(i, j, sep = ".")
          cnt = length(df[df[, i] == j & df[, "y"] == k, i])
          prob = cnt / length(df[df[, "y"] == k, "y"])
          out = data.frame(y_index, x_index, cnt, prob)
          output = rbind(output, out)
        }
      }
    }
  }
  return(output)
}

prior <- fun_prior(df_train)


fun_test_prob <- function(df_test = df_test) {
  test_prob <- data.frame()
  
  for (i in colnames(df_test[, grep("y", names(df_test), invert = T)])) {
    prob <- as.data.frame(table(df_test[i]) /  nrow(df_test))
    names(prob) <- c('ind', 'prob')
    prob$ind <- paste(i, prob$ind, sep = '.')  
    test_prob <- rbind(test_prob, prob)
  }
  return(test_prob)
}

test_prob <- fun_test_prob(df_test[,grep("y", names(df_test),invert = T )])



fun_bayes  <- function (t_val, prior, test_prob, all_y_class = F) {
  
  output <- data.frame()
  
  for (l in unique(prior$y_index)) {
    if (l == "n.ob") {
    }
    else{
      df <- prior[prior$y_index == l,]
      numer_ls <-
        c(df[df$x_index %in% t_val, 'prob'], df[df$x_index == 'n.ob', 'prob'])
      numer <- 1
      for (m in numer_ls) {
        numer <- numer * m
      }
      numer
      
      denom_ls <- test_prob[test_prob$ind %in% t_val, 'prob']
      denom <- 1
      for (m in denom_ls) {
        denom <- denom * m
      }
      denom
      
      prob_y = numer / denom
      output <- rbind(output, data.frame(l, prob_y))
    }
  }
  
  names(output) <- c("y_class", "y_prob")
  
  if (all_y_class == F) {
    output <- as.character(output[output$y_prob == max(output$y_prob), 'y_class'])
    output <- gsub("y.", "", output)
  }
  
  return(output)
}

t_val <- paste(names(df_test[1,]), df_test[1,], sep = ".")
fun_bayes(t_val = t_val, prior = prior, test_prob = test_prob,   all_y_class = F)



for (i in 1:nrow(df_test)) {
  if (i == 1){
    y_pred <- c()
  }
  
  t_val <-
    paste(names(df_test[i, grep("y", names(df_test), invert = T)]), df_test[i, grep("y", names(df_test), invert = T)], sep = ".")
  
  y_pred <- c(y_pred, fun_bayes(
    t_val = t_val,
    prior = prior,
    test_prob = test_prob,
    all_y_class = F
  ))
}

table(y_pred, df_test$y)
















### version 1

## calculate prior prob_d and prob_c


# fun_prior <- function(df){
#   df$x_index <- apply(df[, grep( "y|index", names(df), invert = T)],MARGIN = 1, paste, collapse = ".")
#   
#   df_cnt <- as.data.frame(table( df$y, df$x_index))
#   df_cnt <-  data.frame(df_cnt, prob_d_c = rep(NA, nrow(df_cnt)))
#   names(df_cnt) <- c("y", "x_index", "cnt", "prob_d_c")
#   
#   
#   for (i in unique(df_cnt$y)) {
#     df_cnt[df_cnt$y ==i, "prob_c"] <- sum(df_cnt[df_cnt$y ==i, "cnt"]) / ob
#     for (j in unique(df_cnt$x_index)){
#       df_cnt[df_cnt$x_index ==j, "prob_d"] <- sum(df_cnt[df_cnt$x_index ==j, "cnt"]) / ob
#       df_cnt[df_cnt$x_index ==j, "prob_d_c"] <- sum(df_cnt[df_cnt$x_index ==j, "cnt"]) / sum(df_cnt[df_cnt$y ==i, "cnt"])
#     }
#   }
#   
# 
#   out <- df_cnt[order(df_cnt[,"y"], df_cnt[,"x_index"]),]
#   rownames(out) <-  1:nrow(out)
#   
#   return(out)
# }
# 
# 
# train_prior<- fun_prior(df_train)
# 
# View(train_prior)
#  
# train_prior[2,] 
# 
# 
# ## testing set
# 
# 
# fun_test <- function(df_test, prior_df) {
#   df_test$x_index <- apply(df_test[, grep( "y|index", names(df), invert = T)],MARGIN = 1, paste, collapse = ".")
#   
#   pred<- c()
#   
#   for (i in 1 :nrow(df_test)){
#     x <- df_test[i, "x_index"]
#     
#     x_set <- prior_df[prior_df$x_index == x, ]
#     
#     x_set$prob_c_d <- x_set$prob_d_c * x_set$prob_c / x_set$prob_d 
#     
#     pred <- c(pred, x_set[which(x_set$prob_c_d == max(x_set$prob_c_d)), "y"])
#     
#   }
#   
#   out <- cbind(df_test, pred)
#   
#   return(out)
# }
# 
# 
# result <- fun_test(df_test, train_prior)
# 
# table(result$y, result$pred)
# 
# 
# 
# for (i in unique(df_cnt$y)) {
#   df_cnt[df_cnt$y ==i, "prob_d_c"] <- df_cnt[df_cnt$y ==i, "cnt"] / sum(df_cnt[df_cnt$y ==i, "cnt"])
# }
# 
#  
# ( train_prior$prob_d_c[217] * train_prior$prob_c[217] ) /train_prior$prob_d[2] 






