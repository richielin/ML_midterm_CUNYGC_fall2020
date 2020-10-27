

## synthetic dataset generator, with 3 parameters:
### number of classes of each variale (not appliable to Y)
### probablity of each class in each variable
### total number of observations

## random number generater
rand_gen <- function(Y, size) {
  # generate a uniform distribution from 0 to 1, of Y number of elements
  q_Y = runif(Y)
  
  # normalize the probability
  q_Y = q_Y / sum(q_Y)
  
  # loop through the distribution, generate the cummulative distribution
  for (i in 1:length(q_Y)) {
    if (i == 1) {
      p_Y = q_Y[i]
    } else {
      p_Y = c(p_Y, p_Y[length(p_Y)] + q_Y[i])
    }
  }
  
  # generate another uniform distribution from 0 to 1, of the number of elements defined as variable size
  d = runif(size)
  out = c()
  
  # loop through each of the elements, compare with the elements of the cummulative disrtribution of p_Y
  for (i in 1:size) {
    for (j in 1:length(p_Y)) {
      p1 = ifelse(j ==1, 0, p_Y[j - 1])
      p2 = p_Y[j]
      
      if (p1 < d[i] &  d[i] <= p2) {
        # assign the class based on the comparison result
        n_class = j
        break
      }
    }
    # stack the distribution into a array
    out = c(out, n_class)
  }
  
  output <- list(q_Y, out)
  return(output)
}



## The random array generate works very well, except the performance was not as good when generating array of more than 1 million elements. 
## due to the performance issue, I am going to the system preset unifrom randam number generater

## set the random seed
set.seed(78901)

## set the number of observation as 3 millions
ob = 3000000

## generate the class y and the measurement space of Xs, each of the X will be one feature in the model and will be combined as tuples in the calculation
y <- sample(x = c(1:4), size = ob, 
            prob = c(0.25, 0.25, 0.2, 0.3), replace = T)
x1 <- sample(x = c(1:2), size = ob, 
             prob = c(.3, .7), replace = T)
x2 <- sample(x = c(1:6), size = ob, 
             prob = c(.1, .1, .3, .2,.1,.2), replace = T)
x3 <- sample(x = c(1:3), size = ob, 
             prob = c(.1, .19 ,.71), replace = T)
x4 <- sample(x = c(1:3), size = ob, 
             prob = c(.57, .23, .2), replace = T)
x5 <- sample(x = c(1:4), size = ob, 
             prob = c(.24, .13, .27, .36), replace = T)
x6 <- sample(x = c(1:2), size = ob, 
             prob = c(.3, .7), replace = T)


df <- data.frame(y, x1, x2, x3, x4, x5, x6)

## generate a economic gain matrix. by defult it is made to be a identity matrix
# econ_gain <-  diag(length(unique(y)))

## or for more complex simulation, made a 6 * 6 matrix with random selected econ gain scores
econ_gain <- matrix(
    c(1.2,0,0,0,
      1.2,1,0,0,
      1.2,0,1,0,
      1.2,0,0,1), ncol = 4, nrow = 4, byrow = T)






## calculate prior prob_d and prob_c

## build a function to calculate prior probability 
fun_prob <- function(df){
  
  # combine all variables into a measurement tuple named
  df$x_tuple <- apply(df[, grep( "y|tuple", names(df), invert = T)],MARGIN = 1, paste, collapse = ".")
  
  ob <- nrow(df) 
  
  # use the table function in R to generate the count of each given y, 
  df_cnt <- as.data.frame(table( df$y, df$x_tuple))
  df_cnt <-  data.frame(df_cnt, prob_d_c = rep(NA, nrow(df_cnt)))
  names(df_cnt) <- c("y", "x_tuple", "cnt", "prob_d_c")

  # calculate the prior probability of given class
  for (i in unique(df_cnt$y)) {
    df_cnt[df_cnt$y ==i, "prob_c"] <- sum(df_cnt[df_cnt$y ==i, "cnt"]) / ob
    for (j in unique(df_cnt$x_tuple)){
      df_cnt[df_cnt$x_tuple ==j, "prob_d_c"] <- 
        sum(df_cnt[df_cnt$x_tuple ==j, "cnt"]) / sum(df_cnt[df_cnt$y ==i, "cnt"])
    }
  }

  
  out <- df_cnt[order(df_cnt[,"x_tuple"], df_cnt[,"y"]),]
  rownames(out) <-  1:nrow(out)
  # the output of this prior conditional probability calculation function returns 5 columns:
    ## Y as the true class value
    ## x_tuple as the unique x_tuple
    ## cnt as the count of the observations of each x_tuple in each given y classes
    ## Prob_d_c as the probability of the x_tuple give each classes
    ## Prob_c as the probablity of each class
  return(out)
}

## testing set


fun_test <- function(df_test, prior_df = prior_df, show_e = F) {
  
  ## calculate the condistional probablity of the testing set with the same function
  test_meta <- fun_prob(df_test)
  
  # initiate a empty data frame for the result
  result <- data.frame()
  for (i in test_meta$x_tuple) {
    ## for each unique x tuple, multiply the prob_d_c for all values of class with the the probablity of each value of class to calculate the denominator of the bayes theorom 
    tmp_set_denom <- test_meta[test_meta$x_tuple == i,]
    denom <- sum(tmp_set_denom$prob_d_c * tmp_set_denom$prob_c)
    
    ## for each of the values of the class in the traning set, we will subset the x tuple and desired values of class, multiply them to calculate the numerator of the bayes theorom calculation 
    out <- data.frame()
    for (j in unique(test_meta$y)) {
      tmp_set_numer <- prior_df[prior_df$y == j & prior_df$x_tuple == i, ]
      numer <- tmp_set_numer$prob_d_c * tmp_set_numer$prob_c
      
      PT_C_comma_D <- numer / denom
      
      out <- rbind(out, data.frame(i, j, PT_C_comma_D))
    }
    ## the probablilty of all possible assigning class is calculated in the loop above and stored by stacking the result of all the x tuples into one data fame
    
    out <- out[order(out$j),]
    
    ## we multiply the probability of each possible asignment of a x tuple with the economics gain matrix to build the final bayes rule
    ## the result of each x tuple will be sum together as a evaluation of each possible assignment.
    e_out<- colSums(out$PT_C_comma_D * econ_gain)
    ## the assignment with the maximized economic gain will be chosen as the bayes rule class assignment
    e <- max(e_out)
    asgn_class <- which(e_out == max(e_out))
    
    ##the true class that have the highest probabilty in each x tuple will also be saved for the optimization process. 
    true_class <- which(tmp_set_denom$cnt == max(tmp_set_denom$cnt))
    
    # print(data.frame(i, e,asgn_class))
    
    
    # then we stack the result of each x tuple for the final output
    result <- rbind(result, data.frame(i, e, asgn_class, true_class))
    
    }
   result <- result[!duplicated(result),]
   
   names(result) <- c("x_tuple", "e", "asgn_class", "true_class")
  
   ## the sum of the expected economics gain is calculated for the evaluation and optimization
   E = sum(result$e)
   
   ## adding  an option to show either the result table of the assignment, or the sum of the economics gain
   if (show_e){
     return(E)
   } else {
     return(result) 
   }

}


# Building traning and testing set for one time evaluation

## choosing the number of folds we want to divide the dataset on
n_fold = 10
i <- 1
##build an spliting index based on the number of folds assigned eariler
test_set_index <-
  ((i - 1) * 1 / n_fold * nrow(df) + 1) : 
    ((i) * 1 / n_fold * nrow(df))

## use the index to split the traning and testing set
df_test <- df[test_set_index, ]
df_train <- df[-test_set_index , ]




prior_df = fun_prob(df_train)


# fun_test(df_test, show_result = T)

## Optimization
# set the number of iteration wanted 
iter <- 12

for (i in 1:iter) {
  ## if it is the first round of iteration, set the prior probability to the initial calculation from the training set
  
  ## use the testing function built earlier to build the bayes rule classifier 
  delta_set <-
    fun_test(df_test = df_test,
             prior_df = prior_df,
             show_e = F)
  
  if (i ==  1) {
    prior_df = fun_prob(df_train)
  } else {
    ## set a change margin of the probabilityin the prior condistional probability, as instructed by the professor, it should be less than 0.05
    delta <- 0.02
    
    ## merge the subset the calculated bayes rule result that had a incorrect assignment, then increase the conditional probability of these x tuples by the marginal change delta
    prior_df <-
      merge(
        prior_df,
        delta_set[delta_set$asgn_class == delta_set$true_class,],
        by.x = c("x_tuple", "y"),
        by.y = c("x_tuple", "true_class"),
        all.x = T
      )
    
    prior_df$prob_d_c[!is.na(prior_df$asgn_class)] <-
      prior_df$prob_d_c[!is.na(prior_df$asgn_class)] + delta
    
    prior_df <-
      prior_df[, c('x_tuple', 'y', "cnt" , "prob_d_c", "prob_c")]
    # then normalize the condistional probablity by dividing the sum of all possible values of the class
    for (j in unique(prior_df$y)) {
      prior_df[prior_df$y == j, "prob_d_c"] <-
        prior_df[prior_df$y == j, "prob_d_c"] / sum(prior_df[prior_df$y == j, "prob_d_c"])
    }
    
  }

  result <- list()
  result[[i]] <- delta_set
  
  print(paste0("iteration: ", i))
  print(paste0("Correct assignment rate: ", sum(result[[i]]$asgn_class == result[[i]]$true_class) / nrow(result[[i]])))
  print(paste0("Expected gain: ", sum(result[[i]]$e)))
}




# ## CV
# n_fold = 10
# for (i in 1:n_fold) {
#   test_tuple <-
#     ((i - 1) * 1 / n_fold * nrow(df) + 1):((i) * 1 / n_fold * nrow(df))
#   
#   df_test <- df[test_tuple, ]
#   df_train <- df[-test_tuple , ]
#   print(fun_test(
#     df_test = df_test,
#     prior_df = prior_df,
#     show_e = T
#   ))
# }













