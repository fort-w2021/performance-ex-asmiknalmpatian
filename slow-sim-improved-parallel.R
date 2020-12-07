# Function purpose: Simulates the distribution of estimated regression
# coefficients in a linear model with t-distributed error terms

# Inputs: 
# reps: how many replications (models to fit)
# seed: for reproducibility
# data
# true coef: true effects to be estimated
# df: degrees of freedom for t-distribution

# Output: 
# A matrix with reps times estimated coefficients 

simulate_improved <- function(reps, seed, data, true_coef = 0:ncol(data), df = 4) {
  # Missing inputchecks added
  
  checkmate::assert_count(reps)
  checkmate::assert_count(seed)
  checkmate::assert_count(df)
  checkmate::assert_data_frame(data)
  checkmate::assert_numeric(true_coef, len = 1 + ncol(data))
  
  set.seed(seed)
  
  expected <- get_linear_predictor(data, true_coef)
  
  coefs <- matrix(nrow = 1 + ncol(data), ncol = reps)

  parallel_future_busier <- function(reps) {
    future.apply::future_apply(coefs, 2, function(x){simulate_once_improved(expected, data, df)})
  }
  future::plan("multiprocess", workers = 3)

  return(structure(coefs, seed = seed))
}

  
coef_to_matrix <- function(rep) {
  coefs[, rep] <- simulate_once_improved(expected, data, df)
  coefs[, rep]
}

get_linear_predictor <- function(data, true_coef){
  design <- model.matrix(~ ., data = data)
  expected <- design %*% true_coef
  expected
}


simulate_once_improved <- function(expected, data, df) {
  response <- simulate_response_improved(expected, data, df)
  estimate_coef_improved(data, response)
}


simulate_response_improved <- function(expected, data, df) {
  response <- expected + rt(nrow(data), df = df)
  response
}

estimate_coef_improved <- function(data, response) {
  model <- lm(response ~ ., data = data)
  unname(coef(model))
}

