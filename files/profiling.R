# test function
tmp_data <- make_sim_data(n = 100, p = 10, q = 5, b = 0.2, sigma = 1, mc = "med")

# split data into 5 folds
kfolds <- rsample::vfold_cv(data = tmp_data, v = 5, repeats = 1)

# setup temporary file
utils::Rprof(tmp <- tempfile())

# run code to be profiled

# loop through each iteration to work on each set of kfolds
simulation$results_cv <- NA
for (j in 1:1) {
  
  # extract resample dataframe
  kfolds = simulation[j, "kfolds"]$kfolds[[1]]
  
  # fit models for k-fold cross-validation
  # calculate predictions for each fold
  # summarize cross-validation results
  preds = kfolds$splits %>% map(\(split) holdout_results(split, formula(y ~ -1 + .)))
  results = preds %>% map(function(df) {
    data.frame(rmse = yardstick::rmse_vec(truth = df$y,estimate = df$`.fitted`),
               rsq = yardstick::rsq_vec(truth = df$y, estimate = df$`.fitted`))
  })
  simulation[j, "results_cv"] <- nest(
    results %>% 
      bind_rows %>% 
      summarize(across(everything(), mean))
  )
}

# close profiling
Rprof()

# view results
summaryRprof(tmp)

# delete temporary file
unlink(tmp)
