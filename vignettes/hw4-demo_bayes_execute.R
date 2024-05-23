rm(list=ls())
set.seed(10)
library(UWBiost561)

imp_numbers <- 1:25
trials <- 5
alpha_vec <- c(0.5, 0.95)

# loop over the levels
level_trial_list <- lapply(alpha_vec, function(alpha){
  print(paste("Value of alpha:", alpha))
  
  # loop over the different trials for this level
  trial_list <- lapply(1:trials, function(trial){
    print(paste("Working on trial:", trial))
    set.seed(trial) # to freeze the randomness of adj_mat
    
    # generate the data
    data <- UWBiost561::generate_partial_clique(n = 10)
    adj_mat <- data$adj_mat
    
    # loop over the methods for this trial
    result_list <- lapply(imp_numbers, function(imp_number){
      set.seed(trial) # to freeze the randomness of the method
      cat('*')
      result <- UWBiost561::compute_maximal_partial_clique_master(
        adj_mat = adj_mat,
        alpha = alpha,
        number = imp_number,
        time_limit = 30
      )
      
      
      return(result)
    })
    names(result_list) <- paste("Implementation:", imp_numbers)
    cat("\n")
    
    return(result_list)
  })
  names(trial_list) <- paste("Trial:", 1:trials)
  print("====")
  
  return(trial_list)
})
names(level_trial_list) <- paste0("alpha:", alpha_vec)

# it's always useful to save the date and R session info
date_of_run <- Sys.time()
session_info <- devtools::session_info()

save(level_trial_list, # save your results
     alpha_vec, # save which alphas you used (for convenience)
     date_of_run, session_info,
     file = "~/demo_simulation.RData")