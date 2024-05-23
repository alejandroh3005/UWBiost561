# Please run this after running hw4-demo_bayes_execute.slurm

rm(list=ls())
load("~/demo_simulation.RData")
library(ggplot2)
library(cowplot)

# the plot we would like to make is:
#   For different levels (i.e., values of alpha), how often each of the 25 methods 
#   get the maximal partial clique (over the trials)

# loop over the levels
ggplot_list <- lapply(1:length(level_trial_list), function(level_number){
  trial_list <- level_trial_list[[level_number]]
  alpha <- alpha_vec[level_number]
  
  # first tabulate the results across trials
  trial_mat <- sapply(trial_list, function(result_list){
    
    # enumerate only the clique indicies of each method
    clique_list <- lapply(result_list, function(x){x$clique_idx})
    
    # check which implementations had a valid partial clique
    bool_vec <- sapply(result_list, function(x){x$valid})
    
    # find which implementations had the largest clique
    valid_idx <- which(bool_vec == TRUE)
    clique_list_valid <- clique_list[valid_idx]
    size_vec_valid <- sapply(clique_list_valid, length)
    winning_methods <- names(clique_list_valid)[which(size_vec_valid == max(size_vec_valid))]
    
    # prepare the tabulation for this trial
    winner_vec <- rep(FALSE, length = length(result_list))
    names(winner_vec) <- names(result_list)
    winner_vec[winning_methods] <- TRUE
    
    return(winner_vec)
  })
  
  tabulate_vec <- rowSums(trial_mat)
  
  method_names <- as.character(1:25)
  df <- data.frame(method = method_names,
                   number_wins = tabulate_vec)
  
  gg <- ggplot2::ggplot(df, ggplot2::aes(x=method, y=number_wins))
  gg <- gg + ggplot2::geom_bar(stat = "identity")
  gg <- gg + ggplot2::scale_x_discrete(limits = method_names)
  gg <- gg + ggplot2::labs(x = "Method", 
                           y = "Number of wins", 
                           title = paste("For alpha =", alpha))
  return(gg)
})

plot_all <- cowplot::plot_grid(plotlist = ggplot_list, ncol = 1)
# this is assuming your UWBiost561 package was installed into your home directory on Bayes!
ggplot2::ggsave(plot_all, file = "~/UWBiost561/vignettes/hw4-demo_bayes_plot.png",
                height = 7, width = 9, units = "in")