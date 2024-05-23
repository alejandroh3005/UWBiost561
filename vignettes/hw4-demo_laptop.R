set.seed(10)
library(UWBiost561)

data <- UWBiost561::generate_partial_clique(n = 10,
                                            clique_fraction = 0.5,
                                            clique_edge_density = 0.8)
adj_mat <- data$adj_mat

# imp_numbers <- 1:25
imp_numbers <- c(1, 17)

result_list <- lapply(imp_numbers, function(imp_number){
  print(paste("Working on implementation:", imp_number))
  result <- UWBiost561::compute_maximal_partial_clique_master(adj_mat = adj_mat,
                                                              alpha = 0.95,
                                                              number = imp_number,
                                                              time_limit = 30)
  return(result)
})
names(result_list) <- paste("Implementation:", imp_numbers)

# print out all the results
result_list

# enumerate only the clique indicies of each method
clique_list <- lapply(result_list, function(x){x$clique_idx})

# check which implementations had a valid partial clique
bool_vec <- sapply(result_list, function(x){x$valid})
bool_vec
table(bool_vec)

# check which implementations had a valid partial clique
valid_idx <- which(bool_vec == TRUE)
clique_list_valid <- clique_list[valid_idx]
size_vec_valid <- sapply(clique_list_valid, length)
names(clique_list_valid)[which(size_vec_valid == max(size_vec_valid))]

