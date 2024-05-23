# This is version for May-16 (version 3)

#' Master file for computing maximal partial clique
#'
#' This function runs a implementation (dictated by \code{number}) using the inputs
#' \code{adj_mat} and \code{alpha}. For example, \code{compute_maximal_partial_clique_master(adj_mat, alpha, number = 5)}
#' executes \code{compute_maximal_partial_clique5(adj_mat, alpha)}.
#'
#' If the implementation errors (i.e., crashes), then the output \code{status} will be \code{error}.
#' If the implementation did not complete within \code{time_limit} number of seconds (i.e., took too long), then the output \code{status} will be \code{timed_out}.
#'
#' The `valid` output is a boolean on whether or not the provided `clique_idx`
#' forms a valid partial clique. If the method had status `timed_out` or `error`,
#' the method would (by default) have a `valid=FALSE`.
#'
#' @param adj_mat a symmetric `matrix` with only values `0` or `1`, has `1`'s along its diagonal, has no row- or column-names, and will have between 5 to 50  rows/columns (inclusive)
#' @param alpha a single `numeric` (i.e., a length of 1), and has a value between 0.5 and 1 (inclusive)
#' @param number an implementation number to use
#' @param time_limit the number of seconds the implementation has before it times out
#'
#' @return a list with `clique_idx`, `edge_density` (which are from the implementation itself)
#' and `status` and `valid` (which are added by this function)
#' @export
compute_maximal_partial_clique_master <- function(adj_mat,
                                                  alpha,
                                                  number,
                                                  time_limit = 30){
  stopifnot(number %in% c(1:25))
  
  # see https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
  string <- paste0("compute_maximal_partial_clique", number,
                   "(adj_mat = adj_mat, alpha = alpha)")
  result <- .interruptor(eval(parse(text = string)),
                         time_limit = time_limit)
  
  if("status" %in% names(result) && result$status == "error"){
    result <- list(clique_idx = NA,
                   edge_density = NA,
                   status = "error")
  } else if("status" %in% names(result) && result$status == "timed_out"){
    result <- list(clique_idx = NA,
                   edge_density = NA,
                   status = "timed_out")
  } else if(any(is.na(result$clique_idx)) ||
            any(is.nan(result$clique_idx)) ||
            any(!is.numeric(result$clique_idx)) ||
            !("clique_idx" %in% names(result))){
    result <- list(clique_idx = NA,
                   edge_density = NA,
                   status = "error")
  } else {
    result$status <- "completed"
  }
  
  # check if it's a valid partial clique
  if(!all(is.na(result$clique_idx))){
    true_density <- compute_correct_density(adj_mat = adj_mat,
                                            clique_idx = result$clique_idx)
    valid <- (true_density >= alpha)
  } else {
    valid <- FALSE
  }
  
  result$valid <- valid
  
  return(result)
}

#' Compute the correct density, given a set of nodes
#'
#' We will define a \code{clique_idx} of length 1 to have a density of 1.
#'
#' @param adj_mat a symmetric `matrix` with only values `0` or `1`, has `1`'s along its diagonal, has no row- or column-names, and will have between 5 to 50  rows/columns (inclusive).
#' @param clique_idx a `numeric` vector of index numbers corresponding to the nodes (i.e., values between 1 and `nrow(adj_mat)`) that your function deems to be in the maximum partial clique. This vector cannot have duplicate elements, must be positive integers, and the largest value cannot exceed `nrow(adj_mat)`
#'
#' @return a numeric, which is the density of edges among \code{adj_mat[clique_idx,clique_idx]}
#' @export
compute_correct_density <- function(adj_mat, clique_idx){
  if(length(clique_idx) == 0 ||
     any(is.na(clique_idx)) ||
     any(is.nan(clique_idx)) ||
     any(!is.numeric(clique_idx))) return(0)
  
  stopifnot(is.matrix(adj_mat),
            nrow(adj_mat) == ncol(adj_mat),
            sum(abs(adj_mat - t(adj_mat))) <= 1e-6,
            all(diag(adj_mat) == 1),
            all(adj_mat %in% c(0,1)),
            all(clique_idx %% 1 == 0),
            all(clique_idx > 0),
            length(clique_idx) <= nrow(adj_mat))
  
  clique_idx <- unique(clique_idx)
  
  n <- nrow(adj_mat)
  m <- length(clique_idx)
  
  if(m == 1) return(1)
  
  numerator <- (sum(adj_mat[clique_idx, clique_idx]) - m)/2
  denominator <- m*(m-1)/2
  return(numerator/denominator)
}

#######

# from https://stackoverflow.com/questions/34346619/how-to-stop-a-function-in-r-that-is-taking-too-long-and-give-it-an-alternative
.interruptor <- function(FUN, time_limit) {
  
  setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  
  start_time <- Sys.time()
  result <- tryCatch({
    FUN
  }, error = function(e) {
    return(list(status = "timed_out"))
  })
  end_time <- Sys.time()
  
  diff_time <- difftime(end_time, start_time, units = "secs")
  
  # account for a small fudge factor
  if(diff_time < 29 && "status" %in% names(result) && result$status == "timed_out"){
    return(list(status = "error"))
  } else if (diff_time > 29 && "status" %in% names(result) && result$status == "timed_out"){
    return(list(status = "timed_out"))
  } else {
    return(result)
  }
}
