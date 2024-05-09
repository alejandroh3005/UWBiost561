# store some useful information
date_of_run <- Sys.time()
session_info <- devtools::session_info()
set.seed(10)

# generate a random matrix
p <- 2000
mat <- matrix(rnorm(p^2), p, p)
mat <- mat + t(mat)

# print out some elements of the matrix
print(mat[1:5,1:5])

# compute eigenvalues
res <- eigen(mat)

# save the results
save(mat, res,
     date_of_run, session_info,
     file = "~/demo_bayes_output.RData")

print("Done! :)")
