#########################################################################
# This is the kernel for initialize{test|hoffman}.R
#########################################################################

set.seed(555)

# Download the newest version of the Local FCI package
devtools::install_github("stephenvsmith/LocalFCI") # ,quiet=TRUE

alpha <- c(0.01,0.001,0.0001)
mb_alpha <- c(0.01,0.05,0.1,0.2)
net_names <- gsub(".rds","",list.files(rds_dir))
nets_to_skip <- c("diabetes","link","munin","munin1")
net <- setdiff(net_names,nets_to_skip)
high <- c(1,2,5)/20 # For variances
ub <- c(1,2,5,10)/100 # For coefficients
n <- c(100,500,1000)
algos <- c("MMPC","SES")

simulation_values <- expand.grid(alpha=alpha,mb_alpha=mb_alpha,net=net,high=high,ub=ub,n=n,algos=algos)

go_to_dir(result_dir)
write.csv(simulation_values,file = "sim_vals.csv")
