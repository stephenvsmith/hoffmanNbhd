#########################################################################
# This is the kernel for initialize{test|hoffman}.R
#########################################################################

set.seed(555)

# Download the newest version of the Local FCI package
devtools::install_github("stephenvsmith/LocalFCI") # ,quiet=TRUE

net_names <- gsub(".rds","",list.files(rds_dir))
nets_to_skip <- c("diabetes","link","munin","munin1")
net <- setdiff(net_names,nets_to_skip)
net <- c("alarm","barley","child","insurance","link","mildew","hepar2","pigs","andes","munin2")
high <- c(0.1) # For variances
ub <- c(0.5) # For coefficients
n <- c(10)

simulation_values <- expand.grid(net=net,high=high,ub=ub,n=n)

go_to_dir(result_dir)
write.csv(simulation_values,file = "sim_vals.csv")
