##########################################################################
# This file provides the kernel for arrayscript{test|hoffman}.R
##########################################################################

source(data_gen_file)
num_trials <- 1
max_targets <- 4

data.grid <- data.frame(network = "asia",
                        data.type = "continuous",
                        n.dat = num_trials,
                        n.obs = 1000,
                        c.ratio = 0,
                        max.in.degree = Inf,
                        lb = 0.4,  # lower bound of coefficients
                        ub = 5,  # upper bound of coefficients
                        low = 0.01,  # lower bound of variances if continuous, of number of levels if discrete
                        high = 5,  # upper bound of variances if continuous, of number of levels if discrete
                        scale = FALSE,
                        stringsAsFactors = FALSE)


go_to_dir(result_dir)
sim_vals <- read.csv("sim_vals.csv",stringsAsFactors = FALSE)[,-1]
net <- sim_vals$net[array_num]
high <- sim_vals$high[array_num]
ub <- sim_vals$ub[array_num]
n <- sim_vals$n[array_num]

cat("This is row",array_num,"of our simulation grid.\n")
cat("Simulation Parameters:\n")
cat("Network:",net,"\n")

# Obtain network information, including the true DAG adj. mat.

network_info <- get_network_DAG(net)

# Generate/Retrieve Targets
targets <- check_targets_defined_get_targets(network_info)

# Set up for simulated data and directory for context
data.grid$network <- net
data.grid$n.obs <- n
n <- n
ub <- ub
high <- high
data.grid$ub <- ub
data.grid$high <- high

# Generate the data
simulation_data_creation()

# Grab simulated data
df_list <- lapply(1:num_trials,function(i) grab_data(i))

# Keep network directory
curr_dir <- getwd()

# Get results for each trial
results_list <- lapply(1:num_trials,function(num){
  
  trial_num <- num
  # Run Local FCI Algorithm
  results_lfci_df <- mclapply(targets,
                              run_fci_target,
                              curr_dir,
                              mc.preschedule = FALSE,mc.cores = 1)
  results_lpc_df <- mclapply(targets,
                             run_pc_target,
                             curr_dir,
                             mc.preschedule = FALSE,mc.cores = 1)
  results_final_df_lfci <- data.frame(do.call(rbind,results_lfci_df))
  results_final_df_lpc <- data.frame(do.call(rbind,results_lpc_df))
  results_final_df <- results_final_df_lfci %>% 
    left_join(results_final_df_lpc,by=c("targets"),suffix=c("_lfci","_lpc"))
  saveRDS(results_final_df,paste0("results_",array_num,"_",num,".rds"))
  setwd(curr_dir)
  return(results_final_df)
})

results_iteration <- data.frame(do.call(rbind,results_list))
saveRDS(results_iteration,paste0("results_",array_num,".rds"))
# TODO: SAVE LOCALFCI OBJECTS AND PC OBJECTS IN CASE IT HELPS
# Remove temporary files (if the program gets to this point)
for (i in 1:num_trials){
  unlink(paste0("results_",array_num,"_",i,".rds"))
}


