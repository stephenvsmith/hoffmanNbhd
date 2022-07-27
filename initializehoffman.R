### For Hoffman Cluster
home_dir <- '/u/home/s/stephens/'
scratch_dir <- '/u/scratch/s/stephens/'
data_gen_file <- paste0(home_dir,'data_gen.R')
result_dir <- paste0(scratch_dir,'ResultsNbhdPop-',format(Sys.Date(),"%m-%y"))
rds_dir <- paste0(home_dir,'Networks/rds')
source(paste0(home_dir,'HoffmanNbhdPop/helperfunctions.R'))

source(paste0(home_dir,'HoffmanNbhdPop/initializekernel.R'))