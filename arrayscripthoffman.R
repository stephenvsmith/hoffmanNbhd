### Directories (Hoffman)
home_dir <- '/u/home/s/stephens'
scratch_dir <- '/u/scratch/s/stephens'
result_dir <- paste0(scratch_dir,'/ResultsNbhd-',format(Sys.Date(),"%m-%y"))
rds_dir <- paste0(home_dir,'/Networks/rds')
data_gen_file <- paste0(home_dir,'/data_gen.R')

### Setup (Hoffman)
source(paste0(home_dir,'/hoffmanNbhd/helperfunctions.R'))
array_num <- as.numeric(Sys.getenv("SGE_TASK_ID"))

source(paste0(home_dir,'/hoffmanNbhd/arraykernel.R'))