### For Testing
#home_dir <- '/home/stephen/'
home_dir <- '/Users/stephensmith/'
scratch_dir <- home_dir
data_gen_file <- paste0(home_dir,'Dropbox/Academics/Research/Code/Scripts/data_gen.R')
result_dir <- paste0(home_dir,'Desktop/','ResultsNbhdPop-',format(Sys.Date(),"%m-%y"))
rds_dir <- paste0(home_dir,'Dropbox/Academics/Research/Code/Networks/rds')
source(paste0(home_dir,'Dropbox/Academics/Research/Code/Scripts/HoffmanNbhdPop/helperfunctions.R'))

source(paste0(home_dir,'Dropbox/Academics/Research/Code/Scripts/HoffmanNbhdPop/initializekernel.R'))