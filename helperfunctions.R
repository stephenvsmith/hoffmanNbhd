library(tidyverse,quietly=TRUE,verbose = FALSE,warn.conflicts = FALSE)
library(bnlearn,quietly=TRUE,verbose = FALSE,warn.conflicts = FALSE)
library(pcalg,quietly=TRUE,verbose = FALSE,warn.conflicts = FALSE)
library(parallel,quietly=TRUE,verbose = FALSE,warn.conflicts = FALSE)
library(LocalFCI,quietly=TRUE,verbose = FALSE,warn.conflicts = FALSE)

# DAG Info -------------------------------------------------------------

# Build the file structure for this simulation setting and get network info
# Obtain network information and simulated data
get_network_DAG <- function(net){
  current_wd <- getwd() # Save the current working directory to return to at the end
  setwd(rds_dir) # Go to the directory with the network .rds files
  
  if (paste0(net,".rds") %in% list.files()){
    network <- readRDS(paste0(net,".rds"))
    p <- length(names(network))
    node_names <- names(network)
  } else {
    stop('Invalid Network Name')
  }
  
  true_dag <- amat(network) # Get the adjacency matrix of the true DAG
  
  network_cpdag <- cpdag(network) # Get the CPDAG of the true DAG
  true_cpdag <- amat(network_cpdag) # Adjacency matrix of the true CPDAG
  
  setwd(current_wd) # Return to original working directory
  
  return(list(
    "net"=net,
    "p"=p,
    "node_names"=node_names,
    "network"=network,
    "true_dag"=true_dag,
    "cpdag"=true_cpdag
  ))
}


# Simulation Data Functions -----------------------------------------------

simulation_data_creation <- function(){
  # Check to see if the data has been generated already
  go_to_dir("data")
  sims_not_created <- check_sims_created(n)
  # Generate Data
  if (sims_not_created){
    sims_text_output()
    gdg <- generate.data.grid(data.grid,
                              out.dir=getwd(),
                              array_num=array_num,
                              verbose=FALSE,
                              path.start=home_dir)
    # dir.create(paste0(net,"_",array_num))
    # data_files <- list.files(paste0(net,"; n = ",n,"; c = 0"))
    # sapply(data_files,function(f){
    #   file.copy(paste0(net,"; n = ",n,"; c = 0/",f),
    #             paste0(net,"_",array_num))
    # })
    cat("finished",file = paste0(net,"_",array_num,"/marker.txt"))
    #unlink(paste0(net,"; n = ",n,"; c = 0"),recursive = TRUE)
  } else {
    Sys.sleep(60)
  }
  setwd("..") # Return to the original directory
}

# Check whether or not simulations have been created so they do not have to be created again
check_sims_created <- function(n){
  sim_file <- paste0(net,"_",array_num)
  if (dir.exists(sim_file)){
    files <- list.files(sim_file) # Get the list of files in this directory\
    sims_not_created <- !any(str_detect(files,"data[[:digit:]]+.txt"))
  } else {
    sims_not_created <- TRUE
  }
  return(sims_not_created)
  
}

sims_text_output <- function(){
  q1 <- format(paste("Generating Datasets for",net),width = 35,justify = "left")
  q2 <- format(paste("U.B. Variance:",data.grid$high),width = 20,justify = "centre")
  q3 <- format(paste("U.B. Coefs:",data.grid$ub),width = 20,justify = "centre")
  q4 <- format(paste("n =",data.grid$n.obs),width = 10,justify = "centre")
  cat(q1,"|",q2,"|",q3,"|",q4,"\n")
}

# Grab dataframe for network at given sample size
grab_data <- function(df_num){
  go_to_dir("data")
  go_to_dir(paste0(net,"_",array_num))
  check <- check_file(paste0("data",df_num,".txt"))
  if (!check){
    cat("The data did not pass the check. File size:",file.size(paste0("data",df_num,".txt")),"\n")
  } else {
    cat("The data did pass the check. File size:",file.size(paste0("data",df_num,".txt")),"\n")
  }
  df <- read.table(paste0("data",df_num,".txt"))
  colnames(df) <- network_info$node_names
  setwd("../..")
  return(df)
}

check_file <- function(f_name){
  files <- list.files()
  i <- 0
  while (!(file.exists("marker.txt")) & i < 20){
    i <- i + 1
    Sys.sleep(3)
    files <- list.files()
  }
  return(f_name %in% list.files())
}

# Target functions --------------------------------------------------------

check_targets_defined_get_targets <- function(net_info){
  # Determine whether or not targets have been defined
  if (!file.exists(paste0("targets_",net,".rds"))){
    targets <- get_targets(net_info$p)
    saveRDS(targets,paste0("targets_",net,".rds"))
  } else {
    targets <- readRDS(paste0("targets_",net,".rds"))
    cat("Targets for",net,"obtained from file.\n")
  }
  
  return(targets)
}

get_targets <- function(p){
  max_targets_per_category <- 25
  if (p<25){
    max_targets_per_category <- p
  }
  targets <- lapply(1:max_targets,function(num){
    if (p<200){
      target_options <- combn(1:p,num)
      return(target_options[,sample(1:ncol(target_options),max_targets_per_category)])
    } else {
      targets <- matrix(nrow = num,ncol = max_targets_per_category)
      for (j in 1:max_targets_per_category){
        targets[,j] <- sample(1:p,num)
      }
      return(targets)
    }
  })
  final_target_list <- list()
  final_target_list <- c(final_target_list,targets[[1]]) # Add individual targets
  offset <- length(final_target_list)
  j <- 1
  for (i in 2:max_targets){
    while (j < ncol(targets[[i]])){
      final_target_list[[offset+j]]<-targets[[i]][,j]
      j <- j + 1
    }
    offset <- length(final_target_list)
    j <- 1
  }
  
  return(final_target_list)
}

# Local FCI ---------------------------------------------------------------

run_fci_target <- function(t,curr_dir){
  output_text(t,num,algo)
  setwd(curr_dir)
  # vars <- create_target_directory(t) 
  # Run local FCI
  results <- run_local_fci(t,curr_dir)
  
  return(results)
}

run_pc_target <- function(t,curr_dir){
  output_text(t,num,algo)
  setwd(curr_dir)
  # vars <- create_target_directory(t) 
  # Run local PC

  results <- run_local_pc(t,curr_dir)
  
  return(results)
}


# Local FCI Helpers -------------------------------------------------------

run_local_fci <- function(t,curr_dir){
  true_dag <- network_info$true_dag
  start <- Sys.time()
  
  localfci_result <- localfci(true_dag=true_dag,
                              targets=t,
                              verbose = FALSE)
  end <- Sys.time()
  diff <- end - start
  units(diff) <- "mins"
  results_timing <- list()
  results_timing$time_diff[["Local FCI"]] <- diff
  results_timing$num_tests[["Local FCI"]] <- localfci_result$NumTests
  results <- neighborhood_results(t,localfci_result,results_timing)
  
  return(results)
}

run_local_pc <- function(t,curr_dir){
  true_dag <- network_info$true_dag
  start <- Sys.time()
  
  localpc_result <- localpc(true_dag=true_dag,
                            targets=t,
                            verbose = FALSE)
  end <- Sys.time()
  diff <- end - start
  units(diff) <- "mins"
  results_timing <- list()
  results_timing$time_diff[["Local PC"]] <- diff
  results_timing$num_tests[["Local PC"]] <- localpc_result$NumTests
  results <- neighborhood_pc_results(t,localpc_result,results_timing)
  
  return(results)
}

create_target_directory <- function(t){
  go_to_dir(paste0("Target",ifelse(length(t)>1,"s",""),"=",paste(t,collapse = ",")))
  return()
}

# Compile all results about the simulation
neighborhood_results <- function(t,localfci_result,results_timing){
  if (length(t)>1){
    nbhd_list <- lapply(t,function(target){
      check_neighbors_retrieval(network_info$p,network_info$node_names,network_info$true_dag,target-1)+1
    })
    nbhd <- unique(unlist(nbhd_list))
  } else {
    nbhd <- check_neighbors_retrieval(network_info$p,network_info$node_names,network_info$true_dag,t-1)+1
  }
  nbhd <- sort(unique(c(t,nbhd)))
  # Zoom in on estimated and true DAGs (only the target and first-order neighbors)
  nodes_zoom <- network_info$node_names[nbhd]
  true_neighborhood_graph <- network_info$cpdag[nbhd,nbhd] # CPDAG is Ground Truth
  localfci_mat <- localfci_result$amat[nbhd,nbhd]
  # Compare results
  if (length(nbhd)==1){
    localfci_mat <- as.matrix(localfci_mat,nrow=1,ncol=1)
    true_neighborhood_graph <- as.matrix(true_neighborhood_graph,nrow=1,ncol=1)
  }
  results <- all_metrics(localfci_mat,
                         true_neighborhood_graph,
                         localfci_mat,
                         sapply(t,function(target) {which(nbhd==target)-1}),verbose = FALSE) # Need to check out the sapply here
  nbhd_metrics <- neighborhood_metrics(true_neighborhood_graph)
  
  results <- cbind(nbhd_metrics,results)
  results <- results %>% 
    mutate(fci_num_tests=results_timing$num_tests[["Local FCI"]],
           fci_time=results_timing$time_diff[["Local FCI"]]) %>%
    mutate(sim_number=array_num,
           net=net,
           num_targets=length(t),
           targets=paste(t,collapse = ","),
           p=network_info$p) %>% 
    mutate(totalSkeletonTime=localfci_result$totalSkeletonTime,
           targetSkeletonTimes=paste(localfci_result$targetSkeletonTimes,collapse = ","),
           totalcpptime=localfci_result$totalTime,
           nodes=paste(localfci_result$Nodes,collapse = ",")
    )
  
  results <- cbind(results,interNeighborhoodEdgeMetrics(localfci_mat,
                                                        network_info$true_dag,
                                                        t))
  

  return(results)
}

neighborhood_pc_results <- function(t,localpc_result,results_timing){
  if (length(t)>1){
    nbhd_list <- lapply(t,function(target){
      check_neighbors_retrieval(network_info$p,network_info$node_names,network_info$true_dag,target-1)+1
    })
    nbhd <- unique(unlist(nbhd_list))
  } else {
    nbhd <- check_neighbors_retrieval(network_info$p,network_info$node_names,network_info$true_dag,t-1)+1
  }
  nbhd <- sort(unique(c(t,nbhd)))
  # Zoom in on estimated and true DAGs (only the target and first-order neighbors)
  nodes_zoom <- network_info$node_names[nbhd]
  true_neighborhood_graph <- network_info$cpdag[nbhd,nbhd] # CPDAG is Ground Truth
  localpc_mat <- localpc_result$amat[nbhd,nbhd]
  # Compare results
  if (length(nbhd)==1){
    localpc_mat <- as.matrix(localpc_mat,nrow=1,ncol=1)
    true_neighborhood_graph <- as.matrix(true_neighborhood_graph,nrow=1,ncol=1)
  }
  results <- all_metrics(localpc_mat,
                         true_neighborhood_graph,
                         localpc_mat,
                         sapply(t,function(target) {which(nbhd==target)-1}),verbose = FALSE) # Need to check out the sapply here
  nbhd_metrics <- neighborhood_metrics(true_neighborhood_graph)
  
  results <- cbind(nbhd_metrics,results)
  results <- results %>% 
    mutate(lpc_num_tests=results_timing$num_tests[["Local PC"]],
           lpc_time=results_timing$time_diff[["Local PC"]]) %>%
    mutate(sim_number=array_num,
           net=net,
           num_targets=length(t),
           targets=paste(t,collapse = ","),
           p=network_info$p) %>% 
    mutate(lpc_totalSkeletonTime=localpc_result$totalSkeletonTime,
           lpc_targetSkeletonTimes=paste(localpc_result$targetSkeletonTimes,collapse = ","),
           lpc_totalcpptime=localpc_result$totalTime,
           nodes=paste(localpc_result$Nodes,collapse = ",")
    )

  results <- cbind(results,interNeighborhoodEdgeMetrics(localpc_mat,
                                                        network_info$true_dag,
                                                        t))
  
  
  return(results)
}

# General Functions -------------------------------------------------------

go_to_dir <- function(file){
  if (!dir.exists(file)){
    dir.create(file)
  }
  setwd(file)
}

find_neighbors <- function(t,true_dag){
  # Parents of node t
  parents <- which(true_dag[,t]==1)
  # Children of node t
  children <- which(true_dag[t,]==1)
  # Spouses share the same children
  spouses <- c()
  for (c in children){ # loop through all of t's children
    # Find all parents of current child
    potential <- which(true_dag[,c]==1)
    # Add all parents that are not t to spouses
    spouses <- c(spouses,potential[potential!=t])
  }
  return(unique(c(parents,children,spouses)))
}

find_all_neighbors <- function(t,true_dag){
  if (length(t)==1){
    return(find_neighbors(t,true_dag))
  } else {
    neighbors <- lapply(t,find_neighbors,true_dag=true_dag)
    all_neighbors <- sort(unique(unlist(neighbors)))
    return(all_neighbors)
  }
}

# Check whether these simulations have been completed
check_completion <- function(dir){
  if (!dir.exists(dir)){
    return(NULL)
  } else {
    current_wd <- getwd()
    setwd(dir)
    rds_files <- list.files(pattern = "*.rds$")
    file_l <- length(rds_files)
    if (file_l==0){
      setwd(current_wd)
      return(NULL)
    } else if (file_l==1){
      cat("Simulations Completed. Name of file:",rds_files," ")
      results <- readRDS(rds_files)
      cat("Number of rows from file:",nrow(results),"\n")
      setwd(current_wd)
      return(results)
    } else {
      stop("Something wrong with simulation completion check code.")
    }
  }
}

output_text <- function(t,num,algo){
  q2 <- format(paste("Net:",net),width = 25,justify = "centre")
  q5 <- format(paste("Targets:",paste(t,collapse=",")),width = 23,justify = "left")
  cat(q2,"|",q5,"\n")
}
