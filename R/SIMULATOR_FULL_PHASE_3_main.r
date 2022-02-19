#==============================PHASE 3: COPY-NUMBER PROFILES OF A SAMPLE
SIMULATOR_FULL_PHASE_3_main <- function(package_clonal_evolution,package_sample) {
#---------------------------------------------Input the clonal evolution
    T_current                                   <- package_clonal_evolution[[1]]
    N_clones                                    <- package_clonal_evolution[[4]]
    genotype_list_ploidy_chrom                  <- package_clonal_evolution[[5]]
    genotype_list_ploidy_block                  <- package_clonal_evolution[[6]]
    genotype_list_ploidy_allele                 <- package_clonal_evolution[[7]]
    evolution_traj_time                         <- package_clonal_evolution[[13]]
    evolution_traj_divisions                    <- package_clonal_evolution[[14]]
    evolution_traj_clonal_ID                    <- package_clonal_evolution[[15]]
    evolution_traj_population                   <- package_clonal_evolution[[16]]
#-------------------------------------------------------Input the sample
    sample_cell_ID                              <- package_sample[[2]]
    sample_clone_ID                             <- package_sample[[3]]
#-----------------------------------Initialize phylogeny in hclust style
#   Initialize information to build phylogeny in hclust style
    hclust_row                                  <- 0
    hclust_nodes                                <- rep(0,1,2*N_sample-1)
    hclust_nodes[N_sample:(2*N_sample-1)]       <- (-1:-N_sample)
    hclust_labels                               <- sample_cell_ID
#   Initialize actual phylogeny in hclust style
    hclust_merge                                <- matrix(0,nrow=N_sample-1,ncol=2)
    hclust_height                               <- rep(0,1,N_sample-1)
#--------------------------------------Initialize phylogeny in our style
    phylogeny_origin                            <- rep(0,length=2*N_sample-1)
    phylogeny_elapsed_gens                      <- rep(0,length=2*N_sample-1)
    phylogeny_elapsed_genotypes                 <- vector("list",length=2*N_sample-1)
    phylogeny_genotype                          <- rep(0,length=2*N_sample-1)
    phylogeny_birthtime                         <- rep(0,length=2*N_sample-1)
    phylogeny_deathtime                         <- rep(0,length=2*N_sample-1)
#   Initialize the current list of node genotypes
    node_genotype_current                       <- sample_clone_ID
#   Initialize the current list of nodes in the sample phylogeny
    node_list_current                           <- N_sample:(2*N_sample-1)
#   Initialize data for leaves of sample phylogeny
    phylogeny_elapsed_gens[node_list_current]   <- 1
    for (node in N_sample:2*N_sample-1) {
        phylogeny_elapsed_genotypes[[node]]     <- node_genotype_current[node-N_sample+1]
    }
    phylogeny_genotype[node_list_current]       <- node_genotype_current
    phylogeny_deathtime[node_list_current]      <- T_current
#--------------------------------------------Initialize the runif vector
    vec_runif                                   <- runif(10000000)
    N_runif                                     <- 0
#----------------------------------------Build the sample phylogeny tree



TIME_COUNT=0;



    for (i in seq(length(evolution_traj_divisions),1,-1)) {
#       Get time point
        time                                    <- evolution_traj_time[i]



print(i)
if ((i%%1000)==0){
print(TIME_COUNT)


TIME_COUNT=0;
}



#       Get current total clonal population (after divisions)
        total_clonal_ID                         <- evolution_traj_clonal_ID[[i+1]]
        total_clonal_population                 <- evolution_traj_population[[i+1]]
#       Get current sample clonal population (after divisions)
        sample_clonal_population                <- rep(0,length=N_clones)
        for (node in 1:length(node_genotype_current)) {
            genotype                            <- node_genotype_current[node]
            sample_clonal_population[genotype]  <- sample_clonal_population[genotype]+1
        }
#       Get list of eligible nodes of each genotype
        sample_eligible_nodes                   <- vector("list",length=N_clones)
        for (node in 1:length(node_genotype_current)) {
            genotype                            <- node_genotype_current[node]
            sample_eligible_nodes[[genotype]]   <- c(sample_eligible_nodes[[genotype]], node_list_current[node])
        }
#       Get list of divisions
        matrix_division                         <- evolution_traj_divisions[[i]]
        if (is.null(matrix_division)){
            next
        }
#       Redo runif vector if necessary
        if (N_runif>=length(vec_runif)-4*sum(matrix_division[,1])){
            vec_runif                           <- runif(10000000)
            N_runif                             <- 0
        }
#       For each type of divisions...
        for (event_type in 1:nrow(matrix_division)) {
#           Get number of divisions
            no_divisions                        <- matrix_division[event_type,1]
#           Get genotype of mother
            genotype_mother                     <- matrix_division[event_type,2]
#           Get genotype of 1st daughter
            genotype_daughter_1                 <- matrix_division[event_type,3]
            position_daughter_1                 <- which(total_clonal_ID==genotype_daughter_1)
#           Get genotype of 2nd daughter
            genotype_daughter_2                 <- matrix_division[event_type,4]
            position_daughter_2                 <- which(total_clonal_ID==genotype_daughter_2)
#           If daughter genotypes are not in current nodes, move on
            if ((sample_clonal_population[genotype_daughter_1]<=0)&&(sample_clonal_population[genotype_daughter_2]<=0)) {
                next
            }
#           For each specific division...



start.time_mini <- Sys.time()



            for (division in 1:no_divisions) {
#               If these genotypes are not in current nodes, move on
                if ((sample_clonal_population[genotype_daughter_1]<=0)&&(sample_clonal_population[genotype_daughter_2]<=0)) {
                    next
                }
#               Choose the first daughter node
                # logic_node_1                                                    <- runif(1)<sample_clonal_population[genotype_daughter_1]/total_clonal_population[position_daughter_1]
                N_runif                                                         <- N_runif+1
                logic_node_1                                                    <- vec_runif[N_runif]<(sample_clonal_population[genotype_daughter_1]/total_clonal_population[position_daughter_1])
                if (logic_node_1==1) {
                    # pos_node_1                                                  <- sample.int(sample_clonal_population[genotype_daughter_1],size=1)
                    N_runif                                                     <- N_runif+1
                    pos_node_1                                                  <- ceiling(vec_runif[N_runif]*sample_clonal_population[genotype_daughter_1])
                    node_1                                                      <- sample_eligible_nodes[[genotype_daughter_1]][pos_node_1]
                    sample_eligible_nodes[[genotype_daughter_1]]                <- sample_eligible_nodes[[genotype_daughter_1]][-pos_node_1]
                    sample_clonal_population[genotype_daughter_1]               <- sample_clonal_population[genotype_daughter_1]-1
                    total_clonal_population[position_daughter_1]                <- total_clonal_population[position_daughter_1]-1
                }
                else {
                    node_1                                                      <- 0
                    total_clonal_population[position_daughter_1]                <- total_clonal_population[position_daughter_1]-1
                }
#               Choose the second daughter node
                # logic_node_2                                                    <- runif(1)<sample_clonal_population[genotype_daughter_2]/total_clonal_population[position_daughter_2]
                N_runif                                                         <- N_runif+1
                logic_node_2                                                    <- vec_runif[N_runif]<(sample_clonal_population[genotype_daughter_2]/total_clonal_population[position_daughter_2])
                if (logic_node_2==1) {
                    # pos_node_2                                                  <- sample.int(sample_clonal_population[genotype_daughter_2],size=1)
                    N_runif                                                     <- N_runif+1
                    pos_node_2                                                  <- ceiling(vec_runif[N_runif]*sample_clonal_population[genotype_daughter_2])
                    node_2                                                      <- sample_eligible_nodes[[genotype_daughter_2]][pos_node_2]
                    sample_eligible_nodes[[genotype_daughter_2]]                <- sample_eligible_nodes[[genotype_daughter_2]][-pos_node_2]
                    sample_clonal_population[genotype_daughter_2]               <- sample_clonal_population[genotype_daughter_2]-1
                    total_clonal_population[position_daughter_2]                <- total_clonal_population[position_daughter_2]-1
                    }
                else {
                    node_2                                                      <- 0
                    total_clonal_population[position_daughter_2]                <- total_clonal_population[position_daughter_2]-1
                }
#               Update the nodes
                if ((node_1==0)&&(node_2==0)) {
#                   There is no merging....
                    next
                    }
                else { if ((node_1>0)&&(node_2==0)) {
#                   There is no merging but node 1 has one more division...
#                   Update phylogeny in our style
                    phylogeny_elapsed_gens[node_1]                          <- phylogeny_elapsed_gens[node_1]+1
                    phylogeny_elapsed_genotypes[[node_1]]                   <- c(genotype_mother, phylogeny_elapsed_genotypes[[node_1]])
#                   Update phylogeny records in our style
                    node_genotype_current[which(node_list_current==node_1)] <- genotype_mother
                    }
                else { if ((node_1==0)&&(node_2>0)) {
#                   There is no merging but node 2 has one more division...
#                   Update phylogeny in our style
                    phylogeny_elapsed_gens[node_2]                          <- phylogeny_elapsed_gens[node_2]+1
                    phylogeny_elapsed_genotypes[[node_2]]                   <- c(genotype_mother, phylogeny_elapsed_genotypes[[node_2]])
#                   Update phylogeny records in our style
                    node_genotype_current[which(node_list_current==node_2)] <- genotype_mother
                    }
                else { if ((node_1>0)&&(node_2>0)) {
#                   Nodes 1 and 2 are mergning...
                    node_mother                                             <- min(node_list_current)-1
#                   Update phylogeny in hclust style
                    hclust_row                                              <- hclust_row+1
                    hclust_nodes[node_mother]                               <- hclust_row
                    hclust_merge[hclust_row,1]                              <- hclust_nodes[node_1]
                    hclust_merge[hclust_row,2]                              <- hclust_nodes[node_2]
                    hclust_height[hclust_row]                               <- T_current-time
#                   Update phylogeny in our style
                    phylogeny_origin[node_1]                                <- node_mother
                    phylogeny_origin[node_2]                                <- node_mother
                    phylogeny_elapsed_gens[node_mother]                     <- 1
                    phylogeny_elapsed_genotypes[[node_mother]]              <- c(genotype_mother)
                    phylogeny_genotype[node_mother]                         <- genotype_mother
                    phylogeny_birthtime[node_1]                             <- time
                    phylogeny_birthtime[node_2]                             <- time
                    phylogeny_deathtime[node_mother]                        <- time
#                   Update phylogeny records in our style
                    pos_delete                                              <- c(which(node_list_current==node_1),which(node_list_current==node_2))
                    node_genotype_current                                   <- node_genotype_current[-pos_delete]
                    node_genotype_current                                   <- c(genotype_mother, node_genotype_current)
                    node_list_current                                       <- node_list_current[-pos_delete]
                    node_list_current                                       <- c(node_mother, node_list_current)
                    }
                }}}
            }



end.time_mini   <- Sys.time()
time.taken_mini <- end.time_mini - start.time_mini
TIME_COUNT      <- TIME_COUNT+time.taken_mini



        }
    }
#   Assign original cell to be born at the beginning of clonal evolution
    phylogeny_birthtime[1]                          <- evolution_traj_time[1]
#-----------------------------------------Reorder the nodes for plotting
#---Find an order on all nodes of the phylogeny in our style
#   Find number of progeny of each node
    progeny_count                                   <- rep(0,2*N_sample-1)
    progeny_count[N_sample:(2*N_sample-1)]          <- 1
    for (node in (2*N_sample-1):2){
        mother_node                                 <- phylogeny_origin[node]
        progeny_count[mother_node]                  <- progeny_count[mother_node]+progeny_count[node]
    }
#   Reorder the sample phylogeny tree based on progeny counts
    phylogeny_order                                 <- rep(0,2*N_sample-1)
    phylogeny_order[1]                              <- 1
    for (node in 1:(2*N_sample-1)){
        vec_daughter_nodes                          <- which(phylogeny_origin==node)
        if (length(vec_daughter_nodes)==2){
            daughter_node_1                         <- vec_daughter_nodes[1]
            progeny_count_1                         <- progeny_count[daughter_node_1]
            daughter_node_2                         <- vec_daughter_nodes[2]
            progeny_count_2                         <- progeny_count[daughter_node_2]
            if (progeny_count_1<progeny_count_2){
                phylogeny_order[daughter_node_1]    <- phylogeny_order[node]
                phylogeny_order[daughter_node_2]    <- phylogeny_order[node]+progeny_count_1
            }
            else{
                phylogeny_order[daughter_node_1]    <- phylogeny_order[node]+progeny_count_2
                phylogeny_order[daughter_node_2]    <- phylogeny_order[node]
            }
        }
    }
#---Extract the order for phylogeny in hclust style
    hclust_order_inverse                            <- phylogeny_order[N_sample:(2*N_sample-1)]
    hclust_order                                    <- rep(0,N_sample)
    for (i_cell in 1:N_sample){
        loc                                         <- hclust_order_inverse[i_cell]
        hclust_order[loc]                           <- i_cell
    }
#------------------------------------------------Create clustering table
#   Change clone ID from genotype index in simulation to [A,B,C,...]
    sample_clone_ID_numeric                         <- sample_clone_ID
    sample_clone_ID_unique_numeric                  <- unique(sample_clone_ID_numeric)
    sample_clone_ID_unique_letters                  <- LETTERS[as.numeric(1:length(sample_clone_ID_unique_numeric))]
    sample_clone_ID_letters                         <- c()
    for (i_clone in 1:length(sample_clone_ID_unique_numeric)){
        clone_ID_numeric                            <- sample_clone_ID_unique_numeric[i_clone]
        clone_ID_letters                            <- sample_clone_ID_unique_letters[i_clone]
        vec_cell_ID                                 <- which(sample_clone_ID_numeric==clone_ID_numeric)
        sample_clone_ID_letters[vec_cell_ID]        <- clone_ID_letters
    }
#   Create clustering table
    hclust_clustering                               <- data.frame(sample_cell_ID,sample_clone_ID_letters)
    names(hclust_clustering)                        <- c('cell_id','clone_id')
#--------------------------------Create phylogeny object in hclust style
#   Create phylogeny object in hclust style
    phylogeny_hclust                                <- list()
    phylogeny_hclust$merge                          <- hclust_merge
    phylogeny_hclust$height                         <- hclust_height
    phylogeny_hclust$order                          <- hclust_order
    phylogeny_hclust$labels                         <- sample_cell_ID
    class(phylogeny_hclust)                         <- "hclust"
#---------------------------------Create phylogeny object in phylo style
#   Create phylogeny object in phylo style
    phylogeny_phylo                                 <- ape::as.phylo(phylogeny_hclust,use.labels=TRUE)
#   Create object containing both phylo-style tree and clustering
    phylogeny_clustering_truth                      <- list()
    phylogeny_clustering_truth$tree                 <- phylogeny_phylo
    phylogeny_clustering_truth$clustering           <- hclust_clustering
#---------------------------------Output package of data from simulation
    output                                          <- list()
    output[[1]]                                     <- phylogeny_clustering_truth
    output[[2]]                                     <- phylogeny_origin
    output[[3]]                                     <- phylogeny_elapsed_gens
    output[[4]]                                     <- phylogeny_elapsed_genotypes
    output[[5]]                                     <- phylogeny_genotype
    output[[6]]                                     <- phylogeny_birthtime
    output[[7]]                                     <- phylogeny_deathtime
    output[[8]]                                     <- phylogeny_order
    return(output)
}
