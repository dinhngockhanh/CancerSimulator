simulator_full_program <- function(model = "",
                                   n_simulations = 0,
                                   stage_final = 0,
                                   n_clones_min = 0,
                                   n_clones_max = Inf,
                                   save_newick_tree = FALSE,
                                   save_cn_profile = FALSE,
                                   format_cn_profile = "none",
                                   internal_nodes_cn_info = FALSE,
                                   model_readcount = FALSE) {
    if (model_readcount == TRUE) {
        dir.create(model)
    }
    for (i in 1:n_simulations) {
        cat("\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
        cat(paste("BEGINNING SIMULATION-", i, "...\n", sep = ""))
        simulation <- one_simulation(model, stage_final, save_cn_profile, internal_nodes_cn_info, format_cn_profile, model_readcount)
        #------------------------------------Save the simulation package
        cat("\nSave simulation package...\n")
        filename <- paste(model, "_simulation_", i, ".rda", sep = "")
        save(simulation, file = filename)
        ################################################################
        ################################################################
        ################################################################
        ################################################################
        ################################################################
        cat("\nSave GC & mappability in WIG format...\n")
        filename_gc <- paste(model, "_gc.wig", sep = "")
        filename_map <- paste(model, "_map.wig", sep = "")
        p2_write_gc_map_as_wig(filename_gc, filename_map)
        ################################################################
        ################################################################
        ################################################################
        ################################################################
        ################################################################
        #-----------------------------------Save the sampled CN profiles
        if (save_cn_profile == TRUE) {
            if ((format_cn_profile == "long") | (format_cn_profile == "both")) {
                cat("\nSave true CN profiles in long format...\n")
                cn_profiles_long <-
                    simulation$sample$cn_profiles_long
                filename <- paste(model, "_cn_profiles_long_",
                    i, ".csv",
                    sep = ""
                )
                write.csv(cn_profiles_long,
                    filename,
                    row.names = FALSE
                )
            }
            if ((format_cn_profile == "wide") | (format_cn_profile == "both")) {
                cat("\nSave true CN profiles in wide format...\n")
                cn_profiles_wide <-
                    simulation$sample$cn_profiles_wide
                filename <- paste(model, "_cn_profiles_wide_",
                    i, ".csv",
                    sep = ""
                )
                write.csv(cn_profiles_wide,
                    filename,
                    row.names = FALSE
                )
            }
        }
        #------------------Save the table of CN events in cell phylogeny
        if (internal_nodes_cn_info == TRUE) {
            cat("\nSave table of CN events in cell phylogeny...\n")
            hclust_CN_events <- simulation$sample_phylogeny$package_cell_phylogeny_hclust_extra$hclust_CN_events
            filename <- paste(model, "_cn_events_",
                i, ".csv",
                sep = ""
            )
            write.csv(hclust_CN_events,
                filename,
                row.names = FALSE
            )
        }
        #-------------------------------------Save the noisy CN profiles
        if (model_readcount == TRUE) {
            cat("\nSave noisy CN profiles in long format...\n")
            noisy_cn_profiles_long <- simulation$sample$noisy_cn_profiles_long
            filename <- paste(model, "_noisy_cn_profiles_long_",
                i, ".csv",
                sep = ""
            )
            write.csv(noisy_cn_profiles_long,
                filename,
                row.names = FALSE
            )
            cat("\nSave noisy CN profiles in WIG format...\n")
            sample_cell_ID <- simulation$sample$sample_cell_ID
            noisy_cn_profiles_long <- simulation$sample$noisy_cn_profiles_long
            for (cell in 1:length(sample_cell_ID)) {
                cell_ID <- sample_cell_ID[cell]
                filename <- paste(model, "/", model, "_noisy_cn_profiles_long_",
                    i, "_", cell_ID, ".wig",
                    sep = ""
                )
                ########################################################
                ########################################################
                ########################################################
                ########################################################
                ########################################################
                p2_write_cn_as_wig(filename, noisy_cn_profiles_long, cell_ID)
                ########################################################
                ########################################################
                ########################################################
                ########################################################
                ########################################################
            }
        }
        #-------------------------Save the sampled cells' phylogeny tree
        if (save_newick_tree == TRUE) {
            cat("\nSave sampled cells' phylogeny in Newick format...\n")
            cell_phylogeny_hclust <-
                simulation$sample_phylogeny$cell_phylogeny_hclust
            filename <- paste(model, "_cell_phylogeny_", i, ".newick", sep = "")
            # write(hc2Newick(cell_phylogeny_hclust), file = filename)
            write(hc2Newick_MODIFIED(cell_phylogeny_hclust), file = filename)
        }
    }
}

one_simulation <- function(model, stage_final, save_cn_profile, internal_nodes_cn_info, format_cn_profile, model_readcount) {
    #-----------------------------------------------Load model variables
    SIMULATOR_VARIABLES_for_simulation(model)
    #---------------------------------------------Produce one simulation
    flag_success <- 0
    while (flag_success == 0) {
        #----------------------------------Simulate the clonal evolution
        cat("\nStage 1: clonal evolution...\n")
        output <- SIMULATOR_FULL_PHASE_1_main()
        flag_success <- output$flag_success
        package_clonal_evolution <- output$package_clonal_evolution
        if (flag_success == 0) {
            print("SIMULATION CONDITIONS NOT SATISFIED; REDOING...")
            next
        }
        #--------------------------------------------Simulate the sample
        if (stage_final >= 2) {
            cat("\nStage 2: sampling...\n")
            package_sample <-
                SIMULATOR_FULL_PHASE_2_main(package_clonal_evolution)
            N_clones <- nrow(package_sample$table_clone_ID_vs_letters)
            if ((N_clones < n_clones_min) | (N_clones > n_clones_max)) {
                flag_success <- 0
                cat("SIMULATION CONDITIONS NOT SATISFIED; REDOING...\n")
                next
            }
        }
        #---------------------------Simulate the phylogeny of the sample
        if (stage_final >= 3) {
            cat("\nStage 3: sample phylogeny...\n")
            package_sample_phylogeny <- SIMULATOR_FULL_PHASE_3_main(package_clonal_evolution, package_sample)
        }
    }
    #------------------------------------Put the output package together
    #---Phase 1 (clonal evolution) and related data
    if (stage_final >= 1) {
        simulation <- list()
        simulation$clonal_evolution <- package_clonal_evolution
    }
    #---Phase 2 (sampling) and related data
    if (stage_final >= 2) {
        simulation$sample <- package_sample
        if (save_cn_profile == TRUE) {
            if ((format_cn_profile == "long") | (format_cn_profile == "both")) {
                cat("\nExtra: build CN profile table in long format...\n")
                simulation <- p2_cn_profiles_long(simulation)
            }
            if ((format_cn_profile == "wide") | (format_cn_profile == "both")) {
                cat("\nExtra: build CN profile table in wide format...\n")
                simulation <- p2_cn_profiles_wide(simulation)
            }
        } else {
            if (model_readcount == TRUE) {
                cat("\nExtra: build CN profile table in long format (for modeling CN with noise)...\n")
                simulation <- p2_cn_profiles_long(simulation)
            }
        }
        if (model_readcount == TRUE) {
            cat("\nExtra: simulate CN profiles with noise...\n")
            simulation <- p2_reads_from_cn(simulation)
        }
    }
    #---Phase 3 (sample phylogeny) and related data
    if (stage_final >= 3) {
        simulation$sample_phylogeny <- package_sample_phylogeny
        if (save_cn_profile == TRUE) {
            if (internal_nodes_cn_info == TRUE) {
                cat("\nExtra: find CN information for internal nodes...\n")
                simulation <- p3_cn_profiles_internal(simulation)
                if ((format_cn_profile == "long") | (format_cn_profile == "both")) {
                    cat("\nExtra: add CN profiles for internal nodes in long format...\n")
                    simulation <- p3_internal_node_cn_profiles_long(simulation)
                }
                if ((format_cn_profile == "wide") | (format_cn_profile == "both")) {
                    cat("\nExtra: build CN profile table in wide format...\n")
                    simulation <- p3_internal_node_cn_profiles_wide(simulation)
                }
                cat("\nExtra: build table of CN events...\n")
                simulation <- p3_cn_events_table(simulation)
            }
        }
    }
    return(simulation)
}
