# ggtree pipeline for annotation of phylogenetic tree data. 
#
# Input a Nexus file containing the consensus tree from MrBayes analysis
# to combine with DNA specimen data for plotting onto the phylogenetic tree.
library(ggtree)
library(plyr)
library(magrittr)

# Source ggplot aesthetic variables for Physaria species.
source("R/ggplot_aes_vars.R")

# Establish data frame from list of DNA extracted herbarium specimens.
source("Phys_DNA/DNA_specimens.R")
dna_meta <- ldply(dna_specimens)  # voucher associated metadata
names(dna_meta) <- c("taxa_label", "Collector", "Collection_Number",
                     "Herbarium", "State", "County",
                     "Latitude", "Longitude")

# Function to match DNA_META ID with herbarium specimen data.
spp_match <- function(dna_meta_id) {
  
  # Establish DNA specimen record and identify row in specimen data frame.
  dna_id <- dna_meta[dna_meta_id, c(2,3)]
  total_phys_rowid <- intersect(grep(dna_id$Collection_Number, 
                                     total_physaria$Collection_Number),
                                grep(dna_id$Collector,
                                     total_physaria$Collector))
  
  # Store data frame row(s) of TOTAL_PHYSARIA matching DNA_ID
  matched_id <- total_physaria[total_phys_rowid, 
                               grep("Collector|Collection_Number",
                                    names(total_physaria))]
  
  # Check for duplicate records, remove by calling unique() on matched_id.
  if(nrow(matched_id) > 1) {
    matched_id <- unique(matched_id)
    if (nrow(matched_id) > 1) {
      stop("Error in matching record to total_physaria row: ", 
           total_phys_rowid)
    }
  }
  
  # Specimen ID with the value of $taxa_label for matching row in DNA_META.
  spp_row <- as.data.frame(NULL)
  spp_row[1, 1] <- dna_meta[dna_meta_id, "taxa_label"]
  names(spp_row) <- "taxa_label"
  
  # Return specimen ID info using find_spp() function and row_id argument.
  prior_spps <- find_spp(taxa_frame = total_physaria, row_id = total_phys_rowid,
                         priors = TRUE, locale = TRUE, geom = TRUE)
  
  # Combine herbarium specimen data with label from DNA_META.
  spp_row <- cbind(spp_row, prior_spps[1, ])
  
  # Additional duplicate check for find_spp() call.
  if (nrow(spp_row) > 1) { stop(print(dna_meta_id)) }
  return(spp_row)
}

# Read in bayesian inference data from MrBayes consensus tree file
# as ggtree formal class treedata and combine with herbarium specimen metadata.
herb_tree <- function(bayes_file, label_issue = FALSE,
                      spp_id = "Physaria_syn") {
  bayes_treedata <- read.mrbayes(bayes_file)
  bayes_df <- as.data.frame(fortify(bayes_treedata))  # Tidy data frame
  
  # Establish empty data frame to combine DNA specimen and bayes data.
  phys_names <- grep("Physaria|Taxon_a_posteriori", 
                     names(total_physaria), value = TRUE)
  bayes_names <- c(union(names(dna_meta), names(bayes_df)), phys_names)
  bayes_ext <- data.frame(matrix(NA, ncol = length(bayes_names)))
  names(bayes_ext) <- bayes_names[order(bayes_names)]
  label_resolve <- bayes_ext  # duplicate data frame to resolve label issues
  resolve_count <- 1
  
  # Establish log file to write which taxa labels don't match DNA_META indices.
  log_filename <- gsub("infile.nex.con.tre", "label_resolve.log", bayes_file)
  if (file.exists(log_filename)) {
    file.remove(log_filename)
  } 
  label_resolve_log <- file(description = log_filename, open = "w")
  
  # Combine each row of bayes data frame with specimen metadata in DNA_META.
  for (i in which(!is.na(bayes_df$label) == TRUE)) {
    
    # Index DNA specimen row id in DNA_META 
    dna_meta_id <- grep(bayes_df$label[i], dna_meta$taxa_label)
    
    # Resolve label issue if the BAYES_DF label does not match DNA_META.
    if (length(dna_meta_id) == 0) {
      cat(file = label_resolve_log, sep = "\n", append = TRUE,
          paste(bayes_df$label[i], "Does not match DNA_META, i == ", i))
      id_seq_label <- bayes_df$label[i]
      id_dupes <- unlist(strsplit(id_seq_label, "_P"))
      id_dupes_fix <- c(id_dupes[1],
                        sapply(id_dupes[2:length(id_dupes)], USE.NAMES = FALSE, 
                               function(id) { paste0("P", id) }))
      dna_meta_id <- grep(id_dupes_fix[1], dna_meta$taxa_label)
      label_issue <- TRUE
    }
    
    # Check for label match between bayes data and herbarium specimens.
    if (length(dna_meta_id) == 1) {
      
      # Retrieve bayes_df (i.e. beast data frame) row matching "label",
      # and get the column subset excluding $label from the set of names(). 
      # Subset is column bound to SPP_ROW to join BEAST data to specimen data.
      spp_row <- cbind(spp_match(dna_meta_id), as.data.frame(bayes_df[i, ]))
      bayes_ext[i, 1:ncol(spp_row)] <- spp_row[1, order(names(spp_row))]
    }
    
    # For nodes with multiple taxa, combine specimen and bayes data.
    if (label_issue == TRUE) {
      invisible(sapply(id_dupes_fix, USE.NAMES = FALSE, 
             function(label) {
               dna_meta_id <- grep(label, dna_meta$taxa_label)
               spp_row <- cbind(spp_match(dna_meta_id),
                                as.data.frame(bayes_df[i, ]))
               label_resolve[resolve_count, ] <<- spp_row[1, order(names(spp_row))]
               resolve_count <<- resolve_count + 1
             }))
    }
    label_issue <- FALSE  # reset label issue counter
  }
  # Bind rows of data missing labels (i.e. node parents and posterior probs)
  for (i in which(is.na(bayes_df$label) == TRUE)) {
    # plyr function extends the number of columns of the row to bind.
    bayes_ext <- rbind.fill(bayes_ext, bayes_df[i, ])
  }
  # Global variable assignment for nodes with multiple identical specimens.
  assign(x = "label_resolve", label_resolve, envir = .GlobalEnv)
  return(bayes_ext)
}

# Annotate ggtree objects taking into account nodes representing
# multiple specimens with identical DNA sequence data.
tree_taxa <- function(ggtree_df, bayes_file, x_adjust_factor = 2,
                      spp_id = "Physaria_syn", 
                      legend_title = "Reviewed Annotations", ...) {

  # Subset Bayesian probabilities at nodes (i.e. non-tip branches).
  bayes_probs <<- ggtree_df[which(ggtree_df$isTip == FALSE &
                                    !is.na(ggtree_df$prob)), ]
  
  # Index single nodes and x axis adjustment.
  single_nodes <- which(ggtree_df$node %in% unique(label_resolve$node) == FALSE)
  
  # Index for x axis adjustment
  label_table <- label_resolve[, c("node", spp_id)]
  label_table <- label_table[!duplicated(label_table), ]
  x_adjust <- max(range(ggtree_df$x)) / (3 * max(table(label_table$node)))
  
  # Build tree using ggtree package and specimen metadata layers.
  bayes_tree <- ggtree(ggtree_df) + 
    
    # Add text strings for probabilities to nodes.
    geom_text(data = bayes_probs, 
              aes(label = sprintf("%0.3f", round(as.numeric(bayes_probs$prob),
                                                 digits = 3))), 
              vjust = -0.45, hjust = 1.1, size = 2) + 
    
    # Add points for single specimen annotations by color and shape.
    geom_point(data =  ggtree_df[single_nodes, ], 
               aes_string(colour = spp_id, shape = spp_id), 
               size = 3, na.rm = TRUE) +
    scale_color_manual(legend_title, values = spp_color) +  
    scale_shape_manual(legend_title, values = spp_shape)
  
  # Add tip geoms scaled for multiple species obvservations per node.
  node_species <- label_resolve[, c("node", spp_id, "x", "y")] %>%
    arrange(node, get(spp_id))
  node_species_undupe <- node_species[!duplicated(node_species), ]  # remove dupes
  for (node in unique(node_species_undupe$node)) {
    species_obs <- unique(node_species_undupe[which(node_species_undupe$node == node), 
                                       spp_id])
    geom_size <- seq(from = 7, to = 3, length.out = length(species_obs))
    i <- 1
    x_adj_count <- 0
    for (species in species_obs) {
      node_data <- node_species_undupe[intersect(which(node_species_undupe$node == node),
                                          which(node_species_undupe[, spp_id] == species)), ]
      bayes_tree <- bayes_tree +
        geom_point(data = node_data,
                   aes_string(x = (node_data$x + x_adj_count), 
                              colour = spp_id, shape = spp_id),
                   size = round(geom_size[i], digits = 2))
      i <- i + 1
      x_adj_count <- x_adj_count + x_adjust
    }
  }
  
  # Add tip labels for single taxa nodes.
  x_max_adjust <<- max(range(ggtree_df$x)) + (x_adjust * x_adjust_factor)
  single_taxa_node <- ggtree_df[single_nodes, ]
  bayes_tree <- bayes_tree +
    geom_tiplab(data = single_taxa_node, mapping = aes(x = x_max_adjust),
                size = 3, linesize = .25, align = TRUE)

  # Add tip labels for multi taxa nodes.
  # https://guangchuangyu.github.io/2018/04/rename-phylogeny-tip-labels-in-treeio/
  multi_taxa_node <- ggtree_df[!(ggtree_df$node %in% single_nodes), ]
  multi_node_order <- table(node_species$node)[order(table(node_species$node), 
                                                     decreasing = TRUE)]
  multi_node_names <- as.numeric(names(multi_node_order))
  multi_taxa_ordered <- multi_taxa_node[match(multi_taxa_node$node, 
                                              multi_node_names), ]
  allele_names <- sapply(seq(1, length(multi_node_order)), USE.NAMES = FALSE,
                         function(allele) {
                           allele_name <- paste0("Genotype~", allele)
                           })
  multi_taxa_labels <- data.frame(label = multi_taxa_ordered$label,
                                 alleles = allele_names)
  bayes_tree <- bayes_tree %<+% multi_taxa_labels + 
    invisible(geom_tiplab(aes(label=paste0("bold(", alleles, ')'), 
                              x = x_max_adjust), 
                size = 3, linesize = .25, align = TRUE, parse = TRUE))
  
  # Write LABEL_RESOLVE data frame to .csv order by genotype to match tree.
  identical_seqs <- label_resolve[, c("label", "taxa_label", "node",
                                      "Collection_Number", "Collector", 
                                      "County", "State", "Herbarium", 
                                      "Physaria_syn", "Taxon_a_posteriori")]
  genotype_order <- sapply(multi_node_names, USE.NAMES = FALSE, function(node) {
    which(identical_seqs$node %in% node)
  })
  write.csv(x = identical_seqs[unlist(genotype_order), ],
            file = gsub("infile.nex.con.tre", "label-resolve.csv", 
                        bayes_file))
  
  # Return ggplot object.
  return(bayes_tree)
}

# Adjust scales and legends of Physaria tree.
tree_scale <- function(bayes_ggtree, x_expand = 0.0225, 
                       legend_y_pos = c(0, 0.8), ...) {
  bayes_ggtree <- bayes_ggtree + 
    theme(legend.position = legend_y_pos, 
          legend.justification = c(0, 1),
          legend.direction = "vertical",
          legend.text = element_text(size = 7),
          legend.box.background = element_blank()) +
    guides(colour = guide_legend(ncol = 1, byrow = TRUE)) +
    expand_limits(x = x_expand) # Optional adjustment of x-axis expansion.
  return(bayes_ggtree)
}

# Function to plot graphical object of ggtree build without x-axis clipping.
tree_grid <- function(bayes_ggtree_plot) {
  # Change layout panel clipping default from "on" to "off".
  # # From: https://gist.github.com/primaryobjects/700fe43b9631412fe0e1
  bayes_gtree <- ggplot_gtable(ggplot_build(bayes_ggtree_plot))
  bayes_gtree$layout$clip[bayes_gtree$layout$name == "panel"] <- "off"
  grid.arrange(bayes_gtree)
}

# Wrapper function for plotting ggtree objects with specimen annotations.
tree_plot <- function(consensus_tree_file, ...) {
  arguments <- list(...)
  herbarium_bayes_df <- invisible(herb_tree(bayes_file = consensus_tree_file))
  
  # Parse args and call function tree_taxa().
  tree_taxa_args <- arguments[which(names(arguments) %in%
                                      names(formals(tree_taxa)))]
  tree_taxa_arg_list <- c(list(ggtree_df = herbarium_bayes_df,
                               bayes_file = consensus_tree_file),
                          tree_taxa_args)
  bayes_tree <- do.call(what = tree_taxa, args = tree_taxa_arg_list)
  
  # Parse args and call function tree_scale().
  tree_scale_args <- arguments[which(names(arguments) %in%
                                       names(formals(tree_scale)))]
  tree_scale_arg_list <- c(list(bayes_ggtree = bayes_tree),
                           tree_scale_args)
  bayes_tree <- do.call(what = tree_scale, args = tree_scale_arg_list)
  
  return(bayes_tree)
}
