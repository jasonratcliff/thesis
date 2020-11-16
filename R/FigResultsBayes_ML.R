library(ThesisPackage)
library(ggplot2)
library(magrittr)
library(ggtree)

# Results: MrBayes multi-locus ggtree plot
FigResultsBayes_ML <-
  list.files(system.file("extdata/MrBayes", package = "ThesisPackage"),
             full.names = TRUE, pattern = "ml-infile.nex.con.tre") %>%
  read_tree(tree_file = .) %>%
  join_bayes(tree_data = ., id_column = "prior_id",
             scale_vector = c(4, 8)) %>%
  bayes_plot(joined_ggtree = .,
             id_column = "prior_id", scale_name = "Prior Annotation",
             x_expand = 0.022, plot_x = .15, plot_y = 0.45)

ThesisPackage::save_plot(
  gg_plot = FigResultsBayes_ML,
  height = 8, width = 6
)
