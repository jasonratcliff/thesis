library(ThesisPackage)
library(ggplot2)
library(magrittr)
library(ggtree)

# Results: MrBayes rITS ggtree plot
FigResultsBayes_rITS <-
  list.files(system.file("extdata/MrBayes", package = "ThesisPackage"),
             full.names = TRUE, pattern = "rITS-infile.nex.con.tre") %>%
  read_tree(tree_file = .) %>%
  join_bayes(tree_data = ., id_column = "prior_id",
             scale_vector = c(4, 12)) %>%
  bayes_plot(joined_ggtree = .,
             id_column = "prior_id", scale_name = "Prior Annotation",
             x_expand = 0.025, plot_x = 0.125, plot_y = 0.425)

ThesisPackage::save_plot(gg_plot = FigResultsBayes_rITS)
