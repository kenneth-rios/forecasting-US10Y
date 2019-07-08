
## Plot Decision Trees for each of the training sets used in the analysis
library(tree)

# Create function that takes training dataset

plot_tree <- function(data, num_nodes = 5, p.title){
  
  tree.mod <- tree(UST10Y ~. - FF3Y, data = data)
  prune.tree.mod <- prune.tree(tree.mod, best = num_nodes)
  
  plot(prune.tree.mod)
  text(prune.tree.mod, pretty = 0)
  title(main = p.title)
  
}

# Plot decision trees for tight loose
plot_tree(train_tight, num_nodes = 5, p.title = "Pruned Decision Tree: Tight Block \n")
plot_tree(train_loose, num_nodes = 5, p.title = "Pruned Decision Tree: Loose Block \n")
plot_tree(train_moderation, num_nodes = 5, p.title = "Pruned Decision Tree: Great Moderation \n")
plot_tree(train_zirp, num_nodes = 5, p.title = "Pruned Decision Tree: Zero Interest Rate Policy \n")
