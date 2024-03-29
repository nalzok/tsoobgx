% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bgx.plot.tree.R
\name{bgx.plot.tree}
\alias{bgx.plot.tree}
\title{Plot a boosted tree model}
\usage{
bgx.plot.tree(feature_names = NULL, model = NULL, trees = NULL,
  plot_width = NULL, plot_height = NULL, render = TRUE,
  show_node_id = FALSE, ...)
}
\arguments{
\item{feature_names}{names of each feature as a \code{character} vector.}

\item{model}{produced by the \code{bgx.train} function.}

\item{trees}{an integer vector of tree indices that should be visualized.
If set to \code{NULL}, all trees of the model are included.
IMPORTANT: the tree index in tsoobgx model is zero-based
(e.g., use \code{trees = 0:2} for the first 3 trees in a model).}

\item{plot_width}{the width of the diagram in pixels.}

\item{plot_height}{the height of the diagram in pixels.}

\item{render}{a logical flag for whether the graph should be rendered (see Value).}

\item{show_node_id}{a logical flag for whether to show node id's in the graph.}

\item{...}{currently not used.}
}
\value{
When \code{render = TRUE}:
returns a rendered graph object which is an \code{htmlwidget} of class \code{grViz}.
Similar to ggplot objects, it needs to be printed to see it when not running from command line.

When \code{render = FALSE}:
silently returns a graph object which is of DiagrammeR's class \code{dgr_graph}.
This could be useful if one wants to modify some of the graph attributes
before rendering the graph with \code{\link[DiagrammeR]{render_graph}}.
}
\description{
Read a tree model text dump and plot the model.
}
\details{
The content of each node is organised that way:

\itemize{
 \item Feature name.
 \item \code{Cover}: The sum of second order gradient of training data classified to the leaf.
       If it is square loss, this simply corresponds to the number of instances seen by a split
       or collected by a leaf during training.
       The deeper in the tree a node is, the lower this metric will be.
 \item \code{Gain} (for split nodes): the information gain metric of a split
       (corresponds to the importance of the node in the model).
 \item \code{Value} (for leafs): the margin value that the leaf may contribute to prediction.
} 
The tree root nodes also indicate the Tree index (0-based).

The "Yes" branches are marked by the "< split_value" label.
The branches that also used for missing values are marked as bold
(as in "carrying extra capacity").

This function uses \href{http://www.graphviz.org/}{GraphViz} as a backend of DiagrammeR.
}
\examples{
data(agaricus.train, package='tsoobgx')

bst <- tsoobgx(data = agaricus.train$data, label = agaricus.train$label, max_depth = 3,
               eta = 1, nthread = 2, nrounds = 2,objective = "binary:logistic")
# plot all the trees
bgx.plot.tree(model = bst)
# plot only the first tree and display the node ID:
bgx.plot.tree(model = bst, trees = 0, show_node_id = TRUE)

\dontrun{
# Below is an example of how to save this plot to a file. 
# Note that for `export_graph` to work, the DiagrammeRsvg and rsvg packages must also be installed.
library(DiagrammeR)
gr <- bgx.plot.tree(model=bst, trees=0:1, render=FALSE)
export_graph(gr, 'tree.pdf', width=1500, height=1900)
export_graph(gr, 'tree.png', width=1500, height=1900)
}

}
