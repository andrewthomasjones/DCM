#' fixed_model_graph
#'
#' @param fitted fitted model fixed only for now
#' @returns graph object
#' @export
fixed_model_graph <- function(fitted){

  x_names <- fitted$model$data$attribute_names
  n <- nrow(fitted$results)

  lhs <- rep("V",n)
  op  <- rep("~",n)
  rhs <- x_names
  est <- fitted$results$estimate

  paths <- data.frame(lhs,op,rhs,est)

  label <- c(rhs, "V")
  shape <- c(rep('rectangle', n), "circle")
  type <- c(rep('a', n), "b")

  node_set <- DiagrammeR::create_node_df(n=length(label), label=label, type=type, shape=shape)

  paths <- dplyr::filter(paths, op == "~")
  paths <- dplyr::rename(paths, to = lhs, from = rhs, label = est)
  paths <- dplyr::mutate(paths, style = "solid")
  paths <- dplyr::select(paths, from, to, style, label)

  edge_set <- DiagrammeR::create_edge_df(fontsize = '10', from=match(paths$from, node_set$label), to=match(paths$to, node_set$label), rel="a", label=paste0(greeks("mu"),1:n,"=",round(paths$label,3)))


  g <- DiagrammeR::create_graph(
    nodes_df = node_set,
    edges_df = edge_set)


  DiagrammeR::set_node_attrs(g, node_attr = "fixedsize",values = FALSE) -> g1
  DiagrammeR::add_global_graph_attrs(g1,"layout", "dot", "graph") -> g2
  DiagrammeR::add_global_graph_attrs(g2, "concentrate", "true", "graph") ->g3
  DiagrammeR::add_global_graph_attrs(graph = g3, attr = "ranksep", value = 2, attr_type="graph")  ->g4
  DiagrammeR::render_graph(g4)

  return(g4)

}
