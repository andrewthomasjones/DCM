#' generateModelMatrices
#'
#' @param object model or results object
#' @param model_type string
#' @returns model matrices
#' @export
modelGraph <- function(object, model_type = NULL) {

  have_estimates <- FALSE
  estimates <- NA

  if ("results" %in% names(object)) {

    have_estimates <- TRUE
    results <- object$results
    estimates <- results$estimate

    model <- object$model
    names <- model$data$attribute_names

  }else {
    have_estimates <- FALSE
    model <- object
    names <- model$data$attribute_names

  }
  estimates <- estimates
  estimated_params <- parameterLabels(model)

  betas <- which(model$beta == 1, arr.ind = TRUE)
  gammas <- which(model$gamma != 0, arr.ind = TRUE)

  zeta_nodes <- colnames(model$beta)
  eta_nodes <- rownames(model$epsilon)

  mus <- stringr::str_match(estimated_params, "epsilon_\\[([0-9]{1,})")[, 2]
  mus <- mus[!is.na(mus)]

  sigmas <- stringr::str_match(estimated_params, "delta_sig_\\[([0-9]{1,})")[, 2]
  sigmas <- sigmas[!is.na(sigmas)]


  zeta_labels <- paste0(zeta_nodes,  ": ", greekLetters::greeks("zeta"), "@_{", seq_len(length(zeta_nodes)), "}")

  if (have_estimates) {
    mu_labels <- paste0(eta_nodes, ": ", greekLetters::greeks("mu"), "@_{", mus, "}", " = ",
                        round(results$estimate[stringr::str_detect(estimated_params, "epsilon_")], 3))
  } else {
    mu_labels <- paste0(eta_nodes, ": ", greekLetters::greeks("mu"), "@_{", mus, "}")
  }


  if (have_estimates) {
    beta_labels <-  paste0(greekLetters::greeks("beta"), "@_{",  betas[, 1], ", ", betas[, 2], "}",
                           " = ", round(results$estimate[stringr::str_detect(estimated_params, "beta_")], 3))
  } else {
    beta_labels <- paste0(greekLetters::greeks("beta"), "@_{",  betas[, 1], ", ", betas[, 2], "}")
  }

  if (sum(model$delta[, 2] == 1) > 0) {
    if (have_estimates) {
      gamma_labels <- paste0(greekLetters::greeks("sigma"), "@_{",  sigmas, "}", " = ",
                             round(results$estimate[stringr::str_detect(estimated_params, "delta_sig_")], 3))
    } else {
      gamma_labels <- paste0(greekLetters::greeks("sigma"), "@_{", sigmas, "}")
    }
  }else {

    if (have_estimates) {
      gamma_labels <-  paste0(greekLetters::greeks("gamma"), "@_{", gammas[, 1], ", ", gammas[, 2],
                              "}", " = ", round(results$estimate[stringr::str_detect(estimated_params, "gamma_")], 3))
    } else {
      gamma_labels <-  paste0(greekLetters::greeks("gamma"), "@_{", gammas[, 1], ", ", gammas[, 2], "}")
    }
  }

  node_set_z <- DiagrammeR::create_node_df(n = length(zeta_nodes), label = zeta_labels, type = "zeta", shape = "circle")
  node_set_e <- DiagrammeR::create_node_df(n = length(eta_nodes), label = mu_labels, type = "eta", shape = "circle")
  node_set <- DiagrammeR::combine_ndfs(node_set_z, node_set_e)

  edge_set_b <- DiagrammeR::create_edge_df(from = betas[, 1], to = betas[, 2], rel = "beta", label = beta_labels)
  edge_set_g <- DiagrammeR::create_edge_df(to = gammas[, 1] + length(zeta_nodes), from = gammas[, 2],
                                           rel = "gamma",  label = gamma_labels)
  edge_set <- DiagrammeR::combine_edfs(edge_set_b, edge_set_g)

  zeta_node_check <- node_set[node_set$type == "zeta", "id"]
  zeta_node_check2 <- c()

  for (i in seq_len(length(zeta_node_check))) {
    if (!(zeta_node_check[i] %in% edge_set$from || (zeta_node_check[i] %in% edge_set$to))) {
      zeta_node_check2 <- c(zeta_node_check2, i)
    }
  }

  if (!is.null(zeta_node_check2)) {
    node_set <- node_set[-zeta_node_check2, ]
  }


  if (nrow(betas) > 0) {

    # node_set$y <- NA
    # node_set$x <- NA
    #
    #
    #
    #
    # node_set[node_set$type == "beta", "y"] <- 0
    # node_set[node_set$type == "eta", "y"] <- 0.33
    # node_set[node_set$type == "sigma", "y"] <- 0.66
    # node_set[node_set$type == "zeta", "y"] <- 1
    #
    # node_set[node_set$type == "eta", "x"] <- seq_len(length(eta_nodes)) /length(eta_nodes)
    # node_set[node_set$type == "zeta", "x"] <- seq_len(length(zeta_nodes)) / length(zeta_nodes)
    # node_set[node_set$type == "sigma", "x"] <- seq_len(length(sigma)) / length(sigma)
    # node_set[node_set$type == "beta", "x"] <- seq_len(length(mus))/length(mus)

#     node_set[6, "y"] <- 8
#
#     node_set[1, "y"] <- 0
#     node_set[2, "y"] <- 0
#
#     node_set[6, "x"] <- max(seq_len(length(zeta_nodes)) * 2, 2 * seq_len(length(eta_nodes))) / 2 + 1
#
#     n_methods <- 2
#     method_spots <- seq(0, max(seq_len(length(zeta_nodes)), seq_len(length(eta_nodes))), length.out = n_methods + 2)
#
#     node_set[1, "x"] <- method_spots[2] * 2 + 1
#     node_set[2, "x"] <- method_spots[n_methods + 2 -  1] * 2 + 1
#
#     node_set[, "x"] <- node_set[, "x"] * 1.5
  }


  g <- DiagrammeR::create_graph(
    nodes_df = node_set,
    edges_df = edge_set
  )


  if (nrow(betas) == 0) {
    g <- DiagrammeR::set_node_attrs(g, node_attr = "fixedsize", values = FALSE)
    g <- DiagrammeR::add_global_graph_attrs(g, "layout", "dot", "graph")
    g <- DiagrammeR::add_global_graph_attrs(g, "concentrate", "true", "graph")
    g <- DiagrammeR::add_global_graph_attrs(graph = g, attr = "ranksep", value = 2, attr_type = "graph")
  }

  return(g)

}
