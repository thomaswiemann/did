#' @title Generate Cross-Fitting Subsamples
#'
#' @description Utility functions for generating cross-fitting subsamples,
#'  optionally grouped by treatment cohort and/or clustered.
#'
#' @keywords internal
#' @noRd

# Simple function to generate subsamples.
generate_folds <- function(nobs, num_folds) {
  sampleframe <- rep(1:num_folds, ceiling(nobs/num_folds))
  fold_ids <- sample(sampleframe, size=nobs, replace=FALSE)
  subsamples <- sapply(1:num_folds,
                       function(x) {which(fold_ids == x)},
                       simplify = FALSE)
  subsamples
}

# Simple function to create a temp cluster variable for efficient mapping
get_cluster_map <- function(cluster_variable) {
  tmp_cluster <- as.numeric(factor(cluster_variable))
  cluster_map <- split(seq_along(tmp_cluster), tmp_cluster)
  list(tmp_cluster = tmp_cluster, cluster_map = cluster_map,
       n_cluster = length(unique(tmp_cluster)))
}

#' @title Get Cross-Fitting Subsamples by Group
#'
#' @description Computes cross-fitting subsamples stratified by treatment
#'  group \code{G}. Supports cluster-level splitting when units share a
#'  cluster identifier.
#'
#' @param cluster_variable Vector of cluster identifiers (or unit IDs
#'  if no clustering).
#' @param G Vector of treatment group indicators.
#' @param num_folds Number of cross-fitting folds.
#' @param subsamples_byG Optional pre-computed subsamples (named list by
#'  group level). If \code{NULL}, subsamples are generated.
#'
#' @return A named list (keyed by group level) of fold assignments. Each
#'  element is a list of \code{num_folds} index vectors.
#'
#' @keywords internal
#' @noRd
get_crossfit_subsamples <- function(cluster_variable, G,
                                    num_folds = 10,
                                    subsamples_byG = NULL) {

  # Data parameters
  nobs <- length(cluster_variable)
  cluster <- !identical(cluster_variable, 1:nobs)
  G_levels <- sort(unique(G))
  n_G_levels <- length(G_levels)

  if (is.null(subsamples_byG)) {
    # Create sample fold tuple by group levels
    subsamples_byG <- rep(list(NULL), n_G_levels)
    for (g in 1:n_G_levels) {
      if (cluster) {
        tmp_cl <- get_cluster_map(cluster_variable[G==G_levels[g]])
        subsamples_temp <- generate_folds(tmp_cl$n_cluster, num_folds)
        subsamples_byG[[g]] <- lapply(subsamples_temp, function(x) {
          unname(unlist(tmp_cl$cluster_map[x]))
        })
      } else {
        subsamples_byG[[g]] <- generate_folds(sum(G==G_levels[g]), num_folds)
      }
    }
    names(subsamples_byG) <- G_levels
  }

  subsamples_byG
}
