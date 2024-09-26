#' @title Process Results from [compute.att_gt()]
#'
#' @param attgt.list list of results from [compute.att_gt()]
#'
#' @return list with elements:
#' \item{group}{which group a set of results belongs to}
#' \item{tt}{which time period a set of results belongs to}
#' \item{att}{the group time average treatment effect}
#'
#' @export
process_attgt <- function(attgt.list) {
  nG <- length(unique(unlist(BMisc::getListElement(attgt.list, "group"))))
  nT <- length(unique(unlist(BMisc::getListElement(attgt.list, "year"))))

  # create vectors to hold the results
  group <- c()
  att <- c()
  tt <- c()
  i <- 1
  ddml_weights <- ddml_mspe <- ddml_rf <- rep(list(NULL), nG * nT)

  # populate result vectors and matrices
  for (f in 1:nG) {
    for (s in 1:nT) {
      group[i] <- attgt.list[[i]]$group
      tt[i] <- attgt.list[[i]]$year
      att[i] <- attgt.list[[i]]$att
      ddml_weights[[i]] <- attgt.list[[i]]$ddml_fit$weights
      ddml_mspe[[i]] <- attgt.list[[i]]$ddml_fit$mspe
      ddml_rf[[i]] <- attgt.list[[i]]$ddml_fit$rf
      i <- i+1
    }
  }

  list(group=group, att=att, tt=tt,
       ddml_weights = ddml_weights,
       ddml_mspe = ddml_mspe,
       ddml_rf = ddml_rf)
}
