#' @title Dimensionality Check
#' @description `dimensionality_check` performs a dimensionality assessment of a set of elementary indicators
#' using either the Item Response Theory (IRT) framework or the Factor Analysis (FA).
#' @param indicator_list list of outputs about each indicator computable for the target unit
#' (e.g., company or contracting authority), as returned by, for example, [ind_1()], [ind_2()], etc.
#' @param dim_method method for the dimensionality assessment, to be chosen between `"IRT"` and `"FA"`.
#' If the former is selected, the dimensionality of elementary indicators is evaluated in the IRT framework
#' using [mirt::mirt()] function. On the other hand, exploratory factor analysis is used by means of function
#' [psych::fa()]. See Details.
#' @param cutoff threshold for dichotomising the indicators (see [normalise()]).
#' @param missing method for imputing missing values (see [manage_missing()]):
#'
#'  - `missing = 0`: missing values are replaced with '0' (not at risk);
#'
#'  - `missing = 1`: missing values are imputed using logistic regression.
#'
#' @param max_ndim maximum number of dimensions to check in the IRT framework (not greater than the number
#' of elementary indicators).
#' @param nrep number of replicates for random initialisation of the algorithm for fitting IRT models.
#' @param ... optional arguments for [mirt::mirt()] (e.g., estimation algorithm, convergence threshold,
#' etc.) or [psych::fa()] (e.g., method for factor extraction, rotation method, etc.).
#' @return different objects according to `dim_method`:
#'
#' - `dim_method = "IRT"`: a list of IRT models (as returned by [mirt::mirt()]) for each possible dimensional
#' solution, from one to `max_ndim` dimensions;
#'
#' - `dim_method = "FA"`: best factorial solution (as returned by [psych::fa()]).
#'
#' @details The function for dimensionality evaluation about a set of the elementary indicators is implemented
#' as follows. Firstly, it deals with **dichotomised** indicators (as those proposed in CO.R.E.),
#' **without missing values**. Consequently, before carrying out the dimensionality assessment, the user has to provide
#' the list of indicators (see argument `indicator_list`) together with two further arguments for their dichotomisation
#' (`cutoff`) and missing management (`missing`).
#'
#' Then, the dimensionality check is performed according to the chosen method (`dim_method`).
#'
#' If `dim_method = "IRT"`, the IRT framework is considered (by means of [mirt::mirt()] function). In this case,
#' as first step, the function evaluates the model fitting of the *Rasch model* against the *2PL model*
#' (two-parameter logistic), two of the most widely used IRT models for binary data. It is evaluated under the
#' unidimensional setting, in order to understand which type of model has a better fit on the data at hand
#' (using common penalised likelihood metrics, such as AIC, SABIC, BIC, etc.).
#'
#' As second step, multidimensional models are estimated by incrementing the number of dimensions each time, from two
#' onwards (until `max_ndim`). For a given number of dimensions, say \eqn{d}, several estimates of the IRT model
#' (i.e., Rasch or 2PL, according to step 1) are obtained on the data at hand according the different initialisations
#' of the estimation algorithm:
#'
#' - a first initialisation is *deterministic*, based on observed data;
#'
#' - the others (according to `nrep`) are *random*, in order to completely explore the likelihood function to maximise.
#'
#' Finally 1 + `nrep` estimates of the IRT model with \eqn{d} dimensions are obtained and that with the largest
#' value of maximised likelihood is saved in the list to be returned.
#'
#' Step 2 is repeated starting from \eqn{d = 2} until \eqn{d =} `max_ndim`. As the function ends, a list of `max_ndim`
#' IRT models is returned, one for each potential number of dimensions. Moreover, a summary of the dimensionality check
#' is displayed, showing, for each \eqn{d}, the model fitting metrics of the best model with \eqn{d} dimensions.
#' This summary helps in selecting the most suitable dimensional solution.
#'
#'
#' If `dim_method = "FA"`, exploratory factor analysis is considered (using [psych::fa()] function). In particular,
#' in order to find the suitable number of factors to extract, this function computes the eigenvalues of the
#' correlation matrix among the elementary indicators (computed using the tetrachoric correlation, given the binary
#' nature of our indicators). Then, given the "eigenvalues > 1" rule, the suitable number of factors is retained and
#' used in calling [psych::fa()]. The plot of the eigenvalues against the number of factors is also displayed.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  mock_data_core_variants <- unnest(mock_data_core, varianti, keep_empty = TRUE)
#'  out_companies <- ind_all(data = mock_data_core,
#'                          data_ind8 = mock_data_core_variants,
#'                          emergency_name = "coronavirus",
#'                          target_unit = "companies")
#'   out_dim <- dimensionality_check(indicator_list = out_companies,
#'                                 dim_method = "IRT",
#'                                 max_ndim = 4,
#'                                 cutoff = 0.95,
#'                                 missing = 0,
#'                                 nrep = 3,
#'                                 TOL = 0.1,
#'                                 verbose = TRUE,
#'                                 method = "QMCEM")
#'    }
#'  }
#' @seealso
#'  \code{\link[mirt]{mirt}}
#'  \code{\link[psych]{fa}}
#' @rdname dimensionality_check
#' @export
#' @importFrom mirt mirt
#' @importFrom psych fa

dimensionality_check <- function(indicator_list,
                                 dim_method = "IRT",
                                 cutoff = 0.95,
                                 missing = 0,
                                 max_ndim = length(indicator_list),
                                 nrep = 5,
                                 seed = NULL, #random
                                 arg_tech_list= NULL,
                                 ...) {

  match.arg(dim_method, c("IRT", "FA"))
  Q <- length(indicator_list)
  if (max_ndim > Q) stop("Number of dimensions can not be greater than the number of elementary indicators")
  data_matrix <- create_indicator_matrix(out_list = indicator_list)
  data_matrix <- normalise(data = data_matrix, method = "binary", cutoff = cutoff)
  data_matrix2 <- manage_missing(data = data_matrix, missing = missing)

  cat(paste0("\n\n************************** DIMENSIONALITY CHECK through ", dim_method, " **************************\n"))
  if (dim_method == "IRT") {

    # first check: Rasch or 2PL? Look at unidimensional model for the best one
    cat("---------------------------------------------------\n")
    cat("Fitting unidimensional Rasch model \n")
    out_rasch1 <- mirt::mirt(data = data_matrix2[, -c(1, 2)],
                             model = 1,
                             itemtype = "Rasch",
                             technical = arg_tech_list,
                             ...)
    cat("---------------------------------------------------\n")
    cat("Fitting unidimensional 2PL model \n")
    out_2pl1 <- mirt::mirt(data = data_matrix2[, -c(1, 2)],
                           model = 1,
                           itemtype = "2PL",
                           technical = arg_tech_list,
                           ...)

    cat("---------------------------------------------------\n")
    cat("\nStep 1: Rasch vs. 2PL \n")
    summary_uni <- anova(out_rasch1, out_2pl1)
    rownames(summary_uni) <- c("rasch", "2pl")
    print(summary_uni)

    out_mirt <- list()
    bic_rasch <- anova(out_rasch1)$BIC
    bic_2pl <- anova(out_2pl1)$BIC
    if(bic_rasch < bic_2pl) {
      best <- "Rasch"
      out_mirt[[1]] <- out_rasch1
    } else {
      best <- "2PL"
      out_mirt[[1]] <- out_2pl1
    }

    if (length(nrep) == 1) nrep <- rep(nrep, max_ndim - 1)

    cat("\n\nStep 2: multidimensional models")
    # start fitting multidimensional models (from 2 dimensions onwards)
    for (d in 2:max_ndim) {
      cat("\n---------------------------------------------------\n")
      cat(paste0("Fitting ", best, " model with ", d, " dimensions \n"))

      # deterministic start
      out_mirt_det <- mirt::mirt(data = data_matrix2[, -c(1, 2)],
                                 model = d,
                                 itemtype = best,
                                 GenRandomPars = FALSE,
                                 technical = arg_tech_list,
                                 ...)
      bic_det <- anova(out_mirt_det)$BIC

      # random starts
      out_mirt_rnd <- list()
      bic_rnd <- NULL

      for (i in 1:(nrep[d - 1])) {
        if (is.null(seed)) tech_list <- arg_tech_list
        else {
          if (length(seed) != nrep[d - 1]) stop("Please, check seed length")
          tech_list <- append(arg_tech_list, seed[i])
          names(tech_list)[length(tech_list)] <- "set.seed"
        }
        out_mirt_rnd[[i]] <- mirt::mirt(data = data_matrix2[, -c(1, 2)],
                                        model = d,
                                        itemtype = best,
                                        GenRandomPars = TRUE,
                                        technical = tech_list,
                                        ...)
        bic_rnd <- c(bic_rnd, anova(out_mirt_rnd[[i]])$BIC)
      }
      # best random
      cat(paste0("\nFit of models with ", d, " dimensions\n"))
      summary_det <- data.frame(replicate = "det", BIC = bic_det)
      summary_rnd <- data.frame(replicate = paste0("rnd", 1:length(bic_rnd)),
                                BIC = bic_rnd)
      summary_models <- rbind(summary_det, summary_rnd)
      print(summary_models, row.names = FALSE)

      best_rnd <- out_mirt_rnd[[which.min(bic_rnd)]]

      if (bic_det < min(bic_rnd)) out_mirt[[d]] <- out_mirt_det
      else out_mirt[[d]] <- best_rnd
    }

    cat("---------------------------------------------------\n")
    cat("\n\nSummary of the dimensionality check\n")
    print(data.frame(dim = 1:max_ndim,
                     sapply(out_mirt, anova) %>% t),
          row.names = FALSE)
    out <- out_mirt
  }

  # factor analysis using tetrachoric correlation
  if (dim_method == "FA") {
    R <- psych::tetrachoric(data_matrix2[, -c(1, 2)])$rho %>% suppressMessages()
    lambda <- eigen(R)$values

    # Eigenvalue plot
    plot(lambda, type="o", xlab="Number of factors", ylab="Eigenvalues", pch=20)
    abline(h=1, lty=2)

    nfact <- which(lambda > 1) %>% tail(1)
    cat(paste0("According to eigenvalues, ", nfact, " factors are retained\n"))

    out <- psych::fa(R, nfactors = nfact, n.obs=nrow(data_matrix2), ...)

    # fm="pa", rotate="oblimin"
    # x <- psych::fa(R, nfactors = 1, n.obs=nrow(data_matrix2), ...)
    # x2 <- psych::fa(R, nfactors = 2, n.obs=nrow(data_matrix2), ...)
    # x3 <- psych::fa(R, nfactors = 3, n.obs=nrow(data_matrix2), ...)
    # anova(x, x2, x3)

  }


  return(out)
}
