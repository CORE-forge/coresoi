#' Create matrix of elementary indicators
#' @title create_indicator_matrix
#' @description `create_indicator_matrix` creates the data matrix of elementary indicators
#' (row = target unit; columns = indicator values). **It is an internal function.**
#' @param out_list list of outputs about each indicator computable for the target unit
#' (e.g., company or contracting authority), as returned by, for example, [ind_1()], [ind_2()], etc.
#' @return data matrix with aggregation ID of the target units as first column and
#' indicator values as subsequent columns (according to `out_list`).
#' @details `out_list` must be manually created by including all the outputs of the functions
#' for computing the elementary indicators ([ind_1()], [ind_2()], etc.) in a list.
#' Note that each target unit has its own set of elementary indicators. Moreover, target units
#' in each output of `out_list` can be different and a full join is carried out for merging all the
#' indicators and building the final data matrix.
#' @keywords internal
#' @export
#' @rdname create_indicator_matrix
#' @importFrom dplyr select full_join
create_indicator_matrix <- function(out_list) {
  cat("---------------------------------------------------\n")
  cat("Creating the matrix of elementary indicators \n")

  # check that aggregation_type is the same for each element of the list
  aggr_type <- sapply(out_list, function(x) x$aggregation_type[1])
  if (length(unique(aggr_type)) != 1) {
    stop("Please, check indicator list: different aggregation types are found")
  }
  aggr_type <- aggr_type[1] # codice_fiscale or cf_amministrazione_appaltante
  n <- length(out_list) # no. of indicators (7 or 6)
  ind_id <- sapply(out_list, function(x) x$indicator_id[1])

  X <- out_list[[1]] %>%
    dplyr::select(aggregation_name, indicator_value) %>%
    dplyr::full_join(out_list[[2]] %>%
      dplyr::select(aggregation_name, indicator_value), by = "aggregation_name")
  for (i in 3:n) {
    X <- X %>%
      dplyr::full_join(out_list[[i]] %>%
        dplyr::select(aggregation_name, indicator_value), by = "aggregation_name")
  }
  names(X)[-1] <- paste0("ind", ind_id)
  # X is a matrix/dataframe with:
  # first column: aggregation name (ID)
  # other columns: elementary indicators
  return(X)
}


#' Manage missing values in elementary indicators
#' @title manage_missing
#' @description `manage_missing` deals with the imputation of missing values in the elementary indicators.
#' @param data data matrix of elementary indicators (as returned by [create_indicator_matrix()]).
#' @param missing method for imputing missing values:
#'
#'  - `missing = 0`: missing values are replaced with '0' (not at risk);
#'
#'  - `missing = 1`: missing values are imputed using logistic regression. See Details.
#'
#' @param verbose whether a summary of imputed values has to be printed (when `missing = 1`).
#' @param seed seed for the random draw from a Bernoulli r.v. (when `missing = 1`). See Details.
#' @return data matrix with no missing value.
#' @details When `missing = 1`, elementary indicators are split up into two groups, according to
#' the presence/absence of (at least one) missing values. Hence, the set of indicators without
#' missing values (taken as covariates) is used to 'predict' the missing values in each of the other
#' indicators (seen as dependent variable), using several logistic regression models, one for each
#' indicator with missing values. As such, these models can predict a probability for each combination
#' of observed indicators; then, a random draw from a Bernoulli distribution with the predicted
#' probability as parameter is performed for imputing '0' or '1' in the indicator with missing values.
#' @keywords internal
#' @seealso
#'  \code{\link[tidyr]{drop_na}}
#'  \code{\link[dplyr]{select}}
#' @rdname manage_missing
#' @export
#' @importFrom tidyr drop_na
#' @importFrom dplyr select
manage_missing <- function(data,
                           missing = 0,
                           verbose = TRUE,
                           seed = 1234) {
  data2 <- data
  if (missing == 0) {
    cat("---------------------------------------------------\n")
    cat("Replacing missing values in each indicator with '0' \n")
    data2[is.na(data2)] <- 0
  }
  if (missing == 1) {
    cat("---------------------------------------------------\n")
    cat("Imputing missing values in each indicator using logistic regression \n")
    # logistic regression of each indicator with (at least one) NAs on all the indicators with no NAs
    flag_missing <- apply(data[, -1], 2, function(x) 1 * any(is.na(x)))
    ind_nomiss <- which(flag_missing == 0) %>% names()
    ind_miss <- which(flag_missing == 1) %>% names()

    # if all indicators have at least one missing value --> external method to impute
    if (length(ind_miss) == length(flag_missing)) {
      stop("All the indicators have at least one missing value: please, use an external method to impute them")
    }

    cat("\n")
    for (i in 1:length(ind_miss)) {
      cat("---------------------------------------------------\n")
      cat(paste0("Imputation for '", ind_miss[i], "'"))
      ind_tomodel <- ind_miss[i]

      # complete data for logistic model
      data_for_model <- data %>%
        tidyr::drop_na(ind_tomodel) %>%
        dplyr::select(ind_nomiss)

      # data with missing values for imputation (no dplyr!)
      data_to_impute <- data[is.na(data[, ind_tomodel]), ind_nomiss] %>%
        as.data.frame()
      names(data_to_impute) <- ind_nomiss
      cat(paste0(" - observations to impute: ", nrow(data_to_impute), " out of ", nrow(data), "\n"))

      formula_x <- paste(ind_nomiss, collapse = " + ")
      formula_glm <- paste(ind_tomodel, formula_x, sep = " ~ ")

      mod <- glm(formula = formula_glm, data = data, family = binomial)
      fitted_prob <- predict(mod, type = "response", newdata = data_to_impute)
      # Bernoulli draw using predicted probabilities
      set.seed(seed)
      ind_imputed <- rbinom(n = length(fitted_prob), size = 1, prob = fitted_prob)
      if (verbose) {
        # print(summary(mod))
        cat("\nSummary of the imputed values: \n")
        print(summary(ind_imputed))
      }
      data2[is.na(data2[, ind_tomodel]), ind_tomodel] <- ind_imputed
    }
  }
  return(data2)
}


#' Normalise the elementary indicators
#' @title normalise
#' @description `normalise` normalises the elementary indicators using a suitable normalisation method
#' (e.g., ranking, min-max, dichotomisation, etc.).
#' @param data data matrix of elementary indicators (as returned by [create_indicator_matrix()])
#' @param method normalisation method, to be chosen among:
#'
#'  - `"binary"`: each elementary indicator is dichotomised (0/1) using a suitable threshold,
#'  to be specified through the argument `cutoff`. Specifically, the normalised indicator
#'  will be equal to 1 if the original indicator is greater then the threshold, and 0 otherwise;
#'
#'  - `"ranking"`: each elementary indicator is normalised according to the ranking (see [rank()]);
#'
#'  - `"z-score"`: each elementary indicator is standardised into z-scores (see [scale()]).
#'  Let \eqn{x_{qc}} be the original value of elementary indicator \eqn{q} for target unit \eqn{c}. Then,
#'  the z-score is obtained as follows:
#'
#'  \deqn{I_{qc} = \frac{x_{qc} - \mu_q(x_{qc})}{\sigma_q(x_{qc})}}
#'
#'  - `"minmax"`: each elementary indicator is normalised using the 'min-max' criterion:
#'
#'  \deqn{I_{qc} = \frac{x_{qc} - min(x_{qc})}{(max(x_{qc}) - min(x_{qc}))}}
#'
#'  - `"distref"`: each elementary indicator is normalised by dividing it by its maximum;
#'
#'  - `"catscale"`: each elementary indicator is discretised into five categories, according to suitable
#'  sample quantiles.
#'
#' @param cutoff threshold for dichotomising the indicators (when `method = "binary"`).
#' @return data matrix of normalised indicators according to the chosen method.
#' @details In the CO.R.E. project, according to the proposed set of elementary indicators,
#' a suitable normalisation method should be the *'dichotomisation'* (i.e., `method = "binary"`),
#' in order to make all the indicators as binary. In particular, if a given normalised elementary
#' indicator is equal to 1, this means that the target unit is considered **at risk** on the basis
#' of that indicator. On the other hand, the target unit is considered **not at risk**.
#'
#' In fact, some elementary indicators perform a statistical test and return, as risk metric, one minus
#' the p-value of the test (hence, a continuous scale, even if bounded in \eqn{[0,1]}); some others
#' do not consider any statistical tests, are binary by nature and directly return the target unit as
#' 'not at risk/at risk' (0/1).
#'
#' In order to bring all the elementary indicators to the same metric, that is, 'not at risk/at risk' (0/1),
#' the group of indicators that rely on statistical testing must be normalised (i.e., dichotomised)
#' by specifying a suitable threshold for the significance of the involved tests.
#' It corresponds to one minus the threshold for the significance of the p-value of the test performed
#' by the indicator. Example: `cutoff = 0.95` means that the threshold for the significance of the p-values
#' is the usual 0.05.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   normalise(data_matrix, method = "binary", cutoff = 0.95)
#' }
#' }
#' @rdname normalise
#' @export
normalise <- function(data,
                      method = "binary",
                      cutoff = 0.95) {
  match.arg(method, c("binary", "ranking", "z-score", "minmax", "distref", "catscale"))

  cat("---------------------------------------------------\n")
  cat(paste0("Normalising the indicators according to method '", method, "' \n"))

  if (method == "binary") {
    if (length(cutoff) != 1) stop("Please, define a unique value for cutoff")
    cat("\nMaking binary indicators according to cut-off", cutoff, "\n")
    if (cutoff < 0.9) {
      warning("The cut-off for labelling 'at risk' is less than those usually employed (at least 0.90, corresponding to p-value of 0.10)")
    }
    data_norm <- data.frame(
      data[, 1],
      apply(
        X = data[, -1],
        MARGIN = 2,
        FUN = function(x) {
          cut(x, breaks = c(-Inf, cutoff, Inf), labels = c(0, 1)) %>%
            as.character() %>%
            as.numeric()
        }
      )
    )
  }
  if (method == "ranking") {
    data_norm <- data.frame(
      data[, 1],
      apply(
        X = data[, -1],
        MARGIN = 2,
        FUN = rank
      )
    )
  }
  if (method == "z-score") {
    data_norm <- data.frame(
      data[, 1],
      scale(data[, -1])
    )
  }
  if (method == "minmax") {
    data_norm <- data.frame(
      data[, 1],
      apply(
        X = data[, -1],
        MARGIN = 2,
        FUN = function(x) {
          (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
        }
      )
    )
  }
  if (method == "distref") {
    data_norm <- data.frame(
      data[, 1],
      apply(
        X = data[, -1],
        MARGIN = 2,
        FUN = function(x) {
          x / max(x, na.rm = TRUE)
        }
      )
    )
  }
  if (method == "catscale") {
    data_norm <- data.frame(
      data[, 1],
      apply(
        X = data[, -1],
        MARGIN = 2,
        FUN = function(x) {
          tmp <- quantile(x, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
          res <- x * 0
          res + 0.25 * (x > tmp[1]) + 0.25 * (x > tmp[2]) + 0.25 * (x > tmp[3]) + 0.25 * (x > tmp[4])
        }
      )
    )
  }
  return(data_norm)
}


#' Get the set of weights for building the composite indicator
#' @title get_weights
#' @description `get_weights` returns the weights for constructing the composite indicator.
#' @param data data matrix of **binary** elementary indicators (without missing values).
#' @param method method for getting the set of weights. Possible choices are:
#' `"equal"`, `"experts"` or `"irt"`. See Details.
#' @param ... optional arguments for [mirt::mirt()] function. See Details.
#' @return vector of weights.
#' @details This function returns a vector of weights, whose dimension is equal to the
#' number of elementary indicators in `data` (say, \eqn{Q}). Three choices are offered:
#'
#' - `"equal"`: each elementary indicator receives the same weight, equal to \eqn{1/Q};
#'
#' - `"experts"`: each elementary indicator receives a specific weight provided by experts;
#'
#' - `"irt"`: each elementary indicator receives a specific weights provided by the IRT framework.
#' Specifically, a unidimensional 2PL IRT model is estimated on `data` through [mirt::mirt()] function,
#' for which possible additional arguments can be provided (`...`), such as estimation algorithm,
#' numerical optimiser, convergence threshold, etc. Once the model is fitted, weights are computed
#' by rescaling the estimated discrimination parameters (in such a way they sum to 1).
#' *Note: for the moment, IRT weights can be obtained only when the elementary indicators are all binary.*
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[mirt]{mirt}}, \code{\link[mirt]{MDISC}}
#' @rdname get_weights
#' @export
#' @importFrom mirt mirt MDISC
get_weights <- function(data,
                        method,
                        ...) {
  match.arg(arg = method, choices = c("equal", "experts", "irt"))
  Q <- ncol(data[, -1]) # number of elementary indicators

  if (method == "irt") {
    cat("---------------------------------------------------\n")
    cat("Computing IRT weights using discrimination parameters of a 2PL model \n")
    # Check whether the elementary indicators are all binary
    nu <- apply(
      X = data[, -1],
      MARGIN = 2,
      FUN = function(x) length(unique(x))
    )
    if (any(nu > 2)) stop("Elementary indicators must be binary for the moment")

    # Check for a unique response modality in the data matrix: in this case, IRT weights are not computable
    flag_alleq <- apply(
      X = data[, -1],
      MARGIN = 2,
      FUN = function(x) 1 * (all(x == 0, na.rm = TRUE) | all(x == 1, na.rm = TRUE))
    )
    if (all(flag_alleq == 0)) {
      # unidimensional 2PL
      mod2pl <- mirt::mirt(
        data = data[, -1],
        model = 1,
        itemtype = "2PL",
        ...
      )
      w <- mirt::MDISC(mod2pl) / sum(mirt::MDISC(mod2pl))
      cat("\n Resulting IRT weights: \n")
      print(round(w, 3))
    } else {
      cat("\n Resulting IRT weights: not feasible! Indicator values are all '1' or '0' \n")
      w <- NA
    }
  }
  if (method == "equal") {
    w <- rep(1 / Q, Q)
  }
  if (method == "experts") {
    if (Q == 7) { # companies
      w <- c(0.184, 0.153, 0.146, 0.115, 0.189, 0.089, 0.124)
    } else { # contracting authorities
      w <- c(0.216, 0.130, 0.203, 0.202, 0.094, 0.155)
    }
  }
  return(w)
}


#' Aggregate the elementary indicators and compute the composite
#' @title aggregate
#' @description `aggregate` aggregates the set of elementary indicators through the selected
#' method and computes the composite according to the specified set of weights.
#' @param data data matrix with the set of **normalised** elementary indicators
#' (without missing values).
#' @param method aggregation method. Possible choices: `"linear"` (default) and `"non-linear"`. See Details.
#' @param w vector of weights, as returned by [get_weights()].
#' @return vector of composite indicators for each target unit in `data`.
#' @details The choice of the aggregation method heavily depends on the degree of compensability
#' or substitutability of the elementary indicators. A compensatory approach requires the use of
#' *linear* functions (e.g., a linear combination of the elementary indicators), while
#' a partially compensatory (or non-compensatory) approach involves *non-linear* functions
#' (e.g., a multiplicative approach).
#'
#' In the first case, which corresponds to set `method = "linear"`, the composite indicator
#' for target unit \eqn{c} is obtained as weighted (according to `w`) arithmetic mean of the \eqn{Q}
#' elementary (and normalised) indicators \eqn{I_{qc}}:
#'
#' \deqn{CI_c = \sum_{q=1}^Q w_q I_{qc}}
#'
#' In the second case, using `method = "non-linear"`, the resulting composite indicator is obtained
#' as weighted geometric mean of the elementary indicators:
#'
#' \deqn{CI_c = \prod_{q=1}^Q I_{qc}^{w_q}}
#'
#' @rdname aggregate
#' @export
aggregate <- function(data,
                      method = "linear",
                      w) {
  match.arg(method, c("linear", "non-linear"))

  if (any(is.na(w))) {
    cat("Composite indicator not computable due to unavailable weights \n")
    composite <- rep(NA, nrow(data))
  } else {
    if (!all.equal(sum(w), 1)) stop("Weight vector must sum up to 1")
    if (length(w) != (ncol(data) - 1)) stop("Number of weights and indicators differ")

    # arithmetic mean
    if (method == "linear") {
      composite <- as.matrix(data[, -1]) %*% w %>%
        as.numeric()
    }
    # geometric mean
    if (method == "non-linear") {
      composite <- apply(
        X = data[, -1],
        MARGIN = 1,
        FUN = function(x) prod(x^w)
      )
    }
  }

  return(composite)
}


#' Perform sensitivity analysis (only about methods) of a composite indicator
#' @title composite_sensitivity_methods
#' @description `composite_sensitivity_methods` performs the sensitivity analysis about
#' **only the methodological choices** for computing the composite: *normalisation* (cut-off values),
#' method for *managing missing values* and *weighting* scheme.
#' @keywords internal
#' @export
composite_sensitivity_methods <- function(indicator_list,
                                          cutoff = c(0.9, 0.95, 0.99, 0.995),
                                          ...) {
  # step 0: get the data matrix of elementary indicators
  data_matrix <- create_indicator_matrix(indicator_list)
  n <- length(indicator_list)

  # step 1: cut-off (let's suppose k values)
  cat("\n\n")
  k <- length(cutoff)
  D1 <- list()
  for (i in 1:k) {
    D1[[i]] <- normalise(data_matrix, cutoff[i]) # binary normalisation
  }
  names(D1) <- paste0("c", as.character(cutoff))


  # step 2: imputation (2 methods) --> 2*k combinations
  cat("\n")
  D2_m0 <- D2_m1 <- list()
  for (i in 1:k) {
    cat(paste0("\n*** Managing missing for cut-off ", cutoff[i], " ***\n"))
    D2_m0[[i]] <- manage_missing(data = D1[[i]], missing = 0, verbose = TRUE)
    D2_m1[[i]] <- manage_missing(data = D1[[i]], missing = 1, verbose = TRUE)
  }
  names(D2_m0) <- names(D2_m1) <- names(D1)


  # step 3: weights (3 sets) --> 3*2*k combinations
  cat("\n")
  # set 1: equal weights
  w_equal <- get_weights(data = D1[[1]], method = "equal")

  # set 2: expert weights
  w_experts <- get_weights(data = D1[[1]], method = "experts")

  # set 3: IRT weights
  w_irt_m0 <- w_irt_m1 <- list()
  for (i in 1:k) {
    cat(paste0("\n\n*** Cut-off ", cutoff[i], ", missing = 0 ***\n"))
    # w_irt_m0[[i]] <- get_weights_irt(data = D2_m0[[i]], ...)
    w_irt_m0[[i]] <- get_weights(data = D2_m0[[i]], method = "irt", ...)

    cat(paste0("\n*** Cut-off ", cutoff[i], ", missing = 1 ***\n"))
    # w_irt_m1[[i]] <- get_weights_irt(data = D2_m1[[i]], ...)
    w_irt_m1[[i]] <- get_weights(data = D2_m1[[i]], method = "irt", ...)
  }


  # step 4: computing composites
  cat("\n")

  # equal weights
  cat("\n*** Computing composite indicator - equal weights (for each combination of missing methods and cut-off values) ***\n")
  CI_m0_equal <- lapply(X = D2_m0, FUN = aggregate, w = w_equal)
  names(CI_m0_equal) <- paste0(names(CI_m0_equal), ".m0.w_eq")
  CI_m1_equal <- lapply(X = D2_m1, FUN = aggregate, w = w_equal)
  names(CI_m1_equal) <- paste0(names(CI_m1_equal), ".m1.w_eq")

  # expert weights
  cat("\n*** Computing composite indicator - expert weights (for each combination of missing methods and cut-off values) ***\n")
  CI_m0_experts <- lapply(X = D2_m0, FUN = aggregate, w = w_experts)
  names(CI_m0_experts) <- paste0(names(CI_m0_experts), ".m0.w_exp")
  CI_m1_experts <- lapply(X = D2_m1, FUN = aggregate, w = w_experts)
  names(CI_m1_experts) <- paste0(names(CI_m1_experts), ".m1.w_exp")

  # IRT weights
  cat("\n*** Computing composite indicator - IRT weights (for each combination of missing methods and cut-off values) ***\n")
  CI_m0_irt <- CI_m1_irt <- list()
  for (i in 1:k) {
    CI_m0_irt[[i]] <- aggregate(D2_m0[[i]], w = w_irt_m0[[i]])
    CI_m1_irt[[i]] <- aggregate(D2_m1[[i]], w = w_irt_m1[[i]])
  }
  names(CI_m0_irt) <- paste0(names(D1), ".m0.w_irt")
  names(CI_m1_irt) <- paste0(names(D1), ".m1.w_irt")

  out_wide <- cbind(
    do.call(what = "cbind", CI_m0_equal),
    do.call(what = "cbind", CI_m1_equal),
    do.call(what = "cbind", CI_m0_experts),
    do.call(what = "cbind", CI_m1_experts),
    do.call(what = "cbind", CI_m0_irt),
    do.call(what = "cbind", CI_m1_irt)
  )

  out_wide <- data.frame(aggregation_name = data_matrix$aggregation_name, out_wide)

  weights <- rep(c("w_eq", "w_exp", "w_irt"), each = 2 * k) # for each missing method
  miss <- rep(c("m0", "m1"), times = 3, each = k) # for each set of weights
  co <- rep(cutoff, times = 6) # for each combination of missing method (2) and weight set (3)
  aggr_name <- rep(out_wide$aggregation_name, each = ncol(out_wide[, -1]))
  out_long <- data.frame(
    aggregation_name = aggr_name,
    ci = c(t(out_wide[, -1])),
    cutoff = co,
    miss,
    weights
  )

  return(list(out_wide = out_wide, out_long = out_long))
}


#' Perform a complete sensitivity analysis of the composite indicator
#' @title composite_sensitivity
#' @description `composite_sensitivity` performs a complete sensitivity analysis of the composite
#' indicator, by computing it using all the possible combinations of methodological choices --
#' about *normalisation*, *management of missing values* and *weighting* -- as well as evaluating
#' the contribution of each elementary indicator to the final composite, by removing each indicator
#' at a time from the computation.
#' *Note: the unique normalisation method considered within the CO.R.E. project is the 'dichotomisation'.*
#' *Then, the sensitivity is based on the choice of the threshold. See Details.*
#' @param indicator_list list of outputs about each indicator computable for the target unit
#' (e.g., company or contracting authority), as returned by, for example, [ind_1()], [ind_2()], etc.
#' @param cutoff vector of thresholds for normalising the indicators (i.e., for their dichotomisation)
#' @param ... optional arguments of [mirt::mirt()] function (for getting IRT weights).
#' @return a list with two versions (wide and long) of a dataframe (`sens_wide` and `sens_long`),
#' which includes, for each target unit, all the possible values of the composite indicator obtained
#' by combining methodological choices and indicator removals. See Details.
#' @details This is the main function for carrying out the sensitivity analysis of the composite
#' indicator. It requires a list of indicator outputs, as returned by the single functions for their
#' computation, such as [ind_1()], [ind_2()], etc. This list is given to the internal function
#' [create_indicator_matrix()] for obtaining the data matrix of elementary indicators.
#'
#' Thereafter, several steps for the sensitivity analysis are performed, as follows.
#'
#' 1. Elementary indicators are *normalised* through the 'dichotomisation' method, using several
#' thresholds provided through argument `cutoff`.
#'
#' 2. *Missing values* in the elementary indicators (if any) are managed with both the proposed methods,
#' that is, by replacing missing values with '0' ('not at risk'), or by means of logistic regression
#' models (see internal function [manage_missing()]).
#'
#' 3. Vectors of *weights* are obtained according to all the three proposed ways, that is, equal weights,
#' expert weights and IRT weights (see [get_weights()]). The last method can require much time, as it
#' depends on the data at hand, which are different every time according to step 1 and 2.
#'
#' 4. *Composite indicator computation*. For each target unit, the composite indicator is computed
#' on the basis of each combination of the above methodological choices (steps 1-3),
#' hence \eqn{k \times 2 \times 3} combinations, where \eqn{k} is the number of normalisation thresholds
#' (`cutoff`). In addition, the composite indicator is computed by removing each elementary indicator
#' at a time from the computation. Finally, given \eqn{Q} elementary indicators, the composite indicator is
#' computed, for each target unit, \eqn{k \times 2 \times 3 \times Q+1} times.
#'
#' Results are returned in two dataframes. In the **wide** version (`sens_wide`), we have the target unit
#' in the rows and as many columns as the number of the above combinations, which report
#' the computed composite. Specifically, the first column is the target unit ID, whereas the subsequent
#' columns contain the composite computed according to the different combinations (steps 1-4 above).
#' The names of these columns have the following structure: c**x**.m**y**.w_**abc**.**rrr**, where
#'
#'  - **x** is the cut-off value for normalising the elementary indicators (e.g., 0.95);
#'
#'  - **y** is the label for missing management method (0 or 1, see [[manage_missing()]);
#'
#'  - **abc** is the weighting scheme ('eq' for equal weights; 'exp' for expert weights;
#'  'irt' for IRT weights);
#'
#'  - **rrr** is the indication of the removed indicator ('all' means that no indicator is removed).
#'
#'  For example, column labelled as `c0.95.m0.w_eq.all` contains the composite indicators computed using:
#'  **0.95** as cut-off value for the normalisation; method *'0'* for missing management;
#'  equal weights (**w_eq**); without removal of elementary indicators (**all**).
#'
#' The function directly returns also the **long** version of the above dataframe (`sens_long`), where
#' the target unit is repeated for each 'sensitivity combination'. Here, the columns refer to the variables
#' that enter in the sensitivity analysis. In particular, we have:
#'
#' - `aggregation_name`: target unit ID
#'
#' - `ci`: value of the composite
#'
#' - `cutoff`: possible cut-off values for the normalisation
#'
#' - `miss`: '0' or '1'
#'
#' - `weights`: 'eq', 'exp' or 'irt'
#'
#' - `ind_removed`: 'none', '-ind1', '-ind2', ...,
#'
#' The long version can be useful for further specific analysis.
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname composite_sensitivity
#' @export
composite_sensitivity <- function(indicator_list,
                                  cutoff = c(0.9, 0.95, 0.99, 0.995),
                                  ...) {
  n <- length(indicator_list)

  # output of sensitivity with all the elementary indicators together
  out_sens_all <- composite_sensitivity_methods(indicator_list, cutoff, ...)
  out_sens_all_wide <- out_sens_all$out_wide
  names(out_sens_all_wide)[-1] <- paste0(names(out_sens_all_wide)[-1], ".all")
  out_sens_all_long <- out_sens_all$out_long
  out_sens_all_long$ind_removed <- "none"

  sens_wide <- out_sens_all_wide # only aggregation_name
  sens_long <- out_sens_all_long
  # output of sensitivity with the removal of one indicator at a time
  for (i in 1:n) {
    ind_toremove <- indicator_list[[i]]$indicator_id[1]
    cat("\n\n\n")
    cat("########################################################################\n")
    cat(paste0("####################### REMOVING INDICATOR 'ind", ind_toremove, "'#######################\n"))
    cat("########################################################################\n")
    out_sens1 <- composite_sensitivity_methods(indicator_list[-i], cutoff, ...)

    # wide
    out_sens1_wide <- out_sens1$out_wide
    names(out_sens1_wide) <- paste0(names(out_sens1_wide), ".ind", ind_toremove)
    # adding the current output to the object to be returned
    sens_wide <- data.frame(sens_wide, out_sens1_wide[, -1])

    # long
    out_sens1_long <- out_sens1$out_long
    out_sens1_long$ind_removed <- paste0("-ind", ind_toremove)
    # adding the current output to the object to be returned
    sens_long <- rbind(sens_long, out_sens1_long)
  }
  return(list(sens_wide = sens_wide, sens_long = sens_long))
}
