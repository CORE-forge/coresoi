#' compute Wilcoxon-Mann-Whitney test in dplyr https://it.wikipedia.org/wiki/Test_di_Wilcoxon-Mann-Whitney
#' @description  compute Wilcoxon-Mann-Whitney test pvalue
#' @keywords internal
#' @export
compute_wilcox <- function(data, var, group, exact = TRUE, alternative = "greater", paired = FALSE) {
  test_res <- data %>%
    wilcox.test(var ~ group, data = ., exact = exact, alternative = alternative) %>%
    suppressWarnings()
  c(
    p_value = round(test_res$p.value, 3),
    estimate = round(test_res$statistic, 3)
  )
}

#' compute Kolmogorov Smirnov test in dplyr https://it.wikipedia.org/wiki/Test_di_Kolmogorov-Smirnov
#' @description  compute Kolmogorov Smirnov test pvalue
#' @keywords internal
#' @export
compute_kolmogorov_smirnoff <- function(data, var, group, alternative = "less") {
  test_res <- suppressWarnings({
    data %>%
      ks.test(var ~ group, data = ., alternative = alternative)
  })

  c(
    p_value = round(test_res$p.value, 3),
    estimate = round(test_res$statistic, 3)
  )
}



#' compute Fisher-exact test https://en.wikipedia.org/wiki/Fisher%27s_exact_test
#' @description  compute fisher test pvalue and estimate in piped expression
#' @keywords internal
#' @export
compute_fisher <- function(a, b, c, d) {
  if (any(is.na(list(a, b, c, d)))) {
    stop("All inputs must be non-missing")
  }

  data <- matrix(c(a, b, c, d), ncol = 2)
  c(
    p_value = round(fisher.test(data, alternative = "greater")$p.value, 3),
    estimate = round(fisher.test(data, alternative = "greater")$estimate, 3)
  )
}

#' compute Barnard test https://en.wikipedia.org/wiki/Barnard%27s_test
#' @description  compute Barnard test pvalue and estimate in piped expression
#' @keywords internal
#' @export
compute_barnard <- function(a, b, c, d, method = "boschloo") {
  if (any(is.na(list(a, b, c, d)))) {
    stop("All inputs must be non-missing")
  }
  # only pre
  if ((a + b) > 0 & (c + d) == 0) {
    1
  }
  # only post
  else if ((a + b) == 0 & (c + d) > 0) {
    0
  } else {
    data <- matrix(c(d, b, c, a), ncol = 2)
    out_barn <- DescTools::BarnardTest(data, alternative = "greater", method = "boschloo") %>%
      suppressWarnings()
    c(
      p_value = round(out_barn$p.value, 5),
      estimate = round(out_barn$estimate, 3)
    )
  }
}

#' compute Z-test proportional
#' @description  compute Z-test pvalue and estimate in piped expression
#' @keywords internal
#' @export
compute_prop_test <- function(a, b, c, d, correct = FALSE) {
  if (any(is.na(list(a, b, c, d)))) {
    stop("All inputs must be non-missing")
  }

  m_1 <- a + b
  m_2 <- c + d
  p_1 <- b / m_1
  p_2 <- d / m_2
  diff_p2_p1 <- p_2 - p_1

  c(
    p_value = stats::prop.test(
      x = c(d, b),
      n = c(m_2, m_1),
      correct = correct,
      alternative = "greater"
    )$p.value %>% suppressWarnings(),
    estimate = stats::prop.test(
      x = c(d, b),
      n = c(m_2, m_1),
      correct = correct,
      alternative = "greater"
    )$estimate %>% suppressWarnings()
  )
}


#' compute unpaired t-test test
#' @description  compute unpaired t test
#' @keywords internal
#' @export
compute_unpaired_ttest <- function(.data, var, group, alternative = "less", paired = FALSE) {
  test_res <- suppressWarnings({
    .data %>%
      t.test(formula = .[var] ~ .[group], data = ., alternative = alternative, paired = paired)
  })

  c(
    p_value = round(test_res$p.value, 3),
    estimate = round(test_res$statistic, 3)
  )
}

#' switch test wrt statistical circumstances set 1 (indicators 1, ...)
#' @description switch test wrt statistical circumstances
#' @keywords internal
#' @export
test_set_1 <- function(a, b, c, d, test_type) {
  switch(test_type,
    "barnard" = {
      compute_barnard(a, b, c, d)
    },
    "fisher" = {
      compute_fisher(a, b, c, d)
    },
    "z-test" = {
      compute_prop_test(a, b, c, d)
    },
    stop(paste0("No handler for ", test_type))
  )
}

#' switch test wrt statistical circumstances set 2 (indicators 2,3,4 ...)
#' @description switch test wrt statistical circumstances
#' @keywords internal
#' @export
test_set_2 <- function(data, var, group, test_type) {
  # temporary: if two levels in group are not found:
  if (length(unique(group)) != 2) {
    # print("999")
    999
  } else {
    # print("test")
    switch(test_type,
      "ks" = {
        compute_kolmogorov_smirnoff(data, var, group)
      },
      "wilcoxon" = {
        compute_wilcox(data, var, group)
      },
      stop(paste0("No handler for ", test_type))
    )
  }
}
