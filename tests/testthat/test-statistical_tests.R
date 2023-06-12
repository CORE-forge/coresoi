# library(dplyr)
# library(testthat)
#
# # Test compute_wilcox
# data("iris")
# wilcox_res <- compute_wilcox(iris, "Sepal.Length", "Species", exact = FALSE, alternative = "greater")
# expect_equal(summarise(iris, wilcox.test(Sepal.Length ~ Species, exact = FALSE)$p.value) %>% pull(), wilcox_res$p_value)
#
# # Test compute_kolmogorov_smirnoff
# ks_res <- compute_kolmogorov_smirnoff(iris, "Sepal.Length", "Species", alternative = "less")
# expect_equal(summarise(iris, ks.test(Sepal.Length ~ Species)$p.value) %>% pull(), ks_res$p_value)
#
# # Test compute_fisher
# fisher_res <- compute_fisher(5, 10, 10, 5)
# expect_equal(fisher.test(matrix(c(5, 10, 10, 5), ncol = 2), alternative = "greater")$p.value, fisher_res$p_value)
#
# # Test compute_barnard
# barnard_res <- compute_barnard(5, 10, 10, 5)
# expect_equal(DescTools::BarnardTest(matrix(c(5, 10, 10, 5), ncol = 2), alternative = "greater", method = "boschloo")$p.value, barnard_res$p_value)
#
# # Test compute_prop_test
# prop_test_res <- compute_prop_test(5, 10, 10, 5)
# expect_equal(stats::prop.test(x = c(5, 10), n = c(15, 15), alternative = "greater")$p.value, prop_test_res$p_value)
#
# # Test compute_unpaired_ttest
# ttest_res <- compute_unpaired_ttest(iris, "Sepal.Length", "Species", alternative = "less", paired = FALSE)
# expect_equal(summarise(iris, t.test(Sepal.Length ~ Species, alternative = "less")$p.value) %>% pull(), ttest_res$p_value)
#
# # Test test_set_1
# barnard_res2 <- test_set_1(5, 10, 10, 5, "barnard")
# expect_equal(DescTools::BarnardTest(matrix(c(5, 10, 10, 5), ncol = 2), alternative = "greater", method = "boschloo")$p.value, barnard_res2$p_value)
#
# # Test test_set_2
# wilcox_res2 <- test_set_2(iris, "Sepal.Length", "Species", "wilcoxon")
# expect_equal(summarise(iris, wilcox.test(Sepal.Length ~ Species)$p.value) %>% pull(), wilcox_res2$p_value)
