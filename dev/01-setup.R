usethis::use_roxygen_md()
usethis::use_news_md()

person(
  given = "Niccolò",
  family = "Salvini",
  role = c("aut", "cre", "cph"),
  email = "niccolo.salvini27@gmail.com",
  comment = c(ORCID = "0000-0002-3444-4094")
)



usethis::use_readme_rmd()
usethis::use_logo("~/Pictures/hex-corel190.png")
usethis::use_cran_badge()
usethis::use_lifecycle_badge("experimental")

usethis::use_code_of_conduct(contact = "Niccolò Salvini")

usethis::use_spell_check()
spelling::spell_check_package()
spelling::update_wordlist()

usethis::use_data_raw()

usethis::use_testthat()

usethis::use_test("foo") # `test_that("foo works", expect_null(foo()))`
devtools::test()         # see it fails!!
usethis::use_r("foo")    # define `foo <- function() NULL`
devtools::test()         # see it passes!!

usethis::use_tidy_description()


devtools::check()
devtools::build()
devtools::build_readme()
styler::style_pkg()

#' R-CDM-check and coverage
#' --------------------------------------------------------------------

usethis::use_github_actions_badge("R-CMD-check")
usethis::use_github_actions_badge("covr")



## fist commit something
usethis::use_pkgdown_github_pages()
pkgdown::build_site_github_pages()
usethis::use_github_action("pkgdown")
usethis::use_github_actions_badge("pkgdown")



#'
#' > Update and knit the `README.Rmd`
#'
usethis::use_version("dev")


#'
#' Start to develop
#' ====================================================================
#'
fs::file_create("dev/02-development.R")
rstudioapi::navigateToFile("dev/02-development.R")
#'
#' commit and push...
#' Happy packaging!
#'








#'
#' Optional
#' ====================================================================
#'
#' Add pipe (`%>%`) support
#' --------------------------------------------------------------------
#'
usethis::use_pipe()




#'
#' Add `tibble` support
#' --------------------------------------------------------------------
#'
usethis::use_tibble()
