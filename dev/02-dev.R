


# on a rutine
spelling::spell_check_package()
spelling::update_wordlist()

# Before pull requests

styler::style_pkg()
lintr::lint_package()
goodpractice::gp()

devtools::test()
devtools::check()
devtools::build()

pkgdown::build_articles()
pkgdown::build_site_github_pages()

devtools::build_readme()

## when huge release
#' > Update the `NEWS.md` file and..
usethis::use_version()



#' CRAN submission's cycle

# usethis::use_release_issue() # at the first start only
devtools::build_readme()
devtools::check(remote = TRUE, manual = TRUE)
devtools::check_win_devel()
cran_prep <- rhub::check_for_cran()
