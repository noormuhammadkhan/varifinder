usethis::use_git() # activate git for the project/package
usethis::git_vaccinate() # add stuff to make you git experience safer
usethis::use_readme_rmd() # setup a readme with RMD superpower!
devtools::build_readme() # update readme even if your package does not exist yet
usethis::use_package_doc() # create file (do not touch it) for pkg documentation
usethis::use_roxygen_md() # extremaly easily way to write func docs
usethis::use_testthat() # the best (unbiased opinion!!) test environment for R!
usethis::use_spell_check()

install.packages("lintr")
usethis::use_github_action("lint")
