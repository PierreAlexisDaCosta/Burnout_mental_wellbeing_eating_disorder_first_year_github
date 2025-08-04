#github configuration ####
library(usethis)
library(gitcreds)
usethis::edit_git_config() #Veryfy username and mail
usethis::use_git()
usethis::create_github_token()
# This will take you to the appropriate page on the GitHub website,
# where you’ll give your token a name and copy it (don’t lose it because
# it will never appear again!).

gitcreds::gitcreds_set()

usethis::use_git()
usethis::use_github()
usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)

