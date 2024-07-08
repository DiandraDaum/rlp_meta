# Install the package usethis, for git control
# install.packages("usethis")
library(usethis)
use_git()
use_github()

create_github_token()
library(gitcreds)
gitcreds_set("https://github.com/DiandraDaum")
