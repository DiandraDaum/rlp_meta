# Install the package usethis, for git control
# install.packages("usethis")
library(usethis)

create_github_token()
library(gitcreds)
gitcreds_set("https://github.com/DiandraDaum")
git_sitrep()

use_git_config(user.name="DiandraDaum", user.email="diandra.daumiller@gmail.com")
git_sitrep()

use_git()
use_github()

# I have made a change!
