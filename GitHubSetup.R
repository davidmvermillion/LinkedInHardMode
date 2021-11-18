# Setting up Github using packages usethis and gitcreds
# Credit to https://rfortherestofus.com/2021/02/how-to-use-git-github-with-r/

# Correction to methodology to load multiple packages at once: https://gist.github.com/RobertMyles/96665be044540e9ac668ee697dac6db7
packages <- c("usethis", "gitcreds")
lapply(packages, library, character.only = TRUE)

create_github_token() # to redirect to correct webpage
gitcreds_set() # to insert token