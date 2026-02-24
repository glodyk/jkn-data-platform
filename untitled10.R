system("git config --global user.name \"Dody Goutama\"")
system("git config --global user.email \"glodyk@gmail.com\"")


system("git config --list")

install.packages("usethis")
library(usethis)

usethis::edit_r_environ()
