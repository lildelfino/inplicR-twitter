my_packages = c("DT", "shinydashboard","rtweet","httr","purrr","graphics","stringr","tm","syuzhet","dplyr","wordcloud","RColorBrewer","SPARQL","splus2R")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
