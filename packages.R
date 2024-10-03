# Installing packages

pacotes <- c( 'tidyverse',  # datawrangling
             'rpart',      # library for the trees
             'rpart.plot', # plot the tree
             'gtools',     # quantcut - create factor using specific quantiles
             'Rmisc',      # descriptive statistics
             'scales',     # colour palette
             'caret'       # machine learning functions
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
