#install the needed apps for the dashboard if they are not already installed
list.of.packages <- c("shiny", "ggplot2", "data.table", "dplyr", "tidyr", "Rfast")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load shiny and run the script from the github repository
library(shiny)
runGitHub('MoritzKaufmann/BIO364_assignment')

