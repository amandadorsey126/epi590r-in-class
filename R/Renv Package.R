install.packages("renv")
library(renv)

#Initialize the project. Makes a renv.lock file
renv::init()

#Install a new R package
install.packages("hadley/emo")

#Run a line of code dependent on the package
emo::ji("banana")
emo::ji("apple")
emo::ji("pear")
emo::ji("orange")
emo::ji("pineapple")

#Make sure the project picked up the package as a dependence
renv::status()

#Make sure the package is in your lockfile
renv::snapshot()

