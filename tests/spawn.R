# Run tests/initialisation-other.R and tests/initialisation-initiatee.R

source("tests/spawn-other.R")

gc()

spawn(initiatee, self, other_replier)
