# Utilize Jim Albert functions to pull and roughly format Retrosheet data
# Ensure the correct game files are in the current working directory before calling the functions

source("compute.runs.expectancy.R")
source("prob.win.game3.R")
source("compute.win.probs.R")

start <- 2015
end <- 2018
current <- 2015
data_2015_to_2018 <- data.frame()

# Get 2015 to 2018 data
while (current <= end) {
  step1 <- compute.runs.expectancy(current)
  step2 <- prob.win.game3(current)
  step3 <- compute.win.probs(step1, step2)
  data_2015_to_2018 <- rbind(data_2015_to_2018, step3)
  current <- current + 1
}

# Clean up variables
rm(step1, step2, step3, start, end, current)
