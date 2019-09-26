# Simulate US Presidential Election Based On Electoral Votes

# Set work directory
wd <- "/Users/emma/Desktop/BIS 679/R/R HW/Data"
setwd(wd) 

# Read in the data set
election <- read.csv("Election data.csv", header=T)

# First, create a vote function to simulate the election based on electoral 
# votes for only once
# Documentation for the vote function
################################################################################

# Function: vote

# Author: Xiaojing Zhu

# Creation Date: Dec 3, 2018 

# Purpose: This function simulates one US presidential election based on 
# electoral votes
# tallied from 50 states and DC, assuming either a 3-party or a 2-party system. 
# A party must secure 270 electoral votes to win this mock election.

# Required Parameters: 
#      probdata = a matrix containing probabilities of winning each state by 
#      either 2 or 3 parties

# Output:  Assuming 3-party system, the function outputs a list of tallied 
# electoral votes for democrats, republicans and others, as well the national 
# winner based on one mock election
#          Assuming 2-party system, the function outputs a list of tallied 
# electoral votes for democrats and republicans, as well the national winner 
# based on one mock election

# Example: Simulate a US presidential election assuming 3 parties
# vote(cbind(election$Democrat_Per, election$Republican_Per, election$Other_Per))

################################################################################  
vote <- function(probdata){
  # Initialize entries in the outputting list
  Dvotes <- 0
  Rvotes <- 0
  Ovotes <- 0
  # Assume 3 parties:
  if (ncol(probdata) == 3){
    # Loop through each state to determine the state winner
    # Sample Democrats(D), Republicans (R), Others (O) based on probabilities of 
    # winning that state
    state <- apply(probdata, 1, function(x) sample(c("D","R","O"), 1, prob = x))
    # Tally electoral votes based on the winner of each state
    for (i in 1:length(state)){
      if (state[i] == "D"){Dvotes = Dvotes + election[i,5]}
      else if (state[i] == "R"){Rvotes = Rvotes + election[i,5]}
      else if (state[i] == "O"){Ovotes = Ovotes + election[i,5]}
    }
    # Count the national electoral votes to determine the national winner
    if (Dvotes >= 270 & Dvotes > Rvotes) {Win = "Democrats"}
    else if (Rvotes >= 270 & Rvotes > Dvotes) {Win = "Republicans"}
    else if (Ovotes >= 270 & Ovotes > Dvotes & Ovotes > Rvotes) {Win = "Other"}
    else {Win = "Undecided"}
    # Return a list of total votes and the national winner in this mock election
    once <- list(Democrats_Votes = Dvotes, Republicans_Votes = Rvotes, 
                 Others_Votes = Ovotes, Winner = Win)
    return(once) 
  }
  # Assume 2 parties:
  if (ncol(probdata) == 2){
    # Loop through each state to determine the state winner
    # Sample Democrats(D) and Republicans (R) based on probabilities of winning 
    # that state
    state <- apply(probdata, 1, function(x) sample(c("D","R"), 1, prob = x))
    for (i in 1:length(state)){
      # Tally electoral votes based on the winner of each state
      if (state[i] == "D"){Dvotes = Dvotes + election[i,5]}
      else if (state[i] == "R"){Rvotes = Rvotes + election[i,5]}
    }
    # Count the national electoral votes to determine the national winner
    if (Dvotes >= 270 & Dvotes > Rvotes) {Win = "Democrats"}
    else if (Rvotes >= 270 & Rvotes > Dvotes) {Win = "Republicans"}
    else {Win = "Undecided"}
    # Return a list of total votes and the national winner in this mock election
    once <- list(Democrats_Votes = Dvotes, 
                 Republicans_Votes = Rvotes, Winner = Win)
    return(once) 
  }
}

# Then, create a simulation function to replicate the vote functions multiple 
# times and output reults
# Documentation for the simfunc function
################################################################################

# Function: simfunc

# Author: Xiaojing Zhu

# Creation Date: Dec 3, 2018 

# Purpose: This function conducts multiple times of mock elections by 
# replicating the vote function.

# Required Parameters: 
#      data = a matrix containing probabilities of winning each state by either 
#      2 or 3 parties

# Optional Parameters: 
#      seed = the seed to ensure reproducible simulations - default is 2018
#      nsims = number of simulations, which equals to the number of mock 
#      elections and is the same as the number of times the vote function will 
# be replicated - default is 10000

# Other functions required:
#      The simfunc function requires the vote function to be already called

# Output:  The simfunc function outputs an overlaid histogram of the 
# distribution of electoral votes
# for Democrats and Republicans. In addition, it outputs a table of probability 
# of Democrats winning, Republicans winning, and having an undecided election 
# based on the default 10000 mock elections.

# Example: Simulate 10000 US presidential elections assuming 3 parties
# simfunc(data = cbind(election$Democrat_Per, election$Republican_Per, 
# election$Other_Per))

################################################################################  
simfunc <- function(seed = 2018, nsim = 10000, data){
  set.seed(seed)
  sims <- replicate(nsim, vote(probdata = data))
  # Create a histogram for the distribution of electoral votes for Republicans 
  # and Democrats
  allDvotes <- unlist(sims[1, ])
  allRvotes <- unlist(sims[2, ])
  #blue for Democrats and red for Republicans
  hist(allDvotes, col = rgb(0, 0, 1, 0.25), xlab = "Electoral Votes",
       main = paste("Distribution of Electoral Votes for Republicans and Democrats",
                    "\nAfter",nsim,"Simulations"))
  hist(allRvotes, col = rgb(1, 0, 0, 0.25), add = TRUE)
  abline(v = 270, col = "red", lwd = 2)
  text(270 + 60, 1500, "Cut-off = 270", col = "red")
  legend("right", legend = c("Democrats", "Republicans"), 
         col = c(rgb(0, 0, 1, 0.25), rgb(1, 0, 0, 0.25)),
         pch = 15, bty = "n", pt.cex = 1, cex = 0.9, 
         text.col = "black", horiz = F)
  # Create a table to output the probability of Democrats winning, Republicans 
  # winning, or undecided based on the 10000 mock elections 
  Prob_Table <- unlist(sims[nrow(sims), ])
  table(Prob_Table)/length(Prob_Table)
}


# Deliverables:
#1) Simulate 10000 elections using all 3 parties - Democrats, Republicans, and Other
simfunc(data = cbind(election$Democrat_Per, election$Republican_Per, election$Other_Per))

#2) Simulate 10000 elections assuming a 2 party system with probability of a state voting 
# Democratic=Democrats+Other
simfunc(data = cbind(election$Democrat_Per + election$Other_Per, election$Republican_Per))

#3) Simulate 10000 elections assuming a 2 party system with probability of a state voting 
# Republican=Republicans + Other
simfunc(data = cbind(election$Democrat_Per, election$Republican_Per + election$Other_Per))

#4) Simulate 10000 elections assuming a 2 party system with
# Republicans and Democrats evenly splitting the Other probability
simfunc(data = cbind(election$Democrat_Per + 0.5*election$Other_Per,
                     election$Republican_Per + 0.5*election$Other_Per))

