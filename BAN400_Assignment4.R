library(tidyverse)
library(magrittr)
library(ggplot2)
library(tweedie)

# Part_1 ----

# Task 1

# Assign sample size for simulation (N)
sample_size <- 10

# Simulation using rnorm
# Note the assigned default values
# The function returns the mean of a simulation with population N
simulation <- 
  function(N, mu = 0, sigma = 1){
      rnorm(N, mu, sigma) |> 
      mean()
  }

set.seed(123) 
simulation(N = 1000)
simulation(N = 1000, mu = 10)
simulation(N = 1, sigma = 1000)

# Task 2

# Assign the length of an empty vector
# Later used to store results from running "simulation"
M <- 20
sample_size <- 10

# This is the empty vector
results_vec <- c(1:M)

# For loop utilized for filling results_vec with results from the func 
# "simulation". The result from the funct is assigned to the index that
# corresponds to the value of i in the loop's current iteration.
# This happens for all the values in the empty vector.
for(i in results_vec){
  results_vec[i] <- simulation(sample_size)
}

# Task 3

# st_dev is the standard deviation for all the values in results_vec
results_sd <- sd(results_vec)
# Since the default value for sigma is used in the function the true value is:
true_value <- 1/sqrt(sample_size)

# The values of results_sd and true_value become more similar as sample_size
# increases.

# Task 4
# Create tibble called results_tibble according to the requirements of the task
results_tibble <- tibble(
  N = 10:200, # Column N filled with numbers from 10 to 200
  st_dev = 0, # All rows in column st_dev is assigned the value 0
  sigma = 1, # All rows in the column sigma is assigned the value 1
) |> 
  mutate(theoretical = sigma/sqrt(N)) # Adds and fills the column "theoretical"
# with the theoretical values of the standard deviation

M <- 100

# Task 5
# Nested for loop for assigning values to the st_dev column of values
for(i in 1:nrow(results_tibble)){
  sample_means <- rep(NA_real_, M) # Generate vector with M length with only NA
  # as values. This happens again for each iteration (row of tibble).
  for(j in 1:M){
    # Fills the vector with results from the function "simulation" using inputs
    # from the tibble "results_tibble". Happens for the length of the vector/
    # size of M.
    sample_means[j] <-
      simulation(
        N = results_tibble$N[i], 
        sigma = results_tibble$sigma[i])
  }
  # The standard deviation is calculated based on the values in the now filled
  # vector. This is done for each row in the tibble.
  results_tibble$st_dev[i] <- sd(sample_means)
}

# Task 6
# Define the X axis of plot as N in results_tibble
results_tibble |> ggplot( aes(x = N)) + 
  # One line for each of the columns that are plotted
  geom_line(aes(y = st_dev)) +
  # Linetype 2 to get dashed line
  geom_line(aes(y = theoretical), linetype = 2)

# Part_2 ----

# Task 1
# Funct for runing a t test on the tweedie distrubution
simTweedieTest <-
  function(N){
    # Function for t-test
    t.test(
      # Values in rtweedie function as defined in task 1
      rtweedie(N,
               mu = 10000,
               phi = 100,
               power = 1.9),
      mu = 10000)$p.value
  }

# Task 2

# I have chosen to declare alpha here for ease of access and the fact that it
# stays the same across all tasks.
alfa <- 0.05

# Funct like described in task 2
MTweedieTests <-
  function(N, M, alfa){
    sum(
      # This replicate function runs funct simTweedieTest and puts the results
      # in a vector. Then the values are changed to either TRUE or FALSE, based
      # on wether or not the value was higher than alfa.
      # Then all the TRUE values are added up and divided by M to show how many
      # tests ended with a result less than alfa out of the M total.
      replicate(N, simTweedieTest(N)) < alfa) / M
  }
#set.seed(123) # seed set for bugtesting
MTweedieTests(N = 100, M = 100, alfa = alfa)

# expand.grid for df with all combinations of given values

# Task 3
# Tibble created according to task description
tweedie_results <- tibble(N = c(10, 100, 1000, 5000),
                          M = 100,
                          share_reject = NA)
# Column share_reject filled with results from funct MTweedieTests using inputs
# from the other columns in tweedie_results
for(i in 1:nrow(tweedie_results)){
  tweedie_results$share_reject[i] <- MTweedieTests(N = tweedie_results$N[i],
                                                   M = tweedie_results$M[i],
                                                   alfa = alfa)
}

# Plots the values from the column N and share reject from tweedie_results 
# against each other in a line plot
tweedie_results |> ggplot(aes(x = N, y = share_reject)) +
  geom_line()
# The results mean that the sample is "large enough" to get consistent results
# when you cross a value around 1000 for N. This can be seen by the fact that
# the slope of the line is steeper when N < 1000, and consistent when N > 1000.

# Task 4

# Vector for N values in new tibble
vect_N <- c(10, 100, 5000)
# Size of the test, I.E the amount of times the tests are run
M_size <- 100
# Alfa value of the tests
alfa2 <- .05
# The mu value in the t.tests down below
true_value_mean <- 1000

# Tweedie variables (for funct simTweeTests)
# The values are the same as in task 1.
twee_mu <- 10000
twee_phi <- 100
twee_pow <- 1.9

# Creates a tibble where N is based on vect_N, M is based on M_size and the cols
# share_rejected_norm and share_rejected_twee are empty
norm_vs_tweedie <- tibble(N = vect_N,
                          M = M_size,
                          share_reject_norm = NA,
                          share_reject_twee = NA)

# Simulates t.tests upon the normal distrubution using inputs from norm_vs_tweedie
# Returns the p-value of the test
simNormTests <-
  function(N, mu = 0, sd = 1){
    t.test(rnorm(n = N,
          mean = mu,
          sd = sd), mu = true_value_mean)$p.value
  }

# Simulates t.tests upon the tweedie distribution using inputs from norm_vs_tweedie
# Returns the p-value of the test
simTweeTests <-
  function(N, mu, phi, power){
    t.test(rtweedie(N,
                    mu = twee_mu,
                    phi = twee_phi,
                    pow = twee_pow),
                    mu = true_value_mean)$p.value
  }

# Runs funct simNormTests OR simTweeTests based upon the arguments given to the 
# function. Checks if the results of a given iteration of the test has a value 
# less than alfa. Marks as TRUE if the value is less than alfa. Adds up the 
#  number of TRUE. Makes a ratio of how many of M runs were true.
MTests <-
  function(N, M, alfa, type){
    if(type == "norm"){
      sum(replicate(N, simNormTests(N)) < alfa) / M
    }else if(type == "twee"){
      sum(replicate(N, simTweeTests(N)) < alfa) / M
    }
  }

# Calls upon the function MTests to initiate tests based on info in the tibble
# norm_vs_tweedie to fill the columns share_reject_norm and share_reject_twee
for(i in 1:nrow(norm_vs_tweedie)){
  norm_vs_tweedie$share_reject_norm[i] <- 
    MTests(N = norm_vs_tweedie$N[i],
           M = norm_vs_tweedie$M[i],
           alfa = alfa2,
           type = "norm")
  
  norm_vs_tweedie$share_reject_twee[i] <-
    MTests(N = norm_vs_tweedie$N[i],
           M = norm_vs_tweedie$M[i],
           alfa = alfa2,
           type = "twee")
}

# Plots the values from the column N on the x axis and the values for
# share_reject_norm and share_reject_twee
norm_vs_tweedie |> ggplot(aes(x = N)) +
  geom_line(aes(y = share_reject_norm, color = "blue")) +
  geom_line(aes(y = share_reject_twee, color = "red")) +
  labs(y = "Share rejected") +
  theme_dark()
