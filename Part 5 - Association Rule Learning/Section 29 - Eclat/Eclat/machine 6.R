#REINFORCEMENT LEARNING

# Upper Cnfidence Bound (UCB)

#Importing the dataset
dataset = read.csv("Ads_CTR_Optimisation.csv")

#Implementing UCB
N = 10000
d = 10
ads_selected = integer()
number_of_selections = integer(d)
sums_of_rewards = integer(d)
total_reward = 0
for (n in 1:N) {
  ad = 0
  max_upper_bound = 0
  for (i in 1:d) {
    if (number_of_selections[i] > 0) {
      average_reward = sums_of_rewards[i] / number_of_selections[i]
      delta_i = sqrt(3/2 + log(n) / number_of_selections[i])
      upper_bound = average_reward + delta_i
    } else {
      upper_bound = 1e400
    }
    if(upper_bound > max_upper_bound) {
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  number_of_selections[ad] = number_of_selections[ad] + 1
  reward = dataset[n, ad]
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward
  total_reward = total_reward + reward
}

#Visualising the results - Histogram
hist(ads_selected,
     col = "blue",
     main = "Histogram of ads selection",
     xlab = "Ads",
     ylab = "Number of times each ad was selected")

#---------------------- Thompson Sampling

#Importing the dataset
dataset = read.csv("Ads_CTR_Optimisation.csv")

#Implementing the Tompson Sampling
N = 10000
d = 10
ads_selected = integer()
number_of_rewards_1 = integer(d)
number_of_rewards_0 = integer(d)
total_reward = 0
for (n in 1:N) {
  ad = 0
  max_random = 0
  for (i in 1:d) {
    random_beta = rbeta(n = 1,
                        shape1 = number_of_rewards_1[i] + 1,
                        shape2 = number_of_rewards_0[i] + 1)
    if(random_beta > max_random) {
      max_random = random_beta
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  reward = dataset[n, ad]
  if (reward == 1) {
    number_of_rewards_1[ad] = number_of_rewards_1[ad] + 1
  } else {
    number_of_rewards_0[ad] = number_of_rewards_0[ad] + 1
  }
  total_reward = total_reward + reward
}

#Visualising the results - Histogram
hist(ads_selected,
     col = "blue",
     main = "Histogram of ads selection",
     xlab = "Ads",
     ylab = "Number of times each ad was selected")

#----------------------------------------