library(tidyverse)

# functions
soft_max <- function(rewards, beta) {
  values <- exp(rewards * beta)
  return(values / sum(values))
}

prob_generative_model <- function(mental_state_proposal, rationality) {
  probabilities <- soft_max(mental_state_proposal$rewards, beta = rationality)
  sample(mental_state_proposal$objects, 1, prob = probabilities)
}

generate_hypothesis <- function(ObjectList) {
  possibilites <- runif(length(ObjectList))
  
  data.frame(
    objects = ObjectList,
    rewards = possibilites # this would be uniform prior
  )
}

# Monte Carlo parameters
size_parameter_space <- 10000
set.seed(666)

# decision tasks parameters
nTrials <- 1000
objects <- c("dessert", "fruit", "garbage")

# real target generative model
true_rewards <- c(0.9, 0.8, 0.1) 
true_rationality <- 1
true_generative_model <- data.frame(objects = objects,
                                    rewards = true_rewards)

# simulate real agent choices
obsereved_choices <- rep(NA, nTrials)
for(i in 1:nTrials) {
  obsereved_choices[i] <- prob_generative_model(true_generative_model, true_rationality)
}

# create containers for data
values_space <- data.frame(matrix(ncol = length(objects) + # internal values
                                    1 + # internal parameters
                                    length(objects) + # probability of certain object choice
                                    1, # negative log likelihood of sequence of choices
                                  nrow = size_parameter_space))

colnames(values_space) <- c(paste0("values_", objects), 
                            "rationality",
                            paste0("probability_", objects),
                            "negative_log_likelihood")

# infer internal values
for(i in 1:size_parameter_space) { # monte carlo sampling of parameter space
  print(i)
  
  hypothesis_generative_model <- generate_hypothesis(objects)
  hypothesis_rationality <- 1
  
  objects_probability <- soft_max(deframe(hypothesis_generative_model), 
                                  hypothesis_rationality)
  
  # calculate the negative log likelihood of sequence of choices
  negative_log_likelihood <- -sum(log(objects_probability[obsereved_choices]))
  
  # save results
  values_space[i, ] <- c(hypothesis_generative_model$rewards,
                         hypothesis_rationality,
                         objects_probability[objects], # the last "[objects]" just makes sure the order of the objects is correct
                         negative_log_likelihood)
}

# retrieve the inferred internal values (MAP)
MAP <- values_space |>
  filter(negative_log_likelihood == min(negative_log_likelihood)) 

# visualize results
ggplot(values_space) +
  geom_point(aes(values_dessert, negative_log_likelihood)) +
  geom_vline(aes(xintercept = true_generative_model[1, 2]), color = "red", linewidth = 1)

ggplot(values_space) +
  geom_point(aes(values_fruit, negative_log_likelihood)) +
  geom_vline(aes(xintercept = true_generative_model[2, 2]), color = "red", linewidth = 1)

ggplot(values_space) +
  geom_point(aes(values_garbage, negative_log_likelihood)) +
  geom_vline(aes(xintercept = true_generative_model[3, 2]), color = "red", linewidth = 1)

ggplot(values_space) +
  geom_point(aes(values_dessert, values_fruit, 
                 color = negative_log_likelihood)) +
  scale_color_viridis_c(option = "viridis",
                        direction = -1) +
  geom_point(aes(true_generative_model[1, 2],
                 true_generative_model[2, 2]), 
             size = 8, shape = 4, stroke = 5,
             color = "red")

ggplot(values_space) +
  geom_point(aes(values_dessert, values_garbage, 
                 color = negative_log_likelihood)) +
  scale_color_viridis_c(option = "viridis",
                        direction = -1) +
  geom_point(aes(true_generative_model[1, 2],
                 true_generative_model[3, 2]), 
             size = 8, shape = 4, stroke = 5,
             color = "red")

ggplot(values_space) +
  geom_point(aes(values_fruit, values_garbage, 
                 color = negative_log_likelihood)) +
  scale_color_viridis_c(option = "viridis",
                        direction = -1) +
  geom_point(aes(true_generative_model[2, 2],
                 true_generative_model[3, 2]), 
             size = 8, shape = 4, stroke = 5,
             color = "red")

