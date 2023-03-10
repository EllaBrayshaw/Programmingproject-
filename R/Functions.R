# Functions 

####
# two stagbe probability of observing Y_1 and Y_2 responses 
prob_y1_y2 <- function(y_1, y_2, n1, n2, theta) {
  
  dbinom(y_1, n1, theta)*dbinom(y_2, n2 - n1, theta)
}

####
#one stage probability of observing Y_1 responses in stage 1
prob_y1 <- function(y1, n1) {
  
  choose(n1, y1) * beta(y1 + 0.5, n1 - y1 + 0.5) / beta(0.5, 0.5)
}

###
# Function that evaluates Type 1 error under the Null hypothesis: theta = 0.5
Type1_exact <- function(lambda, gamma, n1, n2){
  
  # Thresholds to determine progression, based on the decision rule.
  C1 <- 1 - lambda * (n1 / n2)^gamma
  C2 <- 1 - lambda * (n2 / n2)^gamma
  
  # matrix of possible successes in stage 1 and 2.
  ys <- expand.grid(y_1 = 0:n1,
                    y_2 = 0:(n2 - n1))
  # Vector of corresponding progression decisions at stage 1.
  go_1 <- pbeta(0.5, ys$y_1 + 0.5, n1 - ys$y_1 + 0.5) < C1
  
  # Vector of corresponding progression decisions at stage 2.
  go_2 <- pbeta(0.5, ys$y_1 + ys$y_2 + 0.5, n2 - ys$y_1 - ys$y_2 + 0.5) < C2
  
  # for type 1 error we would go at both stages = results in success at end of stage 2 
  go_T1 <- go_1 & go_2
  
  #probability of outcomes y1 and y2
  probs_null <- prob_y1_y2(ys$y_1, ys$y_2, n1, n2, 0.5)
  
  # evaluate the type 1 error 
  t1prob <- sum(go_T1*probs_null)
}

###
# Function to evaluate type 2 error exactly under the alternative hypothesis : theta = 0.7
Type2_exact <- function(lambda, gamma, n1, n2){
  
  # Thresholds to determine progression, based on the decision rule.
  C1 <- 1 - lambda * (n1 / n2)^gamma
  C2 <- 1 - lambda * (n2 / n2)^gamma
  
  # matrix of possible successes in stage 1 and 2.
  ys <- expand.grid(y_1 = 0:n1,
                    y_2 = 0:(n2 - n1))
  
  # Vector of corresponding progression decisions at stage 1.
  go_1 <- pbeta(0.5, ys$y_1 + 0.7, n1 - ys$y_1 + 0.7) < C1
  
  # Vector of corresponding progression decisions at stage 2.
  go_2 <- pbeta(0.5, ys$y_1 + ys$y_2 + 0.7, n2 - ys$y_1 - ys$y_2 + 0.7) < C2
  
  #For type 2 error we would either not go at stage 1 or not go at stage 2
  go_T2 <- (!go_1) | (!go_2)
  
  #probability of outcomes y1 and y2
  probs_alt <- prob_y1_y2(ys$y_1, ys$y_2, n1, n2, 0.7)
  
  # evaluate the type 1 error 
  t2prob <- sum(go_T2*probs_alt)
}

### Evaluate T1 over a grid for lambda and gamma
evaluate_seq_T1 <- function(lambda_gamma_seq, n1, n2){
  lambda <- lambda_gamma_seq[1]
  gamma <- lambda_gamma_seq[2]
  
  Type1_exact(lambda, gamma, n1, n2)
}

### Evaluate T2 over a grid for lambda and gamma
evaluate_seq_T2 <- function(lambda_gamma_seq, n1, n2){
  lambda <- lambda_gamma_seq[1]
  gamma <- lambda_gamma_seq[2]
  
  Type2_exact(lambda, gamma, n1, n2)
}

###
# Function to evaluate sample size exactly 
Sample_size_exact <- function(lambda, gamma, n1, n2, theta) {
  
  # Calculate the expected sample size of a design defined by its 
  # decision rule parameters (lambda, gamma) and sample size 
  # parameters (n1, n2), along with its standard error.
  
  # Threshold to determine progression, based on the decision rule.
  C1 <- 1 - lambda * (n1 / n2)^gamma
  
  # Vector of possible stage 1 outcomes.
  y_1s <- 0:n1
  
  # Vector of corresponding progression decisions.
  stops <- pbeta(0.5, y_1s + theta, n1 - y_1s + theta) > C1
  
  # For each outcome, calculate its probability.
  y_1_probs <- prob_y1(y_1s, n1)
  
  sum(n1 * stops * y_1_probs + n2 * (!stops) * y_1_probs)
}








#Function to test P(y1) sums to one
test_prob_y1 <- function() {
  # Check that the probabilities sum to 1.
  n1 <- 30
  s <- sum(prob_y1(0:n1, n1))
  return(all.equal(s, 1))
}