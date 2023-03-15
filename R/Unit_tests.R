### Test for that prob_y1_y2 sums to one 
prob_y1_y2 <- function(y_1, y_2, n1, n2) {
  
  dbinom(y_1, n1, 0.5)*dbinom(y_2, n2 - n1, 0.5)
}

test_prob_y1_y2 <- function() {
  n1 <- 30
  n2 <- 60
  ys <- expand.grid(y_1 = 0:n1,
                    y_2 = 0:(n2 - n1))
  
  s <-sum(prob_y1_y2(ys$y_1, ys$y_2, n1, n2))
  return(all.equal(s, 1))
}
test_prob_y1_y2()



