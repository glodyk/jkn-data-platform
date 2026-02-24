set.seed(5263)
smkpbk <- c("1","2","3","4")
flips <- sample(c("1","2","3","4"), 95, replace=TRUE, prob = c(0.025,0.30,0.425,0.25))
flips


set.seed(7638)
smkpbk <- c("4","5","6")
flips <- sample(c("4","5","6"), 22, replace=TRUE, prob = c(0.20,0.50,0.30))
flips


set.seed(6427)
smkpbk <- c("1","2","3","4","5","6")
flips <- sample(c("1","2","3","4","5","6"), 32, replace=TRUE, prob = c(0.002,0.003,0.025,0.15,
                                                                       0.30,0.52))
flips




sample(smkpbk, size = 10)
runif(10,3,5)
rnorm(smkpbk)
sample <- (smkpbk, 3)
runif(3, min = 3, max = 5)

set.seed(101) # set 101 as seed
rnorm(5) # generate 5 std normal random numbers N(0,1)
rnorm(5) # another 5, but without set.seed()
set.seed(101)
rnorm(5) # with set.seed(); result same as the first
set.seed(101)
sample(x = 1:100, size = 10) # with set.seed()
sample(x = 1:100, size = 10) # without set.seed()
set.seed(101)
sample(x = 1:100, size = 10) # agian with set.seed()
