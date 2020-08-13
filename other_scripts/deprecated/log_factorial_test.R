
test1 <- function(N, m){
  return(factorial(N)/(factorial(N - m)*factorial(m - 1)))
}

test2 <- function(N, m){
  if(m == 1 | m == N){return(N)}
  tmax <- c(max(c(N-m, m-1)), min(c(N-m, m-1)))
  exp(sum(log((tmax[1]+1):N)) - sum(log(1:tmax[2])))
}

for(i in 1:10){
  x <- sample(1:100, 1)
  m <- sample(1:x, 1)
  
  x1 <- test1(x, m)
  x2 <- test2(x, m)
  
  print(all.equal(x1, x2))
}


