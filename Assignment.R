makeCacheMatrix <- function(a = matrix()){
  inv <- NULL
  set1 <- function(b){
    a <<- b
    inv <<- NULL
  }
  get1 <- function(){a}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function(){inv}
  list(set1 = set1, get1 = get1, setinverse = setinverse, getinverse = getinverse)
} 

cacheSolve <- function(a, ...) {
  inv <- a$getinverse()
  if(!is.null(inv)){
    message("get the cache data!")
    return(inv)
  }
  mtrx <- a$get1()
  inv <- solve(mtrx, ...)
  a$setinverse(inv)
  inv
}


source("Assignment.R")
pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
pmatrix$get1()
pmatrix$getinverse()
cacheSolve(pmatrix)
pmatrix$getinverse()
