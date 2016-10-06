
## makeCacheMatrix
## Parameter is a matrix which defaults to empty matrix
## creates a matrix with four functions, namely
##     set - sets its data (and invalidates cache)
##     get - gets its data
##     setinverse - sets its inverse into cache
##     getinverse - gets its inverse from cache 

makeCacheMatrix <- function(matrixData = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    matrixData <<- y
    matrixInverse <<- NULL
  }
  get <- function() matrixData
  setinverse <- function(inverse) matrixInverse <<- inverse
  getinverse <- function() matrixInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## First parameter is a matrix created by makeCacheMatrix
## May accept other ... parameters which will be passed down to solve() function 
## Caches the inverse of 'x' and also returns it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##.........testCase...........
##> A <- matrix(c(2, 4, 3, 1, 5, 7, 2, 9, 1), nrow=3, ncol=3) 
##> makeCacheMatrix(A)
##> A$getinverse()
##NULL
##> cacheSolve(A)
##[,1]        [,2]        [,3]
##[1,]  0.8656716 -0.19402985  0.01492537
##[2,] -0.3432836  0.05970149  0.14925373
##[3,] -0.1940299  0.16417910 -0.08955224
##> A$getinverse()
##[,1]        [,2]        [,3]
##[1,]  0.8656716 -0.19402985  0.01492537
##[2,] -0.3432836  0.05970149  0.14925373
##[3,] -0.1940299  0.16417910 -0.08955224
