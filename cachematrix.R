## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly  

## This assignment is to write a pair of functions that cache the inverse of a matrix.

## Part 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}

## Part 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

## Testing:
# > x = rbind(c(3, 4), c(5, 6))
# > x
# [,1] [,2]
# [1,]    3    4
# [2,]    5    6
# > mymatrix = makeCacheMatrix(x)
# > mymatrix$get()
# [,1] [,2]
# [1,]    3    4
# [2,]    5    6
# > cacheSolve(mymatrix)
# [,1] [,2]
# [1,] -3.0  2.0
# [2,]  2.5 -1.5
# > cacheSolve(mymatrix)
# getting cached data
# [,1] [,2]
# [1,] -3.0  2.0
# [2,]  2.5 -1.5

