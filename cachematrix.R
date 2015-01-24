## Both functions work together to first cache the inverse of a matrix 
## that the user supplied and then return this inverse matrix. If this 
## inverse has already been solved it will just retrieve the cache data;
## if not the solve() command works to solve the inverse and print it out.

## makeCacheMatrix simply models from the makeVector function given in the 
## intsruction. It first sets the inverse matrix "i" , gets its value, sets
## value of its inversion and gets it. Values are stored at a cache 
## environment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function again models from the example cachemean function,
## returning the inverse of "x" supplied above. The function itself 
## evaluates the inverse if it has not been evaluated. If it is evaluated
## it simply prints out the evaluated answer from the cache environment
## with a prompt of "getting cached data".

cacheSolve <- function(x, ...) {  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

## This set of function works by first defining a matrix (e.g. "x") and 
## assigning another variable (e.g. "i_of_x"): 
## i_of_x <- makeCacheMatrix(x)
## running cacheSolve(i_of_x) returns the inverse should x itself be
## an invertible matrix.
