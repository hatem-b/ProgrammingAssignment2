## this function return a special matrix that allows to cache its inverse
## with a list of getters and setters.
makeCacheMatrix <- function(mat = matrix()) {
  
  invMat <- NULL
  
  set <- function(value) {
    mat <<- value
    invMat <<- NULL
  }
  get <- function() mat
  
  setInv <- function(inv) invMat <<- inv
  
  getInv <- function() invMat
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## this function return the inverse of the special matrix
## from the cached value if it was calculated, or calculate and save it.
cacheSolve <- function(mat, ...) {
  
  inv <- mat$getInv()
  
  ## return the inverse if available
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## else return the inverse after calculation
  message("getting calculated data")
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setInv(inv)
  inv
}
