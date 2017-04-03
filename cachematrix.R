makeCacheMatrix <- function(p = matrix()) {
  inver <- NULL
  set <- function(q) {
    p <<- q
    inver <<- NULL
  }
  get <- function() p
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(p, ...) {
  inver <- p$getInverse()
  if (!is.null(inver)) {
    message("Fetching Cached Data")
    return(inver)
  }
  matri <- p$get() 
  inver <- solve(matri, ...)
  p$setInverse(inver)
  inver
}