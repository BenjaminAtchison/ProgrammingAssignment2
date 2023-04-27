makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL #inverse as NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m #retreive matrix
  setInverse <- function() inv <<- solve(m)
  getInverse <- function() inv 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) { #retreives cache data
  inv <- x$getInverse()
  if (!is.null(inv)) { #discovers whether inverse is null
    message("getting cached data")
    return(inv) #return inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...) #Calculate inverse value
  x$setInverse(inv)
  inv #Return inverse(x) matrix
}

#Note to self: cache and test inverse of square matrix. 
#1. assign "makeCacheMatrix" to vector
#2. vector$set(matrix)/vector$get()
#3. set$inverse, get$inverse
#4. Test cache with cacheSolve(x)
