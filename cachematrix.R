## makeCacheMatrix function gets the matrix object for x and stores it



makeCacheMatrix <- function(x = matrix()) {
  Inversa <- NULL
  set <- function(y) {
    x <<- y
    Inversa <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inversa <<- inverse
  getinverse <- function() Inversa
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function of x, it calls the makeCacheMatrix to get the value of the inverse
## and stores it in the created object (which I called Inversa) if it was not NULL in the cache created 
## on the previous function it will retrieve it from there and return it as the value for the inverse
## otherwise it calculates the inverse of x with solve()

cacheSolve <- function(x, ...) {
  Inversa <- x$getinverse()
  if(!is.null(Inversa)) {
    message("getting cached data")
    return(Inversa)
  }
  data <- x$get()
  Inversa <- solve(data, ...)
  x$setinverse(Inversa)
  Inversa
}
