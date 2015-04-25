## This functions create a Special Matrix that can cache its inverse.
## If the Inverse has already been calculated, it is accesed from the 
## cached value, otherwise it is calculated and cached for the next 
## time it is needed.


makeCacheMatrix <- function(x = matrix()) {
## This function stores the original matrix and its inverse (if calculated)
## with the functions get and set and allows to access both, original
## and inverse matrix (if already calculated).
    m <- NULL    
    set <- function(y) {                      ## "set" stores the input matrix
      x <<- y
      m <<- NULL
    }
    get <- function() x                       ## "get" returns input matrix
    setinv <- function(inv) m <<- inv         ## "setinv" sets cached inverse matrix if calculated
    getinv <- function() m                    ## "getinv" returns inverse matrix if calculated
    list(set = set, get = get,                ## stores the previous 4 functions in a list
         setinv = setinv,
         getinv = getinv)
  
}


## This function takes the special cached matrix created with makeCacheMatrix
## and returns the cached inverse matrix for it (if it has already been calculated
## and cached). Otherwise it calculates the inverse matrix and caches it with the
## function "setinv"

cacheSolve <- function(x, ...) {
    m <- x$getinv()                   ## Assigns the inverse matrix if it exist to "m".
    if(!is.null(m)) {                 ## if inverse matrix is cached, returns message and inverse matrix
      message("getting cached data")
      return(m)
    }
    data <- x$get()                   ## if inverse matrix hasn't been calculated and cached,
    m <- solve(data, ...)             ## it calculates it and stores it 
    x$setinv(m)
    m
  
## Return a matrix that is the inverse of 'x'
}
