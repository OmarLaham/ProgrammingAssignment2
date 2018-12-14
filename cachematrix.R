## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    ic <- NULL
    set <- function(y) {
      x <<- y
      ic <<- NULL
    }
    get <- function() x
    setic <- function(inversecache) ic <<- inversecache
    getic <- function() ic
    list(set = set, get = get,
         setic = setic,
         getic = getic)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ic <- x$getic()
    if(!is.null(ic) && x == x$get()) {
      message("already cashed")
      return(ic)
    }
    m <- x$get()
    im <- solve(m, ...)
    x$setic(im)
    m
}
