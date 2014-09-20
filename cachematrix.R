## Added comments below provides an overall description of what these functions do...

## Function accepts a matrix object arguement and creates the inverse matrix object in cache.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL  ## initialize inverse matrix to NULL object, every time create new matrix x
    }
    get <- function() x
    setMatrixInverse <- function(solve) m <<- solve  ## use solve() function to evaluate inverse matrix (& cache it!)
    getMatrixInverse <- function() m
    
    list(set = set, 
         get = get, 
         setMatrixInverse = setMatrixInverse, 
         getMatrixInverse = getMatrixInverse)  ## returns list of 4 function definitions
}


## Function accepts a square matrix (object constructed from makeCacheMatrix()) and returns the inverse matrix.
## First time inverse is calculated and stored in cache. 
## Subsequent calls of this function will get matrix inverse from cache.
## NOTE: if you pass a noninvertible matrix, user will get an error message from solve() function.
cacheSolve <- function(x, ...) {
    m <- x$getMatrixInverse()  ## first time thru is NULL, subsequent times m is cached
    
    if(!is.null(m)) {
        message("getting cache data")
        return(m)
    }
    data <- x$get()  ## get the matrix object
    m <- solve(data)  ## invoke solve() to calculate inverse matrix (first time thru)
    x$setMatrixInverse(m) ## set inverse matrix in cache (for subsequent returns)
    m  ## Return a matrix that is the inverse of 'x'
}
