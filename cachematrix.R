## makeCacheMatrix and cacheSolve work together to calculate
## the inverse of a matrix and save it into the cache.
## If that inverse has already been calculated then it retrieves
## the saved value.

## makeCacheMatrix makes a 'matrix' object.  If the inverse
## of the matrix has already been calculated it is stored here.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## cacheSolve returns the inverse of a matrix;
## if the inverse has already been calculated then it retrieves
## it from the cache, otherwise it calculates the inverse and
## saves it into the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
    
    m <- x$getInverse()   #gets inverse matrix from makeCacheMatrix
    if(!is.null(m)) {     #if m is not null (inverse has been saved already)
        message("getting cached data") 
        return(m)         #retrieves inverse
    }
    data <- x$get()       
    m <- solve(data, ...) #calculates inverse of data
    x$setInverse(m)       #sets inverse in cache
    m                     #returns inverse
    
}
