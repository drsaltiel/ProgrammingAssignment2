## makeCacheMatrix and cacheSolve work together to calculate
## the inverse of a matrix and save it into the cache.
## If that inverse has already been calculated then they retrieve
## the stored value.

## makeCacheMatrix makes a 'matrix' object and can store values
## for the matrix and matrix's inverse.
## It has subfunctions set, get, setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL               #initializes cache
    set <- function(y) {    #defines set function
        x <<- y             #stores matrix as x
        m <<- NULL          #clears cache
    }
    get <- function() x                           
                            #def get function, returns matrix
    setInverse <- function(inverse) m <<- inverse 
                            #def setInverse, sets inverse as cache
    getInverse <- function() m
                            #def getInverse, returns inverse from cache
    list(set = set, get = get, #returns list of values to tags
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## cacheSolve returns the inverse of a matrix;
## if the inverse has already been calculated then it retrieves
## it from the cache, otherwise it calculates the inverse and
## saves it into the cache (and returns it)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
    
    m <- x$getInverse()   #gets cached value from makeCacheMatrix
    if(!is.null(m)) {     #if cache isn't empty
        message("getting cached data") 
        return(m)         #retrieves cached inverse and returns
    }
    data <- x$get()       #otherwise gets matrix from makeCacheMatrix
    m <- solve(data, ...) #calculates inverse of matrix
    x$setInverse(m)       #sets inverse in cache
    m                     #returns inverse
    
}
