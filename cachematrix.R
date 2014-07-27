## cachematrix.R: contains two functions to find and cache the inverse of a
## matrix. If inverse is already cached, it will use that instead of 
## recaculating.

## makeCacheMatrix: creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## initializes m, which will store inverse
    set <- function(y) { ## sets value of matrix
            x <<- y ## <<- searches for existing y in cachematrix()
            m <<- NULL ## initializes global m
    }
    get <- function() x ## gets value of matrix
    setinverse <- function(inverse) m <<- inverse ## caches inverse of matrix
    getinverse <- function() m ## gets inverse of matrix
    list(set = set, get = get, ## returns list of functions that cacheSolve()
        setinverse = setinverse, ## can use
        getinverse = getinverse)
    }
}

## cacheSolve: computes the inverse of the matrix from makeCacheMatrix() or 
## retrieves already-cached inverse
cacheSolve <- function(x, ...) {
    m <- x$getinverse() ## sets m to match  the m of makeCacheMatrix()
        if(!is.null(m)) { ## if m is already defined, inverse is already cached
            message("getting cached data")  
        return(m) ## returns cached inverse, skipping calculation
    }
    data <- x$get() ## gets matrix data from makeCacheMatrix()
    m <- solve(data, ...) ## solves for inverse if not cached
    x$setinverse(m) ## caches inverse
    (m) ## returns inverse
    }  
}
