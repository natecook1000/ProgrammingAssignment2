## The functions below show two methods of storing a matrix along with its
## cached inverse. The first method follows on the example given, separating
## out the calculation of the inverse from its storage. The second method
## encapsulates the calculation and caching of the inverse within one
## structure, so the user gets the cached inverse if available, or a
## freshly generated inverse if not, without needing an additional method


#####################################
## Method 1

## Create an "inverse-caching matrix object" with get, set, getinverse,
## and setinverse methods.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Solve the matrix stored in the "inverse-caching matrix object" that
## is passed in. If the inverse is already cached, use that, otherwise
## solve and cache the inverse before returning it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("using cached inverse")
        return(inv)
    }
    message("generating inverse")
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}


#####################################
## Method 2

## Create a "self-caching matrix object" with get, set, and getinverse
## methods. The getinverse method returns a cached inverse if available,
## or solves and caches the inverse if not. Note that the setinverse
## method isn't necessary with this implementation, since the details of
## the caching are hidden from the user.

makeSelfCachingMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getinverse <- function() {
        if (is.null(inv)) {
            message("generating inverse")
            inv <<- solve(x)
        } else {
            message("using cached inverse")
        }
        inv
    }
    list(set = set, get = get,
         getinverse = getinverse)
}


#####################################
## Testing

# create test matrices

m1 <- matrix(1:4, 2, 2)
m2 <- matrix(c(1,1,1,2,3,1,2,3,4), 3, 3)

# method 1

cacheMatrix <- makeCacheMatrix(m1)
cacheSolve(cacheMatrix)
cacheSolve(cacheMatrix)

cacheMatrix$set(m2)
cacheSolve(cacheMatrix)
cacheSolve(cacheMatrix)

# method 2

selfCachingMatrix <- makeSelfCachingMatrix(m1)
selfCachingMatrix$getinverse()
selfCachingMatrix$getinverse()

selfCachingMatrix$set(m2)
selfCachingMatrix$getinverse()
selfCachingMatrix$getinverse()


