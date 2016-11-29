
# The purpose of this whole code is to cache inverse computations. When an 
# inverse has been computed, the code will cache the value so that when 
# it is needed in the future, R will simply read the value instead of doing 
# the whole computation again.

# Two main functions are introduced in this code. The first function is a list 
# of various functions necessary to compute the inverse of a matrix while the 
# second function will first determine whether the inverse of the matrix has 
# been computed first before proceeding with the computation.

# The first function makeCacheMatrix creates a special "Matrix" which contains 
# a list containing furnctions of 
# 1. Setting the value of the matrix
# 2. Getting the value of the matrix
# 3. Setting the value of the inverse
# 4. Getting the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)  {
                m <<- NULL
                x <<- y
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


# The following function finds the inverse of the above special "Matrix" 
# created by the above function. It first checks whether the inverse is already
# computed. If it is already computed, the function will 'get' the inverse from
# the cache and skip the computation. If the inverse haven't computed yet, it 
# will through the function. It will first compute the inverse and sets the 
# the inverse in the cache via the 'setInverse' function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
        
}

## Return a matrix that is the inverse of 'x'
