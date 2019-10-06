## The makeCacheMatrix function defines an object that stores a matrix and the inverse of that matrix as well.
## When the object created by the makeCacheMAtrix is fed to the cacheSolve function as an argument, the cached 
## inverse in the object's environment is then returned.

makeCacheMatrix <- function(x = matrix()) {         # initialises x as a matrix
    m <- NULL                                       
    set <- function(y){
        x <<- y                                     # sets the value of y to x in the parent environment
        m <<- NULL                                  # sets m to NULL in the parent environment
    }
    get <- function() x                             # gets x from the parent environment 
    setinverse <- function(solve) m <<- solve       # sets inverse of m present in the parent environment
    getinverse <- function() m
    list(set = set,                                 # The function returns this list, which contains four 
         get = get,                                 # named functions so that they are accessible using 
         setinverse = setinverse,                   # the $ sign later
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    m <- x$getinverse()                             # Checking if the inverse already exists in the parent environment
    if(!is.null(m)){                                # if inverse exists, return the inverse
        message("getting cached data")
        return(m)
    }
    data <- x$get()                                 # otherwise, set the data to the input matrix
    m <- solve(data, ...)                           # calculate and store the inverse in m
    x$setinverse(m)
    m
                                                    # Return a matrix that is the inverse of x
}

a <- matrix(c(1,2,0,3), 2, 2)
my_matrix <- makeCacheMatrix(a)
cacheSolve(my_matrix)
