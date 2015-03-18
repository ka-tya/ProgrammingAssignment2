## makeCacheMatrix function  creates a list of 4  functions: 
## set, get, setInverse and getInverse.
## It is used by cacheSolve to get and set the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
# initiate the cache value with NULL,
# this is where the inversed matrix will be stored
        inv <- NULL
  
# set a matrix to the list object created by the makeCacheMatrix
         set <- function(y) {
                x <<- y         # The assignment operator <<- is used to hide
                inv <<- NULL    # the internal variables from the outside environment.
        }

        get <- function() x     #return the input matrix

# set the inverted matrix and store it in cache variable
        setInverse <- function(inverse) inv <<- inverse

# get the value of the inverted matrix
        getInverse <- function() inv

# return a list of the functions, so they can be further used
# like x$set to change the matrix, x$get to get the matrix, etc
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
 }
## cacheSolve function returns the inversed matrix 
## for the matrix passed by the makeCacheMatrix function.
## If the inverted matrix doesn't exist in cache,
## it is first calculated and stored in a variable.

cacheSolve <- function(x, ...) {
# first check if the inverted mattix value is stored,
# retrive the value of the attribute
        inv <- x$getInverse()
# if it is available print it out
        if(!is.null(inv)) {            
                message("getting cached data")
                return(inv)
        }
 
# if the inverted matrix is not available:
        data <- x$get()          # get the original matrix value
        inv <- solve(data, ...)  # calculate the inverse matrix
        x$setInverse(inv)        # set the inverse matrix value to the inv variable
        inv                      # return the value of the inverted matrix
}
