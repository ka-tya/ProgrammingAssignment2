## makeCacheMatrix function  creates a list of 4  functions: 
## set, get, setInverse and getInverse.
## The assignment operator <<- is used to hide
## the internal variables from the
## outside environment. 
## It is used by cacheSolve to get and set the inversed matrix.


makeCacheMatrix <- function(x = matrix()) {
    #initiate the cache value with NULL,
    #this is where the inversed matrix will be stored
    cache <- NULL
    # set function, set a matrix to the list object created 
    #by the makeCacheMatrix
    
    set <- function(y){
        x <<- y
        cache <<- NULL
    }
    #return the input matrix
    get <- function() x
    # set the inverted matrix and store it in cache variable
    setInverse <- function(inverse) cache<<-inverse
    #get the value of the inverted matrix
    getInverse <- function() cache
    #return a list of the functions, so they can be further used
    #like x$set to change the matrix,
    #x$get to get the matrix, etc
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function calculates the inversed matrix,
## which is passed by the makeCacheMatrix function.
## If the inverted matrix doesn't exist in cache,
## it is created and stored in cache.

cacheSolve <- function(x, ...) {
       
        #first check if the inverted matrix is calculated
        cache <-x$getInverse
        #if it is available print it out
        if(!is.null(cache)){
            message("getting cached data")
            return(cache)
        }
        
        #if the inverted matrix is not stored, 
        #then the calculation is done:
        data <- x$get()
        cache <- solve(data, ...) # calculate the inverse matrix
        x$setinverse(cache) #the inverse matrix is set
        cache # return the value of the inverted matrix
}
