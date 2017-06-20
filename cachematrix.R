## Put comments here that give an overall description of what your
## functions do
## Irwin

## Creates a list of functions that either sets or gets the matrices

makeCacheMatrix <- function(x = matrix()) 
{
    inverseX <- NULL
    
    set <- function(mat1)                   # sets the matrix 
        {
            x <<- mat1
            inverseX <<- NULL
        }
    
    get <- function()                       # retrievs the matrix
        {
            x
        }
    
    setInv <- function(solve)
        {
            inverseX <<- solve              # sets the inverse of the matrix
        }
    getInv <- function()                    # retrieves the inverse matrix
        {
            inverseX
        }
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)    # special vector
}


## Returns the inverse or calculates the inverse but then sets the cache

cacheSolve <- function(x, ...) 
{
    inverseX <- x$getInv()                   # gets the inverse matrix
    tMatrix <- x$get()                       # gets the actual matrix
    
    if (!is.null(inverseX) & identical(tMatrix,x))    # checks if it is null and if the matrices are the same
    {
        message("Getting cached data")
        return(inverseX)                              # returns the cache matrix
    }
        data <- x$get()                               # else sets the new value of the matrix
        inverseX <- solve(data,...)
        x&setInv(inverseX)
        inverseX
}

