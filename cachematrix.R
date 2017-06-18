## Put comments here that give an overall description of what your
## functions do
## Irwin

## Creates a list of functions that either sets or gets the matrices

makeCacheMatrix <- function(x = matrix()) 
{
    inverseX = NULL
    
    setMatrix = function(mat1)
        {
            x = mat1
        }
    
    getMatrix = function()
        {
            x
        }
    
    setInverse = function(mat2)
        {
            inverseX = solve(mat2)
        }
    
    getInverse = function()
        {
            inverseX
        }
    
    list(set = setMatrix, get = getMatrix, setInv = setInverse, getInv = getInverse)
}


## Returns the inverse or calculates the inverse but then sets the cache

cacheSolve <- function(x, ...) 
{
    tInverse = x$getInverse()
    
    if (!is.null(tInverse) & identical(x&getMatrix(),solve(tInverse)))
    {
        message("Getting cached data")
        return(tInverse)
    }
    else
    {
        x$setInverse(x&getMatrix())
        x&getInverse()
    }
}
