## Function description: creates a matrix object that can cache its inverse
## With a matrix input, the function sets and gets its value and sets and gets
## the inverse matrix. This way, the matrix can cache its own object.

makeCacheMatrix <- function(x = matrix()) {
        
        m <<- NULL
        
        set <- function(y) { 
                x <<- y  ##with this sign you assign a value from another environment
                m <<- NULL
        } #here we set, we "put in" the matrix
        get <- function() x #here we get the matrix
        setInverse <- function(inverse) m <<- inverse #here we set the inverse
        getInverse <- function() m #here we get the inverse
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
        ##here we store everything to call on the next function
}

## Function description: Calculates or retrieves an already calculated inverse

cacheSolve <- function(x, ...) {
        
        m <- x$getInverse()
        if(!is.null(m)) { ##first, we run this to avoid recalculating an already existing answer
                message("getting cached data")
                return(m)
        }
        
        thematrix <- x$get() #we get the matrix
        m <- solve(thematrix, ...) ##as indicated by the assignment, this calculate the inverse
        x$setInverse(m) #we set the new inverse
        m ## Return a matrix that is the inverse of 'x'
}