##creates a special "matrix" object that can cache its inverse
##which is a list containing a function to

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        } ##1 set the value of the matrix
        get <- function() x ##2 get the value of the matrix
        setsolve <- function(slove) s <<- solve 
        ##3 set the value of the inverse matrix
        getsolve <- function() s
        ##4 get the value of the inverse matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


##computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                ##If the inverse has already been calculated
                ##and the matrix has not changed
                message("getting cached data")
                return(s)##retrieve the inverse from the cache
        }
        
        ##If the inverse has not been calculated or the matrix has been changed
        data <- x$get()
        s <- solve(data, ...)##calculated the inverse
        x$setsolve(s)##put the result in to the cache
        s
        
}
