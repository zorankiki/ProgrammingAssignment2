## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {     ##make list of functions...
        m <- NULL                               ##set m to null object
        set <- function(y) {                    ##stores init. values to x and m
                x <<- y                            
                m <<- NULL
        }
        get <- function() x                     ##pull out value of x-matrix
        setsolve <- function(solve) m <<- solve ##put in value to m 
        getsolve <- function() m                ##pull out stored value of m
        list(set = set, get = get,              ##gives output of function as a
             setsolve = setsolve,               #list of functions named set,
             getsolve = getsolve)               #,get,setsolve,getsolve
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()                       ##pull out stored value of m
        if(!is.null(m)) {                       ##check if is stored data of m
                message("getting cached data")  ##value NULL and returns stored
                return(m)                       ##m
        }                                       ##if stored value of m is NULL
        data <- x$get()                         #then it pulls out data of x
        m <- solve(data, ...)                   #finds inverse matrix
        x$setsolve(m)                           #and set solution to m by
        m                                       #by calling function from list
}                                               #$setsolve(m) passing to it m

##if operator <<- is not used then the value of m is not stored by function
#x$setsolve(m) in m variable and can not be pull out from the otside
#environment (makeCacheMatrix function) where it is defined