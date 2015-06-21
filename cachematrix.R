## Following are the pair of functions that cache the inverse of 
## a given matrix. The first function i.e., makeCacheMatrix 
## cache the inverse of a given matrix in s and the second 
## function i.e., cacheSolve first checks the inverse of the 
## special matrix in cache, if found, it directly displays the 
## inverse from the cache otherwise calculates the inverse, 
## store the inverse in cache and then displays it.  



## This function creates a special "matrix" object that can 
## cache its inverse. The function will return a list of four 
## functions namely set, get, setsolve and getsolve.  
 
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	## "set" function creates a special matrix y and store it  	
	## in object x in cache. It then set the inverse s to 
	## NULL and store the same in cache.
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	## "get" function returns the special matrix stored in x
	get <- function() x

	## "setsolve" function store the "solve" matrix in cache 
	## as s
	setsolve <- function(solve) s <<- solve
	
	## "getsolve" function returns the matrix stored in s
	getsolve <- function() s

	## This returns the list containing above four functions
	list(set = set, get = get, 
		setsolve = setsolve,
		getsolve = getsolve)
}


## This function named "cacheSolve" takes special matrix, 
## returned by the makeCacheMatrix, as input and then computes 
## the inverse of that. The function first check the inverse s 
## of the matrix in cache. If s exists in cache, it returns the 
## s matrix else computes the inverse of the input matrix by 
## calling "solve()" function of R. 

cacheSolve <- function(x, ...) {
        ## Retrieve the inverse matrix from cache and store in s	   	
        s <- x$getsolve()
	  
        ## If s found is not null, return inverse matrix s
        if(!is.null(s)) {
                message("getting inverse matrix from cache")
                return(s)
        }
	   
	## If inverse is not found in cache, compute inverse of 
	## special matrix stored in "data" by calling "solve" 
	## function of R, store inverse in s by calling "setsolve" sub-
	## function of makeCacheMatrix function and then return s
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
