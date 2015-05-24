MakeMatrix <- function(Matrix = matrix()) {
	# create an object for a Cached Inverse Matrix
        InvMatrix <- NULL
	
	# set a raw matrix & reset the cached matrix
        set <- function(InputMatrix) {
                Matrix <<- InputMatrix
                InvMatrix <<- NULL
	}
	
	# get the raw matrix
        get <- function() Matrix
	
	# cache the inverse matrix
        cache <- function(inv) InvMatrix <<- inv
	
	# retrieve the cached matrix
        retrieve <- function() InvMatrix
	
	# return a list of the sub-functions defined above
        list(set = set, get = get, 
        	 cache = cache,
        	 retrieve = retrieve)
}



## Write a short comment describing this function

## Function that returns the inverse matrix of a SpecialMatrix
  ## created by using the "makeCacheMatrix()" function

cacheSolve <- function(SpecialMatrix, ...) {

	# retrieve "content" of InvMatrix in SpecialMatrix
        InvMatrix <- SpecialMatrix$retrieve()
        
	# if NOT empty, use it and avoid recomputing the inverse matrix
        if(!is.null(InvMatrix)) {
                message("Using cached inverse matrix")
                return(InvMatrix)
        }
        
        # else, get the raw matrix
        Matrix <- SpecialMatrix$get()
        
        # derive the inverse matrix
        InvMatrix <- solve(Matrix, ...)
        
        # store the inverse matrix in the SpecialMatrix
        SpecialMatrix$cache(InvMatrix)
        
        # return the inverse matrix
        InvMatrix
}
