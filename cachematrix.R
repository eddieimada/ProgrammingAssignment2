MakeCacheMatrix <- function(Matrix = matrix()) {
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




  ## created by using the "makeCacheMatrix()" function

cacheSolve <- function(sMatrix, ...) {

	# retrieve "content" of InvMatrix in sMatrix
        InvMatrix <- sMatrix$retrieve()
        
	# if cached, use it and dont recompute the inverse matrix
        if(!is.null(InvMatrix)) {
                message("Using cached inverse matrix")
                return(InvMatrix)
        }
        
        # if not chached, get the matrix
        Matrix <- sMatrix$get()
        
        # derive the inverse matrix
        InvMatrix <- solve(Matrix, ...)
        
        # store the inverse matrix in the sMatrix
        sMatrix$cache(InvMatrix)
        
        # return the inverse matrix
        InvMatrix
}
