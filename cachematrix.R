## Cache Matrix
# This set of functions allows a matrix to be stored alongside a 
# cached version of its inverse matrix. The functions allow for 
# the inverted matrix to be recalled without having to recalculate
# it.

## The function makeCacheMatrix creates a list of functions that 
# store and retrieve data about a matrix.  

makeCacheMatrix <- function(x = matrix()) {
  my.inverse <- NULL  #my.inverse will store the inverse matrix once 
                      #it is generated. Initially we don't know what 
                      #it is so we set it to NULL
  
  # We will be returning a list of functions for manipulating the
  # matrix and it's cached inverse.  First we define them
  
  # BEGIN DEFINITION OF FUNCTIONS RETURNED IN LIST
  set <- function(y) { #set is used to handle the construction of a 
                        #cachematrix
    x <<- y           #If set gets called, we push the matrix into 
                      #the global environment
    my.inverse <<- NULL  #We also push NULL into the global envir-
                        # onment in since the inverse is not yet
                        # calculated.
    
  }
  
  get <- function() x  # Get returns the original matrix x
  
  setinverse <- function(new.inverse) my.inverse <<- new.inverse
                      # setinverse pushes what is calculated
                      # to be the inverse (new.inverse)
                      # into my.inverse in the global env
                      # this is the cache
  
  getinverse  <- function() my.inverse
                      # get inverse pulls the cached value for the 
                      # inverse matrix
  
  # END DEFINITION OF FUNCTIONS RETURNED IN LIST
  
  #We return a list of the functions we just defined
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function will return the inverse of the matrix stored
# within the list. If the matrix was previously calculated
# the cached inverse is returned. Otherwise the inverse is
# calculated and cached 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #The first step is to see if we have stored a cached
  #value for the inverse
  
  this.inverse <- x$getinverse()
  
  if (is.null(this.inverse)) {
    # If this.inverse is null the inverse hasn't been
    # calculated yet
    
    #We therefore calculate the inverse matrix
    this.inverse <- solve(x$get())
    
    #And cache the calculated value
    x$setinverse( this.inverse )
  }
  
  #Finally regardless of whether we calculated it from
  #scratch, or merely pulled the cached value, we 
  #return it. 
  this.inverse
}
