#######################################################
############## R Programming - Assignement 2###########
#######################################################
###################### S. Casteau######################
#######################################################

#Create a CacheMatrix function with 4 others functions: get, set, getinv & setinv
makeCacheMatrix <- function(x = mat()) {
  
  xinv <- NULL # Create an empty matrix where the results will be stored 
  
  set <- function(y) {
    x <<- y
    xinv <<- NULL # 
  }
  
  get <- function() x # It'll return data matrix
  setinv <- function(inv) xinv <<- inv # It'll set the inverse of the data matrix
  getinv <- function() xinv # It'll return the INVERSE data matrix

  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Here list is use to create a list of the different functions 
# that'll be usefull to change the parameters and use the makeCacheMatrix functions
# with  set = change matrix
#       get = get the matrix
#       setinv = set the inverse matrix
#       getInv = get the inverse matrix

cacheSolve <- function(x, ...) {
  
  m <- x$getinv() #it'll get the inverse matrix from "x" - which is for now equal to null
 
  if(!is.null(m)) { # it'll check if the inverse results are in the datamatrix m 
    print("getting data") #if yes, it'll print "getting data"
    return(m) # and then return the calculated inversion
  }
  data <- x$get() # if no, then, the function will get the matrix object
  m <- solve(data) # then solve 
  x$setinv(m) # get in into the m matric 
  m # and return the result
}



# Test
test <- matrix(runif(12,2,200),1,1)
print (test)

##> print (test)
##[,1]
##[1,] 84.08866
