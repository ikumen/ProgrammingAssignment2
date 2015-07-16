# The MIT License (MIT)
# 
# Copyright (c) 2015 ikumen@gnoht.com
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#     
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
#
# Description: 
#   Creates a special "matrix" object that can cache it's inverse. The
#   special matrix encapsulates a regular matrix which is used during inversing.
#   The resulting inverse matrix is "cache" locally in the special matrix.
#
# Usage:
#   makeCacheMatrix([x = matrix])
#
# Params:
#   x - optional matrix to wrap, otherwise emtpy matrix will be created
#
# Returns:
#   special object representing a matrix
#
makeCacheMatrix <- function(x = matrix()) {
    # holds the computed inverse of matrix "x"
    inverse_matrix <- NULL
    
    # assigns new matrix to x
    set <- function(y) {
        x <<- y
        # re-initialize since we're wrapping a new matrix
        inverse_matrix <<- NULL
    }
    # return the underlying matrix we've wrapped
    get <- function() x
    # sets computed inverse of matrix x
    setInverse <- function(im) inverse_matrix <<- im
    # returns the computed inverse of matrix x
    getInverse <- function() inverse_matrix
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#
# Description:
#   "solve" a special matrix object (it's underlying wrapped matrix) and cache
#   the results.
#
# Usage:
#   cacheSolve(x)
#
# Params:
#   x - special matrix to solve and cache
#
# Returns:
#   an inverse matrix of x
#
cacheSolve <- function(x, ...) {
    # Check if we've solved before, check if special matrix already 
    # has inverse i.e, cache hit
    inverse_matrix <- x$getInverse() 
    if(!is.null(inverse_matrix)) {
        message("Getting cached inverse!")
        return(inverse_matrix)
    }
    
    # Cache miss, so get the wrapped matrix and solve, then cache it
    matrix <- x$get()
    im <- solve(matrix, ...)
    x$setInverse(im)
    
    # return the solved inverse matrix
    im
}
