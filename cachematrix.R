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
# .............................................................................
# 
#
#
# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly -- the 
# following functions serve this purpose. 
#
#
# Creates a special "matrix" object that contains an invertiable matrix and
# a cache of it's inverse. 
#
# usage: makeCacheMatrix([x = matrix])
# params: x - optional invertiable matrix, otherwise an emtpy matrix will be created
# returns: special makeCacheMatrix object
#
makeCacheMatrix <- function(x = matrix()) {
    # holds the computed inverse of matrix "x"
    inverse_x <- NULL
    
    # assigns new invertible matrix to x
    set <- function(y) {
        x <<- y
        # re-initialize since we've got a new matrix
        inverse_x <<- NULL
    }
    # return the underlying matrix we've wrapped
    get <- function() x
    # sets computed inverse of matrix x
    setInverse <- function(ix) inverse_x <<- ix
    # returns the computed inverse of matrix x
    getInverse <- function() inverse_x
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#
# Takes a makeCacheMatrix object, solves for it's inverse
# if the inverse if not already cached.
#
# usage: cacheSolve(x)
# params: x - makeCacheMatrix to solve and cache
# returns: an inverse of given makeCacheMatrix
#
cacheSolve <- function(x, ...) {
    # Check if we've solved before, check if special matrix already 
    # has inverse i.e, cache hit
    inverse_x <- x$getInverse() 
    if(!is.null(inverse_x)) {
        message("Getting cached inverse!")
        return(inverse_x)
    }
    
    # Cache miss, so get the wrapped matrix and solve, then cache it
    matrix <- x$get()
    im <- solve(matrix, ...)
    x$setInverse(im)
    
    # return the solved inverse matrix
    im
}
