# Copyright (c) 2008-2010 Revolution Analytics
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# This super-simple version was derived from the Revolution Analytics
# original, Dec 1, 2021 Charles Eliot

# Create a vector iterator object
iter <- function(obj, checkFunc=function(...) TRUE, recycle=FALSE, ...) {
  state <- new.env()
  state$index <- 0L
  state$obj <- obj
  n <- length(obj)
  it <- list(state=state, length=n, checkFunc=checkFunc, recycle=recycle)
  it
}

hasNext <- function(obj, ...){
  ifelse(obj$state$index + 1 > obj$length, FALSE, TRUE)
}

getIterVal <- function(obj, plus=0L, ...) {
  i <- obj$state$index + plus
  if (i > obj$length)
    stop('SubscriptOutOfBounds', call.=FALSE)
  obj$state$obj[[i]]
}

nextElem <- function(obj, ...) {
  repeat {
    tryCatch({
      if (obj$checkFunc(getIterVal(obj,1L))) {
        obj$state$index <- obj$state$index + 1L
        return(getIterVal(obj))
      }
      obj$state$index <- obj$state$index + 1L
    }, error=function(e) {
      if (any(nzchar(e$message))) {
        if (identical(e$message, "SubscriptOutOfBounds")) {
          if (obj$recycle) {
            obj$state$index <- 0L
          }
          else {
            stop('StopIteration', call.=FALSE)
          }
        }
        else {
          stop(e$message, call.=FALSE)
        }
      }
      else {
        stop('Abort', call.=e)
      }
    })
  }
}
