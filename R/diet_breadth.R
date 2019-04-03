#' @title Diet breadth model
#'
#' @description
#' \itemize{
#'   \item{dietBreadthOptimalSet determines the optimal set of resources to take on encounter}
#'   \item{dietBreadthReturnRate calculates the return rate (kcal/hr) given a set of items in the diet.}
#'   \item{dietBreadthOptimReturnRate calculates return rate for the optimal set.}
#' }
#'
#' @details A forager searches the landscape for resources to procure. Each resource is encountered randomly as a Poisson process with encounter rate lambda_i. On encountering a resource, the forager chooses whether or not to procure it. If not, the forager continues searching. If so, the forager pursues and processes it with handling time h_i, receiving a value from the resource of v_i. Usually the value is the food energy provided by the item. Optionally, search is costly to the forager at the rate c_s_dot. That is, c_s_dot is the rate of loss of value (usually energy / time).
#'
#' The overall return rate of foraging is (using R syntax):
#'
#' returnRate = (-c_s_dot + sum(lambda * v)) / (1 + sum(lambda * h))
#'
#' The optimal diet is the set of items that maximizes this returnRate. Define
#'
#' rank_i = v_i / h_i
#'
#' as the rank (or on encounter return rate) of resource i. To determine the optimal set, order resources by rank from best to worst. Add resources to the diet in this rank order until the return rate no longer increases from adding additional items. This occurs when the on encounter return rate of a new item in the diet is lower than the overall return rate from procuring all the items that are more highly ranked.
#'
#' @references MacArthur and Pianka 1966 -- On Optimal Use of a Patchy Environment. American Naturalist. 100:603-609.

#' @title Diet breadth optimal set
#' @param v Vector of resource values
#' @param h Vector of resource handling times
#' @param lambda Vector of encounter rates
#' @param c_s_dot Loss of value per unit time while searching (default 0)
#' @return A boolean vector indicating whether each resource is in the optimal set
#' @export
dietBreadthOptimalSet <- function(v,h,lambda,c_s_dot=0) {
  N <- length(v) # number of input resources potentially in optimal set

  # Sort by rank (on encounter return rate)
  rank <- v / h
  ord <- order(rank,decreasing=T)
  rank <- rank[ord]
  v <- v[ord]
  h <- h[ord]
  lambda <- lambda[ord]

  bestRate <- 0
  bestSet <- rep(F,N)
  testSet <- rep(F,N)
  for(n in 1:N) {
    testSet[n] <- T

    testRate <- dietBreadthReturnRate(v,h,lambda,c_s_dot,testSet)
    if(testRate >= bestRate) {
        bestRate <- testRate
        bestSet <- testSet
    } else {
        break
    }
  }
  optimalSet <- bestSet
  optimalSet[ord] <- optimalSet
  return(optimalSet)
}

#' @title Diet breadth return rate
#' @param v Vector of resource values
#' @param h Vector of resource handling times
#' @param lambda Vector of encounter rates
#' @param c_s_dot Loss of value per unit time while searching (default 0)
#' @return A boolean vector indicating whether each resource is in the optimal set
#' @export
dietBreadthReturnRate <- function(v,h,lambda,c_s_dot=0,include=NA) {
  N <- length(v)
  if(all(is.na(include))) {
    include <- rep(T,N)
  }
    
  v <- v[include]
  h <- h[include]
  lambda <- lambda[include]
  v_dot = (-c_s_dot + sum(lambda * v)) / (1 + sum(lambda * h))
  return(v_dot)
}

#' @title Diet breadth optimal return rate
#' @param v Vector of resource values
#' @param h Vector of resource handling times
#' @param lambda Vector of encounter rates
#' @param c_s_dot Loss of value per unit time while searching (default 0)
#' @return A boolean vector indicating whether each resource is in the optimal set
#' @export
dietBreadthOptimalReturnRate <- function(v,h,lambda,c_s_dot=0) {
  include <- dietBreadthOptimalSet(v,h,lambda,c_s_dot)
  return(dietBreadthReturnRate(v,h,lambda,c_s_dot,include))
}
