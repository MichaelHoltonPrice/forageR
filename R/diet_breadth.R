#' @title Diet breadth model
#'
#' @description
#' \itemize{
#'   \item{dietBreadthOptimalSet determines the optimal set of resources to take on encounter}
#'   \item{dietBreadthReturnRate calculates the return rate (kcal/hr) given a set of items in the diet.}
#'   \item{dietBreadthOptimReturnRate calculates return rate for the optimal set.}
#' }
#'
#' @details A forager searches the landscape for resources to procure. Each resource is encountered randomly as a Poisson process with encounter rate lambda_i. On encountering a resource, the forager chooses whether or not to procure it. If not, the forager continues searching. If so, the forager pursues and processes it with handling time h_i, receiving a benefit from the resource of u_i. Usually the utility is the food energy provided by the item. Optionally, search is costly to the forager at the rate c_s. That is, c_s is the rate of loss of utility (usually energy / time).
#'
#' The overall return rate of foraging is (using R syntax):
#'
#' returnRate = (-c_s + sum(lambda * u)) / (1 + sum(lambda * h))
#'
#' The optimal diet is the set of items that maximizes this returnRate. Define
#'
#' rank_i = u_i / h_i
#'
#' as the rank (or on encounter return rate) of resource i. To determine the optimal set, order resources by rank from best to worst. Add resources to the diet in this rank order until the return rate no longer increases from adding additional items. This occurs when the on encounter return rate of a new item in the diet is lower than the overall return rate from procuring all the items that are more highly ranked.
#'
#' @references MacArthur and Pianka 1966 -- On Optimal Use of a Patchy Environment. American Naturalist. 100:603-609.

#' @title Diet breadth optimal set
#' @param u Vector of resource utilities
#' @param h Vector of resource handling times
#' @param lambda Vector of encounter rates
#' @param c_s Loss of utility per unit time while searching (default 0)
#' @return A boolean vector indicating whether each resource is in the optimal set
#' @export
dietBreadthOptimalSet <- function(u,h,lambda,c_s=0) {
  N <- length(u) # number of input resources potentially in optimal set

  # Sort by rank (on encounter return rate)
  rank <- u / h
  ord <- order(rank,decreasing=T)
  rank <- rank[ord]
  u <- u[ord]
  h <- h[ord]
  lambda <- lambda[ord]

  bestRate <- 0
  bestSet <- rep(F,N)
  testSet <- rep(F,N)
  for(n in 1:N) {
    testSet[n] <- T

    testRate <- dietBreadthReturnRate(u,h,lambda,c_s,testSet)
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
#' @param u Vector of resource utilities
#' @param h Vector of resource handling times
#' @param lambda Vector of encounter rates
#' @param c_s Loss of utility per unit time while searching (default 0)
#' @return A boolean vector indicating whether each resource is in the optimal set
#' @export
dietBreadthReturnRate <- function(u,h,lambda,c_s=0,include=NA) {
  N <- length(u)
  if(all(is.na(include))) {
    include <- rep(T,N)
  }
    
  u <- u[include]
  h <- h[include]
  lambda <- lambda[include]
  u_dot = (-c_s + sum(lambda * u)) / (1 + sum(lambda * h))
  return(u_dot)
}

#' @title Diet breadth optimal return rate
#' @param u Vector of resource utilities
#' @param h Vector of resource handling times
#' @param lambda Vector of encounter rates
#' @param c_s Loss of utility per unit time while searching (default 0)
#' @export
dietBreadthOptimalReturnRate <- function(u,h,lambda,c_s=0) {
  include <- dietBreadthOptimalSet(u,h,lambda,c_s)
  return(dietBreadthReturnRate(u,h,lambda,c_s,include))
}

#' @title Diet breadth density to encounter rate
#' @param density Density of resource on landscape (Number / Area)
#' @param search_radius Visibility of resource to forager (Length)
#' @param search_speed Speed at which forager moves through the landscape while searching (Length / Time)
#' @return Encounter rate with the resource (Number / Time)
#' @export
dietBreadthDensityToEncounterRate <- function(density,search_radius,search_speed) {
  return(density * 2 * search_radius * search_speed)
}


#' @title Diet breadth add procurement cost
#' @param u0 Vector of base resource utilities
#' @param h Vector of resource handling times
#' @param procure_cost Vector of resource procurement costs (as a rate)
#' @return Vector of resource utilities accounting for procurement cost
#' @export
dietBreadthAddProcureCost <- function(u0,h,procure_cost) {
  return(u0 - h * procure_cost)
}

#' @title Diet breadth summary analysis
#' @param u Vector of resource utilities
#' @param h Vector of resource handling times
#' @param lambda Vector of encounter rates
#' @param c_s Loss of utility per unit time while searching (default 0)
#' @param resNames Vector of resource names (default none)
#' @export
dietBreadthAnalysis <- function(u,h,lambda,c_s=0,resNames=NA) {
  # order by rank
  q <- u / h # on encounter return rate
  ord <- order(q,decreasing=T)
  q <- q[ord]
  u <- u[ord]
  h <- h[ord]
  lambda <- lambda[ord]

  # Add items to the diet sequentially in rank order and calculate the overall
  # return rate
  include <- rep(F,length(u))
  returnRate <- rep(NA,length(u))
  for(n in 1:length(u)) {
    include[n] <- T
    returnRate[n] <- dietBreadthReturnRate(u,h,lambda,c_s,include)
  }

  optimalSet <- rep(F,length(u))
  optimalSet[which.max(returnRate)] <- T
  if(!all(is.na(resNames))) {
    resNames <- resNames[ord]
    return(data.frame(name_lowest_rank=resNames,return_rate=returnRate,q_lowest_rank=q,optimal_set=optimalSet))
  } else {
    return(data.frame(return_rate=returnRate,q_lowest_rank=q,optimal_set=optimalSet))
  }
}
