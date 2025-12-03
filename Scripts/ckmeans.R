#########################################################
# This function was obtained from the archived version  #
# of the conclust package, which was removed from CRAN. #
# The conclust package provided several methods of      #
# supervised clustering, including COP K-means.         #
# This function was not written by this group.          #
#                                                       #
# credit is attributed to the author listed on the      #
# archived conclust website on CRAN:                    #
# Tran Khanh Hiep Nguyen Minh Duc                       #
#########################################################

ckmeans <- function(data, k, mustLink, cantLink, maxIter = 100, tol = 1e-6) {
  
  data <- as.matrix(data)
  n <- nrow(data)
  d <- ncol(data)
  
  # ---- 1. Build simple constraint lists (no transitive expansion) ----
  ML <- vector("list", n)
  CL <- vector("list", n)
  
  if (!is.null(mustLink) && nrow(mustLink) > 0) {
    for (i in 1:nrow(mustLink)) {
      a <- mustLink[i,1]; b <- mustLink[i,2]
      ML[[a]] <- c(ML[[a]], b)
      ML[[b]] <- c(ML[[b]], a)
    }
  }
  if (!is.null(cantLink) && nrow(cantLink) > 0) {
    for (i in 1:nrow(cantLink)) {
      a <- cantLink[i,1]; b <- cantLink[i,2]
      CL[[a]] <- c(CL[[a]], b)
      CL[[b]] <- c(CL[[b]], a)
    }
  }
  
  # ---- 2. Initialize centers by random sample ----
  set.seed(11302025)
  centers <- data[sample(1:n, k), , drop = FALSE]
  labels <- rep(0, n)
  
  # ---- helper: squared Euclidean distance ----
  dist2 <- function(x, y) sum((x - y)^2)
  
  # ---- 3. Main loop ----
  for (iter in 1:maxIter) {
    
    prev_labels <- labels
    
    # ---- Assign each point to nearest allowable cluster ----
    for (i in 1:n) {
      best_k <- -1
      best_d <- Inf
      
      for (cidx in 1:k) {
        
        # must-link: if any ML friend is assigned to a *different* cluster, reject
        violated <- FALSE
        for (m in ML[[i]]) {
          if (prev_labels[m] != 0 && prev_labels[m] != cidx) {
            violated <- TRUE
            break
          }
        }
        if (violated) next
        
        # cannot-link: if any CL friend *already* in cluster, reject
        violated <- FALSE
        for (c in CL[[i]]) {
          if (prev_labels[c] == cidx) {
            violated <- TRUE
            break
          }
        }
        if (violated) next
        
        # distance check
        dval <- dist2(data[i,], centers[cidx,])
        if (dval < best_d) {
          best_d <- dval
          best_k <- cidx
        }
      }
      
      # If no feasible cluster (rare), assign to *closest cluster* ignoring constraints
      if (best_k == -1) {
        best_k <- which.min(colSums((t(centers) - data[i,])^2))
      }
      
      labels[i] <- best_k
    }
    
    # ---- 4. Update centers ----
    new_centers <- matrix(0, nrow = k, ncol = d)
    counts <- table(factor(labels, levels = 1:k))
    
    for (j in 1:k) {
      if (counts[j] > 0) {
        new_centers[j,] <- colMeans(data[labels == j, , drop = FALSE])
      } else {
        # reinitialize empty cluster with random point
        new_centers[j,] <- data[sample(1:n, 1),]
      }
    }
    
    # ---- convergence check ----
    shift <- sum((centers - new_centers)^2)
    centers <- new_centers
    if (shift < tol) break
  }
  
  return(labels)
}

ckmeans2 <- function(data, k, mustLink, cantLink, maxIter = 100, tol = 1e-6) {
  
  data <- as.matrix(data)
  n <- nrow(data)
  d <- ncol(data)
  
  # ---- 1. Build simple constraint lists (no transitive expansion) ----
  ML <- vector("list", n)
  CL <- vector("list", n)
  
  if (!is.null(mustLink) && nrow(mustLink) > 0) {
    for (i in 1:nrow(mustLink)) {
      a <- mustLink[i,1]; b <- mustLink[i,2]
      ML[[a]] <- c(ML[[a]], b)
      ML[[b]] <- c(ML[[b]], a)
    }
  }
  if (!is.null(cantLink) && nrow(cantLink) > 0) {
    for (i in 1:nrow(cantLink)) {
      a <- cantLink[i,1]; b <- cantLink[i,2]
      CL[[a]] <- c(CL[[a]], b)
      CL[[b]] <- c(CL[[b]], a)
    }
  }
  
  # ---- 2. Initialize centers by random sample ----
  set.seed(11302025)
  centers <- data[sample(1:n, k), , drop = FALSE]
  labels <- rep(0, n)
  
  # ---- helper: squared Euclidean distance ----
  dist2 <- function(x, y) sum((x - y)^2)
  
  # ---- 3. Main loop ----
  for (iter in 1:maxIter) {
    
    prev_labels <- labels
    
    # ---- Assign each point to nearest allowable cluster ----
    for (i in 1:n) {
      best_k <- -1
      best_d <- Inf
      
      for (cidx in 1:k) {
        
        # must-link: if any ML friend is assigned to a *different* cluster, reject
        violated <- FALSE
        for (m in ML[[i]]) {
          if (prev_labels[m] != 0 && prev_labels[m] != cidx) {
            violated <- TRUE
            break
          }
        }
        if (violated) next
        
        # cannot-link: if any CL friend *already* in cluster, reject
        violated <- FALSE
        for (c in CL[[i]]) {
          if (prev_labels[c] == cidx) {
            violated <- TRUE
            break
          }
        }
        if (violated) next
        
        # distance check
        dval <- dist2(data[i,], centers[cidx,])
        if (dval < best_d) {
          best_d <- dval
          best_k <- cidx
        }
      }
      
      # If no feasible cluster (rare), assign to *closest cluster* ignoring constraints
      if (best_k == -1) {
        best_k <- which.min(colSums((t(centers) - data[i,])^2))
      }
      
      labels[i] <- best_k
    }
    
    # ---- 4. Update centers ----
    new_centers <- matrix(0, nrow = k, ncol = d)
    counts <- table(factor(labels, levels = 1:k))
    
    for (j in 1:k) {
      if (counts[j] > 0) {
        new_centers[j,] <- colMeans(data[labels == j, , drop = FALSE])
      } else {
        # reinitialize empty cluster with random point
        new_centers[j,] <- data[sample(1:n, 1),]
      }
    }
    
    # ---- convergence check ----
    shift <- sum((centers - new_centers)^2)
    centers <- new_centers
    if (shift < tol) break
  }
  
  return(labels)
}