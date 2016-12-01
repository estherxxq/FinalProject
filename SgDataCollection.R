
# E. Experimental Data Collection

# Calculate the group stability, Sg

# It returns mostly correct-looking values, but occassionally small negative values..why??

calc.sg <- function(results){
  
  x <- results[,3]
  y <- results[,4]
  
  Io <- numeric(n.iteration)
  Ii <- numeric(n.iteration)
  Sg <- numeric(n.iteration)
  
  for (t in 1:n.iteration-1) {
    
    # Step 1: calculate observed instability (Io)
    # which is the sum of the differences in distances between each tadro
    # a. calculate current distance between each tadro
    
    current.tadro.distance <- matrix(nrow = size, ncol = size)
    
    for(i in Tadro){
      A.x <- x[size*t + i]
      A.y <- y[size*t + i]
      for (j in Tadro) {
        B.x <- x[size*t + j]
        B.y <- y[size*t + j]
        current.tadro.distance[i,j] <- distance(A.x, A.y, B.x, B.y)
      }
    } # current distance calc loop ends
    
    
    # b. calculate distance between each tadro at the next iteration
    next.tadro.distance <- matrix(nrow = size, ncol = size)
    
    for(i in Tadro){
      a.x <- x[size*(t+1) + i]
      a.y <- y[size*(t+1) + i]
      for (j in Tadro) {
        b.x <- x[size*(t+1) + j]
        b.y <- y[size*(t+1) + j]
        next.tadro.distance[i,j] <- distance(a.x, a.y, b.x, b.y)
      }
    } # next distance calc loop ends
    
    # c. calculate their difference
    different.tadro.distance <- abs(current.tadro.distance - next.tadro.distance)
    # d. sum the differences
    Io[t] <- sum(different.tadro.distance)/2 
    # divide by 2 because everything was counted twice in the matrix
    
    
    # Step 2: Calculate Ii
    # which is the maximum instability possible for this iteration, based on their speed
    
    # a. compute the actual speed at which each robot traveled
    # this is basically the distance traveled from last position
    speed <- numeric(size)
    
    for (i in Tadro){
      current.x <- x[size*t + i]
      current.y <- y[size*t + i]
      next.x <- x[size*(t+1) + i]
      next.y <- y[size*(t+1) + i]
      speed[i] <- distance(current.x, current.y, next.x, next.y)
    }
    
    # b. find the location that leads to the smallest Id
    # Make hypothetical next positions, h.next, with current speed by towards the center of the group
    # first find the center of the group by averaging all the x and y coordinates of each tadro
    # generate an array of displacement to h.next, h.next.dx and dy
    # then calculate h.next.x and y
    current.group <- Tadro + size*t
    center.x <- mean(x[(current.group)])
    center.y <- mean(y[(current.group)])
    
    h.next.dx <- numeric(length(x))
    h.next.dy <- numeric(length(y))
    
    h.next.x <- numeric(length(x))
    h.next.y <- numeric(length(y))
    
    for(i in Tadro){
      current.x <- x[size*t + i]
      current.y <- y[size*t + i]
      distance.to.center <- distance(current.x, current.y, center.x, center.y)
      h.next.dx[size*t + i] <- (current.x - center.x) * speed[i]/distance.to.center
      h.next.dy[size*t + i] <- (current.y - center.y) * speed[i]/distance.to.center
      h.next.x[size*t + i] <- current.x + h.next.dx[size*t + i]
      h.next.y[size*t + i] <- current.y + h.next.dy[size*t + i]
    }
    
    # c. calculate the distance between each Tadro for this hypothetical situation
    hypothetical.next.tadro.distance <- matrix(nrow = size, ncol = size)
    
    for(i in Tadro){
      aa.x <- h.next.x[size*(t) + i]
      aa.y <- h.next.y[size*(t) + i]
      for (j in Tadro) {
        bb.x <- h.next.x[size*(t) + j]
        bb.y <- h.next.y[size*(t) + j]
        hypothetical.next.tadro.distance[i,j] <- distance(aa.x, aa.y, bb.x, bb.y)
      }
    } # hypothetical next distance calc loop ends
    
    # d. calculate the difference between current iteration and hypothetical next iteration
    h.different.tadro.distance <- abs(current.tadro.distance - hypothetical.next.tadro.distance)
    # d. sum the differences
    Ii[t] <- sum(h.different.tadro.distance)/2 
    # divide by 2 because everything was counted twice in the matrix
  
    # Step 3: Calculate Sg
    # take the ratio between Io and Ii, and subtract it from 1
    Sg[t] <- 1 - Io[t]/Ii[t]
    
  } # iteration loop ends
  
  data.Sg <- data.frame(iteration = c(1:n.iteration - 1), Io = Io, Ii = Ii, Sg = Sg)
  return(data.Sg)
} # Sg calculation function ends

