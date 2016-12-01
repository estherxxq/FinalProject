
# E. Experimental Data Collection

# Calculate the group stability, Sg

calc.sg <- function(data){
  
  x <- data[,3]
  y <- data[,4]
  
  Io <- numeric(n.iteration)
  Ii <- numeric(n.iteration)
  Sg <- numeric(n.iteration)
  
  for (t in 1:n.iteration) {
    
    # Step 1: calculate observed instability (Io)
    # which is the sum of the differences in distances between each tadro
    # 1. calculate current distance between each tadro
    
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
    
    
    # 2. calculate distance between each tadro at the last iteration
    last.tadro.distance <- matrix(nrow = size, ncol = size)
    
    for(i in Tadro){
      a.x <- x[size*(t-1) + i]
      a.y <- y[size*(t-1) + i]
      for (j in Tadro) {
        b.x <- x[size*(t-1) + j]
        b.y <- y[size*(t-1) + j]
        last.tadro.distance[i,j] <- distance(a.x, a.y, b.x, b.y)
      }
    } # last distance calc loop ends
    
    # 3. calculate their difference
    different.tadro.distance <- abs(current.tadro.distance - last.tadro.distance)
    # 4. sum the differences
    Io[t] <- sum(different.tadro.distance)/2 
    # divide by 2 because everything was counted twice in the matrix
    
    
    # Step 2: Calculate Ii
    # which is the maximum instability possible for this iteration, based on their speed
    # 1. compute the actual speed at which each robot traveled
    # this is basically the distance traveled from last position
    speed <- numeric(size)
    
    for (i in Tadro){
      current.x <- x[size*t + i]
      current.y <- y[size*t + i]
      last.x <- x[size*(t-1) + i]
      last.y <- y[size*(t-1) + i]
      speed[i] <- distance(current.x, current.y, last.x, last.y)
    }
    
    # 2. find the location that leads to the smallest Id
    # (need to ask Josh about this)
    #Ii[t] <- Io[t]
    # right now I'm just getting the total speed and using it to adjust the Io
    iteration.speed <- sum(speed)
    Ii[t] <- iteration.speed
    
    # Step 3: Calculate Sg
    # take the ratio between Io and Ii, and subtract it from 1
    Sg[t] <- 1/(Io[t]/Ii[t])
    
    
  } # iteration loop ends
  
  data.Sg <- data.frame(iteration = c(1:n.iteration), Io = Io, Ii = Ii, Sg = Sg)
  return(data.Sg)
} # Sg calculation function ends

Sg <- calc.sg(results)

# plot Sg by iteration
plot(Sg$iteration, Sg$Sg)
# test correlation
cor.test(Sg$iteration, Sg$Sg)

