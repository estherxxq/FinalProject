library(ggplot2)

# Run all the functions in the file Functions.R before running this model

# A. Initializing parameters

# experimental variables
size <- 4 # number of Tadros in the group
goal.direct <- 1 # degree of goal-directedness, between 0 and 1
n.iteration <- 50 # numbers of iteration to run
Tadro <- c(1:size)

# basic parameters for each Tadro
v <- 0.8 # internal velocity of Tadro
Vmax <- 1.5 # maximum velocity of Tadro
Cd <- 1 # coefficient of drag; the greater this is, the more effect inertia has
a <- 0.8 # interaction range, repulsion grows exponentially when d is smaller than this
# this also as this increases the function grows steeper

# environment parameters
# position of the light source, set as (0,0)
light.x <- 0
light.y <- 0
# diameter of the pool
pool.diameter <- 10


# B. Main function

Run <- function(){
  
  # initialize data frame for data collection
  Time <- rep(0:n.iteration, each = size)
  tadro <- rep(c(1:size), n.iteration + 1)
  x <- numeric(length(Time))
  y <- numeric(length(Time))
  
  last.dx <- numeric(size)
  last.dy <- numeric(size)
  
  # initialize initial position (for iteration = 0)
  # One way is to randomize the positions
  # for(i in 1:size){
  #   x[i] <- runif(1, -5, 5)
  #   y[i] <- runif(1, -5, 5)
  # }
  # Another way is to let them start at the pool side
  start.point <- circleFun(c(0,0), pool.diameter, size * 10)
  
  for(i in 1:size){
    s <- i * 10 - 3
    x[i] <- start.point$xx[s]
    y[i] <- start.point$yy[s]
  }
  
  # x and y coordinate can be represent as a function of iteration and tadro number
  # the calculation is [size*iteration + tadro number]
  # or [size*t + i]
  # for example, data$x[5*3+2] the x coordinate for tadro 2 in iteration 3 when group size is 5
  
  # for each iteration
  for(t in 1:n.iteration){
    
    # for each individual Tadro
    for(i in Tadro){
      
      # Read the final coordinates of Tadro i after the last iteration
      current.x <- x[size*(t-1) + i]
      current.y <- y[size*(t-1) + i]
      
      # 1. Calculate attractive force
      distance.light <- light.distance(current.x, current.y) # calculate the distance between tadro and light
      # displacement due to attraction to light is a function of velocity and goal-directedness
      d.light <- calc.attract(distance.light, current.x, current.y)
      dx.light <- d.light[1]
      dy.light <- d.light[2]
      
      # if tadro is not goal directed, it moves randomly
      # alternatively, it moves d = v towards the light
      
      # 2. Calculate inertial force
      # a function of last final displacement, the drag coefficient, and the mass of tadro
      dx.inert = last.dx[i] * Cd # displacement on x-axis
      dy.inert = last.dy[i] * Cd # displacement on y-axis
      
      # 3. Calculate repulsive force
      dx.R <- numeric(size)
      dy.R <- numeric(size)
      
      # calculate the repulsive force from each tadro other than i
      for(j in Tadro[-i]){
        
        # read the coordinates of Tadro j
        other.x <- x[size * (t-1) + j]
        other.y <- y[size * (t-1) + j]
        
        # calculate the distance between the two tadro
        d <- distance(current.x, current.y, other.x, other.y)
        
        # the repulsion is a function of the distance and the repulsion coefficient a
        # when distance is far, repulsion is very small
        # as d decreases, repulsion increases exponentially
        repulsion <- exp(-d/a) * v
        
        # to get displacement on the x and y axes
        dx.R[j] <- (current.x - other.x) * (repulsion/d)
        dy.R[j] <- (current.y - other.y) * (repulsion/d)
        
      } # repulsion loop ends
      
      
      # sum to get total displacement due to repulsion
      dx.repulse <- sum(dx.R)
      dy.repulse <- sum(dy.R)
      
      # 4. Calculate final displacement
      # sum to get total displacement on x and y axes
      dx <- dx.light + dx.inert + dx.repulse
      dy <- dy.light + dy.inert + dy.repulse
      
      # calculate total displacement
      displacement <- sqrt(dx^2 + dy^2)
      
      # if displacement exceeds Vmax, then final displacement equals Vmax
      # calculate new final displacement on x and y axes
      # otherwise, final displacement = dx and dy
      d <- check.velocity(displacement)
      
      final.dx <- dx * d/displacement
      final.dy <- dy * d/displacement
      
      # 5. Update data
      # sum previous coordinate and displacement to get coordinates for new position
      x[size * t + i] <- current.x + final.dx
      y[size * t + i] <- current.y + final.dy
      
      # keep track of the displacement for inertial force calculation next time
      last.dx[i] <- final.dx
      last.dy[i] <- final.dy
      
    } # individual loop ends
    
  } # iteration loop ends
  
  Prev.x <- prev.generate(x)
  Prev.y <- prev.generate(y)
  
  data <- data.frame(Time, Tadro = tadro, x, y, Prev.x, Prev.y)
  return(data)
} # main loop ends

# C. Iterations

# check parameters; if not set as desired values, adjust at top of file
size
goal.direct
n.iteration

results <- Run()





# D. Graphic Representation of Data

# a coordinate graph with gridlines, a circle in the center as light, and path of each
# tadro in different colors.
# 
# note the stability coefficient
# 
# generate for different group size, goal.direct and other parameters
# 

# below code plots the path for the last 10 iteration
n2 <- (n.iteration+1)*size
n1 <- n2 - 10*size + 1
n4 <- n1 - 1
n3 <- n4 - size + 1

# laying out a pool on top of the graph
pool <- circleFun(c(0,0), pool.diameter, size * 10)
results$xx <- c(1:((n.iteration - 9)*size), pool$xx)
results$yy <- c(1:((n.iteration - 9)*size), pool$yy)

# plotting: point of the first iteration being plotted, and then segments with arrows 
plot.result <- ggplot(results[n1:n2,], aes(x = x, y = y)) +
  geom_point(data = results[n3:n4,], aes(x = x, y = y, color = factor(Tadro))) +
  geom_segment(aes(xend = Prev.x, yend = Prev.y, color = factor(Tadro)), 
               arrow = arrow(length = unit(0.15,"cm"), ends = "first")) +
  geom_path(aes(xx, yy))

# view plot
plot.result

# below code plots all iterations
plot.all.result <- ggplot(results, aes(x = x, y = y)) +
  geom_point(data = results[1:size,], aes(x = x, y = y, color = factor(Tadro))) +
  geom_segment(data = results, aes(xend = Prev.x, yend = Prev.y, color = factor(Tadro)), 
               arrow = arrow(length = unit(0.15,"cm"), ends = "first")) +
  geom_path(data = results[n1:n2,], aes(xx, yy))

plot.all.result

# E. Experimental Data Collection

# The Sg function is defined in a separate file and calculates Group Stability
Sg <- calc.sg(results)

max(Sg$Sg)
min(Sg$Sg)
# plot Sg by iteration
plot.Sg <- ggplot(data = Sg, aes(x = iteration, y = Sg)) +
  geom_point() +
  stat_smooth(method = 'lm', formula = y ~ x)

# test correlation
cor.test(Sg$iteration, Sg$Sg)


