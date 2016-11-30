library(ggplot2)
library(gganimate)

# A. Initializing parameters

# experimental variables
size <- 6 # number of Tadros in the group
d.goal <- 1 # degree of goal-directedness, between 0 and 1
n.iteration <- 50 # numbers of iteration to run
Tadro <- c(1:size)

# basic parameters for each Tadro
v <- 1 # internal velocity of Tadro
Vmax <- 1.5 # maximum velocity of Tadro
Cd <- 0.5 # coefficient of drag
a <- 1 # repulsion constant

# position of the light source, set as (0,0)
light.x <- 0
light.y <- 0

# Functions

# Calculates distance between two points
distance <- function(Ax, Ay, Bx, By){
  d <- sqrt((Ax-Bx)^2 + (Ay-By)^2)
  return(d)
}

# Calculates distance between a tadro and the light source
light.distance <- function(x,y){
  d <- sqrt((x-light.x)^2 + (y-light.y)^2)
  return(d)
}

# Calculate the repulsion force on tadro A from tadro B, given the distance between
# calc.repulsion <- function(d){
#   c <- exp(-x/1) 
# }

# Decide whether to be goal directed, and calculate the attractive force
cal.attrac <- function(distance.light){
  goal <- runif(1, 0, 1)
  if(goal < d.goal){
    
  }
  
}

runif(1,0,1)

# Normalize final displacement to not exceed Vmax
check.velocity <- function(d){
  if(d > Vmax){
    d <- Vmax
  } else {
    d <- d
  }
  return(d)
}

# B. Main function

Run <- function(){
  
  # initialize data frame for data collection
  time <- rep(0:n.iteration, each = size)
  tadro <- rep(c(1:size), n.iteration + 1)
  x <- numeric(length(time))
  y <- numeric(length(time))
  
  last.dx <- numeric(size)
  last.dy <- numeric(size)
  # initialize initial position (for iteration = 0)
  for(i in 1:size){
    x[i] <- runif(1, -5, 5)
    y[i] <- runif(1, -5, 5)
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
      d.light <- 
      
      # if tadro is not goal directed, it moves randomly
      # the goal directedness is a lightlihood
      
      # alternatively, it could be something like generating a
      
      dx.light <- (light.x - current.x) * (d.light/distance.light) # displacement on x-axis
      dy.light <- (light.y - current.y) * (d.light/distance.light) # displacement on y-axis
      
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
        repulsion <- exp(-d/a)
        
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
  
  data  <- data.frame(Time = time, Tadro = tadro, x = x, y = y)
  return(data)
} # main loop ends

# C. Iterations

# check parameters; if not set as desired values, adjust at top of file
size
d.goal
n.iteration

data <- Run()

# * calculate the ideal group stability coeffcient and stuff

# D. Graphic Representation of Data

# a coordinate graph with gridlines, a circle in the center as light, and path of each
# tadro in different colors.
# 
# note the stability coefficient
# 
# generate for different group size, d.goal and other parameters
# 

data$prevX <- mapply(function(tadro, time){
  return(subset(data, Tadro=tadro, Time=(time-1))$x)
}, data$Tadro, data$Time)

data$prevY <- mapply(function(tadro, time){
  return(subset(data, Tadro=tadro, Time=(time-1))$y)
}, data$Tadro, data$Time)

results <- ggplot(data[1:36,], aes(x = x, y = y, color = factor(Tadro))) +
  geom_point() +
  geom_segment(aes(xend=prevX, yend=prevY))+
  xlim(-5, 5) +
  ylim(-5, 5) +
  facet_wrap(~Time)

results
gg_animate(results)
