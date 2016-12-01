# Functions

# Calculates distance between two points
distance <- function(Ax, Ay, Bx, By){
  d <- sqrt((Ax-Bx)^2 + (Ay-By)^2)
  return(d)
}


# Calculate distance between two points, given tadro number and iteration
tadro.distance <- function(a, b, t, x, y){
  t <- t
  Ax <- x[size*(t-1) + a]
  Ay <- y[size*(t-1) + a]
  Bx <- x[size*(t-1) + b]
  By <- y[size*(t-1) + b]
  d <- sqrt((Ax-Bx)^2 + (Ay-By)^2)
  return(d)
}

# Calculates distance between a tadro and the light source
light.distance <- function(x,y){
  d <- sqrt((x-light.x)^2 + (y-light.y)^2)
  return(d)
}

# Decide whether to be goal directed, and calculate the attractive force
calc.attract <- function(distance.light, current.x, current.y){
  g <- runif(1, 0, 1)
  if(g <= goal.direct){ # so that the bigger goal.direct is, the more likely this is true
    d.light <- v # displacement due to light = velocity
    dx.light <- (light.x - current.x) * (d.light/distance.light) # displacement on x-axis
    dy.light <- (light.y - current.y) * (d.light/distance.light) # displacement on y-axis
  } else {
    dx.light <- runif(1, -v, v) # otherwise the displacement is a random
    dy.light <- runif(1, -v, v) # randomly generate dx.light, and then calculate dy.light
    # normalize the value so d.random equals v
    v.ratio <- sqrt(dx.light^2 + dy.light^2)/v
    dx.light <- dx.light/v.ratio
    dy.light <- dy.light/v.ratio
  }
  d.light <- c(dx.light, dy.light) # return both values
  return(d.light)
}

# Normalize final displacement to not exceed Vmax
check.velocity <- function(d){
  if(d > Vmax){
    d <- Vmax
  } else {
    d <- d
  }
  return(d)
}


# Takes a column of values, and generate the value from last iteration in the same row in a new column
# This works on both x or y depending on what input it gets
prev.generate <- function(x){
  prev.x <- numeric(length(x))
  for(i in (size + 1):length(x)){
    prev.x[i] <- x[i-size]
  }
  return(prev.x)
}

# This marks half the distance, might make the graph looks a bit clearer
direction.mark.generate <- function(x){
  prev.x <- prev.generate(x)
  direction.mark <- prev.x + (x - prev.x)/2
  return(direction.mark)
}

# 
# a <- (n.iteration+1)*size-10*size
# b <- (n.iteration+1)*size
# 
# results <- ggplot(data[1001:1064,], aes(x = x, y = y, colour = factor(Tadro))) +
#   geom_point(data[1001:1008,], aes(x = x, y = y, colour = factor(Tadro))) +
#   geom_segment(aes(xend = PrevX, yend = PrevY, colour=factor(Tadro)), arrow = arrow(length = unit(0.15,"cm"), ends = "first"))
# 
# results
# 
# results <- ggplot(data[1009:1200,], aes(x = x, y = y, colour = factor(Tadro))) +
#   geom_point(data = data[1001:1008,], aes(x = x, y = y, colour = factor(Tadro))) +
#   # geom_path() +
#   #geom_segment(aes(xend = directionX, yend = directionY, colour=factor(Tadro)),arrow = arrow(length = unit(0.2,"cm")))
#   geom_segment(aes(xend = PrevX, yend = PrevY, colour=factor(Tadro)), arrow = arrow(length = unit(0.15,"cm"), ends = "first"))
# # xlim(-2, 2) +
# # ylim(-2, 2)
# #facet_wrap(~Time)
# 
# results