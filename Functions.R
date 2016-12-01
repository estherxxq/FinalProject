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

# This is a function to make a series of circles that I found on the internet
# source: http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circleFun <- function(center = c(0,0),diameter, npoints){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(xx = xx, yy = yy))
}

# dat <- circleFun(c(0,0), pool.diameter, size)
# #geom_path will do open circles, geom_polygon will do filled circles
# ggplot(dat,aes(x,y)) + geom_path()
