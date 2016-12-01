# Model of the pool
# create:
#   x coordinate (integers right now but try to change in to continuous later?)
#   y coordinate
#   position of light
#   the light gradient at any given point, as a function of x and y value

x <- c(-5:5)
y <- c(-5:5)
light

circleFun <- function(center = c(0,0),diameter = 10, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

dat <- circleFun(c(0,0),2,npoints = 100)
#geom_path will do open circles, geom_polygon will do filled circles
ggplot(dat,aes(x,y)) + geom_path()


library(grid)
qplot(1:10, 1:10, geom="blank") +
  annotation_custom(grob=circleGrob(r=unit(1,"npc")), xmin=2, xmax=4, ymin=4, ymax=6)


dat = data.frame(x=runif(1), y=runif(1))
ggplot() + scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))+
  geom_point(aes(x=x, y=y), data=dat, size=50, shape=1, color="gold4")

ggplot() + geom_rect(aes(xmin=-1,ymin=-1,xmax=1,ymax=1), fill=NA) + coord_polar()

gg_circle <- function(r, xc, yc, color="black", fill=NA, ...) {
  x <- xc + r*cos(seq(0, pi, length.out=100))
  ymax <- yc + r*sin(seq(0, pi, length.out=100))
  ymin <- yc + r*sin(seq(0, -pi, length.out=100))
  annotate("ribbon", x=x, ymin=ymin, ymax=ymax, color=color, fill=fill, ...)
}
square <- ggplot(data.frame(x=0:1, y=0:1), aes(x=x, y=y))
square + gg_circle(r=0.25, xc=0.5, yc=0.5)
square + gg_circle(r=0.25, xc=0.5, yc=0.5, color="blue", fill="red", alpha=0.2)
