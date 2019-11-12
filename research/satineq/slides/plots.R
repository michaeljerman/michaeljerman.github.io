library(ggplot2)
library(ineq)

x <- c(2,5,9,20,30)
x1 <- c(3,7,12,40,55)
x2 <- c(2,6,9,20,29)
x3 <- c(3,5,9,20,29)
x4 <- c(2,10,9,15,30)
x5 <- c(10,10,10,10,30)
x6 <- c(2,6,9,19,30)


g <- sapply(
    list(x,x1,x2,x3,x4,x5,x6),
    function(i) Gini(i)
)

t <- sapply(
    list(x,x1,x2,x3,x4,x5,x6),
    Theil
)

plotLorenz <- function(x.in){
    x <- sort(x.in)
    if (min(x) > 0) {
        x <- c(0, x)
    }
    n <- length(x)
    total <- sum(x)
    x.cum <- cumsum(x)
    x.L <- x.cum/total
    p <- qplot((0:(n-1))/(n-1), x.L, geom="line",
               xlab="Percentile", ylab="Cumulative Share")
    p <- p + geom_point()
    p <- p + theme(
                 axis.line.x=element_line(color="black", size=1),
                 axis.line.y=element_line(color="black", size=1),
                 plot.margin = unit(c(.5,.5,.5,.5), "cm")
             )
    p <- p + geom_line(
                 data = data.frame(x=c(0,1), y=c(0,1)),
                 aes(x=x, y=y),
                 linetype=2
             )
    p <- p + expand_limits(x = 0, y = 0)
    p <- p + scale_x_continuous(expand = c(0, 0))
    p <- p + scale_y_continuous(expand = c(0, 0))
    return(p)
}

x1 <- runif(100)
x2 <- (runif(100)+1)
x1 <- x1[order(x1)]
x2 <- x2[order(x2)]
if (min(x1) > 0) {
    x1 <- c(0, x1)
}
if (min(x2) > 0) {
    x2 <- c(0, x2)
}
n1 <- length(x1)
n2 <- length(x2)
total1 <- sum(x1)
total2 <- sum(x2)
x1.cum <- cumsum(x1)
x2.cum <- cumsum(x2)
x1.L <- mean(x1)*x1.cum/total1
x2.L <- mean(x2)*x2.cum/total2
df.gg <- data.frame(
    x = rep((0:(n1-1))/(n1-1),2),
    y = c(x1.L, x2.L),
    Distribution = c(rep("x",n1), rep("x\'", n2))
)
p <- ggplot(df.gg, aes(x=x, y=y, color=Distribution))
p <- p + geom_line()                
p <- p + theme(
             axis.line.x=element_line(color="black", size=1),
             axis.line.y=element_line(color="black", size=1),
             plot.margin = unit(c(.5,.5,.5,.5), "cm")
         )
## p <- p + geom_abline(intercept=0, slope=1, linetype=2)
p <- p + expand_limits(x = 0, y = 0)
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + labs(x="Percentile", y="Mean * Cumulative Share")
p <- p + theme(legend.position="none")
p <- p + annotate("text", x = .6, y = .7, label = "Distribution A",
                  angle=45)
p <- p + annotate("text", x = .6, y = .25, label = "Distribution B",
                  angle=25)
p <- p + scale_color_manual(values=c("#000000", "#000000"))
p

ggsave("~/Dropbox/Teaching/399/exams/genlorenz1.png", p, height=4, width=4)


p <- plotLorenz(c(2,5,9,20,30))

p
ggsave("img/lorenz1.png", p, height=4, width=4)


png("img/utility.png", height=400, width=400) 
curve(log(x), xlim=c(0,100), ylim=c(0,6),
      xlab="Income",
      ylab="Utility")
dev.off()

xrib <- sort(abs(rnorm(100)))
n <- length(xrib)
total <- sum(xrib)
x.cum <- cumsum(xrib)
x.L <- x.cum/total
df.gg <- data.frame(
    x = (0:(n-1))/(n-1),
    y = x.L
)

p <- ggplot(df.gg, aes(x=x, y=y))
p <- p + geom_line()                
p <- p + theme(
             axis.line.x=element_line(color="black", size=1),
             axis.line.y=element_line(color="black", size=1),
             plot.margin = unit(c(.5,.5,.5,.5), "cm")
         )
p <- p + geom_abline(intercept=0, slope=1, linetype=2)
p <- p + expand_limits(x = 0, y = 0)
p <- p + scale_x_continuous(expand = c(0, 0))
p <- p + scale_y_continuous(expand = c(0, 0))
p <- p + labs(x="Percentile", y="Cumulative Share")
ggsave("img/lorenzGini1.png", p, height=4, width=4)
p <- p + geom_ribbon(aes(ymin=y, ymax=x), fill='blue', alpha=.5)
p <- p + annotate("text", x = .6, y = .45, label = "A")
ggsave("img/lorenzGini2.png", p, height=4, width=4)
p <- p + geom_ribbon(aes(ymin=0, ymax=y), fill='red', alpha=.5)
p <- p + annotate("text", x = .8, y = .25, label = "B")
ggsave("img/lorenzGini3.png", p, height=4, width=4)

x <- c(2,5,9,20,30)
x1 <- x*10

df.gg <- data.frame(
    x = rep(seq(1:6)/6,2),
    y = c(0,0.4,1.4,3.2,7.2,13.2,0,
          4,14,32,72,132),
    Distribution=c(rep("x",6), rep("x\'",6))
)
                   
p <- ggplot(df.gg, aes(x=x, y=y, color=Distribution))
p <- p + geom_line()                
p <- p + theme(
             axis.line.x=element_line(color="black", size=1),
             axis.line.y=element_line(color="black", size=1),
             plot.margin = unit(c(.5,.5,.5,.5), "cm")
         )
p <- p + labs(x="Percentile", y="(Cumulative Share) X (mean)")

ggsave("img/generalizedLorenz.png", p, height=4, width=5.5)

p <- ggplot() + geom_abline(intercept=0, slope=1, linetype=2)
p <- p + expand_limits(x = 0, y = 0)
p <- p + scale_x_continuous(expand = c(0, 0), limits=c(0,1), breaks=c(0,.2,.4,.6,.8,1), labels=c(0, 20, 40, 60, 80, 100))
p <- p + scale_y_continuous(expand = c(0, 0), limits=c(0,1), breaks=c(0,.2,.4,.6,.8,1))
p <- p + labs(x="Percentile", y="Cumulative Share")
p <- p + theme(axis.line.x = element_line(color="black", size = 2),
               axis.line.y = element_line(color="black", size = 2))
p <- p + theme(plot.margin = unit(c(1,1,1,1), "cm"))
p

ggsave("~/Dropbox/Teaching/399/exams/blanklorenz.png", p, height=4, width=4)

