library(lpSolve)

C <- c(9,12,8,7)

A <- matrix(c(1,0,0,0,
              1,1,0,0,
              1,1,1,0,
              1,1,1,1,
              1,0,0,0,
              0,1,0,0,
              0,0,1,0,
              0,0,0,1), byrow = TRUE, ncol = 4, nrow = 8)
b <- c(600, 1500, 2700, 4700, 1300, 1300, 1300, 1300)
constraints_direction <- c(">=", ">=",">=", "=", '<=', '<=', '<=', '<=')

mod <- lp(direction = "min", objective.in = C,
          const.mat = A, const.dir = constraints_direction, const.rhs = b,
          compute.sens = TRUE)

View(mod)

b <- c(600, 1500, 2700, 4800, 1300, 1300, 1300, 1300) #увеличение s4
mod <- lp(direction = "min", objective.in = C,
          const.mat = A, const.dir = constraints_direction, const.rhs = b,
          compute.sens = TRUE) 



b <- c(600, 1500, 2700, 4500, 1300, 1300, 1300, 1300)

b4 <- 0:1000
z <- NULL

for(i in 1:length(b4)) {
  b <- c(600, 1500, 2700, 4500 + b4[i], 1300, 1300, 1300, 1300)
  mod <- lp(direction = "min", objective.in = C,
            const.mat = A, const.dir = constraints_direction, const.rhs = b,
            compute.sens = TRUE)
  z[i] <- mod$objval - 9200
}

plot(b4, z, type = "l")
