# specify whether the interaction is mutualistic or antagonistic
if(as.character(substr(sim_nam, 7, 7)) == "F"){
  sign_host <- 1
  sign_non <- 1
}else if(as.character(substr(sim_nam, 7, 7)) == "M"){
  sign_host <- 1
  sign_non <- 1
}else if(as.character(substr(sim_nam, 7, 7)) == "A"){
  sign_host <- -1
  sign_non <- 1
}

# specify whether the host's fitness is decided by trait matching or trait differences
if(as.character(substr(sim_nam, 8, 8)) == "F"){ # by default, change in fitness under trait matching
  fit_func_host <- function(zi, zj, xi = xi_host, alpha = 1){
    temp = (zi-zj)^2 * (-alpha)
    return(sign_host * xi * exp(temp))
  }
}else if(as.character(substr(sim_nam, 8, 8)) == "M"){ # change in fitness under trait matching
  fit_func_host <- function(zi, zj, xi = xi_host, alpha = 1){
    temp = (zi-zj)^2 * (-alpha)
    return(sign_host * xi * exp(temp))
  }
}else if(as.character(substr(sim_nam, 8, 8)) == "D"){ # change in fitness under trait differences
  fit_func_host <- function(zi, zj, xi = xi_host, alpha = 1){
    temp = (zi-zj) * (-alpha)
    return(sign_host * xi / (1 + exp(temp)))
  }
}

# specify whether the non-host's fitness is decided by trait matching or trait differences
if(as.character(substr(sim_nam, 9, 9)) == "F"){ # by default, change in fitness under trait matching
  fit_func_non <- function(zi, zj, xi = xi_non, alpha = 1){
    temp = (zi-zj)^2 * (-alpha)
    return(sign_non * xi * exp(temp))
  }
}else if(as.character(substr(sim_nam, 9, 9)) == "M"){ # change in fitness under trait matching
  fit_func_non <- function(zi, zj, xi = xi_non, alpha = 1){
    temp = (zi-zj)^2 * (-alpha)
    return(sign_non * xi * exp(temp))
  }
}else if(as.character(substr(sim_nam, 9, 9)) == "D"){ # change in fitness under trait differences
  fit_func_non <- function(zi, zj, xi = xi_non, alpha = 1){
    temp = (zi-zj) * (-alpha)
    return(sign_non * xi / (1 + exp(temp)))
  }
}

 # # visualizing the shape of the fitness function
 # x <- y <- seq(0, 1, length= 50)
 # z <- outer(x, y, fit_func_host)
 # # z <- outer(x, y, fit_func_non)
 # 
 # persp(x, y, z,
 #       main="Fitness function",
 #       zlab = "Height",
 #       theta = 30, phi = 30,
 #       shade = 0.5)
