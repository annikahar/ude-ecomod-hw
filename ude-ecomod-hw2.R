## Logistic growth 
cont_log <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <- r * N * (1-(N/K))
  return(list(dN))
  })
}

parameters <- c(r = 1.7, K = 1000)
state <- c(N = 42)
times <- seq(0, 10, by = 0.01)

out <- deSolve::ode(y = state, times = times, func = cont_log, parms = parameters)
out.g <- as.data.frame(out)
ggplot2::ggplot(out.g,aes(time,N)) + geom_line()

parameters <- c(r = 2.7, K = 2500)
out <- deSolve::ode(y = state, times = times, func = cont_log, parms = parameters)
out.g <- as.data.frame(out)
ggplot2::ggplot(out.g,aes(time,N)) + geom_line()
