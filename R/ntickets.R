#' ntickets for project 1
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#' @importFrom graphics curve
#' @importFrom graphics hist
#' @importFrom graphics layout
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics polygon
#' @importFrom graphics text
#' @importFrom stats dnorm pnorm
#' @importFrom stats pnorm
#' @param N num of seats on plane
#' @param gamma prob plane will be truly overbooked
#' @param p prob of a 'show'
#'
#' @return something
#' @export
#'
#' @examples
#' @export
ntickets <- function(N = 400, gamma = 0.02, p = 0.95) {

  # Calculate number of tickets two ways
  # discrete case
  discrete <- function(n, N, gamma, p){
    1 - gamma - pbinom(N, n, p)
  }

  # continuous case
  cont <- function(n){
    1 - gamma - pnorm(N+.05, mean=n*p, sd=sqrt(n*p*(1-p)))
  }

  # Make and print named list
  obj <- function(N, gamma, p){
    qbinom(1 - gamma, N:(N*1.2), p)
  }

  # nd
  minimum <- function(N, gamma, p) {
    ind <- which(N == qbinom(1 - gamma, N:(N*1.1), p))
    c(N:(N*1.1))[ind]
  }

  n <- obj(N = N, gamma = gamma, p = p)

  nd <- minimum(N = N, gamma = gamma, p = p)

  # Plot obj vs n
  layout(matrix(1:2,ncol =1, nrow = 2, byrow = TRUE))
  plot(x = n,
       y = discrete(n = n, N = N, gamma = gamma, p = p),
       xlab = "n",
       ylab = "Objective",
       xlim = c(N, (N*1.1)),
       ylim = c(0, 1.0),
       pch = 20,
       bg = "blue",
       type = "b",
       lty = 1,
       main = "Objective Vs n to find optimal tickets sold (discrete)")

  abline(v = nd, col = "red", lwd = 2,)
  abline(h = 0, col = "red", lwd = 2)

  curve(cont(n),
        xname = "n",
        ylab = "Objective",
        xlim = c(N, (N*1.1)),
        ylim = c(0, 1.0),
        type = "l",
        lty = 1,
        main = "Objective Vs n to find optimal tickets sold (continuous)")


  abline(h = 0, col = "blue", lwd = 1)

  droot <- uniroot(cont, interval=c(N, N*1.3))

  abline(v = droot$root, col = "blue", lwd = 1)

  list(nd = nd, nc = droot$root, N = N, gamma = gamma, p = p)

}
