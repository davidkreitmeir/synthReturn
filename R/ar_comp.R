NULL
###################################################################################
#' Function that computes AR for treated firm.
#'
#' @description \code{ar_comp} is used calculate the abnormal return for a treated firm. The abnormal return
#' is computed as the raw return of the treated firm minus the return of its synthetic match.
#'
#' @param data The name of the data.table that contains the data.
#'
#' @return A data.table containing the following columns:
#'  \item{tau}{Relative time to event date.}
#'  \item{ar}{Abnormal return.}
#'  \item{sigma}{"Goodness" of fit measure.}
#'

ar_comp <- function(data){

  out <- tryCatch({
    #-----------------------------------------------------------------------------
    # Pre-process data

    # Note: treated corporation has cid == 0
    data <- data.table::setorder(data, cid, t)
    # Grab "effective" length of windows
    estwindlen <- nrow(data[cid == 0 & estwind,])
    # eventwindlen <- nrow(data[cid == 0 & eventwind,])

    # Control firm returns during estimation window
    R_est <- data[(cid > 0 & estwind == 1), c("cid", "r", "t")]
    R_est <- data.table::dcast(R_est, t ~ cid, value.var = "r")
    R_est[, t := NULL]
    R_est <- as.matrix(R_est)
    # and estimation & event window
    R <- data[(cid > 0 & (estwind == 1 | eventwind == 1)), c("cid", "r", "t")]
    R <- data.table::dcast(R, t ~ cid, value.var = "r")
    R[, t := NULL]
    R <- as.matrix(R)

    # Returns of treated firm during estimation ...
    r_est <- data[(cid == 0 & estwind == 1), "r"][["r"]]

    # and estimation & event window
    r <- data[(cid == 0 & (estwind == 1 | eventwind == 1)), "r"][["r"]]

    #-----------------------------------------------------------------------------
    # Solve quadratic programming problem

    # Matrix appearing in the quadratic function to be minimized
    D <- t(R_est) %*% R_est
    # vector appearing in the quadratic function to be minimized
    d <- t(R_est) %*% r_est
    # matrix defining the constraints under which we want to minimize the quadratic function (LHS)
    # here: weights need to sum up to 1 and each weight has to be >= 0
    A <- cbind(rep(1, length(d)), diag(length(d)))
    # vector holding the values of RHS
    # here 1 (sum) for element and 0 otherwise (>=0)
    b0 <- c(1, numeric(length(d)))

    # Check if D is positive definite
    if(corpcor::is.positive.definite(D)) {
      # solve quadratic programming problem
      soln <- quadprog::solve.QP(D, d, A, b0, meq = 1)
      w1 <- soln$solution
    } else {
      # if matrix not positive definite resize the matrix using the algorithm of NJ Higham (1988)
      D <- corpcor::make.positive.definite(D)
      # solve quadratic programming problem
      soln <- quadprog::solve.QP(D, d, A, b0, meq = 1)
      w1 <- soln$solution
    }

    #-----------------------------------------------------------------------------
    # Compute abnormal returns and "goodness" of match

    # Compute synthetic control return series: R %*% w1
    m <- matrix(c(R %*% w1, r), nrow = length(r), ncol = 2L)
    colnames(m) <- c("synth", "treated")

    # compute abnormal returns
    ars <- data.table::as.data.table(m)
    ars[, ar := treated - synth]
    # compute sigma ("goodness" of match)
    ars[, sigma := sigmafun(ars[1:estwindlen, "ar"][["ar"]])]
    # add event window indicator
    ars[, row := 1:.N]
    ars[, eventwind := as.integer(row > estwindlen)]
    # add relative time
    ars[, tau := row - (estwindlen + 1L)]
    ars[, row := NULL]

    # keep only ARs during event window
    out <- ars[eventwind == 1L, c("tau", "ar", "sigma")]
    return(out)
  }, error = function(x) return(NULL))

  return(out)
}
