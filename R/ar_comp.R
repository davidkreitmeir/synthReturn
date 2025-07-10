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

ar_comp <- function(data_treat, data_control, estwind, eventwind) {

  #-----------------------------------------------------------------------------
  # Pre-process data

  # Note: treated corporation has unit_id == 0
  # Grab "effective" length of windows
  estwind_rows <- data_treat[tau %between% estwind, which = TRUE]
  eventwind_rows <- data_treat[tau %between% eventwind, which = TRUE]

  # Control firm returns during estimation & event window
  R <- data.table::dcast(data_control, tau ~ unit_id, value.var = "r")
  R[, tau := NULL]
  R <- as.matrix(R)
  # during estimation window
  R_est <- R[estwind_rows,]

  #-----------------------------------------------------------------------------
  # Solve quadratic programming problem

  # Matrix appearing in the quadratic function to be minimized
  D <- crossprod(R_est)
  # vector appearing in the quadratic function to be minimized
  d <- crossprod(R_est, data_treat[estwind_rows, "r"][["r"]])
  rm(R_est)
  # matrix defining the constraints under which we want to minimize the quadratic function (LHS)
  # Here: weights need to sum up to 1 and each weight has to be >= 0
  nd <- length(d)
  A <- cbind(rep.int(1, nd), diag(nd))
  # vector holding the values of RHS
  # here 1 (sum) for element and 0 otherwise (>=0)
  b0 <- c(1L, rep.int(0L, nd))

  # Check if D is positive definite
  if(!corpcor::is.positive.definite(D)) {
    # if matrix not positive definite resize the matrix using the algorithm of NJ Higham (1988)
    D <- tryCatch(corpcor::make.positive.definite(D), error = function(x) return(NULL))
    if(is.null(D)) {
      return(NULL)
    }
  }

  # Solve quadratic programming problem
  w1 <- tryCatch(quadprog::solve.QP(D, d, A, b0, meq = 1)[["solution"]], error = function(x) return(NULL))
  if(is.null(w1)) {
    return(NULL)
  }

  #-----------------------------------------------------------------------------
  # Compute abnormal returns and "goodness" of match

  # Compute synthetic control return series: R %*% w1
  # Compute abnormal returns
  ars <- data.table::data.table(ar = data_treat[["r"]] - as.vector(R %*% w1))
  rm(R, w1)
  # Compute sigma ("goodness" of match)
  ars[, sigma := sigmafun(ars[estwind_rows, "ar"][["ar"]])]
  # Add relative time
  ars[, tau := data_treat[["tau"]]]

  # Keep only ARs during event window
  out <- ars[eventwind_rows,]
  return(out)
}
