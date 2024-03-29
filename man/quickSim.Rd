% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quickSim.R
\name{quickSim}
\alias{quickSim}
\title{Quick simulation of a data.frame for demonstration purposes.}
\usage{
quickSim(
  n = 1000,
  intercept = 1,
  coefs = c(2, 3),
  errMean = 30,
  errSD = 3,
  seed = 1,
  type = "continuous"
)
}
\arguments{
\item{n}{Sample size, defaults to 1000.}

\item{intercept}{Intercept of a simulated model output, defaults to 1.}

\item{coefs}{Regression coefficients of a simulated model output, defaults to two predictors with coefficients 2 and 3, respectively.}

\item{errMean}{Mean prediction error, present in the simulated data, defaults to 30 (will be ignored, if function argument 'type' (see below) is set to 'binary').}

\item{errSD}{Standard deviation of the error, present in the simulated data, defaults to 3 (will be ignored, if function argument 'type' (see below) is set to 'binary').}

\item{seed}{A single integer value. Setting a seed ensures reproducibility of a once simulated data set.}

\item{type}{A single character value, either 'continuous' or 'binary', depending on what scale the simulated outcome shall have.}
}
\value{
simDf A data.frame with one outcome column y, and as many predictor columns (named: x1, x2, …) as the user selected (default: 2). See \strong{Details}.
}
\description{
Quick simulation of a data.frame, either with a continuous or with a binary outcome. This is merely to enable showcasing the main purpose of the predictMe package.
}
\details{
The returned simulated data set will have as many predictors, as the user entered regression coefficients to the function argument 'coefs'. For instance, coefs = c(.5, -2, -.9) will result in three predictors x1, x2, and x3 in the returned data set.

The simulated data set is merely serving the need to provide the main functions of this package with the data they require (demonstration purpose; several simulation packages exist in R).
}
\examples{
# Simulate data set with continuous outcome (use all default values)
dfContinuous <- quickSim()
# Simulate data set with continuous outcome (set sample size to 149)
dfContinuous <- quickSim(n = 149)
nrow(dfContinuous) # 149
# Simulate data set with binary outcome (set sample size to 100, and
# coefficients to 3, 1, and -2.5)
}
\references{
Simulation code inside this function was largely taken from \href{https://stats.stackexchange.com/questions/46523/how-to-simulate-artificial-data-for-logistic-regression/46525}{Stéphane Laurent's} answer on StackExchange.
}
\author{
Marcel Miché
}
