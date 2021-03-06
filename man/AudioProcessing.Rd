% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AudioProcessing.R
\name{AudioProcessing}
\alias{AudioProcessing}
\title{AudioProcessing}
\usage{
AudioProcessing(d, rate = 256, thresh_right = c(stoptime = 2000,
  last_window = NULL), preprocess = "none", filtering = "lgf",
  output = "rmax", preParams = list(window = 1),
  filtParams = list(threshold = 3.5, roll_window = 30, robust = FALSE),
  outputParams = list(window = 1))
}
\arguments{
\item{d}{A two-column data.frame with the first column being time and the second being signal}

\item{rate}{Sampling rate for the signal}

\item{thresh_right}{Threshold to filter out signal from the end to remove artifacts like
removing device and the like, that can bias results. There are two options:
1. stoptime: Time (in seconds) from the end when we stop
2. last_window: which is the last observation (index) to keep (is this good?)}

\item{preprocess}{Options are 'ma' (Moving average) and 'none'}

\item{filtering}{Options are 'lgf' (local Gaussian Filtering), 'gf' (Gaussian Filtering),
'mgf' (Mixed Gaussian Filtering, not implemented), and 'none'}

\item{output}{Options are 'rmean' (running mean), 'rmedian' (running median),
'apnea', and 'none'}

\item{preParams}{Parameters for the chosen preprocess method, as a list}

\item{filtParams}{Parameters for the chosen filtering method, as a list}

\item{outputParams}{Parameters for the chosen output method, as a list}
}
\description{
Signal processing the zdevice audio signal
}
\details{
This function allows the modular implementation of different pre-processing, filtering
and summarization steps in processing zdevice audio signal.
There are several choices for each of these
steps already implemented.

\strong{Pre-processing}
\enumerate{
\item "none" : No pre-processing
\item "ma" : Run a moving average on the raw signal
\itemize{
\item "window" : The width of the moving average window in seconds
}
}

\strong{Filtering}
\enumerate{
\item "none": No filtering
\item "gf" : Gaussian filtering
\itemize{
\item "threshold": The threshold value beyond which we consider the filtered values to be signal
\item "robust": Use \code{median} and \code{mad} to scale the data instead of \code{mean} and \code{sd}
}
\item "lgf": Local Gaussian Filtering (Default)
\itemize{
\item "threshold": The threshold value beyond which we consider the filtered values to be signal
\item "window" : The window for computing the local mean and sd for scaling the data, in seconds
\item "robust": \emph{not implemented}
}
}

\strong{Summarizing}
\enumerate{
\item "none" : no summarizing
\item "rmax" : Moving maximum (Default)
\itemize{
\item "window": Window of the moving average in seconds
}
\item "rmean" : Moving average
\itemize{
\item "window": Window of moving average in seconds
}
\item "apnea" : The apnea detection methodology (not implemented)
}
}
\examples{

}
