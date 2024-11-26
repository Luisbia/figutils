% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fig_to_excel.R
\name{fig_to_excel}
\alias{fig_to_excel}
\title{Export to excel a data frame}
\usage{
fig_to_excel(.data)
}
\arguments{
\item{.data}{the data.frame, data.table to be exported}
}
\value{
an excel file in a temporary folder
}
\description{
For closer inspection of small data frames this functions exports it to excel and opens the file.
}
\examples{

 mtcars \%>\%
 show_in_excel()
}
