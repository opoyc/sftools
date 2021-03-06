% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_db.R
\name{load_db}
\alias{load_db}
\title{Get Sanofi product descriptors}
\usage{
load_db(db, default_path = TRUE)
}
\arguments{
\item{db}{string. For now "GBU" and "LSD" are available.}

\item{default_path}{logical = TRUE, if a path is provided replace the default path.}
}
\value{
tibble
}
\description{
Databases of Global Business Unit and Life Savings Drugs
}
\examples{
\dontrun{
load_db(db = "GBU", on_globalenv = T)
}
}
\author{
Obryan Poyser
}
