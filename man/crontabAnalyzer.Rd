% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{crontabAnalyzer}
\alias{crontabAnalyzer}
\title{analyze the crontab to check when things are running}
\usage{
crontabAnalyzer(ccFolder = "..", hour = 24)
}
\arguments{
\item{ccFolder}{Either the commandcenter folder or the folder that contains all the repos, default = ".."}

\item{hour}{how many hours worth of logs should be analyzed? default = 24}
}
\value{
a plot showing when tasks ran
}
\description{
FUNCTION_DESCRIPTION
}
\details{
DETAILS
}
\examples{
\dontrun{
if(interactive()){
 #EXAMPLE1
 }
}
}
