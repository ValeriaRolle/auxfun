\name{tabla2}
\alias{tabla2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Table 2
%%  ~~function to do ... ~~
}
\description{
Creates two data frames (alocated in a list), one for continuous variables (with columns: variable, mean, sd, median and IQR), and another for categorical variables (with columns: n and percentage).
}
\usage{
table2(data, digits = 2)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ Data to summarize, pay attention to column types.
%%     ~~Describe \code{x} here~~
}\item{digits}{ Number of decimals to round. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{


A list with two data frames, first one containing the values for continuous variables and second one for categorical variables.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
result <- table2(mtcars)
result
writexl::write_xlsx(result, "exported_table2.xlsx")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
