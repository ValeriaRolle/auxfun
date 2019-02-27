\name{ssummary}
\alias{ssummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Formated summary
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The exact output of summary but formatted to markdown.
}
\usage{
ssummary(columna, var_name = "Variable", decimals = 3, formato = "markdown")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{columna}{ Column to make the summary.
%%     ~~Describe \code{x} here~~
}\item{var_name}{ Name to appear on the output table.
}\item{decimals}{ Number of decimals in the tale.}
\item{formato}{ Argument to pass to kable function.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
ssummary(mtcars$mpg)
ssummary(mtcars$wt, var_name = "Wt")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
