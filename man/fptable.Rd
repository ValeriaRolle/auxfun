\name{fptable}
\alias{fptable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Nice tables for RMarkdown
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fptable(columna, nombre_var = "Variable")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{columna}{Column to make the table with, should be in the form of df$col.
%%     ~~Describe \code{x} here~~
}
\item{nombre_var}{Formal name of the variable (column), to use as title in the table.

}
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
\author{ Valeria Rolle
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
fptable(iris$Species)
fptable(iris$Species, var_name = "Species")
fptable(iris$Species, var_name = "Species", formato = "latex")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
