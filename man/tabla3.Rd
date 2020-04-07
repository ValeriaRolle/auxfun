\name{tabla3}
\alias{tabla3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Table 3
%%  ~~function to do ... ~~
}
\description{
I believe it only works properly for two groups right now (2020-01-21).

Creates two data frames (alocated in a list), one for continuous variables (with columns: variable, mean, sd, median and IQR for EACH level of the grouping variable and results from *t.test()* or *coin::wilcox_test()*, depeding on 'test' value), and another for categorical variables (with columns: n and percentage for each level of the grouping variable and fisher test (*fisher.test()*), p value.).
}
\usage{
table3(data, grupo, grupo_char, digits = 3, decimal.symbol = ",", test = c("median", "mean", "both"))


}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{data}{ Data to summarize, pay attention to column types.}
\item{grupo}{ Grouping variable given as column. }
\item{grupo_char}{ Grouping variable given as character string. }
\item{digits}{ Number of decimals to round. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned

A list with two data frames, first one containing the values for continuous variables and second one for categorical variables.
In the continuous data frame there is going to be parameteres responding to t.test(), or coin::wilcox_test(), or both, depeding on the value of 'test' argument.
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
result <- table3(mtcars, mtcars$am, "am")
result
writexl::write_xlsx(result, "exported_table3.xlsx")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
