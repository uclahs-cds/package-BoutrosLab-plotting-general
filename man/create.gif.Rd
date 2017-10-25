\name{create.gif}
\alias{create.gif}
\title{Make a gif}
\description{Takes a function and several sets of parameters and makes a gif of their function calls}
\usage{
create.gif(
	execFunc,
	parameters,
	numberOfFrames, 
	delay = 40, 
	filename)
}
\arguments{
    \item{execFunc}{The function that will be used to make the plots for the gif}
    \item{parameters}{Parameter list to be sent to the exec func at each frame}
    \item{numberOfFrames}{Total number of frames to be made (must match number of parameter lists)}
    \item{delay}{Delay between each frame in the gif}
    \item{filename}{Name of output file  (must end in .gif)}
}
\author{Jeffrey Green}
\seealso{\code{\link[lattice]{stripplot}}, \code{\link[lattice]{lattice}} or the Lattice book for an overview of the package.}
\examples{
set.seed(223);

simple.data1 <- data.frame(
    x = sample(1:15, 10),
    y = LETTERS[1:10]
    );

simple.data2 <- data.frame(
    x = sample(1:15, 10),
    y = LETTERS[1:10]
    );

simple.data3 <- data.frame(
    x = sample(1:15, 10),
    y = LETTERS[1:10]
    );

p = list(
	list(formula = x ~ y,data = simple.data1, yat = seq(0,16,2)),
	list(formula = x ~ y,data = simple.data2, yat = seq(0,16,2)),
	list(formula = x ~ y,data = simple.data3, yat = seq(0,16,2))
	)

create.gif(
	execFunc = create.barplot, 
	parameters = p, 
	numberOfFrames = 3, 
	delay = 20, 
	filename='test.gif'
	) 

}
