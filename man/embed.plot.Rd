\name{embed.plot}
\alias{embed.plot}
\title{Facilitate the embedding of plots within other plots}
\description{Embeds a plot into the already drawn plot}
\usage{
embed.plot(filename = NULL,height = 8, width = 8, resolution = 1600, 
main.plot = NULL,embedded.plot = NULL, xleft = 0, ybottom = 0, xright = 1, 
ytop = 1)
}
\arguments{
    \item{filename}{The filename to be outputted to}
    \item{height}{The height (in inches) of the output file}
    \item{width}{The width (in inches) of the output file}
    \item{resolution}{The resolution of the output file}
    \item{main.plot}{The plot in which you would like to be embedded upon}
    \item{embedded.plot}{The plot in which you would like to embed into the current plot}
    \item{xleft}{The left most x coordinate of where the plot will be embedded}
    \item{ybottom}{The bottom most y coordinate of where the plot will be embedded}
    \item{xright}{The right most x coordinate of where the plot will be embedded}
    \item{ytop}{The top most y coordinate of where the plot will be embedded}
}
\value{Embeds a plot into the already drawn plot}
\author{Jeff Green}
\examples{
set.seed(253647)
# create some temporary data
tmp.data <- data.frame(
    x = c(
        runif(n = 150, min =  0, max =  20),
        runif(n = 150, min = 40, max = 60),
        runif(n = 700, min =  0, max = 40)
        ),
    y = c(
        runif(n = 150, min =  0, max =  20),
        runif(n = 150, min = 40, max = 60),
        runif(n = 700, min =  0, max = 40)
        )
    );

plot1 <- create.densityplot(
    x = list(
        X = tmp.data$x,
        Y = tmp.data$y
        ),
    xlab.label = 'X Axis Title',
    ylab.label = 'Y Axis Title',
    xlimits = c(-50,150),
    ylimits = c(0,0.03),   
    xat = seq(-50,150,50),
    yat = seq(0,0.03,0.005),
    description = 'Image description goes here'
    );

plot2 <- create.densityplot(
    x = list(
        X = tmp.data$x,
        Y = tmp.data$y
        ),
xlab.label = '',
ylab.label = '',
xlimits = c(50,75),
ylimits = c(0,0.015),   
xat = seq(0,150,10),
yat = seq(0,0.015,0.005),
xaxis.tck = 0,
description = 'Image description goes here'
);

embed.plot(
    filename = "embed_plot_example.tiff",
    main.plot = plot1,
    embedded.plot = plot2,
    xleft = 0.6,
    ybottom =0.5,
    xright =0.97,
    ytop = 0.9,
    resolution = 50 # Lower resolution decreases file size
    );
}
