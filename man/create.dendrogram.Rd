\name{create.dendrogram}
\alias{create.dendrogram}
\title{Generate a dendrogram}
\description{Takes a matrix and creates a row-wise or column-wise dendrogram}
\usage{
create.dendrogram(
	x,
	clustering.method = 'diana',
	cluster.dimension = 'col',
	distance.method = 'correlation',
	cor.method = 'pearson',
	force.clustering = FALSE,
	same.as.matrix = FALSE
	);
}
\arguments{
    \item{x}{A matrix that is used to create the dendrogram}
    \item{clustering.method}{Method used to cluster the records (can not be none). Accepts all agglomerative clustering methods available in hclust, plus \dQuote{diana} (which is divisive).}
    \item{cluster.dimension}{Should clustering be performed on the rows or columns of x?}
    \item{distance.method}{Method name of the distance measure to be used for clustering. Defaults to \dQuote{correlation}. Other supported methods are same as in ?dist. Also supports \dQuote{jaccard} which is useful for clustering categorical variables.}
    \item{cor.method}{The method used for calculating correlation. Defaults to \dQuote{pearson}}
    \item{force.clustering}{Binary to over-ride the control that prevents clustering of too-large matrices}
    \item{same.as.matrix}{Prevents the flipping of the matrix that the function normally does}
    }
\value{Returns an object of the dendrogram class corresponding to the row-wise or column-wise dendrogram for x}
\author{Lauren Chong}
\examples{
# create temp data
x <- outer(-5:5, -5:5, '*') + matrix(nrow = 11, ncol = 11, data = runif(11 * 11));
colnames(x) <- paste('col', 1:11, sep = '-');
rownames(x) <- paste('row', 1:11, sep = '-');

# example of generating a column-wise dendrogram using default values
create.dendrogram(
    x = x
    );

# example of generating a column-wise dendrogram using different distance and clustering methods
create.dendrogram(
    x = x,
    clustering.method = 'median',
    cluster.dimension = 'cols',
    distance.method = 'euclidean'
    );

# generate row-wise dendrogram using default distance and clustering methods
create.dendrogram(
    x = x,
    cluster.dimension = 'row'
    );

# generate row-wise dendrogram using different distance and clustering methods
create.dendrogram(
    x = x,
    clustering.method = 'ward',
    cluster.dimension = 'rows',
    distance.method = 'manhattan'
    );
}
\keyword{cluster}
