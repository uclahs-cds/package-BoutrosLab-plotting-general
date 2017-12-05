# The BoutrosLab.plotting.general package is copyright (c) 2012 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

### FUNCTION TO CREATE DENDROGRAM ##################################################################
create.dendrogram <- function(
	x, clustering.method = 'diana', cluster.dimension = 'col', distance.method = 'correlation',
	cor.method = 'pearson', force.clustering = FALSE, same.as.matrix = FALSE
	) {


	if (same.as.matrix) {
		x <- t(apply(x, 2, rev));
		}
	# This function will create a dendrogram using either row-wise or column-wise clustering
	# It is called from the create.heatmap function when clustering is required and no dendrograms are provided
	# verify proper input formatting
	if (length(cluster.dimension) > 1) { stop('Only handles one cluster dimension at a time.'); }

	# initialize variable to store dendrogram
	dd <- 0;

	# initialize variable to store distance matrix
	distance.matrix <- 0;

	# Perform column-wise or row-wise clustering as requested
	if (cluster.dimension %in% c('col', 'column', 'cols', 'columns')) {

		# Don't bother trying to cluster if the dimension is too large
		if (dim(x)[2] > 6000 && !force.clustering) {
			stop('Unclusterable matrix: dim(data.cluster)[2] = ', dim(x)[2]);
			}

		# Create dissimilarity matrix
		if ('correlation' == distance.method) {
			distance.matrix <- as.dist(1 - cor(x, use = 'pairwise', method = cor.method));
			}
		else if (distance.method %in% c('euclidean', 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski', 'jaccard')) {
			distance.matrix <- dist(t(x), method = distance.method);
			}
		else {
			stop('Unknown distance.method: ', distance.method);
			}
		}

	else if (cluster.dimension %in% c('row', 'rows')) {

		# Don't bother trying to cluster if the dimension is too large
		if (dim(x)[1] > 6000 && !force.clustering) {
			stop('Unclusterable matrix: dim(data.cluster)[1] = ', dim(x)[1]);
			}

		# Create dissimilarity matrix
		if ('correlation' == distance.method) {
			distance.matrix <- as.dist(1 - cor(t(x), use = 'pairwise', method = cor.method));
			}
		else if (distance.method %in% c('euclidean', 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski', 'jaccard')) {
			distance.matrix <- dist(x, method = distance.method);
			}
		else {
			stop('Unknown distance.method: ', distance.method);
			}
		}
	else {
		# throw an error if we are asked to compute a dendrogram that is not by row or column
		stop('Unknown cluster.dimension for create.dendrogram: ', cluster.dimension);
		}

	# make sure this is a clusterable matrix
	if (any(is.na(distance.matrix))) {
		stop('Unclusterable matrix: some distances are NULL or NA.');
		}

	# now handle all possible clustering methods using the distance matrix
	if ('diana' == clustering.method) {
		dd <- as.dendrogram(as.hclust(diana(x = distance.matrix)));
		}
	else if (clustering.method %in% c('ward', 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median', 'centroid')) {
		dd <- as.dendrogram(hclust(d = distance.matrix, method = clustering.method));
		}
	else {
		stop('Unknown clustering method: ', clustering.method);
		}

	# Return the dendrogram created
	return(dd);
	}
