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

### FUNCTION TO CREATE CORRELATION KEY ############################################################
get.corr.key <- function(
	x, y, label.items = c('spearman', 'spearman.p'), x.pos = 0.03, y.pos = 0.97, key.corner = NULL,
	key.cex = 1, key.title = NULL, title.cex = 1, alpha.background = 0, num.decimals = 2,
	border = 'white') {

	# use 'all' as an alternative to all the label items
	if (label.items[1] == 'all') {
		label.items <- c('spearman', 'pearson', 'kendall', 'beta0', 'beta1', 'spearman.p', 'pearson.p', 'kendall.p', 'beta1.p');
		}

	# define the corner of the key used for the x.pos and y.pos
	if (is.null(key.corner)) {
		if (x.pos < .5 & y.pos < .5)	{ corner <- c(0, 0); }
		if (x.pos < .5 & y.pos >= .5)	{ corner <- c(0, 1); }
		if (x.pos >= .5 & y.pos < .5)	{ corner <- c(1, 0); }
		if (x.pos >= .5 & y.pos >= .5)	{ corner <- c(1, 1); }
		}

	# define some variables
	key.list <- NULL;

	# loop through each item label and add it to the key
	for (i in label.items) {

		# define the symbol
		if (grepl('pearson', i))	{ corr.symbol <- quote(R); }
		if (grepl('spearman', i))	{ corr.symbol <- quote(rho); }
		if (grepl('kendall', i))	{ corr.symbol <- quote(tau); }
		if (grepl('eta', i))		{ corr.symbol <- quote(Beta); }

		# calculate the correlation values
		if (i %in% c('spearman', 'pearson', 'kendall')) {
			key.item <- substitute(
				corr.symbol == paste(correlation, phantom('|')[phantom('|')], phantom('|') ^ phantom('|')),
				env = list(
					correlation = format(
						round(
							BoutrosLab.plotting.general::get.correlation.p.and.corr(
								x = x,
								y = y,
								method = i
								)[1],
							digits = num.decimals
							),
						nsmall = num.decimals
						),
					corr.symbol = corr.symbol
					)
				);
			}

		# calculate beta0 i.e. intercept
		if (i == 'beta0') {
			key.item <- substitute(
				Beta[0] == paste(beta.effect, phantom('|')[phantom('|')], phantom('|') ^ phantom('|')),
				env = list(
					beta.effect = format(round(coef(lm(y ~ x))[1], num.decimals), nsmall = num.decimals)
					)
				);
			}

		# calculate beta1 i.e. slope
		if (i == 'beta1') {
			key.item <- substitute(
				Beta[1] == paste(beta.effect, phantom('|')[phantom('|')], phantom('|') ^ phantom('|')),
				env = list(
					beta.effect = format(round(coef(lm(y ~ x))[2], num.decimals), nsmall = num.decimals)
					)
				);
			}

		# calculate robust beta0 i.e. intercept
		if (i == 'beta0.robust') {
			key.item <- substitute(
				Beta[paste(0, ',rob')] == paste(beta.effect, phantom('|')[phantom('|')], phantom('|') ^ phantom('|')),
				env = list(
					beta.effect = format(round(coef(rlm(y ~ x))[1], num.decimals), nsmall = num.decimals)
					)
				);
			}

		# calculate robust beta1 i.e. slope
		if (i == 'beta1.robust') {
			key.item <- substitute(
				Beta[paste(1, ',rob')] == paste(beta.effect, phantom('|')[phantom('|')], phantom('|') ^ phantom('|')),
				env = list(
					beta.effect = format(round(coef(rlm(y ~ x))[2],  num.decimals), nsmall = num.decimals)
					)
				);
			}

		# calculate the correlation pvalues
		if (i %in% c('spearman.p', 'pearson.p', 'kendall.p')) {
			if (BoutrosLab.plotting.general::get.correlation.p.and.corr(x = x, y = y, method = gsub('.p', '', i, fixed = TRUE))[2] > 0) {
				key.item <- substitute(
					P[corr.symbol] == paste(base %*% 10 ^ exponent, phantom('|')[phantom('|')] ),
					env = list(
						base = unlist(
							BoutrosLab.plotting.general::scientific.notation(
								x = BoutrosLab.plotting.general::get.correlation.p.and.corr(
									x = x,
									y = y,
									method = gsub('.p', '', i, fixed = TRUE)
									)[2],
								digits = 2,
								type = 'list'
								)[1]
							),
						exponent = unlist(
							BoutrosLab.plotting.general::scientific.notation(
								x = BoutrosLab.plotting.general::get.correlation.p.and.corr(
									x = x,
									y = y,
									method = gsub('.p', '', i, fixed = TRUE)
									)[2],
								digits = 2,
								type = 'list'
								)[2]
							),
						corr.symbol = corr.symbol
						)
					);
				}

			# if the p.value is 0 set it the minimum value
			else {
				key.item <- expression(paste('P < 2.2 x', 10 ^ -16, phantom('|')[phantom('|')]));
				}
			}

		# calculate the wald test for beta
		if (i == 'beta1.p') {
			if (coef(summary(lm(y ~ x)))[2, 4] > 0) {
				key.item <- substitute(
					P[B[1]] == paste(base %*% 10 ^ exponent, phantom('|')[phantom('|')]),
					env = list(
						base = unlist(BoutrosLab.plotting.general::scientific.notation(
								x = coef(summary(lm(y ~ x)))[2, 4],
								digits = 2,
								type = 'list'
								)[1]),
						exponent = unlist(BoutrosLab.plotting.general::scientific.notation(
								x = coef(summary(lm(y ~ x)))[2, 4],
								digits = 2,
								type = 'list'
								)[2])
						)
					);
				}

			# if the p.value is 0 set it the minimum value
			else {
				key.item <- expression(paste('P < 2.2 x', 10 ^ -16, phantom('|')[phantom('|')]));
				}
			}

		# calculate the wald test for beta
		if (i == 'beta1.robust.p') {
			if (coef(summary(lm(y ~ x)))[2, 4] > 0) {
				key.item <- substitute(
					P[B[paste(1, ',rob')]] == paste(base %*% 10 ^ exponent, phantom('|')[phantom('|')]),
					env = list(
						base = unlist(BoutrosLab.plotting.general::scientific.notation(
								x = 2 * pt(coef(summary(rlm(y ~ x)))[2, 3],
									df = summary(rlm(y ~ x))$df[2],
									lower.tail = FALSE),
								digits = 2,
								type = 'list'
								)[1]),
						exponent = unlist(BoutrosLab.plotting.general::scientific.notation(
								x = 2 * pt(coef(summary(rlm(y ~ x)))[2, 3],
									df = summary(rlm(y ~ x))$df[2],
									lower.tail = FALSE),
								digits = 2,
								type = 'list'
								)[2])
						)
					);
				}

			# if the p.value is 0 set it the minimum value
			else {
				key.item <- expression(paste(P[phantom(0)], '< 2.2 x', 10 ^ -16, phantom('|')[phantom('|')] ));
				}
			}

		# add each item to the vector of labels
		key.list <- c(key.list, as.expression(key.item));
		}

	# define the key
	key <- list(
		text = list(
			lab = key.list,
			cex = key.cex,
			fontface = 'bold'
			),
		padding.text = 0,
		x = x.pos,
		y = y.pos,
		corner = corner,
		title = key.title,
		cex.title = title.cex,
		background = 'white',
		alpha.background = alpha.background,
		border = border
		);

	return(key);
	}
