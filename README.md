# BoutrosLab.plotting.general

1. [Description](#description)
2. [Included plots](#included-plots)
3. [Installation](#installation)
4. [Function reference](#function-reference)
5. [Resources](#resources)
6. [Getting help](#getting-help)
7. [Contributors](#contributors)
8. [Citation information](#citation-information)

## Description

Contains several plotting functions such as barplots, scatterplots, heatmaps, as well as functions to combine plots and assist in the creation of these plots. 
These functions will give users great ease of use and customization options in broad use for biomedical applications, as well as general purpose plotting. 
Each of the functions also provides valid default settings to make plotting data more efficient and producing high quality plots with standard colour schemes simpler. 
All functions within this package are capable of producing plots that are of the quality to be presented in scientific publications and journals. 
P'ng et al.; [BPG: Seamless, automated and interactive visualization of scientific data](doi:10.1186/s12859-019-2610-2); BMC Bioinformatics 2019.

## Included plots
![](https://media.springernature.com/full/springer-static/image/art%3A10.1186%2Fs12859-019-2610-2/MediaObjects/12859_2019_2610_Fig1_HTML.png?as=webp)

Available chart-types. The basic chart-types available in BPG: **a** density plot, **b** boxplot, **c** violin plot, **d** segplot, **e** strip plot, **f** barplot, **g** scatterplot, **h** histogram, **i** qqplot fit, **j** qqplot comparison, **k** Manhattan plot, **l** polygon plot, **m** heatmap, **n** dotmap and **o** hexbinplot.

![](https://media.springernature.com/full/springer-static/image/art%3A10.1186%2Fs12859-019-2610-2/MediaObjects/12859_2019_2610_Fig2_HTML.png?as=webp)

Multiplot example. BPG's create.multiplot function is able to combine multiple chart-types together into a single figure as shown above.

## Installation
To install the latest public release of BoutrosLab.plotting.general from CRAN:

```
install.packages("Boutros.plotting.general")
```

Or to install the latest development version from Github:
```
# install.packages("devtools")

devtools::install_github("uclahs-cds/public-R-BoutrosLab-plotting-general")
```
## Function reference
| **Functions** | **Description** |
|-----------|---------|
|[`BoutrosLab.plotting.general-package`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/BoutrosLab.plotting.general-package.html) [`BoutrosLab.plotting.general`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/BoutrosLab.plotting.general-package.html)|Boutros Lab general plotting functions|
|[`CNA`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/CNA.html)|Copy number aberration (CNA) data from colon cancer patients|
|[`SNV`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/SNV.html)|Single nucleotide variant (SNV) data from colon cancer patients|
|[`auto.axis()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/auto.axis.html)|Create ideal labels and values for a given dataset (detects log scales)|
|[`colour.gradient()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/colour.gradient.html)|Creates a colour gradient|
|[`covariates.grob()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/covariates.grob.html)|Create one or more covariate bars|
|[`create.barplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.barplot.html)|Make a barplot|
|[`create.boxplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.boxplot.html)|Make a boxplot|
|[`create.colourkey()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.colourkey.html)|Create Colourkey|
|[`create.dendrogram()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.dendrogram.html)|Generate a dendrogram|
|[`create.densityplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.densityplot.html)|Make a density plot|
|[`create.dotmap()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.dotmap.html)|Make a dotmap with coloured background|
|[`create.gif()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.gif.html)|Make a gif|
[`create.heatmap()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.heatmap.html)|Make a heatmap|
|[`create.hexbinplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.hexbinplot.html)|Make a hexagonally binned plot|
|[`create.histogram()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.histogram.html)|Make a histogram|
|[`create.lollipopplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.lollipopplot.html)|Make a lollipopplot|
|[`create.manhattanplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.manhattanplot.html)|Make a Manhattan plot|
|[`create.multipanelplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.multipanelplot.html)|Joins plots together|
|[`create.multiplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.multiplot.html)|Joins plots together|
|[`create.polygonplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.polygonplot.html)|Make a polygonplot|
|[`create.qqplot.comparison()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.qqplot.comparison.html)|Make a quantile-quantile plot of two samples|
|[`create.qqplot.fit()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.qqplot.fit.html)|Make a quantile-quantile plot of a sample|
|[`create.qqplot.fit.confidence.interval()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.qqplot.fit.confidence.interval.html)|Create the confidence bands for a one-sample qq plot|
|[`create.scatterplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.scatterplot.html)|Make a scatterplot|
|[`create.segplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.segplot.html)|Make a segplot|
|[`create.stripplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.stripplot.html)|Make a strip-plot|
|[`create.violinplot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/create.violinplot.html)|Make a violin plot|
|[`critical.value.ks.test()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/critical.value.ks.test.html)|Critical Value for Kolmogorov-Smirnov Test|
|[`default.colours()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/default.colours.html)|Provides default colour schemes.|
|[`display.colours()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/display.colours.html)|Function to display R colors, as well as corresponding R grey colours.|
|[`display.statistical.result()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/display.statistical.result.html)|Utility function to display statistical result in a plot|
|[`dist()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/dist.html)|Distance Matrix Computation|
|[`force.colour.scheme()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/force.colour.scheme.html)|Based on predefined colour schemes, returns a vector of corresponding colours.|
|[`generate.at.final()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/generate.at.final.html)|Generates alternative default tick mark locations for create.densityplot() and create.|scatterplot()
|[`get.corr.key()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/get.corr.key.html)|Correlation Key|
|[`get.correlation.p.and.corr()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/get.correlation.p.and.corr.html)|Calculate a correlation and its statistical significance|
|[`get.defaults()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/get.defaults.html)|Get operating system specific default properties|
|[`get.line.breaks()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/get.line.breaks.html)|Get line breaks|
|[`legend.grob()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/legend.grob.html)|Generate a legend grob|
|[`microarray`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/microarray.html)|Microarray dataset of colon cancer patients|
|[`panel.BL.bwplot`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/panel.BL.bwplot.html)|A lattice::panel.bwplot replacement that fixes colouring issues|
|[`patient`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/patient.html)|Dataset describing qualities of 58 colon cancer patients|
|[`pcawg.colours()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/pcawg.colours.html)|Return standard PCAWG colour palettes.|
|[`scientific.notation()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/scientific.notation.html)|Use scientific notation in plots|
|[`show.available.palettes()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/show.available.palettes.html)|Display the available colour palettes|
|[`thousands.split()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/thousands.split.html)|Divide strings into groups of thousands|
|[`write.metadata()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/write.metadata.html)|Writes Metadata|
|[`write.plot()`](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/reference/write.plot.html)|Simplifies plotting by standardizing and centralizing all output-handling|

## Resources
Available resources on BPG usage can be found at the package [CRAN page](https://cloud.r-project.org/web/packages/BoutrosLab.plotting.general/index.html), [reference manual](https://cloud.r-project.org/web/packages/BoutrosLab.plotting.general/BoutrosLab.plotting.general.pdf), or [vignette](https://cloud.r-project.org/web/packages/BoutrosLab.plotting.general/vignettes/PlottingGuide.pdf).

## Getting help
Looking for guidance or support with Boutros.plotting.general? Check out our [Discussions](https://github.com/uclahs-cds/public-R-BoutrosLab-plotting-general/discussions) page.

Submit bugs, suggest new features or see current works at our [Issues](https://github.com/uclahs-cds/public-R-BoutrosLab-plotting-general/issues) page.

[Pull requests](https://github.com/uclahs-cds/public-R-BoutrosLab-plotting-general/pulls) are also open for discussion.

## Contributors
For lists of contributors please visit [here](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/authors.html) and [here](https://github.com/uclahs-cds/public-R-BoutrosLab-plotting-general/graphs/contributors) at GitHub

## Citation information
P’ng, C., Green, J., Chong, L.C. et al. BPG: Seamless, automated and interactive visualization of scientific data. *BMC Bioinformatics* **20**, 42 (2019). https://doi.org/10.1186/s12859-019-2610-2

## License

Authors: Christine P’ng, Jeffrey Green, Lauren C. Chong, Daryl Waggott, Stephenie D. Prokopec, Mehrdad Shamsi, Francis Nguyen, Denise Y. F. Mak, Felix Lam, Marco A. Albuquerque, Ying Wu, Esther H. Jung, Maud H. W. Starmans, Michelle A. Chan-Seng-Yue, Cindy Q. Yao, Bianca Liang, Emilie Lalonde, Syed Haider, Nicole A. Simone, Dorota Sendorek, Kenneth C. Chu, Nathalie C. Moon, Natalie S. Fox, Michal R. Grzadkowski, Nicholas J. Harding, Clement Fung, Amanda R. Murdoch, Kathleen E. Houlahan, Jianxin Wang, David R. Garcia, Richard de Borja, Ren X. Sun, Xihui Lin, Gregory M. Chen, Aileen Lu, Yu-Jia Shiah, Amin Zia, Ryan Kearns & Paul C. Boutros (PBoutros@mednet.ucla.edu)

BoutrosLab.plotting.general is licensed under the GNU General Public License version 2. See the file LICENSE.md for the terms of the GNU GPL license.

BoutrosLab.plotting.general is a software package for generating publication-quality, customizable plots.

Copyright (C) 2014-2018 Ontario Institute for Cancer Research and 2018-2023 University of California Los Angeles ("Boutros Lab") All rights reserved.

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.