# BoutrosLab.plotting.general

1. [Description](#description)
2. [Included plots](#included-plots)
3. [Installation](#installation)
4. [Function reference](#function-reference)
5. [Resources](#resources)
6. [Getting help](#getting-help)
7. [Contributors](#contributors)
8. [Citation information](#citation-information)
9. [License](#license)

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

> **Note**: Both `create.multipanelplot` and `create.multiplot` are currently supported, however `create.multipanelplot` is preferable to `create.multiplot`. In the medium-term `create.multiplot` will be deprecated.

## Installation

To install the latest public release of BoutrosLab.plotting.general from CRAN:

```
install.packages("BoutrosLab.plotting.general")
```

Or to install the latest development version from GitHub:
```
# install.packages("devtools")

devtools::install_github("uclahs-cds/package-BoutrosLab-plotting-general")
```
## Function reference

For further details on usage, parameters or to see an example of a specific function, see function links below.

| **Functions** | **Description** |
|-----------|---------|
|[`auto.axis()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/auto.axis.html)|Create ideal labels and values for a given dataset (detects log scales)|
|[`colour.gradient()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/colour.gradient.html)|Creates a colour gradient|
|[`covariates.grob()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/covariates.grob.html)|Create one or more covariate bars|
|[`create.barplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.barplot.html)|Make a barplot|
|[`create.boxplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.boxplot.html)|Make a boxplot|
|[`create.colourkey()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.colourkey.html)|Create Colourkey|
|[`create.dendrogram()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.dendrogram.html)|Generate a dendrogram|
|[`create.densityplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.densityplot.html)|Make a density plot|
|[`create.dotmap()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.dotmap.html)|Make a dotmap with coloured background|
|[`create.gif()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.gif.html)|Make a gif|
[`create.heatmap()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.heatmap.html)|Make a heatmap|
|[`create.hexbinplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.hexbinplot.html)|Make a hexagonally binned plot|
|[`create.histogram()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.histogram.html)|Make a histogram|
|[`create.lollipopplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.lollipopplot.html)|Make a lollipopplot|
|[`create.manhattanplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.manhattanplot.html)|Make a Manhattan plot|
|[`create.multipanelplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.multipanelplot.html)|Joins plots together|
|[`create.multiplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.multiplot.html)|Joins plots together|
|[`create.polygonplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.polygonplot.html)|Make a polygonplot|
|[`create.qqplot.comparison()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.qqplot.comparison.html)|Make a quantile-quantile plot of two samples|
|[`create.qqplot.fit()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.qqplot.fit.html)|Make a quantile-quantile plot of a sample|
|[`create.qqplot.fit.confidence.interval()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.qqplot.fit.confidence.interval.html)|Create the confidence bands for a one-sample qq plot|
|[`create.scatterplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.scatterplot.html)|Make a scatterplot|
|[`create.segplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.segplot.html)|Make a segplot|
|[`create.stripplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.stripplot.html)|Make a strip-plot|
|[`create.violinplot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/create.violinplot.html)|Make a violin plot|
|[`critical.value.ks.test()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/critical.value.ks.test.html)|Critical Value for Kolmogorov-Smirnov Test|
|[`default.colours()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/default.colours.html)|Provides default colour schemes.|
|[`display.colours()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/display.colours.html)|Function to display R colors, as well as corresponding R grey colours.|
|[`display.statistical.result()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/display.statistical.result.html)|Utility function to display statistical result in a plot|
|[`dist()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/dist.html)|Distance Matrix Computation|
|[`force.colour.scheme()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/force.colour.scheme.html)|Based on predefined colour schemes, returns a vector of corresponding colours.|
|[`generate.at.final()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/generate.at.final.html)|Generates alternative default tick mark locations for create.densityplot() and create.|scatterplot()
|[`get.corr.key()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/get.corr.key.html)|Correlation Key|
|[`get.correlation.p.and.corr()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/get.correlation.p.and.corr.html)|Calculate a correlation and its statistical significance|
|[`get.defaults()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/get.defaults.html)|Get operating system specific default properties|
|[`get.line.breaks()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/get.line.breaks.html)|Get line breaks|
|[`legend.grob()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/legend.grob.html)|Generate a legend grob|
|[`panel.BL.bwplot`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/panel.BL.bwplot.html)|A lattice::panel.bwplot replacement that fixes colouring issues|
|[`pcawg.colours()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/pcawg.colours.html)|Return standard PCAWG colour palettes.|
|[`scientific.notation()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/scientific.notation.html)|Use scientific notation in plots|
|[`show.available.palettes()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/show.available.palettes.html)|Display the available colour palettes|
|[`thousands.split()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/thousands.split.html)|Divide strings into groups of thousands|
|[`write.metadata()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/write.metadata.html)|Writes Metadata|
|[`write.plot()`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/write.plot.html)|Simplifies plotting by standardizing and centralizing all output-handling|

## Included datasets

| **Dataset** | **Description** |
|-----------|---------|
|[`CNA`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/CNA.html)|Copy number aberration (CNA) data from colon cancer patients|
|[`SNV`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/SNV.html)|Single nucleotide variant (SNV) data from colon cancer patients|
|[`microarray`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/microarray.html)|Microarray dataset of colon cancer patients|
|[`patient`](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/reference/patient.html)|Dataset describing qualities of 58 colon cancer patients|

## Resources

Available resources on BPG usage can be found at the package [CRAN page](https://cran.r-project.org/package=BoutrosLab.plotting.general/index.html), [reference manual](https://cran.r-project.org/package=BoutrosLab.plotting.general/BoutrosLab.plotting.general.pdf), or [vignette](https://cran.r-project.org/package=BoutrosLab.plotting.general/vignettes/PlottingGuide.pdf).

## Getting help

Looking for guidance or support with Boutros.plotting.general? Check out our [Discussions](https://github.com/uclahs-cds/package-BoutrosLab-plotting-general/discussions) page.

Submit bugs, suggest new features or see current works at our [Issues](https://github.com/uclahs-cds/package-BoutrosLab-plotting-general/issues) page.

[Pull requests](https://github.com/uclahs-cds/package-BoutrosLab-plotting-general/pulls) are also open for discussion.

## Contributors

For lists of contributors please visit [here](https://uclahs-cds.github.io/package-BoutrosLab-plotting-general/authors.html) and [here](https://github.com/uclahs-cds/package-BoutrosLab-plotting-general/graphs/contributors) at GitHub

## Citation information

To cite package `BoutrosLab.plotting.general` in publications use:

P’ng, C., Green, J., Chong, L.C. et al. _BPG: Seamless, automated and interactive visualization of scientific data_. BMC Bioinformatics 20, 42 (2019). https://doi.org/10.1186/s12859-019-2610-2

A BibTeX entry for LaTeX users is

```BibTeX
  @Article{,
    title = {BPG: Seamless, automated and interactive visualization of scientific data},
    journal = {BMC Bioinformatics},
    doi = {10.1186/s12859-019-2610-2},
    url = {https://doi.org/10.1186/s12859-019-2610-2},
    volume = {20},
    number = {42},
    year = {2019},
    month = {January},
    day = {21},
    issn = {1471-2105},
    author = {Christine P'ng and Jeffrey Green and Lauren C. Chong and Daryl Waggott and Stephenie D. Prokopec and Mehrdad Shamsi and Francis Nguyen and Denise Y. F. Mak and Felix Lam and Marco A. Albuquerque and Ying Wu and Esther H. Jung and Maud H. W. Starmans and Michelle A. Chan-Seng-Yue and Cindy Q. Yao and Bianca Liang and Emilie Lalonde and Syed Haider and Nicole A. Simone and Dorota Sendorek and Kenneth C. Chu and Nathalie C. Moon and Natalie S. Fox and Michal R. Grzadkowski and Nicholas J. Harding and Clement Fung and Amanda R. Murdoch and Kathleen E. Houlahan and Jianxin Wang and David R. Garcia and Richard de Borja and Ren X. Sun and Xihui Lin and Gregory M. Chen and Aileen Lu and Yu-Jia Shiah and Amin Zia and Ryan Kearns and Paul C. Boutros}
  }
```

## License

Authors: Christine P’ng, Jeffrey Green, Lauren C. Chong, Daryl Waggott, Stephenie D. Prokopec, Mehrdad Shamsi, Francis Nguyen, Denise Y. F. Mak, Felix Lam, Marco A. Albuquerque, Ying Wu, Esther H. Jung, Maud H. W. Starmans, Michelle A. Chan-Seng-Yue, Cindy Q. Yao, Bianca Liang, Emilie Lalonde, Syed Haider, Nicole A. Simone, Dorota Sendorek, Kenneth C. Chu, Nathalie C. Moon, Natalie S. Fox, Michal R. Grzadkowski, Nicholas J. Harding, Clement Fung, Amanda R. Murdoch, Kathleen E. Houlahan, Jianxin Wang, David R. Garcia, Richard de Borja, Ren X. Sun, Xihui Lin, Gregory M. Chen, Aileen Lu, Yu-Jia Shiah, Amin Zia, Ryan Kearns, Stefan Eng, Dan Knight, Julie Livingstone, Chase Bakkeby, Rupert Hugh-White, Adriana Salcedo, Takafumi Yamaguchi, Mohammed Faizal Eeman Mootor, Chenghao Zhu, Caden Bugh & Paul C. Boutros (PBoutros@mednet.ucla.edu)

BoutrosLab.plotting.general is licensed under the GNU General Public License version 2. See the file LICENSE.md for the terms of the GNU GPL license.

BoutrosLab.plotting.general is a software package for generating publication-quality, customizable plots.

Copyright (C) 2014-2018 Ontario Institute for Cancer Research and 2018-2023 University of California Los Angeles ("Boutros Lab") All rights reserved.

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
