# BoutrosLab.plotting.general

BoutrosLab.plotting.general (BPG) is a software package for generating publication-quality, customizable plots. 

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

## Getting help
Looking for guidance or support with Boutros.plotting.general? Check out our [Discussions](https://github.com/uclahs-cds/public-R-BoutrosLab-plotting-general/discussions) page.

Submit bugs, suggest new features or see current works at our [Issues](https://github.com/uclahs-cds/public-R-BoutrosLab-plotting-general/issues) page.

[Pull requests](https://github.com/uclahs-cds/public-R-BoutrosLab-plotting-general/pulls) are also open for discussion.

## Contributors
For lists of contributors please visit [here](https://uclahs-cds.github.io/public-R-BoutrosLab-plotting-general/authors.html) and [here](https://github.com/uclahs-cds/public-R-BoutrosLab-plotting-general/graphs/contributors) at GitHub

## Citation Information
P’ng, C., Green, J., Chong, L.C. et al. BPG: Seamless, automated and interactive visualization of scientific data. *BMC Bioinformatics* **20**, 42 (2019). https://doi.org/10.1186/s12859-019-2610-2

## License

Authors: Christine P’ng, Jeffrey Green, Lauren C. Chong, Daryl Waggott, Stephenie D. Prokopec, Mehrdad Shamsi, Francis Nguyen, Denise Y. F. Mak, Felix Lam, Marco A. Albuquerque, Ying Wu, Esther H. Jung, Maud H. W. Starmans, Michelle A. Chan-Seng-Yue, Cindy Q. Yao, Bianca Liang, Emilie Lalonde, Syed Haider, Nicole A. Simone, Dorota Sendorek, Kenneth C. Chu, Nathalie C. Moon, Natalie S. Fox, Michal R. Grzadkowski, Nicholas J. Harding, Clement Fung, Amanda R. Murdoch, Kathleen E. Houlahan, Jianxin Wang, David R. Garcia, Richard de Borja, Ren X. Sun, Xihui Lin, Gregory M. Chen, Aileen Lu, Yu-Jia Shiah, Amin Zia, Ryan Kearns & Paul C. Boutros (PBoutros@mednet.ucla.edu)

BoutrosLab.plotting.general is licensed under the GNU General Public License version 2. See the file LICENSE.md for the terms of the GNU GPL license.

BoutrosLab.plotting.general is a software package for generating publication-quality, customizable plots.

Copyright (C) 2014-2018 Ontario Institute for Cancer Research and 2018-2023 University of California Los Angeles ("Boutros Lab") All rights reserved.

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.