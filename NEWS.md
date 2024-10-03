# BoutrosLab.plotting.general 7.1.2 (2024-10-02)

## Changed

- Improved "sample.order" argument checking
- Updated NEWS to Markdown NEWS.md format
- Cleaned the C++ namespace by using `R_` versions of Free, Calloc, and Realloc

# BoutrosLab.plotting.general 7.1.1 (2024-01-08)

## Changed

- Improved documentation for type parameter in `create.scatterplot`


## Fixed
- Fix typo in README
- Update package manual for CRAN standards

# BoutrosLab.plotting.general 7.1.0 (2023-10-26)

## Added

- Add testthat framework for package tests
- Use mockr for unit testing
- Improve documentation for automatic axis scaling in:
  - `create.scatterplot`
  - `create.boxplot`
  - `create.manhattanplot`
  - `create.hexbinplot`

# BoutrosLab.plotting.general 7.0.10 (2023-10-09)

## Changed

- Add deprecation warning for test file in preparation for using
  testthat in the v8.0.0 release

# BoutrosLab.plotting.general 7.0.9 (2023-07-21)

## Changed

- Run R CMD check GitHub action weekly and with manual dispatch
- Updated GitHub links to use new "package" prefix

## Removed

- Remove unnecessary package manual file

# BoutrosLab.plotting.general 7.0.8 (2023-05-26)

## Changed

- Change name of print.to.file() to export.to.file() to avoid naming conflict
  with S3 method print()

# BoutrosLab.plotting.general 7.0.7 (2023-05-25)

## Changed

- Expose panel.text arguments for text.above.bar options in `create.barplot`
- Remove duplicated code in `print.multipanel`
- Update format of CRAN links
- Add default label locations for continuous legends

## Fixed

- Fix error with expressions in main for `multipanelplot`s

## Removed

- Remove outdated plotting data storage code

# BoutrosLab.plotting.general 7.0.6 (2023-02-15)

## Added

- Add github PR template

## Changed

- Move CICD workflow from Dockerhub to GitHub packages
- Add text support for `create.histogram`
- Remove axis labels with axis.lab = NULL in `create.boxplot` for consistent behavior with other plot types

## Fixed

- Fix ylab.axis.padding parameter in `create.histogram`
- Fix error when using expressions in `multipanelplot` labels and main

# BoutrosLab.plotting.general 7.0.5 (2023-01-03)

## Changed

- Fix typo in man/`create.violinplot`.Rd
- Add alpha parameter to `create.boxplot`
- Added GitHub repo to package description
- Correctly sort horizontal barplot
- Remove Makefile
- Replaced string class comparison with is()

## Fixed

- Fix NEWS format
- Fix ## Fixed with persisted plotting function output in examples

# BoutrosLab.plotting.general 7.0.4 (2022-08-17)

## Changed

- Fix border specification in `create.polygonplot`

# BoutrosLab.plotting.general 7.0.3 (2022-03-15)

## Changed

- Update .Rbuildignore to match lab R standards

# BoutrosLab.plotting.general 7.0.2 (2022-03-07)

## Fixed

- Add type comparisons for class name comparisons

# BoutrosLab.plotting.general 7.0.1 (2022-03-03)

## Fixed

- Fix object protection issue in distance.c

# BoutrosLab.plotting.general 7.0.0 (2022-02-15)

## Changed

- Renamed ks.test.critical.value() function to critical.value.ks.test()
  to resolve naming conflict with S3 generic ks.test() from base R stats
  package.

## Fixed

- Fix issue when specifying multiple line-widths in `create.polygonplot`()

# BoutrosLab.plotting.general 6.1.0 (2021-10-26)

## Changed

- Add option to use dots in a legend instead of default rectangles
- Enable `create.boxplot` to print different text label on each panel

## Fixed

- Fixed inconsistent grouping of point colours in `create.boxplot`
- Account for case where only one colour is passed to `create.heatmap`

# BoutrosLab.plotting.general 6.0.3 (2021-01-24)

## Changed

- Only use cairo in the examples for covariates.grob and legend.grob
  if capabilities('cairo') is true.

# BoutrosLab.plotting.general 6.0.2 (2021-01-04)

## Changed

- Depends versions: R (>= 3.5.0), lattice (>= 0.20-35),
  latticeExtra (>= 0.6-27), cluster (>= 2.0.0), hexbin (>= 1.27.0)
- Only use cairo when capabilities('cairo') is true to fix CRAN's issue
  of cairo not being available on M1 Macs

# BoutrosLab.plotting.general 6.0.1 (2020-08-05)

## Changed

- Added paper's doi to DESCRIPTION

## Removed

- Removed use of tocstyle package in PlottingGuide as that is obsolete
  and failing CRAN's check

# BoutrosLab.plotting.general 6.0.0 (2020-04-01)

## Changed

- Updated contact email
- Included source code previously imported from BoutrosLab.dist.overload
  and BoutrosLab.statistics.general
- Fixed resolutions for examples to reduce running time
- Added \donttest{} to examples that took >5 seconds to run
- Examples now output plots to tempdir()

## Fixed

- Fixed errors in `create.multiplot` and `create.multipanel` that wasn't
  passing CRAN's check by changing 'is.na' to 'anyNA'
- Fixed errors in `create.multiplot` that was only checking the first
  element in a list by using 'is.element' instead of '=='
- Fixed spacing issue in PlottingGuide
- Added stringsAsFactors = TRUE to data.frame() calls in examples
  (Changes to R-devel in 2020-03-04 changed the defaults)
- Fixed thousands.splits to return a vector instead of a list
- Fixed write.metadata to use double quotes on system calls, correctly
  show BL.plotting.survival version and consolidate exiftool calls
  to speed things up
- Fixed CRAN error misuse of package= for PACKAGE=

## Removed

- Removed BoutrosLab.utilities::recode.vector calls in examples
- Removed requirements for BoutrosLab.utilities, BoutrosLab.dist.overload,
  BoutrosLab.statistics.general

# BoutrosLab.plotting.general 5.9.8 (2018-10-16)

## Changed

- `create.hexbinplot`: added option to modify the default legend title

# BoutrosLab.plotting.general 5.9.7 (2018-09-19)

## Fixed

- Fixed an issue regarding warning detection for class of conditioning variable that didn't recognize character input (barplot, boxplot, hexbinplot, scatterplot)

# BoutrosLab.plotting.general 5.9.6 (2018-07-24)

## Fixed

- Fixed an issue that prevented extra points from plotting correctly in `create.polygonplot`

# BoutrosLab.plotting.general 5.9.5 (2018-06-05)

## Changed

- Added warnings when a formula contains a conditioning variable that is an integer/numeric (barplot, boxplot, hexbinplot, scatterplot)
- Changed default xaxis.tck for boxplot to prevent automatic ticks from appearing at top of plot

## Fixed

- Force.colour.scheme no longer requires 'x' when return.scheme = TRUE
- Fixed an error in `create.scatterplot` preventing automatic axis determination when all y == 0 or all y == -N

## Removed

- Removed append.footnote.R as it was non-functional with no plans to fix

# BoutrosLab.plotting.general 5.9.4 (2018-04-18)

## Changed

- Dotmap can now do symmetrical removing of top left half of dotmap

## Fixed

- Top covariates in heatmap now have proper defaults

# BoutrosLab.plotting.general 5.9.3 (2018-04-18)

## Changed

- Updated dependencies to not include as many
- Boxplot now saves all its data (for later use)
- `multipanelplot` now saves temp plots to temp dir

## Fixed

- Default.colours no longer references wrong grey scale colour

# BoutrosLab.plotting.general 5.9.2 (2018-04-10)

## Changed

- Legend grob for continous legends now auto place themselves -- x and y pos dont do anything anymore
- Print to file will now override previous entries
- Removed printing for individual plot info (too unreliable)

# BoutrosLab.plotting.general 5.9.1 (2018-04-04)

## Changed

- Multipanel plot now uses tob axis labels too for lining up plots

## Fixed

- Multipanel plot now correctly positions plot main labels

# BoutrosLab.plotting.general 5.9.0 (2018-03-23)

## Changed

- Added code for saving data information and trellis object information to each relevant plot

# BoutrosLab.plotting.general 5.8.16 (2018-03-16)

## Changed

- Cleaned up the `multipanelplot` code and ## Changedd the examples

# BoutrosLab.plotting.general 5.8.15 (2018-03-13)

## Changed

- Made `multipanelplot` legend/label placement algorithm more optimal

# BoutrosLab.plotting.general 5.8.14 (2018-03-09)

## Changed

- Now stores data on the mount

## Fixed

- Text now passed to panel in seperate list, stopping it from being reset in between plots

# BoutrosLab.plotting.general 5.8.13 (2018-02-13)

## Changed

- Adjusted polygon plot to work well with roc plot

# BoutrosLab.plotting.general 5.8.12 (2018-02-08)

## Changed

- Added lollipop plot as an extension of scatterplot

# BoutrosLab.plotting.general 5.8.11 (2018-02-05)

## Changed

- Added ability to disable factor sorting in relative plots
- Added example to histogram of type change

# BoutrosLab.plotting.general 5.8.10 (2018-01-22)

## Changed

- Added border colour for histogram bars

# BoutrosLab.plotting.general 5.8.9 (2018-01-12)

## Changed

- Added inside legend auto finder code

## Fixed

- Boxplot addstriplot colour was having issue with multiple boxplots, now works properly

# BoutrosLab.plotting.general 5.8.8 (2017-12-19)

## Changed

- Updated formatting to match coding standard
- Added inside legends to multipanel plot

## Fixed

- Heatmap now accepts both x and y axis covariates

# BoutrosLab.plotting.general 5.8.7 (2017-11-15)

## Changed

- Updated formatting to match coding standard

# BoutrosLab.plotting.general 5.8.6 (2017-11-15)

## Changed

- Legend and covariate grobs can now use x and y coordinates

## Fixed

- Heatmap using colours must faster
- Legacy settings in multipanel plot now affect all individual plots too

# BoutrosLab.plotting.general 5.8.5 (2017-11-01)

## Changed

- Force colour scheme now has heteroplasmy fraction and MT annotation

# BoutrosLab.plotting.general 5.8.4 (2017-10-25)

## Fixed

- Multipanel plot now accepts colorkey and draw key legends
- Get defaults now retrieves ArialMT instead of Arial font (actual name on Linux)

## Changed

- Heatmap can now use colours to specify the squares
- Density plot can now add text (like bar/boxplot)

# BoutrosLab.plotting.general 5.8.3 (2017-08-11)

## Fixed

- Multipanel plot now works with legends from original plots
- Barplot error bars now work with horizontal plots too

## Changed

- Heatmap now accepts inside legends and can specify text not based on grid cooridnates

# BoutrosLab.plotting.general 5.8.2 (2017-08-11)

## Fixed

- Completely reworked how spacing works in multipanel plot -- now creates rectangular grobs instead of using top/left/right/bottom padding

# BoutrosLab.plotting.general 5.8.1 (2017-08-11)

## Fixed

- Multipanel plot background colour
- Multipanel plot not accepting expression labels
- Multipanel deleting plots main labels
- Heatmap legend layout works properly now

# BoutrosLab.plotting.general 5.8.0 (2017-08-11)

## Added

- New `create.multiplot` function (`create.multipanelplot`)

## Fixed

- Auto-axis now gives 5 labels by default instead of 4
- Same.as.matrix can now be applied to create dendrogram outside of heatmap
- Fixed filename error in crete.gif
- Heatmap now loads defaults
- Segplot defaults to bold like rest of plots
- Colours for default dotmap now match the documentation (had to reverse them)

## Changed

\*pcawg.colours: new and renamed tumour type names added to palette, sv class 'insertion' assigned colour

# BoutrosLab.plotting.general 5.7.6 (2017-08-11)

## Fixed

- 1D heatmaps now work -- automatically
- Error bars now work properly with grouped bar plots

## Changed

- Automatic layout justification for legends (using just arg) for continous legends
- Added helper function for seperating thousands by commas, i.e -> (12345) -> (12,345)
- Merged all the relevent colour functions into force.colour.scheme
- Can now extract dendrograms from a heatmap (only one, and may require manual manipulation)

## Fixed

- `create.heatmap` - grid row/column limits reversed
- Legend.grob - continuous legends produce warnings
- Ytop.rectangle in `create.scatterplot`() doesn't work properly.
- Remove the old option (retrieve.plot.labels) and merge into (plot.labels.to.retrieve)

## Changed

- Merged multiple colour functions (old ones to be ## Removed in next commit)
- Multiple colours of error bars in scatterplot
- Add xaxis.rot.top option to `create.multiplot`
- Have legends on both the left AND the right for `create.heatmap`

# BoutrosLab.plotting.general 5.7.4 (2017-07-19)

## Fixed

- Automatic labels not working for stacked barplots
- Removed log scaling from stacked barplots
- Fixed 1 D heatmap issues

## Added

- Search metadata function

# BoutrosLab.plotting.general 5.7.3 (2017-06-06)

## Changed

- Each plot can put xat/yat = "auto"/"auto.linear"/"auto.log" to get automatic labels

## Added

- Pretty axis function -- input data set gives ## Changedd data values and labels

# BoutrosLab.plotting.general 5.7.2 (2017-05-17)

## Changed

- Can now add border to correlation keys
- Issues around multiplot reseting labels on y axis when x asis labels added solved

## Added

- `create.gif` for creating animated plots

# BoutrosLab.plotting.general 5.7.1 (2017-05-17)

## Fixed

- Write.plot.R: fixed an error that didn't allow adding additional plots when returning object to screen (when filename = NULL)
- `create.barplot`.R: fixed a ## Fixed interfering with add.rectangle and ## Removed redundant add.background.shading (repeats add.rectangle functionality)
- Force.colour.scheme: if multiple outputs were requested only one would be returned; now returns a list

## Changed

- `create.barplot`.R: minor improvements to some examples
- Split argument `lwd` into `axes.lwd` and `border.lwd` to be consistent with other functions
- Removed argument `remove.all.border.lines` as this can now be accomplished with `axes.lwd`
- Applied code formatting (BL standards)

# BoutrosLab.plotting.general 5.7.0 (2017-05-11)

## Fixed

- `create.violinplot`: fixed a ## Fixed in colour functionality; now repeats colours when fewer colours are supplied than there are groups

## Changed

- `create.hexbinplot`: changed argument abline.type to abline.lty to be consistent with other functions
- `create.segplot`: added warning for specific use case (multiplot fails when a vertical segplot is the first plot)
- `create.manhattanplot`: changed argument axis.lwd to axes.lwd to be consistent with other functions
- `create.hexbinplot`, `create.histogram`, `create.manhattanplot`, `create.polygonplot`, `create.qqplot`.comparison, `create.qqplot`.fit,
  `create.scatterplot`, `create.stripplot`, `create.segplot`, `create.violinplot`, age.categorical.colour, append.footnote:
  applied code formatting (BL standards)

# BoutrosLab.plotting.general 5.6.24 (2017-04-20)

## Changed

\*`create.multiplot`: Added functionality to add top x-axis tick labels. Has not been extensively tested.

# BoutrosLab.plotting.general 5.6.23 (2017-02-09)

## Fixed

\*pcawg.colours: fixed ## Fixed that prevented project code colours from being returned.

# BoutrosLab.plotting.general 5.6.22 (2017-02-09)

## Changed

\*`create.dotmap`: addition of parameters for rectangle border thickness, rectangle border colour.

# BoutrosLab.plotting.general 5.6.21 (2017-01-30)

## Fixed

- `create.scatterplot`: adding conditional for when pch is not referring
  to a point (this is in reference to KM plots)

# BoutrosLab.plotting.general 5.6.20 (2017-01-28)

## Changed

- `create.scatterplot`: addition of parameter "col.border".
  This allows customizable border colours when pch >= 21.

# BoutrosLab.plotting.general 5.6.19 (2016-12-06)

## Changed

- Pcawg.colours: add project.code to colour.schemes

# BoutrosLab.plotting.general 5.6.18 (2016-11-23)

## Changed

- `create.polygonplot`: option to add rectangles.

# BoutrosLab.plotting.general 5.6.17 (2016-11-08)

## Changed

- Pcawg.colours: consistent colour palette for use on pcawg data

# BoutrosLab.plotting.general 5.6.15 (2016-10-24)

## Changed

- `create.heatmap`: rotation of the top x axis labels is now independent from the bottom x axsis labels.
- `create.multiplot`: options to set the distance between tick marks and tick labels

# BoutrosLab.plotting.general 5.6.14 (2016-10-05)

## Changed

- `create.polygonplot`: option to add text.

# BoutrosLab.plotting.general 5.6.13 (2016-09-28)

## Changed

- `create.polygonplot`: added parameters alpha and median.lwd.

# BoutrosLab.plotting.general 5.6.12 (2016-09-07)

## Changed

- `create.densityplot` & `create.violinplot`: allow adjustment of bandwidth with parameters bandwidth and bandwidth.adjust.

# BoutrosLab.plotting.general 5.6.11 (2016-08-31)

## Changed

- `create.stripplot`: allow different border colour for added points when pch >= 21

# BoutrosLab.plotting.general 5.6.10 (2016-07-27)

## Fixed

- Reintegrated untagged changes to trunk and re-added scatterplot ## Changed

# BoutrosLab.plotting.general 5.6.9 (2016-07-25)

## Changed

- `create.scatterplot`: allow different border colour for added points when pch >= 21

# BoutrosLab.plotting.general 5.6.8 (2016-05-27)

## Changed

- `create.dotmap`:
  Added parameters 'columns' and 'rows' for spot.colour.function to allow dots colours to be grouped by columns or rows, respectively [BPG-16].
  There are 12 distinct colours (the default colours) and the colours start to repeat when there are more than 12 columns/rows.
  It also checks that the columns/rows (depending on which side is selected) are unique and throws and error if they are not.

# BoutrosLab.plotting.general 5.6.7 (2016-05-19)

## Fixed

- `create.multiplot`: fixed ## Fixed where NULL values in parameter list (for xlimits=/ylimits=) were being ignored/unprocessed [BPG-2].

# BoutrosLab.plotting.general 5.6.6 (2016-04-29)

## Changed

- `create.barplot`:
  Added parameters 'add.text', 'text.x', 'text.y', 'text.labels', 'text.col', 'text.cex', 'text.fontface' to allow users to add text labels to plot area.
  Added parameter 'box.ratio' for adjusting thickness of each bar.
  Added parameter 'remove.all.border.lines' to allow barplots with no borders.
  Added example for adding text to plot and adjusting box.ratio.

## Fixed

- `create.barplot`: fixed adding error bars for data-frames [BPG-4]

# BoutrosLab.plotting.general 5.6.5 (2016-04-27)

## Changed

- `create.boxplot`: Added example for adding text to plot.
- `create.hexbinplot`: Added example of stratified multiplot.

# BoutrosLab.plotting.general 5.6.4 (2016-04-21)

## Changed

- `create.dotmap`: NA plotting character can be made different colours with new variable, NA.spot.size.colour.
  Default is set to black.

# BoutrosLab.plotting.general 5.6.2 (2016-04-08)

## Changed

- Get.cor.key: parameterizing number of decimal places to keep for spearman, pearson and kendall correlation values

# BoutrosLab.plotting.general 5.4.3 (2016-01-04)

## Fixed

- Inconsitency in dotmap/heatmap grid lines location fixed -> everything follows heatmaps spec now
  fixed ## Fixed where sample.order would reorde barplots incorrectly when horizontal
- Default font sizes are consistent now
- Default font sizes are more proportional

## Changed

- Ability to add multiple violin fill colours
- Able to completely remove borders from heatmaps
- Add p values automatically to box plots
- Ability to set a sperate set of defaults based on type of publication ( default sets still need to be added)
- Ability to pick what plots to retrieve plot labels from in `create.multiplot`

# BoutrosLab.plotting.general 5.4.2 (2015-12-22)

## Changed

- `create.scatterplot`: added option to specify colour of grid lines.

# BoutrosLab.plotting.general 5.3.9 (2015-10-01)

## Changed

- `create.heatmap`: turns off grid lines when row or column number exceeds a certain threshold.
- Auto-removal of grid lines can be overrided with forcing parameters

BoutrosLab.plotting.general 5.3.7

## Fixed

- Same.as.matrix will now correcting readjust label specifications
- When combining scatterplots, they will no longer only use error bars from one scatter plot
- Yaxis.covariates parameter can now allow specific location specification so it does not overlap the heatmap
- Xgrid.at and ygrid.at now properly ## Changed for all functions
- Background rectangles are now drawn in the background instead of foreground
- When combining scatterplots, they will no longer only use the text specified from one scatter plot
- Removed circular definitions from Rd files
- Standardized parameter defaults

## Changed

- Can now add subtitles through use of xaxis.top parameters
- Can now input a vector of files to be output to
- Can now sort boxes in `create.boxplot` by max, min, mean and median (defaults to median if not set)
- Can now draw both bands and lines on segplots
- Polygon plot will now auto-calculate x and y limits if not specified
- Can now vertically align labels and adjust there coordinates manually

# BoutrosLab.plotting.general 5.3.4 (2015-07-31)

## Changed

- `create.scatterplot`: ## Changedd ROC example to use auto-label placement

# BoutrosLab.plotting.general 5.3.3 (2015-07-28)

## Changed

- Functions containing the word 'colour' now accept both American and Canadian spelling

# BoutrosLab.plotting.general 5.3.2 (2015-07-24)

## Fixed

- `create.heatmap`: fixed ## Fixed which made labels disappear when stratifying dendrogram

# BoutrosLab.plotting.general 5.3.1 (2015-07-24)

## Changed

- Minor Updates to the plotting guide

# BoutrosLab.plotting.general 5.3.0 (2015-07-23)

## Changed

- Standardized parameter names across all plotting functions
- `create.hexbinplot`: added add.xyline and add.curves functionality (same as in `create.scatterplot`)

# BoutrosLab.plotting.general 5.2.20 (2015-07-13)

## Changed

- `create.barplot`, `create.boxplot`, `create.hexbinplot`, `create.manhattanplot`, `create.polygonplot`, `create.scatterplot`, `create.segplot`, `create.stripplot`, `create.violinplot`: Set xlab.label and ylab.label to default to formula components
- Default.colours: greyscale check no longer runs if only one colour is requested

# BoutrosLab.plotting.general 5.2.19 (2015-07-03)

## Changed

- Force.colour.scheme: added snv colour scheme

# BoutrosLab.plotting.general 5.2.18 (2015-07-03)

## Changed

- Standardized all plotting functions to have resolution set to 1600 dpi
- Force.colour.scheme: Added spaces in appropriate tissue names

# BoutrosLab.plotting.general 5.2.17 (2015-06-16)

## Changed

- Removed out-of-date comments from examples

# BoutrosLab.plotting.general 5.2.16 (2015-06-15)

## Changed

- Re-created PlottingGuide to use revamped show.available.palettes function

# BoutrosLab.plotting.general 5.2.13 (2015-05-08)

## Changed

- `create.multiplot`: added example showing how to save parameter options from individual plots

# BoutrosLab.plotting.general 5.2.12 (2015-05-07)

## Changed

- `create.heatmap`: Added two parameters: text.position and text.offset to customize placement of text in cells
- Show.available.palettes: changed display of specific schemes to call `create.heatmap` (more robust method)

# BoutrosLab.plotting.general 5.2.10 (2015-04-22)

## Fixed

- Show.available.palettes: fixed 'general' program to no longer introduces NAs by coersion

# BoutrosLab.plotting.general 5.2.9 (2015-04-21)

## Changed

- `create.hexbinplot`: added comment to explain maxcnt removing datapoints from example plot

# BoutrosLab.plotting.general 5.2.8 (2015-03-12)

## Changed

- `create.barplot`: allow vector of custom sorting order for samples
- `create.multiplot`: has flag to remove all border lines from plots

# BoutrosLab.plotting.general 5.2.6 (2014-12-24)

## Changed

- Write.plot: fixed ## Fixed
- Added new colourkey legend and interface

# BoutrosLab.plotting.general 5.2.5 (2014-12-18)

## Changed

- Updated plotting guide to have more examples and slight wording adjustments

# BoutrosLab.plotting.general 5.2.4 (2014-12-16)

## Changed

- `create.barplot`: ablines now appear in the background of the plot instead of the foreground

# BoutrosLab.plotting.general 5.2.2 (2014-12-09)

## Changed

- Write.plot: Added embed.plot functionality (depreciating said function)
- Embed.plot: ## Removed embed.plot()

# BoutrosLab.plotting.general 5.2.1 (2014-11-05)

## Changed

- Force.colour.scheme: ## Fixed for annovar colour schemes

# BoutrosLab.plotting.general 5.2.0 (2014-10-17)

## Changed

- Added `Nature' style option to all plotting functions

# BoutrosLab.plotting.general 5.1.9 (2014-10-16)

## Changed

- `create.scatterplot`: for add.line.segments, changed line-end style to 'butt'
- `create.stripplot`: NEW ARGUMENTS to allow for addition of lines showing group medians

# BoutrosLab.plotting.general 5.1.7 (2014-10-10)

## Changed

- Default.colours: added new schemes spiral.noon and spiral.night
- Default.colours: rearranged order of qual scheme to decrease red-green combinations
- Force.colour.scheme: changed CNV colours to red/blue/green/white, MSI to green/yellow, risk to red/yellow
- Reorganized default.colours and force.colour.scheme to return deprecated schemes
- Show.available.palettes: reorganized layout of 'specific' scheme output

# BoutrosLab.plotting.general 5.1.6 (2014-10-08)

## Changed

- `create.boxplot`: added arguments to allow for addition of stripplot in background

# BoutrosLab.plotting.general 5.1.5 (2014-09-19)

## Changed

- Show.available.palettes: added new function to show schemes available in default.colours and
  force.colour.scheme
- Default.colours: refactored, changed greyscale warning to auto-calculate

# BoutrosLab.plotting.general 5.1.4 (2014-09-08)

## Changed

- `create.scatterplot`: changed argument name - add.lines > add.line.segments
- `create.scatterplot`: ## Changedd to accept lists of line segments to add

# BoutrosLab.plotting.general 5.1.3 (2014-08-22)

## Changed

- PlottingGuide: converting to use knitr, added further details and references to design portion
- MultiPlotTutorial: deleted, contents migrated to PlottingGuide

# BoutrosLab.plotting.general 5.1.2 (2014-08-14)

## Changed

- `create.manhattanplot`: ## Changedd documentation to use chromosome palette in force.colour.scheme

# BoutrosLab.plotting.general 5.1.1 (2014-08-14)

## Changed

- `create.barplot`: changed default fill colour to black (instead of grey)

# BoutrosLab.plotting.general 5.1.0 (2014-08-12)

## Changed

- `create.scatterplot`: NEW ARGUMENTS text.guess.\* now allows users to specify data points so that
  associated label positions can be automatically determined
- Inside.rectangle: NEW FUNCTION which determines if a set of data points are inside a set of defined
  rectangles
- Inside.ellipse: NEW FUNCTION which determines if a set of data points are inside a set of defined
  ellipses

# BoutrosLab.plotting.general 5.0.28 (2014-08-08)

## Changed

- Updated plotting guide to have extended design advice: added sections on resolution, file types, etc

# BoutrosLab.plotting.general 5.0.27 (2014-07-31)

## Changed

- Updated examples in scatterplot, qqplot comparison, hexbinplot, heatmap, dotmap, and barplot to have better formatted axes labels

# BoutrosLab.plotting.general 5.0.26 (2014-07-31)

## Fixed

- Re-added argument line.lwd in `create.scatterplot`

# BoutrosLab.plotting.general 5.0.25 (2014-07-31)

## Changed

- `create.heatmap`: NEW ARGUMENT gridline.order now allows user to specify in which order
  gridlines should be drawn (horizontal or vertical first)

# BoutrosLab.plotting.general 5.0.24 (2014-07-31)

## Changed

- Adjusted examples in qqplot.comparison, qqplot.fit and scatterplot to use character values for panel groupings

# BoutrosLab.plotting.general 5.0.23 (2014-07-30)

## Changed

- Display.colours now additionally displays names of colours

## Fixed

- Added 'eye' to the 'tissue' palette in force.colour.scheme

# BoutrosLab.plotting.general 5.0.22 (2014-07-29)

## Changed

- Fixed: `create.scatterplot`, add.lines now works with lists of data

# BoutrosLab.plotting.general 5.0.20 (2014-07-24)

## Changed

- Changed multiplot example to use matching gene data

# BoutrosLab.plotting.general 5.0.19 (2014-07-23)

## Changed

- Changing chromosome palette in force.colour.scheme to use more visually distinct colours

# BoutrosLab.plotting.general 5.0.18 (2014-07-23)

## Fixed

- Changed datasets keep common genes among datasets

# BoutrosLab.plotting.general 5.0.17 (2014-07-21)

## Changed

- Updated multiplot tutorial with new examples using generate data

# BoutrosLab.plotting.general 5.0.16 (2014-07-16)

## Fixed

- Default.colours: properly return the requested number of colours when multiple palettes requested

# BoutrosLab.plotting.general 5.0.15 (2014-07-11)

## Changed

- `create.scatterplot`:
  - NEW ARGUMENTs allow for addition of line segments to be drawn

## Fixed

- Get.corr.key: fixed corner definition determined by x.pos and y.pos

# BoutrosLab.plotting.general 5.0.14 (2014-07-07)

## Changed

- `create.scatterplot`:
  - NEW ARGUMENT allows for specification of strip title fontface
- `create.stripplot`:
  - NEW ARGUMENT allows for specification of strip title fontface

# BoutrosLab.plotting.general 5.0.11 (2014-07-04)

## Changed

- Added example to `create.dotmap` clustered by dots and dendrogram added

# BoutrosLab.plotting.general 5.0.10 (2014-07-04)

## Changed

- Renamed colour schemes in default.colours to follow spiral.<timeofday> format

# BoutrosLab.plotting.general 5.0.10 (2014-07-04)

## Changed

- Added biomolecule colour scheme to force.colour.schemes
- Migrated chromosome colour scheme to force.colour scheme from default.colours
- Renamed extra colour schemes in default.colours

# BoutrosLab.plotting.general 5.0.9 (2014-07-03)

## Changed

- Formatting adjustments to examples

# BoutrosLab.plotting.general 5.0.8 (2014-07-03)

## Changed

- Changed SNV dataset numerical coding to be simpler

# BoutrosLab.plotting.general 5.0.7 (2014-07-02)

## Fixed

- Palettes in default.colours function properly return subsets

# BoutrosLab.plotting.general 5.0.6 (2014-06-13)

## Changed

- Force.colour.scheme now accepts both upper and lower case input

# BoutrosLab.plotting.general 5.0.5 (2014-06-11)

## Changed

- Changed default sequential, diverging, and binary schemes

# BoutrosLab.plotting.general 5.0.4 (2014-06-10)

## Changed

- Added more schemes to default.colours

# BoutrosLab.plotting.general 5.0.3 (2014-06-04)

## Changed

- Added plotting guide to vignettes

# BoutrosLab.plotting.general 5.0.2 (2014-06-02)

## Fixed

- `create.barplot`:
  - Re-added origin to panel call to allow for negative bars
  - Set y-axis default values to appear automatically
  - Fixed ordering of bars by increasing data
- `create.histogram`: Silenced warning caused by data parameter

# BoutrosLab.plotting.general 5.0.1 (2014-06-02)

## Changed

- Force.colour.scheme: Added colour schemes in addition to the 'annovar' schemes
- Default.colours: Added additional 5-colour palette

# BoutrosLab.plotting.general 5.0.0 (2014-05-29)

## Added

- `create.scatterplot`.with.error: ## Removed function, merged with `create.scatterplot`
- Panel.BL.bwplot: ## Removed function, functionality no longer needed
- `create.multiple`.stripplots: ## Removed function, functionality contained within `create.stripplot`
- `create.multiple`.boxplots: ## Removed function, functionality contained within `create.boxplot`
- `create.barplot`.with.error: ## Removed function, functionality contained within `create.barplot`
- Rgb.to.greyscale: ## Removed function, moved into display.colours
- Colour.to.grey: ## Removed function, moved into display.colours
- Scientific.notation.2: ## Removed function, functionality contained within scientific.notation
- Scientific.notation.expr: ## Removed function, functionality contained within scientific.notation
- `create.violinplot`: Renamed function from `create.violin`.plot
- `create.polygonplot`: Renamed function from `create.polygon`

## Changed

- Code and documentation base reviewed to match lab formatting standard
- Updating examples in documentation to use standard datasets instead of fake data
- `create.multiplot`: ## Changedd example plots to use standard dataset
- Patient: Added additional data to show proportions of base changes
- `create.scatterplot`: Added example using error bars
- Scientific.notation: Now returns either an expression or list
- Updated test-suite to save a single multiplot to file

# BoutrosLab.plotting.general 4.3.14 (2014-05-28)

## Changed

- Updated test suite to remove soon-to-be deleted plotting functions

# BoutrosLab.plotting.general 4.3.13 (2014-05-23)

## Changed

- Formatted examples in documentation to contain less than 100 characters per line
- SNV: changed encoding of dataset to use numerical values instead of character values

# BoutrosLab.plotting.general 4.3.12 (2014-05-22)

## Changed

- `create.violin`.plot: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.3.11 (2014-05-22)

## Changed

- `create.stripplot`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.3.10 (2014-05-22)

## Changed

- `create.stripplot`: Exposing xat and xlimits parameter to allow for customization of horizontal plots

# BoutrosLab.plotting.general 4.3.9 (2014-05-22)

## Changed

- `create.segplot`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.3.8 (2014-05-21)

## Changed

- `create.scatterplot`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.3.7 (2014-05-21)

## Changed

- Removed old datasets and ## Changedd documentation examples to use newly formatted data
- Set LazyData in DESCRIPTION to 'yes' so that datasets can be referenced by name alone

# BoutrosLab.plotting.general 4.3.6 (2014-05-21)

## Changed

- CNA, SNV, microarray, patient: Added new datasets to data/ folder, each with a corresponding Rd file
- Deleted old 'p value' dataset formerly used for `create.manhattanplot` examples

# BoutrosLab.plotting.general 4.3.5 (2014-05-21)

## Changed

- `create.colourkey`: Fixed formatting in documentation to fit under 90 characters per line
- `create.histogram`: ## Changedd example code to check dataset size instead of use hard-coded values
- Microarray: Decreased size of microarray dataset

# BoutrosLab.plotting.general 4.3.4 (2014-05-20)

## Changed

- `create.colourkey`: Exposed parameter for colourkey cex

# BoutrosLab.plotting.general 4.3.3 (2014-05-20)

## Changed

- `create.qqplot`.fit: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.3.2 (2014-05-20)

## Changed

- `create.qqplot`.comparison: ## Changedd example plots to use standard dataset
- MultiPlotTutorial: Reduced file size

# BoutrosLab.plotting.general 4.3.1 (2014-05-19)

## Changed

- `create.polygon`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.3.0 (2014-05-16)

## Changed

- Run_plots: Added test script to package

# BoutrosLab.plotting.general 4.2.40 (2014-05-16)

## Changed

- `create.histogram`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.2.39 (2014-05-16)

## Changed

- `create.hexbinplot`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.2.38 (2014-05-16)

## Changed

- `create.dotmap`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.2.37 (2014-05-15)

## Changed

- `create.densityplot`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.2.36 (2014-05-15)

## Changed

- `create.boxplot`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.2.35 (2014-05-15)

## Changed

- `create.heatmap`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.2.34 (2014-05-15)

## Fixed

- `create.heatmap`: Fixed error where heatmap threw error if symbols were added, but not 'border' symbols

# BoutrosLab.plotting.general 4.2.33 (2014-05-15)

## Changed

- `create.heatmap`: Added clarification to documentation that 'colour.alpha' can be set to 'automatic'
- `create.multiplot`: Expose parameters for x-axis and y-axis tick mark lengths

# BoutrosLab.plotting.general 4.2.32 (2014-05-14)

## Changed

- `create.violinplot`: ## Removed error message for using pch>=21 with lattice version 20-13
- `create.violinplot`: Modified handling of border and fill colours for extra points

# BoutrosLab.plotting.general 4.2.31 (2014-05-13)

## Changed

- `create.manhattanplot`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.2.30 (2014-05-12)

## Changed

- `create.barplot`: ## Changedd example plots to use standard dataset

# BoutrosLab.plotting.general 4.2.29 (2014-05-12)

## Changed

- `create.dotmap`: enabled default behaviour for NA values to plot 'X' characters size 7

# BoutrosLab.plotting.general 4.2.28 (2014-05-12)

## Changed

- `create.dotmap`: Improved handling of multiple pch with multiple fill colours

# BoutrosLab.plotting.general 4.2.27 (2014-05-08)

## Changed

- `create.dotmap`: Deleted warning for using pch>=21 with lattice version 20-13 (outdated)

# BoutrosLab.plotting.general 4.2.26 (2014-05-02)

## Changed

- `create.dotmap`: Stopped printing of warning regarding using pch>=21 with lattice version 20-13

# BoutrosLab.plotting.general 4.2.25 (2014-04-25)

## Changed

- Fixed spelling errors in documentation and code

# BoutrosLab.plotting.general 4.2.24 (2014-04-24)

## Changed

- Embed.plot: Added example & ## Changedd documentation to use proper parameters
- `create.hexbinplot`: Fixed formatting issue
- `create.scatterplot`: Fixed auto-sensing of labels
- `create.barplot`: Added functionality of adding text above bars
- `create.heatmap`: Added ability to have multi-sized borders
- `create.heatmap`: Adjusted input of 'main' to accept non-list values
- MultiPlotTutorial: Compressed pdf
- Updated formatting of all function documentation
- NAMESPACE: Added imports

# BoutrosLab.plotting.general 4.2.23 (2014-04-17)

## Changed

- `create.densityplot`: Added data-sensing
- `create.scatterplot`: Added data-sensing
- `create.hexbinplot`: Added example of multi-colour scheme

# BoutrosLab.plotting.general 4.2.22 (2014-04-17)

## Changed

- `create.densityplot`: Expose top, bottom, left, and right padding parameters
- `create.hexbinplot`: Expose top, bottom, left, and right padding parameters
- `create.qqplot`.fit: Expose top, bottom, left, and right padding parameters
- `create.qqplot`.comparison: Expose top, bottom, left, and right padding parameters
- `create.violin`.plot: Expose top, bottom, left, and right padding parameters
- `create.heatmap`: Added removal of top bar in stratified dendrograms

## Fixed

- Fixed ## Fixeds, typos, and mismatches in multiplot tutorial sample code
- `create.heatmap`: Fixed coding issues

# BoutrosLab.plotting.general 4.2.21 (2014-04-13)

## Changed

- Embed.plot: Added new function to allow for plot-within-a-plot layouts

# BoutrosLab.plotting.general 4.2.20 (2014-04-13)

## Changed

- `create.heatmap`: Added ability to automatically estimate values for colour.alpha parameter
- `create.heatmap`: Added example for automatic colour.alpha value estimation

## Fixed

- `create.heatmap`: Fixed grid to also draw lines on first row & column
- `create.barplot`: Fixed default units to inches
- `create.heatmap`: Fixed documentation to match changes in code
- `create.boxplot`: Fixed naming issue
- `create.multiplot`: Fixed naming conventions

# BoutrosLab.plotting.general 4.2.19 (2014-04-04)

## Changed

- Added new function 'colour.gradient' to generate sequential colour palettes

# BoutrosLab.plotting.general 4.2.18 (2014-03-31)

## Changed

- `create.heatmap`: Added ability to stratify dendrogram
- `create.boxplot`: Added ability to reorder boxplots based on median values

# BoutrosLab.plotting.general 4.2.17 (2014-03-26)

## Changed

- `create.barplot`: Added ability to draw curves
- `create.boxplot`: Added ability to draw curves
- `create.barplot`.with.error: Adjusted whisker length to not cover whole bar
- `create.heatmap`: Added example of per-cell font characteristics
- `create.barplot`: Added example of grouped labels
- `create.boxplot`: Added option of drawing line either in front of or behind plot

## Fixed

- `create.heatmap`: Fixed issue with symbols

# BoutrosLab.plotting.general 4.2.16 (2014-03-17)

## Changed

- `create.heatmap`: Added global covariate outline
- `create.barplot`: Added grouping of labels for grouped barplot
- `create.heatmap`: Added examples
- `create.multiplot`: Added examples

## Fixed

- Write.metadata: Fixed to stop printing warnings

# BoutrosLab.plotting.general 4.2.15 (2014-03-14)

## Changed

- `create.multiplot`: Added ability to retrieve plot labels & limits from 'original' plots
- `create.multiplot`: ## Changedd examples

## Fixed

- Scientific.notation.expr: Fixed issue & code formatting
- Write.metadata: Fixed issue with dependencies

# BoutrosLab.plotting.general 4.2.14 (2014-03-05)

## Changed

- `create.heatmap`: Added functionality of inserting additional symbols
- `create.heatmap`: Added example to documentation of symbol usage
- `create.scatterplot`: Changed ablines to be drawn in front of plot
- Added datasets files to inst folder for later use in examples
- Added vignettes folder containing multiplot tutorial

## Fixed

- `create.barplot`: Fixing reordering of bars

# BoutrosLab.plotting.general 4.2.13 (2014-02-26)

## Changed

- `create.barplot`: Set default reordering to FALSE
- `create.dotmap`: Added functionality to change dot colour

# BoutrosLab.plotting.general 4.2.12 (2014-02-25)

## Changed

- `create.barplot`: Added functionality to reorder bars to be increasing or decreasing
- `create.hexbinplot`: Added more examples
- Exposed 'fontface' parameter in all plotting functions

# BoutrosLab.plotting.general 4.2.11 (2014-02-23)

## Changed

- Display.statistical.result: ## Changedd defaults in documentation

# BoutrosLab.plotting.general 4.2.10 (2014-02-06)

## Changed

- `create.hexbinplot`: Added background colour
- `create.hexbinplot`: Added ability to have tick marks on top x axis only
- `create.hexbinplot`: Enabled svg output
- `create.hexbinplot`: Added examples to documentation

## Fixed

- `create.barplot`: ## Removed origin from panel call to enable creation of stacked, horizontal, unidirectional plots

# BoutrosLab.plotting.general 4.2.9 (2014-02-03)

## Changed

- Display.statistical.result: Added option to change ":" to a different symbol
- `create.hexbinplot`: Added ability to change background colour
- `create.heatmap`: Added ability to display tick marks on top axis only
- Enabled svg output for all plots

# BoutrosLab.plotting.general 4.2.8 (2014-01-27)

## Changed

- Re-added package

# BoutrosLab.plotting.general 4.2.7 (2014-01-10)

## Added

- Added `create.manhattanplot` function

# BoutrosLab.plotting.general 4.2.4 (2013-11-22)

## Changed

- Added TODO file

# BoutrosLab.plotting.general 4.2.3 (2013-11-18)

## Changed

- `create.boxplot`: Added ability to draw rectangle panels
- `create.boxplot`: Added new example with grey shading between different boxes

# BoutrosLab.plotting.general 4.2.2 (2013-11-13)

## Changed

- `create.barplot`: Added ability to change the width of the box

# BoutrosLab.plotting.general 4.2.1 (2013-11-13)

## Changed

- `create.barplot`: Added new parameters - background.col and draw.grid
- `create.barplot`: Added new example of grey background with white grid lines

# BoutrosLab.plotting.general 4.2.0 (2013-11-12)

## Changed

- Default.colours: Changed default qualitative palette to have brighter colours, and 12 options instead of only 9. Previous qualitative palette still accessible by setting palette.type = 'old.qual'

# BoutrosLab.plotting.general 4.1.8 (2013-11-11)

## Changed

- Default.colours: ## Removed deprecated survival colour scheme
- Changed warning about greyscale compatibility to be more general

# BoutrosLab.plotting.general 4.1.7 (2013-11-11)

## Changed

- Default.colours: ## Removed deprecated old chromosome colour scheme
- Replaced 'sandybrown' with 'tan1' in new chromosome palette

# BoutrosLab.plotting.general 4.1.6 (2013-11-11)

## Changed

- Added `create.colourkey` to NAMESPACE

# BoutrosLab.plotting.general 4.1.5 (2013-11-10)

## Changed

- Update and reorder NAMESPACE

# BoutrosLab.plotting.general 4.1.4 (2013-11-09)

## Changed

- Removed unnecessary requires in get.corr.key

# BoutrosLab.plotting.general 4.1.3 (2013-11-09)

## Changed

- Updated minimum required versions of dependencies

# BoutrosLab.plotting.general 4.1.2 (2013-11-09)

## Changed

- `create.barplot`: ## Changedd manual to reflect addition of 'axis.fontface' argument
- `create.colourkey`: ## Removed 'enable.warnings' argument

# BoutrosLab.plotting.general 4.1.1 (2013-11-08)

## Changed

- `create.barplot`: Added arguments 'xaxis.fontface' and 'yaxis.fontface'

# BoutrosLab.plotting.general 4.1.0 (2013-11-05)

## Added

- Added new function: '`create.colourkey`' to generate and draw colourkey

# BoutrosLab.plotting.general 4.0.7 (2013-11-04)

## Changed

- `create.stripplot`: New arguments 'xaxis.fontface', 'yaxis.fontface' to allow for modification of axis tck label font face
- `create.stripplot`: New argument 'ylab.axis.padding' allows for modification of padding between yaxis label and tck labels

# BoutrosLab.plotting.general 4.0.6 (2013-11-01)

## Changed

- `create.dendrogram`: ## Changedd documentation for jaccard usage
- `create.heatmap`: ## Changedd documentation for jaccard usage
- `create.heatmap`: Moved logic for x.tck out of level plot

# BoutrosLab.plotting.general 4.0.5 (2013-10-25)

## Changed

- `create.heatmap`: Refactoring and cleanup of code

# BoutrosLab.plotting.general 4.0.4 (2013-10-21)

## Changed

- `create.polygon`: Added 'median.lty' (for seq power plot)

# BoutrosLab.plotting.general 4.0.3 (2013-10-18)

## Changed

- `create.hexbinplot`: Changed default of 'mincnt' to 1 (from 0)

# BoutrosLab.plotting.general 4.0.2 (2013-09-26)

## Changed

- `create.scatterplot`.with.error: New arguments to specify plot layout and placement - 'layout', 'as.table', 'x.spacing', 'y.spacing'
- `create.polygon`: Added parameters 'axis.log' and 'yaxis.log'

# BoutrosLab.plotting.general 4.0.1 (2013-08-26)

## Changed

- `create.dotmap`: Added example

# BoutrosLab.plotting.general 4.0.0 (2013-08-13)

## Added

- Added 'write.metadata' function
- Added 'description' parameter to all plotting function documentations
- Write.metadata: Added 'operating system', 'machine', 'system2'
- `create.multiplot`: ## Changedd documentation

# BoutrosLab.plotting.general 3.10.2 (2013-07-29)

## Changed

- Default.colours: ## Changed chromosome colour scheme to be coded by letter-names rather than numbers, and using more easily-distinguished colours

# BoutrosLab.plotting.general 3.10.1 (2013-07-25)

## Changed

- Legend.grob: Added ability to specify size (width) of legend boxes for each legend individually

# BoutrosLab.plotting.general 3.10.0 (2013-07-25)

## Added

- `create.scatterplot`: Added parameter 'axis.key.padding' to adjust spacing between y-axis and key
- `create.scatterplot`: Added parameters 'add.points', 'points.x', 'points.y', 'points.col', 'points.pch', 'points.cex' - allow users to add additional points to plot area
- `create.scatterplot`: Added parameters 'add.text', 'text.x', 'text.y', 'text.labels', 'text.col', 'text.cex', 'text.fontface' - allow users to add text labels to plot area

# BoutrosLab.plotting.general 3.9.6 (2013-07-25)

## Changed

- `create.dotmap`: Added padding parameter

# BoutrosLab.plotting.general 3.9.5 (2013-07-19)

## Changed

- `create.barplot`: Adjustments to spacing and commenting
- `create.dotmap`: Added ability to specify padding on all sides of plot

# BoutrosLab.plotting.general 3.9.4 (2013-07-17)

## Changed

- `create.multiplot`: New argument 'axes.lwd' to specify border line thickness

# BoutrosLab.plotting.general 3.9.3 (2013-07-15)

## Changed

- `create.multiplot`: Fixed 'xlab.to.xaxis.padding' variable to change the correct parameter

# BoutrosLab.plotting.general 3.9.2 (2013-07-12)

## Changed

- `create.heatmap`: Added ability to specify two x-axes, and gave meaning to 'x-alternating = 3' (prints tick marks on top and bottom x axes)
- `create.multiplot`: Added 'xlab.to.xaxis.padding' parameter

# BoutrosLab.plotting.general 3.9.1 (2013-07-09)

## Changed

- `create.dotmap`: Replaced 'grid.colour' with 'row.colour' and 'col.colour'
- `create.barplot`: ## Changedd to accept arguments that handle background shading
- `create.heatmap`: Added 'xaxis.tck' and 'yaxis.tck' parameters to override default values based on covariate size

# BoutrosLab.plotting.general 3.9.0 (2013-07-04)

## Changed

- `create.multiplot`: Added 'layout.skip' parameter to allow for blank spots in layout, and also added a complex example

# BoutrosLab.plotting.general 3.8.7 (2013-06-28)

## Changed

- Rgb.to.greyscale: Changing function to return results, rather than print

# BoutrosLab.plotting.general 3.8.6 (2013-06-27)

## Changed

- Append.footnote: Added function to create plot footnotes
- Default.colours: Fixed method of returning one-colour palettes for backwards compatibility

# BoutrosLab.plotting.general 3.8.5 (2013-06-17)

## Changed

- Default.colours: Adjustments to chromosome colour palette
- Default.colours: Added ability to provide multiple sequential and binary colour schemes

# BoutrosLab.plotting.general 3.8.4 (2013-06-13)

## Changed

- `create.boxplot`: Added ability to define 'abline.v'

# BoutrosLab.plotting.general 3.8.3 (2013-06-12)

## Changed

- Display.colours: Added function to view colour palettes and what they look like in grey scale

# BoutrosLab.plotting.general 3.8.2 (2013-06-11)

## Changed

- Covariates.grob: Added 'reorder.grid.index' parameter to specify ordering of grid indices
- `create.heatmap`: Added 'covariates.reorder.grid.index' and 'covariates.top.reorder.grid.index' parameters to match changes to covariates.grob

# BoutrosLab.plotting.general 3.8.1 (2013-06-10)

## Changed

- `create.heatmap`: Split 'grid.colour' and 'grid.lwd' parameters into 'row.colour/col.colour' and 'row.lwd/col.lwd'
- `create.heatmap`: Added grandfather parameters for 'grid.colour' and 'grid.lwd'

# BoutrosLab.plotting.general 3.8.0 (2013-06-04)

## Added

- Colour.to.grey: Added new function to convert R colours to R grey colours

# BoutrosLab.plotting.general 3.7.0 (2013-05-29)

## Added

- `create.heatmap`: Added 'covariates.row.lines', 'covariates.col.lines', 'covariates.top.row.lines', 'covariates.top.col.lines' corresponding to new covariates.grob arguments, and ## Changedd examples
- Covariates.grob: Added parameters 'row.lines' and 'col.lines' to allow user to specify which grid lines to draw in covariate bars, and ## Changedd examples

# BoutrosLab.plotting.general 3.6.20 (2013-04-23)

## Changed

- Get.corr.key: New arguments to allow for the addition of a title to the key

# BoutrosLab.plotting.general 3.6.19 (2013-04-04)

## Changed

- `create.barplot`: Attempting to make sizes match up
- `create.dotmap`: Can now have custom colours specified
- Write.plot: Code formatting changes

# BoutrosLab.plotting.general 3.6.18 (2013-03-19)

## Changed

- `create.barplot`: Code clean-up to match coding standards

# BoutrosLab.plotting.general 3.6.17 (2013-03-12)

## Changed

- `create.barplot`: Fix to align error bars when barplot is grouped
- Write.plot: Refactor to place extension-specific code in inst/ext2function.txt

# BoutrosLab.plotting.general 3.6.16 (2013-03-07)

## Changed

- Write.plot: Allowing pdf and png output

# BoutrosLab.plotting.general 3.6.15 (2013-03-05)

## Changed

- `create.segplot`: Added 'abline.h' parameter

# BoutrosLab.plotting.general 3.6.14 (2013-03-01)

## Changed

- `create.heatmap`: Centralize parameter-checking code at start of script
- Added pch>=21 warning to appropriate R files, and ## Removed ^M

# BoutrosLab.plotting.general 3.6.13 (2013-02-19)

## Changed

- `create.polygon`: Added line plotting options 'add.xyline', 'abline.h', 'abline.v'
- `create.barplot`: Added examples

# BoutrosLab.plotting.general 3.6.12 (2013-02-14)

## Changed

- `create.multiplot`: Added check for NAs in the legends workaround

# BoutrosLab.plotting.general 3.6.11 (2013-02-12)

## Changed

- `create.multiplot`: Added workaround for lattice being unable to accept multiple legends with the same name via ## Changed.trellis
- `create.multiplot`: Allowed x/y-relations between plots to be set

# BoutrosLab.plotting.general 3.6.10 (2013-02-06)

## Changed

- `create.multiplot`: Added 'xlab.padding', and 'key.bottom.padding'
- Added copyright header to R files

# BoutrosLab.plotting.general 3.6.9 (2013-01-30)

## Changed

- `create.barplot`: Updating with "raster" command
- `create.multiplot`: Making examples prettier, moving key.bottom.padding into argument

# BoutrosLab.plotting.general 3.6.8 (2013-01-30)

## Changed

- `create.barplot`: Merging with `create.barplot`.with.error
- `create.barplot`.with.error: Merging with `create.barplot`
- Create multiplot: Adding 'xaxis.fontface', 'yaxis.fontface' to allow for specification of axis-label fontface

# BoutrosLab.plotting.general 3.6.7 (2013-01-23)

## Changed

- `create.scatterplot`: Added explicit horizontal argument
- `create.barplot`: Added options to specify distance between x-axis and label
- `create.scatterplot`: Add options to pass arbitrary plots onto lattice:xyplot
- `create.barplot`.with.error: Fixing documentation
- `create.barplot`: Allow user to edit 'key.bottom'
- `create.stripplot`: Added parameters to allow for different border/fill colours (for pch 21:25), 'colour.alpha' to allow for plotting character transparencies, 'as.table', 'x.spacing', 'y.spacing' for customization of panels

# BoutrosLab.plotting.general 3.6.6 (2013-01-09)

## Changed

- `create.dotmap`: Added argument 'pch.border.col' to set colour of dot border if pch is 21:25

# BoutrosLab.plotting.general 3.6.5 (2013-01-09)

## Changed

- `create.scatterplot`: Added 'key.left.padding' parameter to place legends to left of graph
- `create.scatterplot`: Added 'axes.lty' to set line style of axes lines
- Force.colour.scheme: Added another colour palette
- `create.multiplot`: Added three parameters - 'xaxis.rot', 'key.left.padding', 'key.right.padding'

# BoutrosLab.plotting.general 3.6.4 (2012-12-14)

## Changed

- `create.heatmap`: Added option of including x and y axis label covariates (between label and heatmap), and examples

# BoutrosLab.plotting.general 3.6.3 (2012-12-14)

## Changed

- Force.colour.scheme: Added alternative colour scheme for annovar output

# BoutrosLab.plotting.general 3.6.2 (2012-12-12)

## Changed

- Get.corr.key: Added missing else case for beta1.p
- Force.colour.scheme: Added synonymous SNV to the list

# BoutrosLab.plotting.general 3.6.1 (2012-12-11)

## Changed

- `create.multiplot`: Changed key.left value to 1 so that legends can be placed to left of multiplot area

# BoutrosLab.plotting.general 3.6.0 (2012-12-11)

## Added

- `create.scatterplot`: Added ability to draw rectangles in background

# BoutrosLab.plotting.general 3.5.17 (2012-12-09)

## Changed

- `create.polygon`: Changed default values of 'xgrid.at' and 'ygrid.at'
- `create.polygon`: Added 'xaxis.fontface' and 'yaxis.fontface'
- `create.polygon`: Added 'fontface = xaxis.fontface' and 'fontface = yaxis.fontface' to scales$x and scales$y
- `create.polygon`: Added example to demonstrate use with legend.grob

# BoutrosLab.plotting.general 3.5.16 (2012-12-08)

## Changed

- `create.polygon`: Adjusted values of par.settings$layout.widths$axis.key.padding and par.settings$layout.heights$axis.key.padding to equal 1 - this lets the legend on the right be properly rendered

# BoutrosLab.plotting.general 3.5.15 (2012-12-07)

## Changed

- `create.barplot`: Added ability to create horizontal barplots - 'plot.horizontal', 'xlimits', and 'xat' parameters added & example

# BoutrosLab.plotting.general 3.5.14 (2012-12-07)

## Changed

- `create.scatterplot`: Fixing example to adjust placement of yaxis title
- `create.dotmap`: Adding parameter 'fill.colour' to allow user to fill background of cells

# BoutrosLab.plotting.general 3.5.13 (2012-11-28)

## Changed

- Force.colour.scheme: Added overload functionality, changed scheme
- Updating date with version

# BoutrosLab.plotting.general 3.5.12 (2012-11-28)

## Changed

- Force.colour.scheme: Added new force colour scheme function

# BoutrosLab.plotting.general 3.5.12 (2012-11-26)

## Changed

- `create.barplot`: Added ability to draw vertical lines

# BoutrosLab.plotting.general 3.5.11 (2012-11-26)

## Changed

- `create.multiplot`: ## Changedd documentation to show to remove tick marks

# BoutrosLab.plotting.general 3.5.10 (2012-11-26)

## Changed

- Get.line.breaks: Added get line breaks function

# BoutrosLab.plotting.general 3.5.9 (2012-11-23)

## Changed

- `create.heatmap`: Added check for ## Changed

# BoutrosLab.plotting.general 3.5.8 (2012-11-23)

## Changed

- `create.polygon`: Reverted functionality back to using if statements instead of ternary operators
- `create.heatmap`: Added covariate bar between heatmap and xaxis labels

# BoutrosLab.plotting.general 3.5.7 (2012-11-13)

## Changed

- `create.dotmap`: Added 'grid.col.lwd' and 'grid.row.lwd' to allow different weights of gridlines

# BoutrosLab.plotting.general 3.5.6 (2012-11-13)

## Changed

- `create.dotmap`: ## Changedd 'key.left', 'key.right', 'key.bottom' to default to 1 so that legends/keys are properly incorporated
- `create.dotmap`: Added example using covariate bar and legend

# BoutrosLab.plotting.general 3.5.5 (2012-11-08)

## Changed

- `create.heatmap`: Use grobPack() fontfamily specification to avoid unknown Arial font specification
- `create.dotmap`: Changed yaxis label default to not be bold
- Legend.grob: Added parameters - 'layout', 'between.col', 'between.row' to allow user to specify layout for multiple legends and set empty space between rows/columns
- Legend.grob: Changed titles in legend to use a text grob instead of `draw.key`
- `create.heatmap`: Added 'legend.layout', 'legend.between.col', and 'legend.between.row' parameters

# BoutrosLab.plotting.general 3.5.4 (2012-11-02)

## Changed

- `create.dotmap`: ## Changedd to use dotmap colour scheme by default, fixed palette creation for >3 colour schemes
- `create.dotmap`: Added example with 5-colour background

# BoutrosLab.plotting.general 3.5.3 (2012-10-30)

## Changed

- `create.heatmap`: Added set.seed() call at beginning of examples

# BoutrosLab.plotting.general 3.5.2 (2012-10-29)

## Changed

- `create.scatterplot`: Fixed codoc error
- General fixes for coding-style standardization and document fixes
- Updates to dependency versions

# BoutrosLab.plotting.general 3.5.1 (2012-10-26)

## Changed

- `create.scatterplot`: Changed default curves to use min/max with na.rm=T

# BoutrosLab.plotting.general 3.5.0 (2012-10-10)

## Added

- Covariates.grob: Added parameters 'grid.row', 'grid.col', 'grid.border' to allow customization of grid lines within covariate bars
- `create.heatmap`: Added parameters 'covariates.grid.row', 'covariates.grid.col', 'covariates.grid.border', 'covariates.top.grid.row', 'covariates.top.grid.col', 'covariates.top.grid.border', 'legend.border', 'legend.border.padding'
- `create.heatmap`: Added parameters 'covariates.padding', and 'covariates.top.padding' to allow for empty space between covariate bars and dendrograms
- Legend.grob: Added parameters 'border', 'border.padding' to allow for border around legend
- Updated examples

# BoutrosLab.plotting.general 3.4.4 (2012-10-10)

## Changed

- Display.statistical.result: Added space before and after "x" sign

# BoutrosLab.plotting.general 3.4.3 (2012-10-10)

## Changed

- `create.heatmap`: Changed 'grid.row' and 'grid.col' to default to false
- `create.heatmap`: No longer assumes that matrices with less than 125 rows/columns should have grid lines

# BoutrosLab.plotting.general 3.4.2 (2012-10-02)

## Changed

- Display.statistical.result: Migrating this function to plotting.general

# BoutrosLab.plotting.general 3.4.1 (2012-09-25)

## Changed

- `create.heatmap`: Added code to close devices opened by the grid::covertUnit function and remove Rplots.pdf files if necessary
- Legend.grob: Added code to remove Rplots.pdf files created by the lattice::`draw.key`

# BoutrosLab.plotting.general 3.4.0 (2012-09-12)

## Added

- Default.colours: Added gender colour palette
- `create.dendrogram`: Interface change - renamed cluster.dimensions (pl.) to cluster.dimension (s.) because only takes scalars
- `create.dendrogram`: Code clean-up
- `create.dendrogram`: Fixed handling of jaccard distance

# BoutrosLab.plotting.general 3.3.7 (2012-09-10)

## Changed

- `create.segplot`: ## Removed warning caused by typo in man file

# BoutrosLab.plotting.general 3.3.6 (2012-08-30)

## Changed

- Legend.grob: Added code to close extra device created by the `draw.key`()

# BoutrosLab.plotting.general 3.3.5 (2012-08-29)

## Changed

- `create.densityplot`: Set groups to an ordered factor (avoids auto-alphabetical ordering by lattice during plotting)

# BoutrosLab.plotting.general 3.3.4 (2012-08-24)

## Changed

- `create.polygon`: Added example

# BoutrosLab.plotting.general 3.3.3 (2012-08-20)

## Changed

- `create.segplot`: ## Removed unused parameter from function definition & fixed inconsistencies

# BoutrosLab.plotting.general 3.3.2 (2012-08-20)

## Changed

- `create.boxplot`: Replaced call to panel.BL.bwplot with call to panel.bwplot

# BoutrosLab.plotting.general 3.3.1 (2012-08-17)

## Changed

- Default.colours: Added new 2-colour scheme for dotmaps
- `create.dotmap`: ## Changedd example to use new dotmap colour scheme from default.colours

# BoutrosLab.plotting.general 3.3.0 (2012-08-17)

## Changed

- `create.segplot`: Added new function to create segplots

# BoutrosLab.plotting.general 3.2.31 (2012-08-16)

## Changed

- `create.heatmap`: Added notes in help file for using euclidean when lots of ties and correlation fails
- Legend.grob: Added notes in help file for using euclidean when lots of ties and correlation fails

# BoutrosLab.plotting.general 3.2.30 (2012-08-14)

## Changed

- `create.violinplot`: Fixed a ## Fixed in extra points argument
- `create.scatterplot`: Added alpha argument to control plotting transparency

# BoutrosLab.plotting.general 3.2.29 (2012-08-14)

## Changed

- `create.dendrogram`: ## Changedd to use dist(method = "jaccard") instead of jaccard.distance()

# BoutrosLab.plotting.general 3.2.28 (2012-08-07)

## Changed

- Default.colours: Changed warning message when survival colour scheme is used with more than 5 colours

# BoutrosLab.plotting.general 3.2.27 (2012-08-02)

## Changed

- `create.polygon`: Fixed border.col argument to allow different coloured borders for multiple groups

# BoutrosLab.plotting.general 3.2.26 (2012-07-31)

## Changed

- `create.dendrogram`: Changed jaccard.distance to use 't(x)' instead of 'x' in order to imitate behaviour of dist function

# BoutrosLab.plotting.general 3.2.25 (2012-07-26)

## Changed

- `create.polygon`: New arguments 'extra.points', 'extra.points.cex', 'extra.points.col', to allow for points plotted on top of main plot
- `create.polygon`: New arguments 'xgrid.at', 'ygrid.at', 'grid.col' to allow for grid lines in the background of the main plot
- `create.violin`.plot: New arguments 'extra.points', 'extra.points.cex', 'extra.points.col', to allow for points plotted on top of main plot
- `create.violin`.plot: New arguments 'fill.colour', 'border.lwd' to allow for control of interior colours and exterior border widths

# BoutrosLab.plotting.general 3.2.24 (2012-07-25)

## Changed

- `create.scatterplot`.with.error: New arguments 'strip.col', 'strip.cex' to control size and background colour of panel labels
- `create.scatterplot`.with.error: New argument 'error.bar.length' to adjust size of error bar whiskers

# BoutrosLab.plotting.general 3.2.23 (2012-07-25)

## Changed

- `create.stripplot`: Added parameters for groups, layout, padding around graph, key and legend, and example for how to use 'groups' parameter

# BoutrosLab.plotting.general 3.2.22 (2012-07-23)

## Changed

- Default.colours: Changed greyscale warning message for survival palette

# BoutrosLab.plotting.general 3.2.21 (2012-07-23)

## Changed

- `create.barplot`: Added example using covariates with labels/covariate legend
- `create.scatterplot`: Added example using covariates with labels/covariate legend

# BoutrosLab.plotting.general 3.2.20 (2012-07-22)

## Changed

- `create.heatmap`: Resolved codoc mismatch introduced in v3.2.19
- Updated package dependencies to newer versions

# BoutrosLab.plotting.general 3.2.19 (2012-07-20)

## Changed

- `create.heatmap`: Replaced arguments 'border' and 'border.lwd' with 'grid.colour' and 'grid.lwd'
- `create.heatmap`: Added ability to turn off interior gridlines using 'grid.row=FALSE', 'grid.col=FALSE' or 'grid.lwd=0'

# BoutrosLab.plotting.general 3.2.18 (2012-07-17)

## Changed

- `create.dendrogram`: ## Changedd to use jaccard.distance instead of vegdist
- Removed vegan dependency from DESCRIPTION

# BoutrosLab.plotting.general 3.2.17 (2012-07-17)

## Changed

- Panel.BL.bwplot: Changed 'nr' to 'nrow' in matrix call to avoid check warnings
- Default.colours: Changed colours in survival palette

# BoutrosLab.plotting.general 3.2.16 (2012-07-17)

## Changed

- Get.corr.key: Added robust regression coefficients and example using outlier
- Added dependency on package MASS

# BoutrosLab.plotting.general 3.2.15 (2012-07-16)

## Changed

- Changing vegan dependency version to be lower
- Default.colours: Added survival palette and explanation in file as to how to add a new palette

# BoutrosLab.plotting.general 3.2.14 (2012-07-12)

## Changed

- Default.colours: Code clean-up and added ability to use chromosome colour scheme

# BoutrosLab.plotting.general 3.2.13 (2012-07-03)

## Changed

- Get.corr.key: Changed default resolution in `create.survival`.plot from 2000 to 100
- `create.histogram`: Added support for horizontal and vertical abline

# BoutrosLab.plotting.general 3.2.12 (2012-06-28)

## Changed

- Default.colours: Fixed return value for Venn diagram palettes to return a list of both fill colours and text colours, as per example in documentation

# BoutrosLab.plotting.general 3.2.11 (2012-06-27)

## Changed

- `create.heatmap`: Added 'text.col' parameter
- `create.heatmap`: Added example of using unicode symbols instead of text - similar to dotmap, but allows clustering

# BoutrosLab.plotting.general 3.2.10 (2012-06-26)

## Changed

- `create.heatmap`: Correct ordering of text in clustered data

# BoutrosLab.plotting.general 3.2.9 (2012-06-15)

## Changed

- `create.scatterplot`: Reordered 'legend' and 'layout.width' arguments to fix padding issues
- `create.scatterplot`.with.error: Reordered 'legend' and 'layout.width' arguments to fix padding issues

# BoutrosLab.plotting.general 3.2.8 (2012-06-12)

## Changed

- `create.stripplot`: Added 'jitter' argument to allow for staggering of data along x-axis

# BoutrosLab.plotting.general 3.2.7 (2012-06-08)

## Changed

- `create.histogram`: Added parameters for padding, spacing, layout, panel
- `create.histogram`: Specifying type = "cairo" for all tiff() calls in Rd files - needed for building package on Mac
- Covariates.grob: Specifying type = "cairo" for all tiff() calls in Rd files - needed for building package on Mac
- `create.multiplot`: Specifying type = "cairo" for all tiff() calls in Rd files - needed for building package on Mac
- Legend.grob: Specifying type = "cairo" for all tiff() calls in Rd files - needed for building package on Mac

# BoutrosLab.plotting.general 3.2.6 (2012-06-05)

## Changed

- `create.scatterplot`: Added option 'abline.v' to draw vertical lines

# BoutrosLab.plotting.general 3.2.5 (2012-05-31)

## Changed

- `create.densityplot`: Fixed ## Fixed when grids are added - now if type=c("g","l"), xyplot.panels do not double call "g" circle symbols instead of lines
- `create.multiple`.boxplots: ## Changedd x-axis label/rotation variable names to match other plotting functions & added argument to allow for specification of main.cex
- `create.stripplot`: ## Changedd x-axis rotation variable name to match other plotting functions & added arguments to specify x/ylab.col, x/yaxis.col/tck/rot
- `create.violinplot`: ## Changedd x-axis label/rotation variable names to match other plotting functions & added arguments to allow for specification of x/ylab.col, x/yaxis.col/tck/rot

# BoutrosLab.plotting.general 3.2.4 (2012-05-29)

## Changed

- `create.histogram`: Added arguments to allow for specification of x/yaxis.col/tck, x/ylab.col, x/yaxis.rot
- `create.polygon`: Added arguments to allow for specification of x/yaxis.col/tck, x/ylab.col, x/yaxis.rot
- `create.qqplot`.comparison: Added arguments to allow for specification of x/yaxis.col/tck, x/ylab.col
- `create.qqplot`.fit: Added arguments to allow for specification of x/yaxis.col/tck, x/ylab.col
- `create.scatterplot`: Added arguments to allow for specification of x/yaxis.col/tck, x/ylab.col
- `create.scatterplot`.with.error: Added arguments to allow for specification of x/yaxis.col/tck, x/ylab.col

# BoutrosLab.plotting.general 3.2.3 (2012-05-28)

## Changed

- `create.barplot`: Added example with covariates

# BoutrosLab.plotting.general 3.2.2 (2012-05-28)

## Changed

- `create.multiplot`: Adjusted formatting and ## Removed de## Fixedging code
- `create.scatterplot`: Added option 'abline.v' to draw vertical lines
- `create.heatmap`: ## Changedd x/y label variable names to match other plotting functions, and added arguments to allow for specification of 'yaxis.rot', 'xaxis.col', 'yaxis.col', 'xlab.col', 'ylab.col', 'main.cex
- `create.hexbinplot`: ## Changedd x/y label variable names to match other plotting functions, and added arguments to allow for specification of 'yaxis.rot', 'xaxis.col', 'yaxis.col', 'xlab.col', 'ylab.col', 'xaxis.tck', 'yaxis.tck'
- `create.dotmap`: ## Changedd x/y label and x/y cex variable names to match other plotting functions, and added arguments to allow for specification of 'yaxis.rot', 'xaxis.col', 'yaxis.col', 'xlab.col', 'ylab.col'

# BoutrosLab.plotting.general 3.2.1 (2012-05-18)

## Changed

- `create.multiplot`: Clarified labels on 6 panel example
- `create.barplot`: ## Changedd examples to use new variables
- `create.densityplot`: ## Changedd examples to include new variables

# BoutrosLab.plotting.general 3.2.0 (2012-05-16)

## Added

- `create.multiplot`: Added example with 6 panels & modified variable names to be consistent
- `create.barplot`: Changed x/yaxis label variable names, ## Changedd to accept arguments for x/yaxis rotation, x/y axis label and tick label colour, and length of x/y axis tick marks
- `create.barplot`.with.error: Changed x/yaxis label variable names, ## Changedd to accept arguments for x/yaxis rotation, x/y axis label and tick label colour, and length of x/y axis tick marks
- `create.boxplot`: Changed x/yaxis label variable names, ## Changedd to accept arguments for x/yaxis rotation, x/y axis label and tick label colour, and length of x/y axis tick marks
- `create.densityplot`: Changed x/yaxis label variable names, ## Changedd to accept arguments for x/yaxis rotation, x/y axis label and tick label colour, and length of x/y axis tick marks

# BoutrosLab.plotting.general 3.1.16 (2012-05-15)

## Changed

- `create.stripplot`: Implemented customizable character colour
- `create.multiple`.stripplots: Implemented customizable character colour

# BoutrosLab.plotting.general 3.1.15 (2012-05-14)

## Changed

- `create.multiplot`: Improved consistency of test output file names and variable names
- `create.densityplot`: Changed defaults to not display axis lines
- `create.hexbinplot`: Changed defaults to not display axis lines
- `create.scatterplot`.with.error: Changed defaults to not display axis lines

# BoutrosLab.plotting.general 3.1.14 (2012-05-11)

## Changed

- `create.dotmap`: Added 'xaxis.lab.cex' and 'yaxis.lab.cex' parameters to specify size of axis labels
- `create.dotmap`: Added 'key.top' parameter to specify size of space between colourkey and xaxis.label

# BoutrosLab.plotting.general 3.1.13 (2012-05-01)

## Changed

- `create.heatmap`: Added 'axes.lwd' parameter to change border width

# BoutrosLab.plotting.general 3.1.12 (2012-04-25)

## Changed

- `create.multiplot`: Added example combining a stripplot and heatmap

# BoutrosLab.plotting.general 3.1.11 (2012-04-23)

## Changed

- `create.heatmap`: Added 'xat' and 'yat' parameters to specify label locations

# BoutrosLab.plotting.general 3.1.10 (2012-04-20)

## Changed

- `create.scatterplot`: Added horizontal abline parameter - able to specify line width and type

# BoutrosLab.plotting.general 3.1.9 (2012-04-19)

## Changed

- `create.dendrogram`: Added 'jaccard' option for calculating distance (for clustering discrete variables)
- `create.heatmap`: Added 'jaccard' option for calculating distance (for clustering discrete variables)
- `create.heatmap`: Added example using covariate legend

# BoutrosLab.plotting.general 3.1.8 (2012-04-18)

## Changed

- `create.heatmap`: ## Changedd parameter for cell text font size & clarified definitions of 'row.lines' and 'col.lines'

# BoutrosLab.plotting.general 3.1.7 (2012-04-13)

## Changed

- `create.heatmap`: Added parameters to control text in cells and to manually specify row/column grid vectors
- Legend.grob: Added parameter to specify border colour of legends

# BoutrosLab.plotting.general 3.1.6 (2012-04-12)

## Changed

- `create.multiplot`: Improved legend functionality
- `create.heatmap`: Added ability to insert text in cells & added example

# BoutrosLab.plotting.general 3.1.5 (2012-04-08)

## Changed

- `create.heatmap`: Added parameters 'title.just' and 'title.fontface' to customize legend titles

# BoutrosLab.plotting.general 3.1.5 (2012-04-03)

## Changed

- `create.heatmap`: Edited documentation to finalize examples

# BoutrosLab.plotting.general 3.1.4 (2012-04-02)

## Changed

- `create.multiplot`: Added parameters for more customization - 'main.cex', 'xlab.cex', 'ylab.cex', 'xlimits', 'ylimits'

# BoutrosLab.plotting.general 3.1.3 (2012-04-19)

## Changed

- `create.dendrogram`: Added 'jaccard' option for calculating distance (for clustering discrete variables)
- `create.heatmap`: Added 'jaccard' option for calculating distance (for clustering discrete variables)
- `create.heatmap`: Added example using covariate legend

# BoutrosLab.plotting.general 3.1.2 (2012-03-30)

## Changed

- `create.heatmap`: ## Changedd examples to be fewer and follow a logical progression
- Covariates.grob: ## Changedd examples to be generated using grid.draw() and produce .tiff output
- Legend.grob: Prevented premature closure of user images by removing "dev.off()" call & ## Changedd examples to .tiff files using grid.draw()

# BoutrosLab.plotting.general 3.1.1 (2012-03-21)

## Changed

- `create.hexbinplot`: Altered layout.widths to automatically adjust key.right if colourkey is added (previously right.padding was adjusted and legend was occasionally cut off)

# BoutrosLab.plotting.general 3.1.0 (2012-03-16)

## Added

- `create.heatmap`: Added ability to take more than three colours

# BoutrosLab.plotting.general 3.0.9 (2012-03-15)

## Changed

- `create.heatmap`: Added parameters 'shrink', 'border', 'border.lwd'
- `create.multiplot`: ## Changedd example to use `create.heatmap` instead of base lattice::levelplot

# BoutrosLab.plotting.general 3.0.8 (2012-03-09)

## Changed

- `create.heatmap`: Added parameter 'force.clustering' to allow user to ignore the row-limitation
- `create.dendrogram`: Added parameter 'force.clustering' to allow user to ignore the row-limitation
- Updated dependency versions

# BoutrosLab.plotting.general 3.0.7 (2012-03-09)

## Changed

- `create.histogram`: ## Removed check of 'x' to ensure it is a formula - problematic is function is called within another function

# BoutrosLab.plotting.general 3.0.6 (2012-02-21)

## Changed

- `create.multiplot`: Changing default of 'xaxis.alternating' parameter
- Modified "see also" documentation of plotting functions to include web versions of lattice graphics

# BoutrosLab.plotting.general 3.0.5 (2012-02-15)

## Changed

- Added "see also" sections of documentation of plotting functions for easy referencing

# BoutrosLab.plotting.general 3.0.4 (2012-02-10)

## Changed

- `create.multiplot`: Added 'panel.widths' parameter & an example to the documentation
- `create.multiplot`: ## Removed internal axes labels in cases where plot axes are identical
- `create.scatterplot`: Fixed file formatting to use tabs

# BoutrosLab.plotting.general 3.0.3 (2012-02-07)

## Changed

- `create.multiplot`: Fixed file formatting to use tabs instead of spaces

# BoutrosLab.plotting.general 3.0.2 (2012-02-02)

## Changed

- `create.multiplot`: Changed name of example output
- `create.heatmap`: Changed default colour scheme to use two colours if data is one-sided and three colours if data is two-sided

# BoutrosLab.plotting.general 3.0.1 (2012-01-25)

## Changed

- `create.heatmap`: ## Changedd documentation to link to related man pages
- Legend.grob: ## Changedd documentation to explain implementation decisions

# BoutrosLab.plotting.general 3.0.0 (2012-01-23)

## Added

- `create.heatmap`: Added parameters 'covariate.legends', 'legend.cex', 'legend.title.cex', and 'legend.side' to allow users to include a legend for the covariates
- Legend.grob: Added this function to draw legends

## Changed

- Updated NAMESPACE to rename `create.plot`.with.levelplot to `create.multiplot`

# BoutrosLab.plotting.general 2.13.1 (2012-01-16)

## Changed

- `create.histogram`: Added 'x' parameter to take in formulas, leaving 'data' parameter to take in data

# BoutrosLab.plotting.general 2.13.0 (2012-01-16)

## Added

- `create.multiplot`: Renamed this function - previously called '`create.plot`.with.levelplot'
- `create.multiplot`: ## Changedd to take any number of plots and display them in the specified layout
- `create.multiplot`: Replaced parameters 'plot.object' and 'levelplot.object' with 'plot.objects
- `create.multiplot`: Replaced parameter 'spacing' with 'yspacing'
- `create.multiplot`: Added parameters 'xspacing' and 'plot.layout'

# BoutrosLab.plotting.general 2.12.5 (2012-01-11)

## Changed

- `create.barplot`: Added parameter 'border.col' to control border colour

# BoutrosLab.plotting.general 2.12.4 (2012-01-11)

## Changed

- `create.barplot`: Added parameters 'abline.h', 'abline.type', 'abline.lwd', 'abline.col' to add superimposed line

# BoutrosLab.plotting.general 2.12.3 (2012-01-11)

## Changed

- `create.boxplot`: Added parameters 'abline.h', 'abline.type', 'abline.lwd', 'abline.col' to add superimposed line

# BoutrosLab.plotting.general 2.12.2 (2012-01-10)

## Changed

- `create.barplot`: Added 'panel.abline'
- `create.boxplot`: Added 'panel.abline'
- `create.heatmap`: Added parameters 'xlab', 'ylab' and ability to place x-axis label on top axis
- `create.plot`.with.levelplot: Added error checking for trellis objects

# BoutrosLab.plotting.general 2.12.1 (2012-01-10)

## Changed

- `create.heatmap`: Added ability to plot covariates without dendrograms
- `create.heatmap`: Added parameters 'row.dendrogram' and 'col.dendrogram' to allow users to pass in dendrograms
- Added two functions - `create.dendrogram`() and covariates.grob()

# BoutrosLab.plotting.general 2.12.0 (2012-01-10)

## Changed

- Get.defaults: Set unix to fall back on default fonts

# BoutrosLab.plotting.general 2.11.10 (2012-01-06)

## Changed

- `create.plot`.with.levelplot: ## Changedd border example

# BoutrosLab.plotting.general 2.11.9 (2012-01-04)

- Committing new version

# BoutrosLab.plotting.general 2.11.8 (2012-01-04)

## Changed

- `create.plot`.with.levelplot: Improved ability to control axis labels
- `create.plot`.with.levelplot: Added padding parameters

# BoutrosLab.plotting.general 2.11.7 (2011-12-23)

## Changed

- Minor change to DESCRIPTION

# BoutrosLab.plotting.general 2.11.6 (2011-12-22)

## Changed

- `create.scatterplot`: ## Changedd to allow for parameters of length one specify properties of all curves with 'add.curves' option
- `create.scatterplot`: Added warning for cases when curves are drawn with data containing groups where grouping variables are being ## Removed

# BoutrosLab.plotting.general 2.11.5 (2011-12-16)

## Changed

- `create.plot`.with.levelplot: Reformatted example code

# BoutrosLab.plotting.general 2.11.4 (2011-12-15)

## Changed

- `create.plot`.with.levelplot: Improved readability of parameter list
- `create.scatterplot`: ## Changedd panel.curve code to avoid 'no visible binding for global variable x' error

# BoutrosLab.plotting.general 2.11.3 (2011-12-13)

## Changed

- `create.scatterplot`.with.error: Minor formatting fix to example
- `create.scatterplot`: Added parameter 'key.top' to control spacing above the key

# BoutrosLab.plotting.general 2.11.2 (2011-12-13)

## Changed

- `create.heatmap`: Renamed parameter 'data.to.cluster' with 'x'

# BoutrosLab.plotting.general 2.11.1 (2011-12-13)

## Changed

- `create.heatmap`: Cleaned up code by removing spurious print statements

# BoutrosLab.plotting.general 2.11.0 (2011-12-13)

## Added

- Updated many plotting examples have better clarity and use default.colours()

## Changed

- Default.colours: Standardized error-handling
- `create.dotmap`: Changed default colour scheme

# BoutrosLab.plotting.general 2.10.12 (2011-12-13)

## Changed

- `create.heatmap`: Renamed parameters 'names.row' and 'names.col' with 'xaxis.lab' and 'yaxis.lab'
- `create.heatmap`: Added new parameter 'xaxis.col' to set colours of axis labels

# BoutrosLab.plotting.general 2.10.11 (2011-12-13)

## Changed

- `create.heatmap`: Cleaned up code by standardizing variable names and adjusting error-handling

# BoutrosLab.plotting.general 2.10.10 (2011-12-12)

## Changed

- Optimal.heatmap.cex.txt: Improved formatting of header
- `create.heatmap`: Cleaned up code
- `create.heatmap`: Added example of how to specify row and column labels

# BoutrosLab.plotting.general 2.10.9 (2011-12-12)

## Changed

- `create.scatterplot`: Changed formatting to produce a smaller gap between title and plot
- `create.scatterplot`: ## Changedd example to include legend and filename

# BoutrosLab.plotting.general 2.10.8 (2011-12-06)

## Changed

- `create.scatterplot`: Cleaned up code by removing inverted line
- Updated default value of 'strip.col' to 'white' instead of 'grey'

# BoutrosLab.plotting.general 2.10.7 (2011-12-02)

## Changed

- `create.scatterplot`: Added example to draw colour depth to scatterpoints

# BoutrosLab.plotting.general 2.10.6 (2011-12-01)

## Fixed

- `create.scatterplot`: Fixed ## Fixed in default behaviour for 'curve.from' and 'curve.to' parameters

## Changed

- `create.scatterplot`: ## Changedd example

# BoutrosLab.plotting.general 2.10.5 (2011-12-01)

## Changed

- `create.scatterplot`: Cleaned up code

# BoutrosLab.plotting.general 2.10.4 (2011-11-30)

## Changed

- `create.scatterplot`: Replace parameter 'add.lines' with 'add.curves', this allowing the user to draw any segment of any curve that can be specified with a function
- `create.scatterplot`: Added example to demonstrate use of 'add.curves' parameter
- `create.polygon`: Fixed 'alternating' parameter to force x-axis labels to be on the bottom row
- `create.polygon`: ## Changedd example

# BoutrosLab.plotting.general 2.10.3 (2011-11-29)

## Changed

- `create.polygon`: ## Changedd 'top.padding' and 'bottom.padding' parameters
- `create.scatterplot`: Added ability to draw line segments on plotting region

# BoutrosLab.plotting.general 2.10.2 (2011-11-17)

## Changed

- `create.violin`.plot: ## Changedd to allow boundaries and scaling

# BoutrosLab.plotting.general 2.10.1 (2011-11-17)

## Changed

- `create.plot`.with.levelplot: Added this function by merging '`create.boxplot`.with.levelplot' and '`create.scatterplot`.with.levelplot'

# BoutrosLab.plotting.general 2.10.0 (2011-10-27)

## Added

- `create.scatterplot`.with.levelplot: Added this function

# BoutrosLab.plotting.general 2.9.44 (2011-10-24)

## Changed

- Temporary change to lattice dependency version

# BoutrosLab.plotting.general 2.9.43 (2011-10-12)

## Changed

- `create.heatmap`: Added parameters to support ?dist based distance measures for clustering
- `create.hexbinplot`: Added warning if specified 'maxcnt' is less than the actual max count

# BoutrosLab.plotting.general 2.9.42 (2011-09-28)

## Changed

- `create.boxplot`: ## Changedd to force axis labels to always be on the left or bottom

# BoutrosLab.plotting.general 2.9.41 (2011-09-27)

## Changed

- Added 'as.table' parameter to plotting functions to allow for panel ordering

# BoutrosLab.plotting.general 2.9.40 (2011-09-21)

## Changed

- `create.qqplot`.comparison: Added option of choosing difference methods to draw a reference line
- `create.qqplot`.fit: Added option of choosing difference methods to draw a reference line
- `create.qqplot`.fit: Added ability to draw confidence bands for grouped data

# BoutrosLab.plotting.general 2.9.39 (2011-09-20)

## Changed

- `create.dotmap`: Standardized parameter names

# BoutrosLab.plotting.general 2.9.38 (2011-09-14)

## Changed

- `create.dotmap`: Added example of using 'padding.text'

# BoutrosLab.plotting.general 2.9.37 (2011-09-13)

## Changed

- `create.qqplot`.fit: Added polygon plots for confidence intervals
- `create.scatterplot`: Fixed 'add.axes' parameter default value in documentation

# BoutrosLab.plotting.general 2.9.36 (2011-09-13)

## Changed

- `create.scatterplot`: Changed default value of 'add.axes' parameter to FALSE
- Default.colours: Change output format to be returned, rather than printed
- `create.polygon`: Added 'use.loess.median' and 'use.loess.border' parameters

# BoutrosLab.plotting.general 2.9.35 (2011-09-08)

## Changed

- Change dependency version for package BoutrosLab.statistics.general

# BoutrosLab.plotting.general 2.9.34 (2011-09-05)

## Changed

- `create.barplot`: Added parameters to change colour and font size of the panel title
- `create.boxplot`: Added parameters to change colour and font size of the panel title
- `create.polygon`: Added parameters to change colour and font size of the panel title
- `create.scatterplot`: Added parameters to change colour and font size of the panel title

# BoutrosLab.plotting.general 2.9.33 (2011-09-01)

## Changed

- Added 'rgb.to.greyscale' function to NAMESPACE

# BoutrosLab.plotting.general 2.9.32 (2011-09-01)

## Fixed

- `create.hexbinplot`: ## Removed appearance of artificial blue dot at origin

# BoutrosLab.plotting.general 2.9.31 (2011-09-01)

## Changed

- Rgb.to.greyscale: Added function to BoutrosLab.plotting.general

# BoutrosLab.plotting.general 2.9.30 (2011-08-30)

## Changed

- `create.qqplot`.fit: Changes to ensure expected functionality

# BoutrosLab.plotting.general 2.9.29 (2011-08-30)

## Changed

- `create.qqplot`.fit: Deleted function 'single.qqplot.CI' and revised corresponding part in function '`create.qqplot`.fit'

# BoutrosLab.plotting.general 2.9.28 (2011-08-30)

## Changed

- Updated maintainer email
- Removed dependency on package 'distr'

# BoutrosLab.plotting.general 2.9.27 (2011-08-26)

## Changed

- `create.histogram`: Changed formatting of documentation
- `create.multiple`.stripplots: Added 'ylab.axis.padding' parameter

# BoutrosLab.plotting.general 2.9.26 (2011-08-26)

## Changed

- Default.colours: Added new function to provide colour schemes for plotting

# BoutrosLab.plotting.general 2.9.25 (2011-08-25)

## Changed

- Optimal.heatmap.cex.txt: Added lookup file for setting default font sizes for heatmaps of varying size
- `create.heatmap`: Changed to set default font sizes from lookup file (based on size of input)

# BoutrosLab.plotting.general 2.9.24 (2011-08-24)

## Changed

- `create.polygon`: Cleaned up code

# BoutrosLab.plotting.general 2.9.23 (2011-08-24)

## Changed

- `create.multiple`.boxplots: Added 'ylab.axis.padding' and 'ylab.cex' parameters

# BoutrosLab.plotting.general 2.9.22 (2011-08-24)

## Changed

- `create.dotmap`: Changed default grid line colour to grey when no background colour is present

# BoutrosLab.plotting.general 2.9.21 (2011-08-23)

## Changed

- Single.qqplot.CI: ## Removed 'subset' function

# BoutrosLab.plotting.general 2.9.20 (2011-08-23)

## Changed

- `create.qqplot`.comparison: Fixed examples

# BoutrosLab.plotting.general 2.9.19 (2011-08-23)

## Changed

- `create.qqplot`.comparison: Fixed examples

# BoutrosLab.plotting.general 2.9.18 (2011-08-23)

## Changed

- `create.qqplot`.comparison: Added functionality of drawing confidence bands
- `create.qqplot`.comparison: Changed label default behaviour
- `create.qqplot`.fit: Changed label default behaviour
- `create.dotmap`: Changed default grid line colour to black if background colour is present
- `create.polygon`: Fixed grouping functionality and example
- `create.polygon`: Added translucent colours

# BoutrosLab.plotting.general 2.9.17 (2011-08-16)

## Changed

- `create.polygon`: Changed input parameter settings to be more intuitive
- `create.polygon`: Added examples (including grouping example)

# BoutrosLab.plotting.general 2.9.16 (2011-08-16)

## Changed

- `create.scatterplot`: ## Removed redundancy in code by parameterizing y=x line, and changed example

# BoutrosLab.plotting.general 2.9.15 (2011-08-15)

## Changed

- `create.boxplot`.with.levelplot: Added more examples

# BoutrosLab.plotting.general 2.9.14 (2011-08-13)

## Changed

- `create.qqplot`.fit: Improvements to code formatting
- `create.qqplot`.comparison: Improvements to code formatting
- Single.qqplot.CI: Improvements to code formatting

# BoutrosLab.plotting.general 2.9.13 (2011-08-11)

## Changed

- `create.hexbinplot`: Added 'xaxis.labels' and 'yaxis.labels' parameters
- `create.boxplot`.with.levelplot: Improved example

# BoutrosLab.plotting.general 2.9.12 (2011-08-11)

## Changed

- `create.barplot`: Added panel parameters
- `create.scatterplot`: Added panel parameters and improved input method of 'groups' parameter
- Boxplot.with.levelplot: Cleaned up example names
- `create.polygon`: Cleaned up example names

# BoutrosLab.plotting.general 2.9.11 (2011-08-10)

## Changed

- `create.scatterplot`: Added 'layout' and 'xaxis.tck' parameters to specify figure arrangement and length of tick marks
- `create.scatterplot`: Added example to show lines connecting dots to the x-axis
- Get.corr.key: Added 'key.cex' parameter to allow users to specify font size of the key

# BoutrosLab.plotting.general 2.9.10 (2011-08-08)

## Changed

- `create.qqplot`.fit: Added example
- Single.qqplot.CI: Renamed from 'qq.single.CI' and ## Changedd examples and documentation

# BoutrosLab.plotting.general 2.9.9 (2011-08-08)

## Changed

- Qq.single.CI: Renamed from 'envelop' function
- `create.qqplot`.comparison: Renamed from '`create.qqplot2`' function
- `create.qqplot`.fit: Renamed from '`create.qqplot`' function
- `create.scatterplot`: ## Changedd example output names for consistency

# BoutrosLab.plotting.general 2.9.8 (2011-08-06)

## Changed

- `create.qqplot`: Code-review
- `create.qqplot2`: Code-review

# BoutrosLab.plotting.general 2.9.7 (2011-08-05)

## Changed

- Envelope: Added function to create one-sample qqplot
- `create.qqplot`: ## Changedd code and documentation
- `create.qqplot2`: ## Changedd code and documentation

# BoutrosLab.plotting.general 2.9.6 (2011-08-05)

## Changed

- `create.qqplot2`: Added function to create two-sample qq plots
- `create.qqplot`: Added function to create one-sample qq plots
- `create.boxplot`.with.levelplot: ## Changedd documentation
- `create.polygon`: ## Changedd documentation

# BoutrosLab.plotting.general 2.9.5 (2011-08-03)

## Changed

- `create.heatmap`: Added 'print.color.key' parameter to enable turning "off" of colourkey printing

# BoutrosLab.plotting.general 2.9.4 (2011-08-03)

## Changed

- `create.boxplot`.with.levelplot: Added new function
- `create.polygon`: Added new function

# BoutrosLab.plotting.general 2.9.3 (2011-08-02)

## Changed

- `create.violin`.plot: ## Changedd examples
- Updated package dependencies

# BoutrosLab.plotting.general 2.9.2 (2011-07-25)

## Changed

- `create.dotmap`: ## Changedd documentation
- Updated package dependencies

# BoutrosLab.plotting.general 2.9.1 (2011-07-23)

## Changed

- `create.boxplot`: Added 'x.tck' parameter to control length of tick marks

# BoutrosLab.plotting.general 2.9.0 (2011-07-21)

## Changed

- `create.dotmap`: Added new plotting function

# BoutrosLab.plotting.general 2.8.3 (2011-07-14)

## Changed

- `create.boxplot`: Added panel parameters
- `create.heatmap`: Added 'x.alternating' parameter to control position of x-axis labels
- Added warning to documentation of plotting functions to alert users of ineffective methods of running functions

# BoutrosLab.plotting.general 2.8.2 (2011-06-27)

## Changed

- `create.multiple`.stripplots: Added 'type' parameter to allow users to change the type of plotting characters
- `create.heatmap`: Added 'xlab' and 'ylab' parameters

# BoutrosLab.plotting.general 2.8.1 (2011-06-22)

## Changed

- `create.heatmap`: Added functionality of removing column and/or row grid-lines
- Updated package dependency versions

# BoutrosLab.plotting.general 2.8.0 (2011-06-17)

## Added

- `create.multiple`.stripplots: Added new plotting function to create multiple stripplots

## Changed

- `create.multiple`.boxplots: Added 'header.cex' parameter to control font size of the header

# BoutrosLab.plotting.general 2.7.7 (2011-06-16)

## Changed

- `create.violin`.plot: Added 'xaxis.labels' and 'yaxis.labels' parameters to control axes labelling
- `create.heatmap`: Added 'top.padding' and 'bottom.padding' parameters to control top and bottom margins

# BoutrosLab.plotting.general 2.7.6 (2011-06-08)

## Changed

- Updated plotting functions for NAMESPACE

# BoutrosLab.plotting.general 2.7.5 (2011-06-08)

## Changed

- `create.histogram`: Added 'lwd' and 'lty' parameters to control line width and line style
- `create.histogram`: Fixed code to work without axes labels
- `create.scatterplot`: Added 'lty' parameter to control line style and adjusted example
- `create.scatterplot`.with.error: Added 'lty' parameter and adjusted example

# BoutrosLab.plotting.general 2.7.4 (2011-06-07)

## Changed

- Added NAMESPACE

# BoutrosLab.plotting.general 2.7.3 (2011-05-31)

## Changed

- Get.corr.key: Fixed syntax error in example
- `create.boxplot`: Reverted functionality to use panel.BL.bwplot
- `create.scatterplot`: ## Changedd examples
- `create.histogram`: ## Changedd examples
- `create.scatterplot`.with.error: ## Changedd examples
- `create.violinplot`: ## Changedd examples

# BoutrosLab.plotting.general 2.7.2 (2011-05-19)

## Changed

- `create.heatmap`: Added 'right.padding' and 'left.padding' parameters to control margins

# BoutrosLab.plotting.general 2.7.1 (2011-05-17)

## Changed

- Get.corr.key: Cleaned code
- `create.hexbinplot`: Cleaned code and fixed links in documentation

# BoutrosLab.plotting.general 2.7.0 (2011-05-12)

## Fixed

- `create.hexbinplot`: Changed implementation to address critical issues related to 'maxcnt' argument

## Changed

- `create.hexbinplot`: ## Changedd documentation with links and example
- `create.barplot`: ## Changedd example
- `create.densityplot`: ## Changedd example

# BoutrosLab.plotting.general 2.6.7 (2011-05-11)

## Changed

- Get.corr.key: Added 'alpha.background' parameter
- `create.hexbinplot`: Development version - ## Fixed related to 'maxcnt' parameter

# BoutrosLab.plotting.general 2.6.6 (2011-05-02)

## Changed

- Get.corr.key: Fixed error
- `create.hexbinplot`: Added 'grid' and 'abline' parameters and corresponding example

# BoutrosLab.plotting.general 2.6.5 (2011-05-02)

## Changed

- `create.hexbinplot`: Added 'colorcut', 'mincnt', 'maxcnt' parameters to allow user to specify number of bins in plot

# BoutrosLab.plotting.general 2.6.4 (2011-04-28)

## Changed

- Get.corr.key: Improved code formatting
- Get.corr.key: Added 'beta0' and 'beta1' options to the key
- `create.hexbinplot`: Added 'aspect' parameter to adjust plot size

# BoutrosLab.plotting.general 2.6.3 (2011-04-21)

## Changed

- `create.hexbinplot`: Added 'xaxis.cex' and 'yaxis.cex' parameters

# BoutrosLab.plotting.general 2.6.2 (2011-04-04)

## Changed

- `create.boxplot`: ## Changedd code to remove unnecessary dependencies
- `create.boxplot`: Added 'fill' parameter to control the fill colour and corresponding example
- `create.scatterplot`: ## Changeds to the examples

# BoutrosLab.plotting.general 2.6.1 (2011-04-01)

## Changed

- `create.dotmap`: Reintroduced 'key' parameter into function
- `create.dotmap`: Added 'legend' parameter to allow for multiple keys

# BoutrosLab.plotting.general 2.6.0 (2011-03-13)

## Added

- `create.scatterplot`: ## Changedd functionality to allow for log-scale x- and y-axes

## Changed

- `create.scatterplot`: Cleaned up documentation

# BoutrosLab.plotting.general 2.5.7 (2011-03-11)

## Changed

- `create.densityplot`: Added 'xaxis.labels' and 'yaxis.labels' parameters

# BoutrosLab.plotting.general 2.5.6 (2011-03-10)

## Changed

- `create.violin`.plot: Added example using legend

# BoutrosLab.plotting.general 2.5.5 (2011-03-10)

## Changed

- `create.scatterplot`.with.error: Added example using legend
- `create.scatterplot`: ## Changedd example

# BoutrosLab.plotting.general 2.5.4 (2011-03-10)

## Changed

- `create.hexbinplot`: Added example using legend
- `create.histogram`: Added example using legend

# BoutrosLab.plotting.general 2.5.3 (2011-03-10)

## Changed

- `create.boxplot`: Added example using legend
- `create.densityplot`: Added example using legend

# BoutrosLab.plotting.general 2.5.2 (2011-03-10)

## Changed

- `create.barplot`.with.error: Added example using legend

# BoutrosLab.plotting.general 2.5.1 (2011-03-10)

## Changed

- `create.barplot`: Added example using legend

# BoutrosLab.plotting.general 2.5.0 (2011-03-09)

## Changed

- `create.barplot`.with.error: ## Changedd example

# BoutrosLab.plotting.general 2.4.0 (2011-03-08)

## Changed

- `create.dotmap`: ## Removed 'legend' parameter
- `create.scatterplot`: ## Changedd example filenames
- `create.scatterplot`: Added example using legend
- `create.barplot`.with.error: Cleaned up code
- `create.densityplot`: ## Changedd code to use a single panel function

# BoutrosLab.plotting.general 2.3.3 (2011-03-03)

## Changed

- `create.heatmap`: ## Changedd examples to be sequential and added example using covariates

# BoutrosLab.plotting.general 2.3.2 (2011-02-23)

## Changed

- `create.scatterplot`: Added 'abline' argument
- `create.scatterplot`.with.error: Added 'abline' argument

# BoutrosLab.plotting.general 2.3.1 (2011-02-23)

## Changed

- `create.scatterplot`: Added ability to add multiple keys to a single plot

# BoutrosLab.plotting.general 2.3.0 (2011-02-22)

## Changed

- `create.scatterplot`: Cleaned up code to use a single panel function
- `create.scatterplot`.with.error: Cleaned up code to use a single panel function
- `create.scatterplot`.with.error: ## Changedd formatting to use tabs instead of spaces
- `create.scatterplot`.with.error: Cleaned up documentation and examples

## Fixed

- `create.scatterplot`.with.error: Fixed ## Fixed that prevented keys from being plotted

# BoutrosLab.plotting.general 2.2.0 (2011-02-22)

## Changed

- `create.densityplot`: Changed 'xat' and 'yat' parameters into 'xgrid.at' and 'ygrid.at' parameters to specify grid lines and added example
- `create.scatterplot`: Changed 'xat' and 'yat' parameters into 'xgrid.at' and 'ygrid.at' parameters to specify grid lines and added example
- `create.scatterplot`.with.error: Changed 'xat' and 'yat' parameters into 'xgrid.at' and 'ygrid.at' parameters to specify grid lines and added example

# BoutrosLab.plotting.general 2.1.0 (2011-02-22)

## Added

- Added 'legend' and 'key' parameters to plotting functions

## Changed

- `create.scatterplot`: Added example using a legend

# BoutrosLab.plotting.general 2.0.24 (2011-02-21)

## Fixed

- Get.corr.key: Fixed ## Fixed related to using phantom instead of phantom(0)

# BoutrosLab.plotting.general 2.0.23 (2011-02-19)

## Changed

- `create.barplot`: ## Changedd to allow use of non-factorial variables for grouping

# BoutrosLab.plotting.general 2.0.22 (2011-02-17)

## Fixed

- `create.scatterplot`.with.error: Fixed error related to grid

# BoutrosLab.plotting.general 2.0.21 (2011-02-14)

## Fixed

- `create.scatterplot`: Fixed error related to 'ylimits' and 'xlimits', 'yat' and 'xat
- `create.scatterplot`.with.error: Fixed error related to grouped line graphs

# BoutrosLab.plotting.general 2.0.20 (2011-02-13)

## Changed

- `create.barplot`: Changed to prevent spurious warnings when specifying colours with ungrouped data

# BoutrosLab.plotting.general 2.0.19 (2011-02-11)

## Changed

- `create.heatmap`: ## Changedd examples

# BoutrosLab.plotting.general 2.0.18 (2011-02-11)

## Changed

- `create.heatmap`: Cleaned up code to remove deprecated parameter 'colour.number.tweak'

# BoutrosLab.plotting.general 2.0.17 (2011-02-09)

## Changed

- `create.scatterplot`: Cleaned up examples to all use the same dataset

# BoutrosLab.plotting.general 2.0.16 (2011-02-07)

## Changed

- `create.scatterplot`.with.error: Cleaned up code to match documentation

# BoutrosLab.plotting.general 2.0.15 (2011-02-04)

## Changed

- Generate.at.final: Added function to add tick marks to plots

# BoutrosLab.plotting.general 2.0.14 (2011-01-31)

## Changed

- Get.corr.key: Added documentation to function for automatically generating a legend with correlation, beta, and p-values

# BoutrosLab.plotting.general 2.0.13 (2011-01-31)

## Changed

- Get.corr.key: ## Changedd to code
- Updated required packages

# BoutrosLab.plotting.general 2.0.12 (2011-01-25)

## Changed

- Generate.at.final: Added documentation
- Get.corr.key: Added option of including beta

## Fixed

- `create.densityplot`: Addressed problem of grid drawing and tick mark locations not lining up when both are required - this achieved by removing default grid lines and redrawing them using panel.abline() at user-specified locations

# BoutrosLab.plotting.general 2.0.11 (2011-01-17)

## Changed

- Get.corr.key: Added flexibility to have rho in Greek
- Write.plot: Added functionality to call CairoTIFF if default tiff has problems
- `create.dotmap`: ## Changedd default value of 'xaxis.lab' and 'yaxis.lab' to NULL in order to accept expressions

# BoutrosLab.plotting.general 2.0.10 (2011-01-07)

## Changed

- Get.corr.key: Added function to add basic correlation key to scatter-like plots
- `create.barplot`: ## Changedd documentation and examples to use more complex datasets and demonstrate use of 'ylim' and 'yat' parameters
- `create.barplot`: Added 'key' parameter
- `create.barplot`: Changed 'groups' parameter to accept an expression or variable

# BoutrosLab.plotting.general 2.0.9 (2010-12-22)

## Changed

- `create.barplot`: Added filename to stacked barplot examples

## Fixed

- `create.barplot`: Fixed problem with groups
- `create.barplot`.with.error: Fixed problem with groups

# BoutrosLab.plotting.general 2.0.8 (2010-12-21)

## Changed

- `create.barplot`: Added user-control for colour picking for groups
- `create.barplot`.with.error: Added user-control for colour picking for groups
- `create.barplot`: Added 'group' and 'stack' parameters

# BoutrosLab.plotting.general 2.0.7 (2010-11-29)

## Changed

- `create.scatterplot`: Added ability to rotate text in axis labels

# BoutrosLab.plotting.general 2.0.6 (2010-11-17)

## Changed

- `create.scatterplot`.with.error: Added group-specific error bar colour handling under no conditioning

# BoutrosLab.plotting.general 2.0.5 (2010-11-17)

## Changed

- `create.scatterplot`.with.error: Added 'add.axes' parameter

# BoutrosLab.plotting.general 2.0.4 (2010-11-16)

## Changed

- `create.scatterplot`.with.error: Added ability to handle grouped data

# BoutrosLab.plotting.general 2.0.3 (2010-11-15)

## Changed

- `create.scatterplot`: Added 'xaxis.labels' and 'yaxis.labels' parameter to allow customization of axis labels
- Get.defaults: ## Changedd to handle font changes with R v2.12.0
- `create.dotmap`: ## Changedd examples

# BoutrosLab.plotting.general 2.0.2 (2010-11-15)

## Changed

- `create.scatterplot`: Added ability to handle grouped data

# BoutrosLab.plotting.general 2.0.1 (2010-11-11)

## Changed

- Updated platform-specific code to work better with changes in R v2.12.0

# BoutrosLab.plotting.general 2.0.0 (2010-11-02)

## Added

- Updated package dependencies
- Updated all plotting functions to take a main title
- Get.defaults: Fixed ## Fixed related to changes in newest versions of R and lattice
- `create.heatmap`: ## Changedd colouring functionality to reduce possibility of error
- `create.histogram`: Added functionality of colouring bars
- `create.hexbinplot`: Added functionality of specifying label sizes

# BoutrosLab.plotting.general 1.11.2 (2010-10-26)

## Changed

- `create.boxplot`: Added 'lwd' parameter to support production of smaller images

# BoutrosLab.plotting.general 1.11.1 (2010-10-20)

## Changed

- `create.boxplot`: ## Changedd to facilitate horizontal boxplots annotations for x-axis

# BoutrosLab.plotting.general 1.11.0 (2010-10-14)

## Added

- Write.plot: Added new function to standardize plotting output - now used in all plotting functions

## Changed

- `create.barplot`.with.error: Changed default line width of bar outline to match line width of error bar
- `create.barplot`: ## Changedd layout of figure to match other plotting functions
- Removed 'compression' parameter from plotting functions

# BoutrosLab.plotting.general 1.10.1 (2010-09-23)

## Changed

- `create.multiple`.boxplots: Reordered parameters to be consisting with other plotting functions
- `create.multiple`.boxplots: Added 'symbol.cex' parameter to control size of boxplot dots

# BoutrosLab.plotting.general 1.10.0 (2010-09-22)

## Fixed

- `create.boxplot`: Fixed so that 'ylimits' argument is no longer silently ignored

# BoutrosLab.plotting.general 1.9.21 (2010-09-16)

## Changed

- `create.violin`.plot: ## Changedd documentation to clarify circumstances when 'x' will be coerced to a factor or shingle (when horizontal argument set to FALSE)
- `create.hexbinplot`: Added 'compression' and 'add.axes' parameters

# BoutrosLab.plotting.general 1.9.20 (2010-09-15)

## Changed

- `create.multiple`.boxplots: ## Changedd 'right.padding' to be set to 2
- `create.barplot`.with.error: Renamed function from `create.barplot`.with.error.r into `create.barplot`.with.error.R

# BoutrosLab.plotting.general 1.9.19 (2010-09-12)

## Changed

- Updated documentation of plotting functions
- Updated plotting functions to output a trellis object by default

# BoutrosLab.plotting.general 1.9.18 (2010-09-03)

## Fixed

- `create.barplot`: Fixed ## Fixed that prevented implicit grouping from working
- `create.barplot`.with.error: Fixed ## Fixed that prevented implicit grouping from working
- `create.scatterplot`.with.error: Fixed ## Fixed that prevented implicit grouping from working

# BoutrosLab.plotting.general 1.9.17 (2010-08-31)

## Fixed

- `create.scatterplot`: Fixed ## Fixed that prevented implicit grouping from working

# BoutrosLab.plotting.general 1.9.16 (2010-08-27)

## Changed

- `create.barplot`.with.error: Added function to create barplots with customizable error bars

# BoutrosLab.plotting.general 1.9.15 (2010-08-26)

## Changed

- `create.violinplot`: Added 'horizontal' parameter to allow user to specify whether to plot horizontally or vertically
- `create.scatterplot`.with.error: Added 'y.error.up', 'y.error.down', 'x.error.right', 'x.error.left' parameter to allow for drawing horizontal and/or vertical error bars (## Removed 'error.plus' and 'error.minus' parameters)
- `create.scatterplot`.with.error: Added 'x.error.bar.col' and 'y.error.bar.col' parameters
- `create.scatterplot`.with.error: Added 'error.whisker.angle' parameter to control the angle of whiskers drawn at both ends of the error bars

# BoutrosLab.plotting.general 1.9.14 (2010-08-19)

## Changed

- `create.boxplot`: Added 'xaxis.labels' parameter

# BoutrosLab.plotting.general 1.9.13 (2010-08-15)

## Changed

- Added warnings framework to plotting functions
- Updated functions to correctly deal with postscript graphics device
- Reimplemented fonts mechanism to support Win/\*Nix systems correctly w.r.t postscript specific exceptions
- `create.heatmap`: Added 'cor.method' parameter

## Fixed

- Panel.BL.bwplot: Fixed function to show outlier points properly when input argument box.colour is a vector

# BoutrosLab.plotting.general 1.9.12 (2010-08-07)

## Changed

- `create.barplot`: ## Changedd default to give x-axis and y-axis scale labels

# BoutrosLab.plotting.general 1.9.11 (2010-08-07)

## Changed

- Updated plotting functions to offer OS specific settings framework

# BoutrosLab.plotting.general 1.9.10 (2010-08-07)

## Changed

- Updated plotting function documentation with "See Also" links and standardizations to the "Value" section
- `create.heatmap`: Renamed 'legend.cex' parameter to 'colorkey.cex'

# BoutrosLab.plotting.general 1.9.9 (2010-08-06)

## Changed

- Updated plotting functions to have the same default font type

# BoutrosLab.plotting.general 1.9.8 (2010-08-05)

## Fixed

- `create.heatmap`: Fixed functionality of 'colour.alpha' parameter

## Changed

- `create.heatmap`: Added example to demonstrate usage of 'colour.alpha' parameter

# BoutrosLab.plotting.general 1.9.7 (2010-08-04)

## Changed

- `create.scatterplot`.with.error: Added function to create scatterplot with error bars overlaid

# BoutrosLab.plotting.general 1.9.6 (2010-08-04)

## Changed

- `create.barplot`: Added 'yaxis.labels' parameter
- `create.scatterplot`: Changed panel function to allow for ## Changed() to change conditioned trellis object
- `create.multiple`.boxplots: Changed c() to stack() to accommodate data.frames
- `create.violin`.plot: ## Changedd to match other plotting functions

# BoutrosLab.plotting.general 1.9.5 (2010-07-29)

## Changed

- `create.barplot`: Added 'col' parameter to control the fill colour of bars
- `create.scatterplot`: Added default values for some parameters
- `create.stripplot`: Added default values for some parameters
- `create.violinplot`: Added default values for some parameters

# BoutrosLab.plotting.general 1.9.4 (2010-07-26)

## Changed

- `create.barplot`: Renamed parameters to standardize with other plotting functions
- `create.multiple`.boxplots: Renamed parameters to standardize with other plotting functions
- `create.heatmap`: Added verbose warning when function cannot generate a figure
- `create.boxplot`: Fixed broken default handling of y-axis scale labels

# BoutrosLab.plotting.general 1.9.3 (2010-06-29)

## Fixed

- `create.heatmap`: Fixed ## Fixed to accept NAs when specifying colour-ranges

# BoutrosLab.plotting.general 1.9.2 (2010-06-28)

## Changed

- `create.barplot`: Changed 'xlab' and 'ylab' parameters into 'xlab.label' and 'ylab.label' and reordered parameters
- `create.boxplot`: Changed 'xlabel', 'ylabel', 'xlabel.cex', and 'ylabel.cex' parameters into 'xlab.lab', 'ylab.lab', 'xlab.cex', 'ylab.cex' and reordered parameters
- `create.hexbinplot`: Changed 'xlabel', 'ylabel' parameters into 'xlab.label', 'ylab.label'
- `create.stripplot`: Changed 'xlabel', 'ylabel', 'xlabel.cex', 'ylabel.cex' into 'xlab.lab', 'ylab.lab', 'xlab.cex', 'ylab.cex' and reordered parameters
- `create.violinplot`: Changed 'ylabel' parameter into 'ylab.label'
- `create.histogram`: Adjusted example
- `create.multiple`.boxplots: Adjusted example

# BoutrosLab.plotting.general 1.9.1 (2010-06-25)

## Changed

- `create.scatterplot`: Renamed 'xlabel', 'ylabel', 'xlabel.cex', 'ylabel.cex' parameters to 'xlab.label', 'ylab.label', 'xlab.cex', 'ylab.cex'
- `create.histogram`: Changed default to plot percentages instead of densities
- `create.scatterplot`: Adjusted example to show extra ## Changed

# BoutrosLab.plotting.general 1.9.0 (2010-06-24)

## Added

- `create.densityplot`: Added new function to plot densities

## Changed

- `create.histogram`: Added many new parameters, changed order of parameters, renamed parameters for standardization
- `create.boxplot`: Improved example
- `create.histogram`: Improved example

# BoutrosLab.plotting.general 1.8.1 (2010-06-24)

## Changed

- Updated parameter ordering of plotting functions to bring 'filename' parameter to the front of each function, and the height, width, and resolution parameters to the back

# BoutrosLab.plotting.general 1.8.0 (2010-06-24)

## Added

- Updated plotting functions to return the trellis object only if filename = NULL
- All example files are named after the method that produced them
- Standardized variable names of plotting objects to trellis.object

## Changed

- `create.multiple`.boxplots: Fixed spacing/indentation to match coding standard
- `create.histogram`: Reordered parameters to have mandatory parameters come first
- `create.stripplot`: Reordered parameters to have mandatory parameters come first
- `create.violinplot`: Reordered parameters to have mandatory parameters come first
- `create.histogram`: Allowed lattice default values for breaks, ylimits, etc
- `create.violinplot`: Allowed lattice default values for ylimits, yat

# BoutrosLab.plotting.general 1.7.3 (2010-06-23)

## Changed

- `create.heatmap`: Added examples to documentation

# BoutrosLab.plotting.general 1.7.2 (2010-06-22)

## Fixed

- `create.heatmap`: Fixed but in example by changing dataset
- Scientific.notation.2: Fixed function to display the last digit as 0 when requested

## Changed

- `create.heatmap`: Added 'colorkey.labels' parameter

# BoutrosLab.plotting.general 1.7.1 (2010-06-15)

## Changed

- `create.boxplot`: Renamed parameter 'ylimit' to 'ylimits' to standardize with other plotting functions
- Updates to documentation and code formatting for standardization

# BoutrosLab.plotting.general 1.7.0 (2010-06-13)

## Added

- Propagated graphics device changes to all plotting functions - enforced graphics type to 'cairo'

# BoutrosLab.plotting.general 1.6.27 (2010-06-11)

## Changed

- `create.heatmap`: Added 'at' and 'colorkey.labels.at' parameters

# BoutrosLab.plotting.general 1.6.26 (2010-06-07)

## Changed

- `create.heatmap`: Enforced graphics type to 'cairo' for all platforms

# BoutrosLab.plotting.general 1.6.25 (2010-06-02)

## Changed

- Panel.BL.bwplot: ## Changedd documentation
- `create.scatterplot`: Renamed 'lwdoutline' parameter to 'axis.lwd' for consistency in parameter names
- `create.scatterplot`: Added parameters for fontfaces of x- and y-axis scales

# BoutrosLab.plotting.general 1.6.24 (2010-06-02)

## Changed

- `create.scatterplot`: ## Changedd to allow alteration of more plotting parameters and to match other plotting functions

# BoutrosLab.plotting.general 1.6.23 (2010-06-01)

## Changed

- `create.stripplot`: ## Changedd to allow for alteration of more plotting parameters

# BoutrosLab.plotting.general 1.6.22 (2010-05-27)

## Changed

- `create.boxplot`: ## Changedd to allow for alteration of more plotting parameters

# BoutrosLab.plotting.general 1.6.21 (2010-05-26)

## Changed

- `create.heatmap`: Changed default colour.scheme from legacy parameter-handling
- `create.heatmap`: ## Removed spurious warnings in legacy code and added error-handling for bad usage

# BoutrosLab.plotting.general 1.6.20 (2010-05-26)

## Changed

- `create.scatterplot`: Added 'compression' parameter for tiff file customization

# BoutrosLab.plotting.general 1.6.19 (2010-05-25)

## Changed

- `create.heatmap`: Allowed clustering of only one dimension at a time

# BoutrosLab.plotting.general 1.6.18 (2010-04-26)

## Changed

- `create.scatterplot`: Added 'lwd' parameter for best-fit lines

# BoutrosLab.plotting.general 1.6.17 (2010-04-24)

## Changed

- `create.heatmap`: Changed ordering of top covariates to match right covariates
- `create.heatmap`: Changed axis tick-marks to be dynamic based on number of rows/columns in the heatmap

# BoutrosLab.plotting.general 1.6.16 (2010-04-23)

## Changed

- `create.scatterplot`: Allowed for removal of axis-lines

# BoutrosLab.plotting.general 1.6.15 (2010-04-14)

## Fixed

- `create.boxplot`: Fixed colouring issues that arose when 'box.colour' argument was passed as a vector

## Changed

- Panel.BL.bwplot: Added this function to fix colouring issues with lattice::panel.bwplot()

# BoutrosLab.plotting.general 1.6.14 (2010-04-12)

## Changed

- `create.heatmap`: Added 'fill.colour' parameter for background
- `create.heatmap`: Allowed specification of arbitrary colour-combinations

# BoutrosLab.plotting.general 1.6.13 (2010-04-11)

## Changed

- `create.heatmap`: Added parameter for covariate-size

# BoutrosLab.plotting.general 1.6.12 (2010-04-09)

## Changed

- `create.heatmap`: Allowed for plotting of arbitrarily large heatmaps when clustering is turned off

# BoutrosLab.plotting.general 1.6.11 (2010-04-09)

## Changed

- `create.boxplot`: ## Removed pass-through parameterization because it can cause crashes when a fixed parameter is duplicated
- `create.boxplot`: Allowed separate sizing of axis labels and axis scales

# BoutrosLab.plotting.general 1.6.10 (2010-04-02)

## Changed

- `create.boxplot`: Renamed 'box.color' to 'box.colour' to match other variables
- `create.boxplot`: Added 'symbol.cex' parameter to adjust outlier point size

# BoutrosLab.plotting.general 1.6.9 (2010-04-01)

## Changed

- `create.barplot`: Parameterized label-sizes

# BoutrosLab.plotting.general 1.6.8 (2010-04-01)

## Changed

- `create.boxplot`: Added 'box.color' parameter
- `create.boxplot`: ## Changedd to return a handle to plot object and added default values to input arguments
- `create.multiple`.boxplots: ## Changedd to get better memory efficiency

# BoutrosLab.plotting.general 1.6.7 (2010-03-31)

## Changed

- `create.barplot`: Adjusted cex parameters
- `create.histogram`: ## Changedd to allow specification of type (percent, count, or density)
- Cleaned up documentation to reduce warnings and be more explicit

# BoutrosLab.plotting.general 1.6.6 (2010-03-24)

## Changed

- Scientific.notation.2: ## Changedd to allow proper handling of zeros

# BoutrosLab.plotting.general 1.6.5 (2010-03-15)

## Changed

- `create.heatmap`: ## Changedd to allow for adjustment of colour-range

# BoutrosLab.plotting.general 1.6.4 (2010-03-14)

## Changed

- `create.heatmap`: Added selective plotting of right/top/left dendrograms
- `create.heatmap`: Improved colour-palette selection by calculating the range of the data
- `create.heatmap`: Allowed specification of main figure title
- `create.heatmap`: Allowed separate output of a file and/or return of a trellis object
- Enforced minimum version numbers on package dependencies

# BoutrosLab.plotting.general 1.6.3 (2010-03-14)

## Changed

- `create.heatmap`: Added BlueWhiteYellow colour-scheme
- `create.heatmap`: ## Changedd to not run wit an invalid colour-scheme

# BoutrosLab.plotting.general 1.6.2 (2010-03-05)

## Changed

- `create.boxplot`: Added option to plot or not plot outliers

# BoutrosLab.plotting.general 1.6.1 (2010-03-03)

## Changed

- `create.hexbinplot`: Added new function to create hexbin plots
- `create.boxplot`: ## Changedd to control x- and y-axis tick sizes

# BoutrosLab.plotting.general 1.5.3 (2010-03-01)

## Changed

- `create.heatmap`: Added ability to change legend size

# BoutrosLab.plotting.general 1.5.2 (2010-02-26)

## Changed

- `create.boxplot`: ## Changedd so that x-axis label does not overlap with x-axis ticks

# BoutrosLab.plotting.general 1.5.1 (2010-02-26)

## Changed

- `create.boxplot`: ## Changedd to be able to set label size

# BoutrosLab.plotting.general 1.5.0 (2010-02-25)

## Added

- `create.dotmap`: Added new function to create dotmaps

## Changed

- `create.scatterplot`: Added examples

# BoutrosLab.plotting.general 1.4.12 (2010-02-24)

## Changed

- `create.scatterplot`: Added parameters for plot-type, point/line size, colour, and symbol

# BoutrosLab.plotting.general 1.4.11 (2010-02-19)

## Changed

- `create.barplot`: Fixed handling of clustering methods other than 'none' or 'diana'
- `create.heatmap`: Allowed for column-wise covariates
- `create.heatmap`: Allowed for any agglomerative clustering method available via hclust

# BoutrosLab.plotting.general 1.4.10 (2010-02-08)

## Changed

- `create.boxplot`: Allowed for rotation of x-axis names

# BoutrosLab.plotting.general 1.4.9 (2010-02-06)

## Changed

- `create.stripplot`: Added new function to create strip plot

# BoutrosLab.plotting.general 1.4.8 (2010-02-03)

## Changed

- `create.barplot`: Added new function to create barplot

# BoutrosLab.plotting.general 1.4.7 (2010-02-01)

## Changed

- `create.boxplot`: ## Changedd to save figure directly and added example

# BoutrosLab.plotting.general 1.4.6 (2010-01-29)

## Fixed

- `create.heatmap`: Fixed ## Fixed in parameter-handling that introduced spurious warnings

# BoutrosLab.plotting.general 1.4.5 (2010-01-29)

## Changed

- `create.multiple`.boxplots: Modified to remove build/check warnings
- `create.multiple`.boxplots: Modified to ensure functionality when 'plot.names' input has less components than the 'data' input

# BoutrosLab.plotting.general 1.4.4 (2010-01-29)

## Fixed

- `create.multiple`.boxplots: Fixed error where \usage called wrong function name and one argument was missing in \arguments

## Added

- `create.boxplot`: Added new function to create boxplots

# BoutrosLab.plotting.general 1.4.3 (2010-01-28)

## Added

- `create.multiple`.boxplots: Added new function to create multiple boxplots

# BoutrosLab.plotting.general 1.4.2 (2010-01-27)

## Changed

- `create.heatmap`: Added ability to have clustered data without dendrograms

# BoutrosLab.plotting.general 1.4.1 (2010-01-27)

## Changed

- `create.heatmap`: Added ability to handle different clustering algorithms and not plot dendrograms

# BoutrosLab.plotting.general 1.4.0 (2010-01-27)

## Added

- `create.histogram`: Added function to create histograms

# BoutrosLab.plotting.general 1.3.0 (2010-01-26)

## Added

- `create.scatterplot`: Added function to create scatterplots
- Scientific.notation.2: Added function to facilitate embedding of scientific notation inside other figures

# BoutrosLab.plotting.general 1.2.0 (2010-01-07)

## Added

- Scientific.notation: Added function to allow for proper scientific notation in plots

# BoutrosLab.plotting.general 1.1.0 (2010-01-07)

## Added

- `create.heatmap`: Added function to create heatmaps

# BoutrosLab.plotting.general 1.0.0 (2010-01-06)

## Added

- Initialized package for plotting functions
- `create.violin`.plot: Added function to create violin plots
