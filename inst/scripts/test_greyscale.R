
# this function is for testing how easily greyscale values are distinguished
### difference = the attempted greyscale cutoff difference
### value = if light or dark values should be emphasized
### total = the total number of greys displayed
print.greys <- function(difference, value, total = 25) {
	start <- 1;
	end <- 100;

	if("light" == value){
		start = 40;
		}
	else if ("dark" == value){
		end = 60;
		}

	greyval <- seq(start, end, difference);
	palette <- c();

	greyval <- sample(greyval, total, replace = TRUE)
	length(greyval)

	for (i in 1:length(greyval)){
		palette <- c(palette, paste("grey", greyval[i], sep=""));
		}

	jumbled.palette <- sample(palette, length(palette))

	BoutrosLab.plotting.general::display.colours(jumbled.palette)
	}

png("test4L.png")
print.greys(4, "light");
dev.off()

png("test4D.png")
print.greys(4, "dark");
dev.off()

png("test6L.png")
print.greys(6, "light");
dev.off()

png("test6D.png")
print.greys(6, "dark");
dev.off()

png("test8L.png")
print.greys(8, "light");
dev.off()

png("test8D.png")
print.greys(8, "dark");
dev.off()

png("test10L.png")
print.greys(10, "light");
dev.off()

png("test10D.png")
print.greys(10, "dark");
dev.off()

png("test12L.png")
print.greys(12,"light");
dev.off()

png("test12D.png")
print.greys(12,"dark");
dev.off()

png("test14L.png")
print.greys(14,"light");
dev.off()

png("test14D.png")
print.greys(14,"dark");
dev.off()

png("test16L.png")
print.greys(16,"light");
dev.off()

png("test16D.png")
print.greys(16,"dark");
dev.off()

png("test18L.png")
print.greys(18,"light");
dev.off()

png("test18D.png")
print.greys(18,"dark");
dev.off()

png("test20L.png")
print.greys(20,"light");
dev.off()

png("test20D.png")
print.greys(20,"dark");
dev.off()

png("test22L.png")
print.greys(22,"light");
dev.off()

png("test22D.png")
print.greys(22,"dark");
dev.off()

png("test24L.png")
print.greys(24,"light");
dev.off()

png("test24D.png")
print.greys(24,"dark");
dev.off()

#perl -w ~/Cluster/svn/BoutrosLab/Resources/code/perl/GeneralPerlUtilities/paper_tile_format.pl -d 300 -s 3x2 -i test4L.png -i test6L.png -i test8L.png -i test10L.png -i test12L.png -i test14L.png  -l 4L 6L 8L 10L 12L 14L -o "testL.tiff"

#perl -w ~/Cluster/svn/BoutrosLab/Resources/code/perl/GeneralPerlUtilities/paper_tile_format.pl -d 300 -s 3x2 -i test4D.png -i test6D.png -i test8D.png -i test10D.png -i test12D.png -i test14D.png  -l 4D 6D 8D 10D 12D 14D -o "testD.tiff"

#perl -w ~/Cluster/svn/BoutrosLab/Resources/code/perl/GeneralPerlUtilities/paper_tile_format.pl -d 300 -s 3x2 -i test16L.png -i test18L.png -i test20L.png -i test22L.png -i test24L.png -i test24L.png  -l 16L 18L 20L 22L 24L 24L -o "test2L.tiff"

#perl -w ~/Cluster/svn/BoutrosLab/Resources/code/perl/GeneralPerlUtilities/paper_tile_format.pl -d 300 -s 3x2 -i test16D.png -i test18D.png -i test20D.png -i test22D.png -i test24D.png -i test24D.png  -l 16D 18D 20D 22D 24D 24D -o "test2D.tiff"


