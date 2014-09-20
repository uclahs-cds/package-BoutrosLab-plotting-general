

print.greys <- function(difference) {

	greyval <- seq(1, 100, difference);
	palette <- c();

	for (i in 1:length(greyval)){
		palette <- c(palette, paste("grey", greyval[i], sep=""));
		}

	jumbled.palette <- sample(palette, length(palette))

	BoutrosLab.plotting.general::display.colours(jumbled.palette)

	}

print.greys(2);
pdf()
dev.off()

print.greys(4);

print.greys(6);

print.greys(8);

print.greys(10);

print.greys(12);

print.greys(14);

print.greys(16);

print.greys(18);

print.greys(20);

print.greys(22);

print.greys(24);


