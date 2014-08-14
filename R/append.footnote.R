append.footnote <- function(text = Sys.Date(), size = 0.5, colour = 'black', position = "bottomright") {

	if (!is.null(text)) {
		pushViewport(viewport());
		if (position == "middle") {
			grid.text(label = text, 
				x = unit(0.5, "npc"),
				y = unit(0.5, "npc"),
				just = c("centre"),
				gp = gpar(cex = size, col = colour)
				);
		} else if (position == "bottomleft") {
			grid.text(label = text, 
				x = unit(2, "mm"),
				y = unit(2, "mm"),
				just = c("left", "bottom"),
				gp = gpar(cex = size, col = colour)
				);
			} else if (position == "topleft") {
			grid.text(label = text, 
				x = unit(2, "mm"),
				y = unit(1, "npc") - unit(2, "mm"),
				just = c("left", "top"),
				gp = gpar(cex = size, col = colour)
				);	
			} else if (position == "topright") {
			grid.text(label = text, 
				x = unit(1, "npc") - unit(2, "mm"),
				y = unit(1, "npc") - unit(2, "mm"),
				just = c("right", "top"),
				gp = gpar(cex = size, col = colour)
				);	
			} else {
			grid.text(label = text, 
				x = unit(1, "npc") - unit(2, "mm"),
				y = unit(2, "mm"),
				just = c("right", "bottom"),
				gp = gpar(cex = size, col = colour)
				);
			}
		popViewport();
		}
	}
