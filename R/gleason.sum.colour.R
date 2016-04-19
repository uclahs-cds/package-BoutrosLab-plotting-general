# The cpcgene.utilities package is copyright (c) 2013 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.


gleason.sum.colour <- function(values) {
  gleason.colours <- c("#FEEBE2", "#FBB4B9", "#F768A1", "#C51B8A", "#7A0177", "slategrey");

  gleason.vals <- vector();
  gleason.cols <- vector();

  if(any(values == 5)){
    gleason.vals <- c(gleason.vals, 1)
    gleason.cols[values ==5] <- gleason.colours[1];
    }
  if(any(values == 6)){
    gleason.vals <- c(gleason.vals, 2)
    gleason.cols[values==6] <- gleason.colours[2];
    }
  if(any(values == 7)){
    gleason.vals <- c(gleason.vals, 3)
    gleason.cols[values == 7] <- gleason.colours[3];
    }
  if(any(values == 8)){
    gleason.vals <- c(gleason.vals, 4)
    gleason.cols[values == 8] <- gleason.colours[4];
    }
  if(any(values == 9)){
    gleason.vals <- c(gleason.vals, 5)
    gleason.cols[values == 9] <- gleason.colours[5];
    }

  # For missing Gleason scores.
  if(any(values=='missing' | values=='NA' | is.na(values))) {
	gleason.vals <- c(gleason.vals, 6)
    gleason.cols[values=='missing' | values=='NA' | is.na(values)] <- gleason.colours[6];
    }
  
  gleason.cols;

  }
