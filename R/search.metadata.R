# The cpcgene.utilities package is copyright (c) 2012 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

search.metadata <- function(path = ".", FileName = NULL, Description = NULL, BLPlottingVersion = NULL, Author = NULL) {
	system(paste(paste("find ", paste(path,"*",sep=""),sep=""), " -type f -print0  | xargs -0 exiftool -S -FileName -ImageDescription -Software -Author | tee image-data.txt",sep=""))
	
	data = readLines("image-data.txt")
	data = paste(data, collapse = "|")
	data = strsplit(data, "======== ")[[1]]
	data = strsplit(data, "|", fixed = TRUE)
	data = data[sapply(data, length) > 0]
	
	extract <- function(d) {
		d <- d[-1]
		d <- strsplit(d, ": ")
		as.list(setNames(sapply(d, function(n) {n[2]}), sapply(d, function(n) {n[1]})))
	}

	data <- lapply(data, extract)
	output = c()
	for (i in 1:length(data)){
		if(!is.null(FileName)){
			if(!grepl(FileName,data[[i]]["FileName"])){
				next
			}
		}
                if(!is.null(Description)){
                        if(!grepl(Description,data[[i]]["ImageDescription"])){
                                next
                        }
                }
                if(!is.null(BLPlottingVersion)){
			notsame = TRUE
			for(n in names(data[[i]])){
				if(grepl("plotting.general",n)){
                        		if(grepl(BLPlottingVersion,n)){
						notsame = FALSE
                        		}
					next
				}
			}
			if(notsame == TRUE){
				next
			}
                }
                if(!is.null(Author)){
                        if(!grepl(Author,data[[i]]["Author"])){
                                next
                        }
                }
		output[length(output) + 1] = "test"
	}
	return(output)
}
