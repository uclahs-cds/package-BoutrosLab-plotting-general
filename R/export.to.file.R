export.to.file <- function(dirname, funcname, data, filename) {

        user <- system('whoami', intern = T);
        out.filename <- 'data-df.tsv';
	date <- as.character(Sys.Date());
        numeric.data <- c();
        num.numeric <- 0;
        num.factor <- 0;
        num.integer <- 0;
	num.rows <- 0;
	num.cols <- 0;

        if (is.null(filename)) {
                filename <- 'None';
                }

	switch(
		as.character(class(data)),
		'list' = {
			num.numeric <- length(data);
	                numeric.data <- unlist(data);
	                num.rows <- length(data[[1]]);
	                num.cols <- length(data);
			},
		'numeric' = {
			num.numeric <- 1;
			numeric.data <- data;
			num.rows <- length(data);
			num.cols <- 1;
			},
		'data.frame' =, 'matrix' = {
			num.rows <- nrow(data);
			num.cols <- ncol(data);
                for (i in 1:num.cols) {
                    switch(
                    	as.character(class(data[, i])),
                    	'numeric' = {
                            num.numeric <- num.numeric + 1;
                            numeric.data <- c(numeric.data, data[, i]);
                            },
                    	'integer' = {
                            num.integer <- num.integer + 1;
                            },
                    	'factor' = {
                            num.factor <- num.factor + 1;
                            }
                    	);
                	}
                }
			);

	df.to.add <- NULL;
        if (num.numeric == 0) {
        	df.to.add <- data.frame(user = c(user), filename = c(filename), date = date, func.name = c(funcname),
                        data.type = c(class(data)), nrow = num.rows, ncol = num.cols, numeric = c(num.numeric),
                        factor = c(num.factor), integer = c(num.integer), max = c(0), min = c(0), median = c(0));
                }
        else {
                df.to.add <- data.frame(user = c(user), filename = c(filename), date = date, func.name = c(funcname),
                        data.type = c(class(data)), nrow = num.rows, ncol = num.cols, numeric = c(num.numeric),
                        factor = c(num.factor), integer = c(num.integer), max = c(max(numeric.data, na.rm = T)),
			min = c(min(numeric.data, na.rm = T)), median = c(median(numeric.data, na.rm = T)));
                }
        if (!is.null(df.to.add)) {
                if (!file.exists(paste(dirname, out.filename, sep = '/'))) {
                        write.table(df.to.add, paste(dirname, out.filename, sep = '/'), sep = '\t', row.names = F, col.names = T);
                        }
                else {
                        datainfo.dataframe <- read.table(paste(dirname, out.filename, sep = '/'), header = T, fill = T);
			index = -1;
			for(i in 1:nrow(datainfo.dataframe)) {
				## check if user, filename, date appears in dataframe
				if(datainfo.dataframe[i,]$user == user && datainfo.dataframe[i,]$filename == filename && datainfo.dataframe[i,]$date == date) {
					index = i;
					}
				}
                        if (index != -1) {
                                datainfo.dataframe[index, ] <- df.to.add;
                                write.table(datainfo.dataframe, paste(dirname, out.filename, sep = '/'), sep = '\t', row.names = F, col.names = T);
                                }
                        else {
                                write.table(df.to.add, paste(dirname, out.filename, sep = '/'), sep = '\t', row.names = F, col.names = F, append = T);
                                }
                        }
                }
        }

