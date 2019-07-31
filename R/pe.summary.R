pe.summary <- function(model){
	if(!any(class(model) == 'aov')){
		stop('Only for aov model objects')
	}

	# Start with the aov summary object.
	model.summary <- summary(model)
	if(length(model.summary) > 1){
		stop('This method only works with aov not having Error strata defined.')
	}

	# Get the terms from the regression portion, and store the residual portion for later
	regression.names <- gsub(' ', '', row.names(model.summary[[1]]))
	Df <- model.summary[[1]][['Df']]
	SS <- model.summary[[1]][['Sum Sq']]
	MS <- model.summary[[1]][['Mean Sq']]

	SSResid <- SS[length(SS)]
	DfResid <- Df[length(Df)]
	MSResid <- MS[length(MS)]
	SS <- SS[1:(length(SS) - 1)]
	Df <- Df[1:(length(Df) - 1)]
	MS <- SS[1:(length(MS) - 1)]
	regression.names <- regression.names[1:(length(regression.names) - 1)]

	# Describe the overall regression SS
	SSRegression <- sum(SS)
	DfRegression <- sum(Df)
	MSRegression <- SSRegression/DfRegression

	# From the model, extract the original data.frame
	model.formula <- model$terms
	mf <- model.frame(model)
	Y <- mf[,1]
	X.m <- model.matrix(model.formula, mf)
	unique.runlevels <- unique(X.m)
	m <- dim(unique.runlevels)[1]
	n <- dim(X.m)[1]

	# Decompose the Residual error into Pure and Lack of Fit
	SSPureError <- sum(apply(unique.runlevels, 1, function(v){
		ix <- apply(t(t(X.m) == v), 1, all)
		sum((Y[ix] - mean(Y[ix]))^2)
	}))
	DfPureError <- n - m
	if(DfPureError > 0){
		MSPureError <- SSPureError/DfPureError
	} else {
		MSPureError <- NaN
	}

	SSLackOfFit <- max(SSResid - SSPureError,0)
	DfLackOfFit <- m-2
	if(DfLackOfFit > 0){
		MSLackOfFit <- SSLackOfFit / DfLackOfFit
	} else {
		MSLackOfFit <- NaN
	}

	# Assemble the returned data frame
	pad <- max(c(nchar(regression.names) + 2, 10))
	other.names <- sprintf(sprintf('%%-%ds', pad), c('Regression','Error','  Lack of Fit','  Pure Error'))
	regression.names <- sprintf(sprintf('  %%-%ds', pad), regression.names)

	# If Pure Error didn't work out, return the aov summary
	if(!is.finite(MSPureError)){
		stop('No repeated runs to use for pure error estimate.')
	}

	# Assemble the output structure
	out <- data.frame(
		'Df'=c(DfRegression, Df, DfResid, DfLackOfFit, DfPureError),
		'Sum Sq'=c(SSRegression, SS, SSResid, SSLackOfFit, SSPureError),
		'Mean Sq'=c(MSRegression, MS, MSResid, MSLackOfFit, MSPureError),
		'F value'=c(MSRegression/MSPureError, MS/MSPureError, NA, MSLackOfFit/MSPureError, NA),
		'Pr(>F)'=c(
			pf(MSRegression/MSPureError, DfRegression, DfPureError, lower.tail=FALSE),
			pf(MS/MSPureError, Df, DfPureError, lower.tail=FALSE),
			NA,
			pf(MSLackOfFit/MSPureError, DfLackOfFit, DfPureError, lower.tail=FALSE),
			NA
		),
		check.names=FALSE,
		row.names=c(other.names[1], regression.names, other.names[2:4])
	)
	class(out) <- c('pe.summary', class(out))
	return(out)
}
