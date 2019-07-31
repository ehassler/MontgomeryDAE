Example6.4 <- cbind(
	expand.grid(
		'Temperature'=c(-1,1),
		'ClampTime'=c(-1,1),
		'ResinFlow'=c(-1,1),
		'ClosingTime'=c(-1,1)
	),
	'Defects'=c(
		5, 11, 3.5, 9, 0.5, 8, 1.5, 9.5,
		6, 12.5, 8, 15.5, 1, 6, 5, 5
	)
)
