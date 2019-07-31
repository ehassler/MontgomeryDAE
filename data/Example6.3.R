Example6.3 <- cbind(
	expand.grid(
		'DrillLoad'=c(-1,1),
		'FlowRate'=c(-1,1),
		'RotationalSpeed'=c(-1,1),
		'Mud'=c(-1,1)
	),
	'DrillRate'=c(
		1.68, 1.98, 4.98, 5.70, 3.24, 3.44, 9.97, 9.07,
		2.07, 2.44, 7.77, 9.43, 4.09, 4.53, 11.75, 16.30
	)
)
