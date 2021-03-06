Table13.9 <- data.frame(
	'GasTemperature'=c(rep(60, 24), rep(75, 24), rep(90, 24)),
	'Operator'=factor(rep(c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6)), 3)),
	'Gauge'=factor(rep(c(1, 1, 2, 2, 3, 3), 12)),
	'PressureDrop'=c(
		-2,-3,-6,4,-1,-2,
		0,-9,-5,-1,-4,-8,
		-1,-8,-8,-2,0,-7,
		4,4,-3,-7,-2,4,
		14,14,22,24,20,16,
		6,0,8,6,2,0,
		1,2,6,2,3,0,
		-7,6,-5,2,-5,-1,
		-8,-8,-8,3,-2,-1,
		-2,20,1,-7,-1,-2,
		-1,-2,-9,-8,-4,-7,
		-2,1,-8,3,1,3
	)
)
