Table6.18 <- as.data.frame(cbind(
	apply(
		expand.grid(
			'Temperature'=c(-1,1),
			'Time'=c(-1,1),
			'Pressure'=c(-1,1),
			'GasFlow'=c(-1,1)
		),
		2,
		function(v){
			rbind(v, v, v, v)
		}
	),
	'OxideThickness'=c(
		378, 376, 379, 379,
		415, 416, 416, 417,
		380, 379, 382, 383,
		450, 446, 449, 447,
		375, 371, 373, 369,
		391, 390, 388, 391,
		384, 385, 386, 385,
		426, 433, 430, 431,
		381, 381, 375, 383,
		416, 420, 412, 412,
		371, 372, 371, 370,
		445, 448, 443, 448,
		377, 377, 379, 379,
		391, 391, 386, 400,
		375, 376, 376, 377,
		430, 430, 428, 428
	)
))
