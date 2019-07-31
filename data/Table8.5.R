Table8.5 <- as.data.frame(expand.grid(
	'A'=c(-1, 1),
	'B'=c(-1, 1),
	'C'=c(-1, 1),
	'D'=c(-1, 1)
))
Table8.5[,'E'] = Table8.5[,'A'] * Table8.5[,'B'] * Table8.5[,'C'] * Table8.5[,'D']
Table8.5[,'Yield'] = c(8, 9, 34, 52, 16, 22, 45, 60, 6, 10, 30, 50, 15, 21, 44, 63)
