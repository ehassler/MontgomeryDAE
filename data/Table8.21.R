Table8.21 <- expand.grid(
	'A'=c(-1, 1),
	'B'=c(-1, 1),
	'C'=c(-1, 1)
)
Table8.21[,'D'] = Table8.21[,'A'] * Table8.21[,'B']
Table8.21[,'E'] = Table8.21[,'A'] * Table8.21[,'C']
Table8.21[,'F'] = Table8.21[,'B'] * Table8.21[,'C']
Table8.21[,'G'] = Table8.21[,'A'] * Table8.21[,'B'] * Table8.21[,'C']
Table8.21[,'Time'] = c(
	85.5, 75.1, 93.2, 145.4, 83.7, 77.6, 95.0, 141.8
)
