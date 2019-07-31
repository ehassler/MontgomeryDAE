Table8.22 <- expand.grid(
	'A'=c(1, -1),
	'B'=c(1, -1),
	'C'=c(1, -1)
)
Table8.22[,'D'] = -Table8.22[,'A'] * Table8.22[,'B']
Table8.22[,'E'] = -Table8.22[,'A'] * Table8.22[,'C']
Table8.22[,'F'] = -Table8.22[,'B'] * Table8.22[,'C']
Table8.22[,'G'] = -Table8.22[,'A'] * Table8.22[,'B'] * Table8.22[,'C']
Table8.22[,'Time'] = c(
	91.3, 136.7, 82.4, 73.4, 94.1, 143.8, 87.3, 71.9
)
