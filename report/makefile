report.pdf: report.Rnw
	./despaceBlocks.bash report.Rnw > turtleReport.Rnw
	R CMD Sweave --pdf turtleReport.Rnw --clean
