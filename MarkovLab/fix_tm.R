fix.steps <- function(tm, steps)
{
	nt <- nrow(tm)
# adjust time steps
	tm <- tm/steps
	
	for (i in 1:nt)
	{
		rs <- 0.0
		for (j in 1:nt)
		{
			if (i != j) 
			{
				rs <- rs + tm[i,j]
			}
		tm[i,i] <- 1.0 - rs
		}
	}
	return(tm)
}
