statar
======

A set of R commands for Stata users built on dplyr and data.table. 

1. The package adds the following vector functions
	````R
	library(dplyr)
	library(data.table)
	library(statar)
	
	# cluster creates quantile categories (corresponds to Stata xtile)
	N=1e6; K=100  
	v2 =  sample(1e6, N, TRUE),                       
	partition(v2, nq = 3)) # 3 groups based on terciles
	partition(v2, cutpoints = c(1e5, 5e5))) # 3 groups based on two cutpoints
	
	# lag_along (corresponds to Stata L. F.)
	## Unbalanced panel
	DT <- data.frame(
	 date  = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
	 value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
	)
	DT %>% mutate(lag(value, 1, order_by = date)) # wrong
	DT %>% mutate(lag(value, 1, along_with = date)) # right
	## Units, used on daily dates variables, can be days, weeks, months, quarters or years
	DT <- data.frame(
	    id = c(1, 1, 1, 1, 1, 2, 2),
	  date = as.Date(c("03/01/1992", "04/03/1992", "07/15/1992", "08/21/1992"), "%m/%d/%Y"),
	 value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
	)
	DT %>% group_by(id) %>% mutate(lag(value, 1, along_with = date, units = "month")) 
	````

2. The package adds the following verbs built on dplyr syntax for data.tables

	````R
	
	N=1e6; K=100
    DT <- data.table(
	  id = 1:N,
	  v1 = sample(5, N, TRUE),
	  v2 = sample(1e6, N, TRUE),
	  v3 = sample(round(runif(100,max=100), 4), N, TRUE)
	  )
	
	# colorder (= Stata order)
	DT  %>% colorder(starts_with("v"))
	DT  %>% colorder(starts_with("v"), inplace = TRUE)
	
	# sum_up (= Stata summarize)
	DT  %>% sum_up
	DT  %>% sum_up(v3, d=T)
	DT  %>% filter(v1==1) %>% sum_up(starts_with("v"))
	
	# expand (= Stata tsfill)
	DT <- data.table(
	    id = c(1, 1, 1, 1, 1, 2, 2),
	  date = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
	 value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
	)
	DT %>% expand(date)
	DT %>% group_by(id) %>% expand(date, type = "within")
	DT %>% group_by(id) %>% expand(date, type = "across")

	# ejoin (= Stata merge)
	ejoin(DTm, DTu, m:1)
	ejoin(DTm, DTu, type = 1:1, keep = "matched", gen = "_merge")
	ejoin(DTm, DTu, m:m, keep = c("master", "matched"), gen = FALSE)
	````

3. `tidyr::spread` has a method for data.tables that uses  `dcast.data.table`, which makes the command more memory efficient

4. The command tempname creates a name not assigned in the environment specified by the second variable

	````R
	tempvar <- tempname("temp", DT)
	tempname <- tempname("temp", globalenv())
	````

The package can be installed via the package `devtools`

````R
devtools::install_github("matthieugomez/tidyr")
devtools::install_github("matthieugomez/lazyeval")
devtools::install_github("matthieugomez/dplyr")
devtools::install_github("matthieugomez/statar")
````
