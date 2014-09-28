statar
======

A *beta* set of R commands for Stata users built on dplyr and data.table. 

1. The package adds the following vector functions. In particular, they can be used inside `dplyr::mutate`

	````R
	library(dplyr)
	library(statar)
	
	N=1e6; K=100
    set.seed(1)
    DT <- data.table(
		  id = 1:N,
		  v1 =  sample(5, N, TRUE),                          # int in range [1,5]
		  v2 =  sample(1e6, N, TRUE),                        # int in range [1,1e6]
		  v3 =  sample(round(runif(100,max=100),4), N, TRUE) # numeric e.g. 23.5749
		)

	### quantile category (=Stata xtile)
	DT %>% group_by(v1) %>% mutate(xtile(v2, nq = 3))
	DT %>% group_by(v1) %>% mutate(xtile(v2, cutpoints = c(1e5,5e5) ))
	### lag along_with (= Stata L. F.)
	DT %>% group_by(id) %>% mutate(lag(v2, order_by = time)) # Balanced dataset
	DT %>% group_by(id) %>% mutate(lag(v2, along_with = time)) # Unbalanced dataset
	````


2. The package adds the following verbs that can be used in a `dplyr` pipe (although it works only on data.tables)

	````R
	library(data.table)
	
	### colorder (= Stata order)
	DT  %>% colorder(starts_with("v"))
	DT  %>% colorder(starts_with("v"), inplace = TRUE)
	
	### sum_up (= Stata summarize)
	DT  %>% sum_up
	DT  %>% sum_up(v3, d=T)
	DT  %>% filter(v1==1) %>% sum_up(starts_with("v"))
	
	### expand (= Stata tsfill)
	DT <- data.table(
	 id = c(1, 1, 1, 1, 1, 2, 2),
	 date = c(1992, 1989, 1991, 1990, 1994, 1992, 1991),
	 value = c(4.1, 4.5, 3.3, 5.3, 3.0, 3.2, 5.2)
	)
	DT %>% group_by(id) %>% expand(date)
	DT %>% expand(date)

	### ejoin (= Stata merge)
	ejoin(DTm, DTu, m:1)
	ejoin(DTm, DTu, type = 1:1, keep = "matched", gen = "_merge")
	ejoin(DTm, DTu, m:m, keep = c("master", "matched"), gen = FALSE)
	````

3. `tidyr::spread` has a method for data.tables that uses  `dcast.data.table`, which makes it more memory efficient

4. Tempname creates a name not assigned in the environment specified by the second variable

	````R
	tempvar <- tempname("temp", DT)
	tempname <- tempname("temp", globalenv())
	````

The package can be installed via the package `devtools`

````R
devtools::install_github("matthieugomez/statar")
````
