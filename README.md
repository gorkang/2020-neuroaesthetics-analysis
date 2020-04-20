Running the `2020-neuroaesthetics-analysis.R` file should get you the final plots and .html tables with the analyses results in the `outputs` folder.



To try to ensure reproducibility we use the {renv} package. To get a working environment you can do:

```R
# Install renv package
install.packages("renv")

# Install libraries contained in the renv.lock file 
renv::restore() 
```



