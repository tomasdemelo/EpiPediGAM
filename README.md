Thank you for downloading this script.

If you have any questions, reach out to me at tomas.demelo@ontariotechu.ca.

The enclosed scripts combine multivariate RT-qPCR data with proxy indicators of disease to produce generalized additive models that can aid in interpreting disease burden.
The scripts are still subject to change and require careful adjustment if you wish to utilize them for your research.

The main script (Data-Summary-for-GAM.rmd) generates the optimal GAM model by accounting for temporal offsets between the datasets. After which, suitable models are subjected to a stochastic simulation where Gaussian noise is added incremently to asses model robustness.

The second script (GAM_summary.rmd) combines outputs from all generated models to help interpret the effects of any data transformation applied and also summarizes the contribution (F-values) of all predictors used.
