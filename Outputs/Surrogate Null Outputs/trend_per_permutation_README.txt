PER-PERMUTATION p-VALUES: NOT INDIVIDUALLY REPORTABLE

surrogate type trend, N = 200, seed 1867

These p-values are unstable at this number of realisations. Each is
(r + 1) / (m + 1) with r a small integer; columns p_lo95 and p_hi95 give
the exact Clopper-Pearson interval on r/m and p_se the normal-approximation
standard error. The intervals routinely span an order of magnitude.

Observed behaviour across seeds: the set of permutations clearing
alpha = 0.05 changes in both size and membership. No individual model
should be named as significant on the basis of this table, and none
survives correction for multiple comparisons across the nine.

The reportable result is the ensemble statistic in
trend_ensemble_statistic.csv, which is dependence-preserving.

Resolution floor: no p below 1/(N+1) = 0.00498 can be claimed.
