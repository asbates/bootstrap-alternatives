# bootstrap-alternatives

# About
This was a project I did for a course on the bootstrap at UCSD. I was interested in situations when the standard i.i.d. bootstrap is invalid and what can be done about it. I looked at two instances when the bootstrap is known to fail: the mean in the infinite variance case, and the maximum order statistic for a uniform distribution. For each case, two alternative bootstrap methods were considered: the m-out-of-n bootstrap, and subsampling (a.k.a. subsampling bootstrap). These methods were compared via a simulation study in which empirical coverage probabilites and average interval length were computed for various sample sizes and several resample sizes. Overall, both methods gave similar coverage results but subsampling tended to produce shorter confidence intervals.

The R file in this project contains the functions I used to carry out the simulations and an example of their usage. The actual report I wrote for the course can be found [here](https://portfolium.com/entry/when-the-bootstrap-fails-and-how-to-fix-it).
