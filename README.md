# critical-infrastructure-analysis

## Project

### Note to Contributors

* the program takes a while to run (~2 min on my PC)

* the time periods are:

  * Wednesday - morning (0:00AM ~ 11:59AM), afternoon (12:00PM ~ 5:59PM), night (6:00PM ~ 11:59PM)

  * Saturday - morning (0:00AM ~ 11:59AM), afternoon (12:00PM ~ 5:59PM), night (6:00PM ~ 11:59PM)

* the program will produce `csv` files in `output` directory

  * the files contain anomalous data points corresponding to above time periods for each test data

  * the `point anomaly detection` is using minute as the x-axis instead of month or season, because this seems to be the most accurate and reasonable approach

  * the `collective anomaly detection` is using observations as the x-axis, so the data points will appear differently than the `point anomaly` graphs

    * the output files for `collective anomaly detection` will have the first `window size` rows as `NA` due to the nature of move average

  * `HMM` does not produce anything in `output` directory, because the assignment description states to only compare the log likelihood and not anomaly detection

* the program will produce pictures in `figs` directory

  * I am not sure if this is the correct way to compare train data with test data; I will go to Prof's office hour to check

  * in all comparison graphs, the `y` values are the `means` of the corresponding time periods; I can plot `min`, `max`, and `sd` if you want, but I don't think they are needed