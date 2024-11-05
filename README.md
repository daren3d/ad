# adc
Anomaly detection and correction

An R package to accompany Kuwaye, D. & Cho, H. (2024).  "Anomaly detection and correction in dense functional data within electronic medical records,"  *Statistics in Medicine, 43*, 4768--4777.

As described in the paper, our method assumes that the data adheres to a smooth functional trajectory, and is tailored to be conservative, focusing on anomalies that signify actual errors in the data collection process while controlling for false discovery rates and type II errors.

The method consists of the following basic steps:
1. Fit penalized spline to the data.
2. Calculate studentized residuals.
3. Adjust for multiple comparisions.
4. Verify significance.
5. Suggest a replacement for observations deemed anamolous.

In addition, this packge contains two vignettes to help you get started.
