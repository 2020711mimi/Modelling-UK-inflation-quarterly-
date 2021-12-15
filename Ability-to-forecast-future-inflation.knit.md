---
title: "EVALUATING EXCLUDING FOOD AND ENERGY CORE INFLATION MEASURES"
author: "Yiyi"
date: "2021-12-15"
output:
  pdf_document:
  header-includes:
  - \usepackage{placeins}
  - \usepackage{multirow}
  - \usepackage{multicolumn}
  - \usepackage{caption}
  - \usepackage[absolute,overlay]{textpos}
  - \usepackage{float}
  - \usepackage{graphicx}
  - \usepackage{booktabs}
  html_document:
    toc: yes
    df_print: paged
  word_document: default
  html_notebook: default
---









# Criteria for evaluation : forecasting future inflation

This work uses Cogley's model to test the forecasting ability of core inflation rate, and compares the forecasting ability of excluding food and energy core inflation rates. Cogley's model is based on Bryan and Cecchetti's definition of core inflation: "Core inflation is changes in the price level that are expected to persist over a long period of time."

According to Bryan and Cecchetti's definition, a valid core inflation is one that is "pure" after removing temporary factors from measured real inflation. On this basis, Cogley developed the following model to evaluate the predictive power of core inflation:




$$\pi_{t+h}-\pi_{t}=\alpha_{h}+\beta_{h}\left(\pi_{t}-\pi_{t}^{c}\right)+u_{t+h}$$

Here, x represents the headline inflation rate and core x represents some core inflation indicator, both year-on-year data. Parameter h is 6,9 and 12 (month) ahead in this case. For sufficiently large H, the core deviation, $\left(\pi_{t}-\pi_{t}^{c}\right)$, should be inversely related to subsequent changes in inflation, $\pi_{t+h}-\pi_{t}$. Moreover, in order for the candidate to satisfy equation (1), the coefficients in the regression, should satisfy $\alpha=0$ and $\beta = -1$.

Of importance to the forecasting model is the estimated coefficient of $\beta$, which indicates whether core inflation has sufficiently purified the transitory component. Because if the absolute value of the estimated coefficient is equal to 1, it indicates that the model is a random walk process, and the components removed from the core inflation do not contain any information that predicts future overall inflation. If $\beta=-1$, the forecasting capacity for core inflation is the best . This proves that core inflation has fully captured the trend components of overall inflation and has a complete forecasting ability for future inflation.

1.  If the $|\beta|<1$, it indicates that subsequent changes in inflation are overestimated;

2.  If the $|\beta|>1$, it shows that underestimation of the current temporary movement in headline inflation.

Therefore, the closer the absolute value of the estimated regression coefficient $\beta$ is to 1, the better the predictive power of core inflation is. In addition, the root mean square error RMSE $=\sqrt{\frac{1}{\mathrm{~T}} \sum_{\mathrm{t}=1}^{\mathrm{T}}\left(\pi_{\mathrm{t}}-\hat\pi_{\mathrm{t}}\right)^{2}}$obtained by Cogley regression represents the deviation between the predicted value and the actual value. $\hat\pi_{\mathrm{t}}$ is the forecast value of the inflation rate. The smaller the RMSE, the more accurate the forecast. and the better the forecast of core inflation.





#Trimmed Mean






\begin{table}
\centering
\begin{tabular}[t]{l|r|r|r|r|r|r}
\hline
\multicolumn{1}{c|}{ } & \multicolumn{6}{c}{Core Inflation 6} \\
\cline{2-7}
\multicolumn{1}{c|}{ } & \multicolumn{3}{c|}{Excluding Food and Energy} & \multicolumn{3}{c}{Trimmed Mean} \\
\cline{2-4} \cline{5-7}
  & RMSE & \$\textbackslash{}alpha\$ & \$\textbackslash{}beta\$ & RMSE & \$\textbackslash{}alpha\$ & \$\textbackslash{}beta\$\\
\hline
1 months ahead & 0.2875491 & 0.0084757 & -0.0195335 & 0.2866793 & 0.0645846 & -0.0510096\\
\hline
2 months ahead & 0.4433265 & 0.0284204 & -0.0724343 & 0.4430588 & 0.1329443 & -0.1059460\\
\hline
3 months ahead & 0.5778391 & 0.0552011 & -0.1493606 & 0.5791695 & 0.2423759 & -0.1956752\\
\hline
4 months ahead & 0.6936614 & 0.0786384 & -0.2277337 & 0.7019321 & 0.2963575 & -0.2439679\\
\hline
5 months ahead & 0.7942037 & 0.0995841 & -0.3039190 & 0.8085734 & 0.3722336 & -0.3111952\\
\hline
6 months ahead & 0.8797910 & 0.1218862 & -0.3837018 & 0.8997629 & 0.4729218 & -0.3983625\\
\hline
7 months ahead & 0.9466686 & 0.1433650 & -0.4598274 & 0.9683200 & 0.6086117 & -0.5132120\\
\hline
8 months ahead & 1.0048770 & 0.1599479 & -0.5362292 & 1.0338213 & 0.6923265 & -0.5903104\\
\hline
9 months ahead & 1.0531805 & 0.1748061 & -0.6051095 & 1.0921331 & 0.7459039 & -0.6422965\\
\hline
10 months ahead & 1.0900066 & 0.1942808 & -0.6803401 & 1.1417008 & 0.8047791 & -0.6967543\\
\hline
11 months ahead & 1.1239888 & 0.2127016 & -0.7557223 & 1.1962377 & 0.8123711 & -0.7108918\\
\hline
12 months ahead & 1.1554869 & 0.2285637 & -0.8169504 & 1.2412841 & 0.8471592 & -0.7446529\\
\hline
\end{tabular}
\end{table}


# Results

From the above discussion, I used my own data to re-simulate the values of the parameters.The results please see Table 1.

## RMSE





```{=tex}
\begin{table}[h]
\centering
\caption{Forecasting future inflation}
\label{tab:my-table}
\begin{tabular}{|c|cll|}
\hline
H                       & \multicolumn{3}{c|}{Excluding Food and Energy}                                               \\ \hline
                        & \multicolumn{1}{c|}{a}       & \multicolumn{1}{c|}{b}        & \multicolumn{1}{c|}{RMSE}     \\ \hline
\multicolumn{1}{|l|}{6} & \multicolumn{1}{l|}{0.12189} & \multicolumn{1}{l|}{-0.38370} & 0.879791                      \\ \hline
\multicolumn{1}{|l|}{9} & \multicolumn{1}{l|}{0.17481} & \multicolumn{1}{l|}{-0.60511} & 1.053181                      \\ \hline
12                      & \multicolumn{1}{c|}{0.229}   & \multicolumn{1}{c|}{-0.81695} & \multicolumn{1}{c|}{1.155487} \\ \hline
\end{tabular}
\end{table}
```
# Conclusions

As expected, all Cogley regressions of the core inflation rate with $\beta$ estimates are all negative. The beta keeps falling as the number of forecast periods rises; The forecast RMSE for core inflation rates increases as the number of forecast periods increases continuously. This suggests that the longer the forecast period, the more inaccurate the forecast.
