***Open the Dta File 
***Generating lag variables
gen cratio_1 = cratio[_n-1]
gen rrate_1 = rrate[_n-1]
gen e_1 = e[_n-1]

***Pioint a: Apply GMM 
gmm({beta=1}* (cratio^(-1*{gamma=1}))*rrate-1), inst(cratio_1 rrate_1 e_1)

***Pioint b: Apply Newey West type HAC with lag=5
gen time=[_n]
gmm({beta=1}* (cratio^(-1*{gamma=1}))*rrate-1), inst(cratio_1 rrate_1 e_1) wmatrix(hac ba 5)

*Comparision: 
*1) Coefficients: The standard GMM estimate and the Newey West GMM estimate yields very colse beta coefficeints; the standard GMM yields relatively smaller gamma estimates (gamma=0.7144) compared with Newey West GMM (gamma=0.77688). 
*2) Standard Error. Using Newey West witg lag=5 yields a slightly higher standard error than the standarsd estimates. 

