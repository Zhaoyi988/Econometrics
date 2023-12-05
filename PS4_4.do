*** Open the dataset MURDER.DTA
***Point a 
reg mrdrte exec unem d90 d93
* the coefficient of exec in linear regression is 0.1628 but it's not significant; the coefficient of unem is 1.3908 and it's significant at 0.05 significance level. The coefficifients of both dummy variables are positivebut insignificant. 

*** Point b
xtset id year
xtreg mrdrte exec unem d90 d93, fe
mat betafe = get(_b)
mat Vfe = get(VCE)
* the coefficient of exec is -0.1383 and it's still insignificant. The coefficient of unem is 0.2213 but now it's insignificant. The coefficients of both dummy variables now are significant under 0.05 significance level.

*** Point c
xi: qui reg mrdrte i.id
predict rmrdrte, residuals
xi: qui reg exec i.id
predict rexec, residuals
xi: qui reg unem i.id
predict runem, residuals
xi: qui reg d90 i.id
predict rd90, residuals
xi: qui reg d93 i.id
predict rd93, residuals

reg rmrdrte rexec runem rd90 rd93, noconstant

*** Point d
xtreg mrdrte exec unem d90 d93
mat betare = get(_b)
mat Vre = get(VCE)
mat hausman=(betafe[1,1..4]-betare[1,1..4])*invsym(Vfe[1..4,1..4]-Vre[1..4,1..4])*(betafe[1,1..4]-betare[1,1..4])'
mat list hausman