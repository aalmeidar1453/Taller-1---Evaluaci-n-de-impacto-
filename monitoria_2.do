*-----------------------------------------------------------------*
* Curso: Evaluación de Impacto con Aplicaciones en Educación      *
* Monitoria 2                                                     *
* Profesor: Felipe Barrera                                        *
* Profesor Asistente: Carlos Bermúdez                             *   
* Fecha: junio 2024                                               *                  
*-----------------------------------------------------------------*

*------------------------------------------*
* Establecer ruta de carpeta de trabajo    *
*------------------------------------------*

global root  "/Users/alejandra/Downloads/Taller_1/"
use "$root/EjercicioStata", clear

*-----------------------*
*       Caso 4          *
*-----------------------*

* Pregunta 1
* Creamos la variable de cambio para cada hogar.  
bysort hhid (round): gen deltha_ophe = ophe - ophe[_n-1]
order hhid round ophe deltha_ophe 

* Pregunta 2

* Diferencia de medias. 

mat diff=J(2,5,.)
mat stars=J(1,5,.)
ttest deltha_ophe if eligible==1, by(treatcom) reverse 
mat diff[1,1]=`r(mu_1)'
mat diff[1,2]=`r(mu_2)'
mat diff[2,1]=(`r(sd_1)')
mat diff[2,2]=(`r(sd_2)')
mat diff[1,4]=(`r(se)')
mat diff[1,3]=`r(mu_1)'-`r(mu_2)'
mat diff[1,5]=`r(p)'
mat stars[1,1]=0
mat stars[1,2]=0
mat stars[1,3]=0
mat stars[1,4]=0
mat stars[1,5]=(r(p)<0.1)+(r(p)<0.05)+(r(p)<0.01)

frmttable using "$root/tab4_2.tex",replace statmat(diff) annotate(stars) sdec(3) asymbol(*,**,***) tex fragment ctitles("" "Mean Round=1" "Mean Round=0" "Diff" "se" "p-value") rtitles("Gasto educación" \ "sd") note("* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")

*Pregunta 3 y 4
eststo clear
eststo m1: reg deltha_ophe treatcom if eligible==1,vce(cluster local) 
eststo m2: reg deltha_ophe treatcom min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh  if eligible==1,vce(cluster local) 
esttab m1 m2 using "$root/tab4_34.tex",star(* 0.10 ** 0.05 *** 0.01)  se nocons nomtitles nonotes label  stats(N, labels("Observaciones")) replace addnote("Errores estándar cluster en paréntsis" "* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")

* Pregunta 5
* El efecto esta entre -7.6 y -7.7

*Pregunta 6 y 7
*sin controles
eststo clear
eststo m3: reg ophe i.round##i.treatcom if eligible==1,vce(cluster local) 
*con controles
eststo m4: reg ophe i.round##i.treatcom min_dist hhsize_basal female_hh educ_sp educ_hh bathroom_basal dirtfloor_basal age_sp age_hh  if eligible==1,vce(cluster local) 
esttab m3 m4 using "$root/tab4_67.tex",star(* 0.10 ** 0.05 *** 0.01)  se nocons nomtitles nonotes label  stats(N, labels("Observaciones")) replace addnote("Errores estándar cluster en paréntsis" "* p \textless 0.10, ** p \textless 0.05, *** p \textless 0.01.")
*Los resultados son similares

*-----------------------*
*       Caso 5          *
*-----------------------*

* Pregunta 1
tab eligible if treatcom==1 & round==0
tab eligible if score>750 & treatcom==1 & round==0
tab eligible if score<=750 & treatcom==1 & round==0

* Alternativa
tw 	(hist score if eligible==1 & treatcom==1 & round==0, lcolor(red) color(none)) ///
	(hist score if eligible==0 & treatcom==1 & round==0, lcolor(blue) color(none)), ///
	xline(750, lcolor(black) lpattern(solid)) legend(label(1 "Eligibles") label(2 "No eligibles"))
	
	*sugiere manipulación, pero en el sentido contrario, lo que es raro.

* Pregunta 2
regress ophe eligible score if round==1 & treatcom==1, vce(cluster local) 



* Pregunta 3
cap drop yhat0
predict yhat0 if e(sample), xb
scatter yhat0 score, xline(750)
