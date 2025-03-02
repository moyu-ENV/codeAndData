options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='19902';
GETNAMES=YES;
RUN;
PROC PRINT DATA=DATA_1;
RUN;
data data; 
set DATA_1;
array Locations{15} $ L1-L15 ('Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne' 'Barataria' 'Pontchartrain' 'Terrebonne' 'Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne' 'Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne');
array Marshtypes{15} $ M1-M15 ('BrackishMarsh' 'BrackishMarsh' 'BrackishMarsh' 'BrackishMarsh' 'FreshwaterMarsh' 'FreshwaterMarsh' 'FreshwaterMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'SalineMarsh' 'SalineMarsh' 'SalineMarsh' 'SalineMarsh');
array units{15} unit1-unit15; 
do unit=1 to 15;
Marshtype = MarshTypes(unit);
Location = Locations(unit);
NDVI=units(unit);
output;
end;
keep year doy unit Marshtype Location NDVI;
run;
proc print data=data;
run;
/*end of import data*/


/*GAUSS distribution*/
ods select ConvergenceStatus Anova ParameterEstimates;
proc sort data=data;
by unit;
run;
proc nlin data=data maxiter=2000 method=newton outest=G_out;
parms b=0.1 A=0.5 mu=0.23 sigma1=0.02 sigma2=0.02; 
model NDVI = b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
by unit;
run;
proc print data=g_out;
run;  

data byunit; 
input unit marshtype$ location$ b A mu sigma1 sigma2 MSe;
datalines;
1 Brackish Baratari 0.41656 0.02675 0.23000 0.02000 0.02000 0.00211
2 Brackish BretonSo 0.45178 0.06355 0.23000 0.02000 0.02000 0.00339
3 Brackish Pontchar 0.47924 0.07200 0.23000 0.02000 0.02000 0.00227
4 Brackish Terrebon 0.15400 0.35281 0.26685 0.16174 0.06538 0.00160
5 Freshwat Baratari 0.59870 0.07736 0.23000 0.02000 0.02682 0.00591
6 Freshwat Pontchar 0.57253 0.07588 0.23000 0.02000 0.02000 0.00402
7 Freshwat Terrebon 0.57948 0.13690 0.23000 0.02000 0.02000 0.00706
8 Intermed Baratari 0.50596 0.11287 0.23000 0.02000 0.02000 0.00257
9 Intermed BretonSo 0.27526 0.27141 0.25766 0.05484 0.04706 0.000776
10 Intermed Pontchar 0.53355 0.12802 0.23000 0.02000 0.02677 0.00410
11 Intermed Terrebon 0.49231 0.12955 0.23000 0.02000 0.02000 0.00675
12 SalineMa Baratari 0.34990 0.04786 0.23000 0.02669 0.06022 0.00166
13 SalineMa BretonSo 0.40941 0.03306 0.23000 0.02000 0.22698 0.00207
14 SalineMa Pontchar 0.40308 0.02639 0.23000 0.02000 0.05496 0.00118
15 SalineMa Terrebon 0.34910 0.06244 0.23582 0.07218 0.02000 0.00253
RUN;
proc print data=byunit;
run;
PROC MEANS DATA = byunit MEAN VAR;
VAR b A mu sigma1 sigma2 MSe;
RUN;
PROC MEANS DATA = byunit MEAN VAR;
VAR b A mu sigma1 sigma2 MSe;
BY marshtype;
RUN; 
proc discrim data= byunit pcov;
class marshtype;
var b a mu sigma1 sigma2;
run;

proc sort data=data;
by unit;
run;
Proc nlmixed data=data method=firo;
if (marshtype='Freshwat') then do;zf=1;zi=0;zb=0;zs=0;end;
if (marshtype='Intermed') then do;zf=0;zi=1;zb=0;zs=0;end;
if (marshtype='Brackish') then do;zf=0;zi=0;zb=1;zs=0;end;
if (marshtype='SalineMa') then do;zf=0;zi=0;zb=0;zs=1;end;
parms b_b=0.3753950 A_b=0.1287775 mu_b=0.2392125 sigma1_b=0.0554350 sigma2_b=0.0313450
      b_f=0.5835700 A_f=0.0967133 mu_f=0.2300000 sigma1_f=0.0200000 sigma2_f=0.0222733
      b_i=0.4517700 A_i=0.1604625 mu_i=0.2369150 sigma1_i=0.0287100 sigma2_i=0.0284575
      b_s=0.0284575 A_s=0.0284575 mu_s=0.2314550 sigma1_s=0.0347175 sigma2_s=0.0347175
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma1=(sigma1_f*zf) + (sigma1_i*zi) + (sigma1_b*zb) + (sigma1_s*zs) + u_sigma1;
sigma2=(sigma2_f*zf) + (sigma2_i*zi) + (sigma2_b*zb) + (sigma2_s*zs) + u_sigma2;
pred =b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
model NDVI ~ normal(pred,0.0031997);
random u_b u_a u_mu u_sigma1 u_sigma2 ~normal([0,0,0,0,0],[0.0103054934,
-.0084001330,0.0079872260,
-.0012007389,0.0010400745,0.0001470594,
-.0035653292,0.0033363440,0.0004416479,0.0016253754,
-.0007165997,0.0009575751,0.0001234720,0.0001444195,0.0025313163]) 
subject=unit;

contrast 'b0 F VS I' b_f, b_i;
contrast 'b0 F VS B' b_f, b_b;
contrast 'b0 F VS S' b_f, b_s;
contrast 'b0 I VS B' b_i, b_b;
contrast 'b0 I VS S' b_i, b_s;
contrast 'b0 B VS S' b_b, b_s;

contrast 'A F VS I' A_f, A_i;
contrast 'A F VS B' A_f, A_b;
contrast 'A F VS S' A_f, A_s;
contrast 'A I VS B' A_i, A_b;
contrast 'A I VS S' A_i, A_s;
contrast 'A B VS S' A_b, A_s;

contrast 'mu F VS I' mu_f, mu_i;
contrast 'mu F VS B' mu_f, mu_b;
contrast 'mu F VS S' mu_f, mu_s;
contrast 'mu I VS B' mu_i, mu_b;
contrast 'mu I VS S' mu_i, mu_s;
contrast 'mu B VS S' mu_b, mu_s;

contrast 'sigma1 F VS I' sigma1_f, sigma1_i;
contrast 'sigma1 F VS B' sigma1_f, sigma1_b;
contrast 'sigma1 F VS S' sigma1_f, sigma1_s;
contrast 'sigma1 I VS B' sigma1_i, sigma1_b;
contrast 'sigma1 I VS S' sigma1_i, sigma1_s;
contrast 'sigma1 B VS S' sigma1_b, sigma1_s;

contrast 'sigma2 F VS I' sigma2_f, sigma2_i;
contrast 'sigma2 F VS B' sigma2_f, sigma2_b;
contrast 'sigma2 F VS S' sigma2_f, sigma2_s;
contrast 'sigma2 I VS B' sigma2_i, sigma2_b;
contrast 'sigma2 I VS S' sigma2_i, sigma2_s;
contrast 'sigma2 B VS S' sigma2_b, sigma2_s;

predict pred out=out_G;
run;

proc sort data=out_G;
by  marshtype location ;
run;
proc print data=out_G;
run;

proc sort data=out_G;
by year marshtype doy  ;
run;
 
PROC MEANS DATA = out_G MEAN;
VAR NDVI;
by year marshtype doy ;
output out=data_NDVI mean=mean;
RUN;
proc print data=data_ndvi;
run;

PROC MEANS DATA = out_G MEAN;
VAR pred;
by year marshtype doy ;
output out=data_pred mean=mean;
RUN;
proc print data=data_pred;
run;

PROC MEANS DATA = out_G MEAN;
VAR lower;
by year marshtype doy ;
output out=data_lower mean=mean;
RUN;
proc print data=data_lower;
run;


PROC MEANS DATA = out_G MEAN;
VAR upper;
by year marshtype doy ;
output out=data_upper mean=mean;
RUN;
proc print data=data_upper;
run;
/*GAUSS distribution*/

