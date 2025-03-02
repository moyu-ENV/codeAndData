options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='19912';
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
parms b=0.1 A=0.5 mu=0.21 sigma1=0.022 sigma2=0.018; 
model NDVI = b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
by unit;
run;
proc print data=g_out;
run;  

data byunit; 
input unit marshtype$ location$ b A mu sigma1 sigma2 MSe;
datalines;
1 Brackish Baratari 0.38793 0.02990 0.21479 0.01369 0.018000 0.00350
2 Brackish BretonSo 0.46623 0.00628 0.22000 0.03152 0.018000 0.00381
3 Brackish Pontchar 0.48000 0.03102 0.22802 0.05880 0.018000 0.00349
4 Brackish Terrebon 0.43181 0.07057 0.22771 0.01903 0.018000 0.00527
5 Freshwat Baratari 0.55122 0.09512 0.22409 0.03010 0.018000 0.00806
6 Freshwat Pontchar 0.55242 0.05841 0.22516 0.04525 0.018000 0.00756
7 Freshwat Terrebon 0.53049 0.13799 0.22331 0.05505 0.018000 0.0134
8 Intermed Baratari 0.48654 0.08013 0.22507 0.02264 0.018000 0.00716
9 Intermed BretonSo 0.37460 0.21513 0.23423 0.02350 0.044961 0.00133
10 Intermed Pontchar 0.51222 0.08539 0.22470 0.01783 0.018000 0.00826
11 Intermed Terrebon 0.49104 0.10433 0.22451 0.02564 0.018000 0.00889
12 SalineMa Baratari 0.33686 0.04729 0.23927 0.04039 0.052767 0.00230
13 SalineMa BretonSo 0.40619 0.02540 0.22000 0.30981 0.018000 0.00182
14 SalineMa Pontchar 0.39584 0.01805 0.22000 0.07862 0.018000 0.00158
15 SalineMa Terrebon 0.36735 0.02824 0.22305 0.02596 0.018000 0.00318
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
parms b_b=0.4414925 A_b=0.0344425 mu_b=0.2226300 sigma1_b=0.0307600 sigma2_b=0.0180000
      b_f=0.5447100 A_f=0.0971733 mu_f=0.2241867 sigma1_f=0.0434667 sigma2_f=0.0180000
      b_i=0.4661000 A_i=0.1212450 mu_i=0.2271275 sigma1_i=0.0224025 sigma2_i=0.0247403
      b_s=0.3765600 A_s=0.0297450 mu_s=0.2255800 sigma1_s=0.1136950 sigma2_s=0.0266918
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma1=(sigma1_f*zf) + (sigma1_i*zi) + (sigma1_b*zb) + (sigma1_s*zs) + u_sigma1;
sigma2=(sigma2_f*zf) + (sigma2_i*zi) + (sigma2_b*zb) + (sigma2_s*zs) + u_sigma2;
pred =b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
model NDVI ~ normal(pred,0.0053073);
random u_b u_a u_mu u_sigma1 u_sigma2 ~normal([0,0,0,0,0],[0.0018010152,
-.0013064420,0.0016223420,
-.0001027000,0.0001288795,0.0000407708,
0.0009559671,-.0001296407,-.0001321815,0.0049388064,
-.0003497438,0.0002855655,0.0000606773,-.0002290005,0.0001319754]) 
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

