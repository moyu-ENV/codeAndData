options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='1993';
GETNAMES=YES;
RUN;
PROC PRINT DATA=DATA_1;
RUN;
data data; 
set DATA_1;
array Locations{16} $ L1-L16 ('Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne' 'Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne' 'Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne' 'Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne');
array Marshtypes{16} $ M1-M16 ('BrackishMarsh' 'BrackishMarsh' 'BrackishMarsh' 'BrackishMarsh' 'FreshwaterMarsh' 'FreshwaterMarsh' 'FreshwaterMarsh' 'FreshwaterMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'SalineMarsh' 'SalineMarsh' 'SalineMarsh' 'SalineMarsh');
array units{16} unit1-unit16; 
do unit=1 to 16;
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
proc nlin data=data method=newton outest=G_out;
parms b=0.2 A=0.5 mu=0.205 sigma1=0.02 sigma2=0.017; 
model NDVI = b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
by unit;
run;
proc print data=g_out;
run;  

data byunit; 
input unit marshtype$ location$ b A mu sigma1 sigma2 MSe;
datalines;
1 Brackish Baratari 0.39152 0.11074 0.20541 0.020000 0.01700 0.00600
2 Brackish BretonSo 0.43621 0.09630 0.20500 0.019563 0.01259 0.00694
3 Brackish Pontchar 0.38282 0.15155 0.23793 0.066470 0.01309 0.00367
4 Brackish Terrebon 0.41945 0.12145 0.20500 0.020000 0.01700 0.00752
5 Freshwat Baratari 0.50853 0.18123 0.19651 0.020000 0.05842 0.0145
6 Freshwat BretonSo 0.39828 0.13915 0.21018 0.020000 0.01700 0.0113
7 Freshwat Pontchar 0.50097 0.17668 0.16533 0.022830 0.01096 0.0217
8 Freshwat Terrebon 0.42001 0.25795 0.13741 0.021271 0.14025 0.00512
9 Intermed Baratari 0.49160 0.16163 0.20764 0.020000 0.00880 0.0131
10 Intermed BretonSo 0.41741 0.13410 0.23525 0.052994 0.01144 0.00737
11 Intermed Pontchar 0.47011 0.15853 0.20500 0.020000 0.06204 0.0140
12 Intermed Terrebon 0.48184 0.16636 0.19544 0.020000 0.02058 0.00882
13 SalineMa Baratari 0.33729 0.07460 0.20801 0.020000 0.01700 0.00284
14 SalineMa BretonSo 0.26705 0.15607 0.14930 0.034718 0.24934 0.00312
15 SalineMa Pontchar 0.31039 0.09144 0.20500 0.018043 0.17469 0.00253
16 SalineMa Terrebon 0.33108 0.08866 0.19497 0.027293 0.01700 0.00341
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
parms b_b=0.4075000 A_b=0.1200100 mu_b=0.2133350 sigma1_b=0.0315083 sigma2_b=0.0149200
      b_f=0.4569475 A_f=0.1887525 mu_f=0.1773575 sigma1_f=0.0210253 sigma2_f=0.0566575
      b_i=0.4652400 A_i=0.1551550 mu_i=0.2108325 sigma1_i=0.0282485 sigma2_i=0.0257150
      b_s=0.3114525 A_s=0.1026925 mu_s=0.1893200 sigma1_s=0.0250135 sigma2_s=0.1145075
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma1=(sigma1_f*zf) + (sigma1_i*zi) + (sigma1_b*zb) + (sigma1_s*zs) + u_sigma1;
sigma2=(sigma2_f*zf) + (sigma2_i*zi) + (sigma2_b*zb) + (sigma2_s*zs) + u_sigma2;
pred =b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
model NDVI ~ normal(pred,0.0082463);
random u_b u_a u_mu u_sigma1 u_sigma2 ~normal([0,0,0,0,0],[0.0014619900,
-.0003198682,0.0011400029,
0.0000006082,-.0005833571,0.0005914431,
-.0002640155,0.0001283678,0.0001074081,0.0002187385,
-.0010722896,0.0015962153,-.0009475039,0.0000481230,0.0044435125]) 
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

contrast 'sigma F VS I' sigma1_f, sigma1_i;
contrast 'sigma F VS B' sigma1_f, sigma1_b;
contrast 'sigma F VS S' sigma1_f, sigma1_s;
contrast 'sigma I VS B' sigma1_i, sigma1_b;
contrast 'sigma I VS S' sigma1_i, sigma1_s;
contrast 'sigma B VS S' sigma1_b, sigma1_s;

predict pred out=out_G;
run;

proc sort data=out_G;
by  marshtype location;
run;
proc print data=out_G;
run;
 
proc sort data=out_G;
by year marshtype doy;
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

