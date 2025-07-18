libname data "~/data/";
PROC IMPORT DBMS=csv OUT=data.msmfinal_imputed replace
DATAFILE="~/data/cs_imputed_2023.csv";
GETNAMES=YES;
/* calculation of inverse probability of treatment weighting */
proc countreg data = data.msmfinal_imputed;
  class edulevel1 hhincome1 emply21 marital1;
  by imputation_number;
  model a2 = age1 sex edulevel1 hhincome1 emply21 marital1 phq_10yesno1 a1 fapgar1 /  dist=negbin ;
  output out = data.pred2  prob=prob2 probcount(0 to 10 by 1);
run;
data data.pred2;
set data.pred2;
keep member_id Imputation_Number prob2;
rename prob2=pdb2;
run;
proc countreg data = data.msmfinal_imputed;
  class edulevel1 hhincome1 emply21 marital1;
  by imputation_number;
  model a2 = age1 sex edulevel1 hhincome1 emply21 marital1 fapgar1 a1 phq_10yesno1 /  dist=negbin ;
  output out = data.predn2  prob=prob2 probcount(0 to 10 by 1);
run;
data data.predn2;
set data.predn2;
keep member_id Imputation_Number prob2 ;
rename prob2=pnb2;
run;
proc countreg data = data.msmfinal_imputed;
  class edulevel1 hhincome1 emply21 marital1 socgpre3;
  by imputation_number;
  model a3 = age1 sex edulevel1 hhincome1 emply21 marital1 phq_10yesno1 a1 fapgar1 phq_10yesno2 a2 fapgar2 socgpre3 par_ocb3 /  dist=negbin ;
  output out = data.pred3  prob=prob3 probcount(0 to 10 by 1);
run;
data data.pred3;
set data.pred3;
keep member_id Imputation_Number prob3;
rename prob3=pdb3;
run;
proc countreg data = data.msmfinal_imputed;
  class edulevel1 hhincome1 emply21 marital1;
  by imputation_number;
  model a3 = age1 sex edulevel1 hhincome1 emply21 marital1 fapgar1 a1 phq_10yesno1 a2 /  dist=negbin ;
  output out = data.predn3  prob=prob3 probcount(0 to 10 by 1);
run;
data data.predn3;
set data.predn3;
keep member_id Imputation_Number prob3 ;
rename prob3=pnb3;
run;
/* to repeat above calculations of IPTWs for the study waves in each analyses */
/* weights stabilized */
data data.msmfinal_weight;
set data.msmfinal_imputed;
merge data.pred2 data.pred3 data.predn2 data.predn3 ;
by Imputation_Number member_id;
sw2=pnb2/pdb2;
sw3=pnb3/pdb3;
cuma=(a2+a3)/2;
sw=sw2*sw3;
rename Imputation_Number=_imputation_;
run;
/* weights trimmed*/
proc univariate data=data.msmfinal_weight noprint;
   var sw;
   output out=percentiles pctlpts=1 99 pctlpre=P;
run;
data _null_;
   set percentiles;
   call symput('p1', P1);
   call symput('p99', P99);
run;
data data.msmfinal_weight_trimmed;
   set data.msmfinal_weight;
   if sw < &p1 then sw = &p1;
   else if sw > &p99 then sw = &p99;
run;
/* outcome model */
proc genmod data = data.msmfinal_weight_trimmed descending;
class edulevel1 hhincome1 emply21 marital1 member_id;
model phq_10yesno_wx = cuma age1 sex edulevel1 hhincome1 emply21 marital1 phq_10yesno1 a1 fapgar1 /dist=binomial link=logit;
weight sw;
repeated subject = member_id/ type = unstr PRINTMLE ;
by _imputation_;
ods output ParameterEstimates=msm;
run;
proc mianalyze parms=msm;
modeleffects cuma;
run;
