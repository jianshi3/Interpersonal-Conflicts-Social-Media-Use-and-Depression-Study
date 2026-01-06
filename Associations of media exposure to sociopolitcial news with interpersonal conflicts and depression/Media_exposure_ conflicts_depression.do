import delimited "~/data/imputed_socialmedia_depression_keyvariables.csv", encoding(ISO-8859-1)clear
mi import flong, m(imputationnumber) id(member_id) clear
mi reshape long polconf phq_10yesno polconfbase phq_10yesnobase fapgarbase phq_10yesnoout polconffc polconffcbase socgpreb ///
 tvgpb prgpb phqbase phqout, i(member_id) j(wave)

mi xeq: encode member_id, generate(idn)
mi xtset idn wave
/* GEE model for associations of media exposure to sociopolitical news with probable depression */
//check effect modifications by study waves
mi estimate, eform: xtgee phq_10yesnoout sexbase agebase i.hhincomebase i.emplybase i.maritalbase i.edulevelbase i.wave wave##socgpreb ib2.socgpreb phq_10yesnobase polconfbase polconffcbase ib2.tvgpb ib2.prgpb, family(binomial) link(logit) corr(uns) vce(robust) 
mi test 4.wave#1.socgpreb 4.wave#3.socgpreb 4.wave#4.socgpreb 8.wave#1.socgpreb 8.wave#3.socgpreb 8.wave#4.socgpreb 9.wave#1.socgpreb 9.wave#3.socgpreb 9.wave#4.socgpreb
//no effect modifications found by study waves
mi estimate, eform: xtgee phq_10yesnoout sexbase agebase i.hhincomebase i.emplybase i.maritalbase i.edulevelbase i.wave ib2.socgpreb phq_10yesnobase polconfbase polconffcbase ib2.tvgpb ib2.prgpb, family(binomial) link(logit) corr(uns) vce(robust) 
/* GEE model for associations of media exposure to sociopolitical news with probable depressive symptoms */
mi estimate, eform: xtgee phqout sexbase agebase i.hhincomebase i.emplybase i.maritalbase i.edulevelbase i.wave wave##socgpreb ib2.socgpreb phqbase polconfbase polconffcbase ib2.tvgpb ib2.prgpb, family(nb) link(log) corr(uns) vce(robust) 
mi test 4.wave#1.socgpreb 4.wave#3.socgpreb 4.wave#4.socgpreb 8.wave#1.socgpreb 8.wave#3.socgpreb 8.wave#4.socgpreb 9.wave#1.socgpreb 9.wave#3.socgpreb 9.wave#4.socgpreb
mi estimate, eform: xtgee phqout sexbase agebase i.hhincomebase i.emplybase i.maritalbase i.edulevelbase i.wave ib2.socgpreb phqbase polconfbase polconffcbase ib2.tvgpb ib2.prgpb, family(nb) link(log) corr(uns) vce(robust) 
/* GEE model for associations of media exposure to sociopolitical news with sociopolitical conflicts with family members */
mi estimate, eform: xtgee polconf polconfbase polconffcbase sexbase agebase i.hhincomebase i.emplybase i.maritalbase i.edulevelbase ib2.socgpreb i.wave wave##socgpreb phq_10yesnobase ib2.tvgpb ib2.prgpb, family(nbinomial) link(log) corr(uns) vce(robust) 
mi test 4.wave#1.socgpreb 4.wave#3.socgpreb 4.wave#4.socgpreb 8.wave#1.socgpreb 8.wave#3.socgpreb 8.wave#4.socgpreb 9.wave#1.socgpreb 9.wave#3.socgpreb 9.wave#4.socgpreb
mi estimate, eform: xtgee polconf polconfbase polconffcbase sexbase agebase i.hhincomebase i.emplybase i.maritalbase i.edulevelbase ib2.socgpreb i.wave phq_10yesnobase ib2.tvgpb ib2.prgpb, family(nbinomial) link(log) corr(uns) vce(robust) 
/* GEE model for associations of media exposure to sociopolitical news with sociopolticial conflicts with friends and colleagues */
mi estimate, eform: xtgee polconffc polconfbase polconffcbase sexbase agebase i.hhincomebase i.emplybase i.maritalbase i.edulevelbase ib2.socgpreb i.wave wave##socgpreb phq_10yesnobase ib2.tvgpb ib2.prgpb, family(nbinomial) link(log) corr(uns) vce(robust) 
mi test 4.wave#1.socgpreb 4.wave#3.socgpreb 4.wave#4.socgpreb 8.wave#1.socgpreb 8.wave#3.socgpreb 8.wave#4.socgpreb 9.wave#1.socgpreb 9.wave#3.socgpreb 9.wave#4.socgpreb
mi estimate, eform: xtgee polconffc polconfbase polconffcbase sexbase agebase i.hhincomebase i.emplybase i.maritalbase i.edulevelbase ib2.socgpreb i.wave phq_10yesnobase ib2.tvgpb ib2.prgpb, family(nbinomial) link(log) corr(uns) vce(robust) 

/* To estimate the weighted level of conflict by media exposure to sociopolitical news in the population*/ 
mi estimate, saving(polconffile, replace) esample(misample) eform: xtgee polconf polconfbase polconffcbase  sexbase agebase i.hhincomebase i.emplybase i.maritalbase i.edulevelbase ib2.socgpreb i.wave phq_10yesnobase  ///
 ib2.tvgpb ib2.prgpb, family(nbinomial) link(log) corr(uns) vce(robust) 
mimrgns socgpreb using polconffile [pweight=weights], esample(misample)  over(wave) eform  // raking weights used for population representativeness
return list
putexcel set media_conflict_mi, replace
matrix social_conflict_mi = r(table)'
putexcel  A1 = matrix(social_conflict_mi), rownames 

mimrgns tvgpb using polconffile [pweight=weights], esample(misample)  over(wave) eform
return list
putexcel set tv_conflict_mi, replace
matrix tv_conflict_mi = r(table)'
putexcel  A1 = matrix(tv_conflict_mi), rownames 

mimrgns prgpb using polconffile [pweight=weights], esample(misample)  over(wave) eform
return list
putexcel set prgpb_conflict_mi, replace
matrix prgpb_conflict_mi = r(table)'
putexcel  A1 = matrix(prgpb_conflict_mi), rownames 
