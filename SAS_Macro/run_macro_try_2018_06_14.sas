libname sasf "C:\Users\noah_padgett1\Desktop\SAS_Macro\";

%include "C:\Users\noah_padgett1\Desktop\SAS_Macro\PIAAC_Tool.sas";

%PIAAC_TOOL(
	method = reg,
	table = Piaac_data,
	wgt = SPFWT0,
	rwgt = SPFWT,
	nrep = 80,
	vemethod = VEMETHOD,
	nb_pv = 10,
	dvar = PVNUM, 
	dvarpv = 1,
	ivar = GENDER_R,
	R2 = 1,
	byvar = CNTRYID
);
