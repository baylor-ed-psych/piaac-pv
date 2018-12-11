libname sasf "C:\Users\noah_padgett1\Desktop\PIAAC_PV\SAS_Macro\MI_with_weights\";

%include "C:\Users\noah_padgett1\Desktop\PIAAC_PV\SAS_Macro\MI_with_weights\PIAAC_Tool.sas";

%PIAAC_TOOL(
	method = reg,
	table = PIAAC_DATA,
	wgt = SPFWT0,
	rwgt = SPFWT,
	nrep = 80,
	vemethod = VEMETHOD,
	nb_pv = 10,
	dvar = PVNUM, 
	dvarpv = 1,
	ivar = Female,
	R2 = 1,
	byvar = CNTRYID
);
