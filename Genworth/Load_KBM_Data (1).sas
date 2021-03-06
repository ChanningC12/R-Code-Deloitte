%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";

DATA RAW.KBM_DATA;
	INFILE "/PROJECTS/GENWORTH_2016/RAW/1609_ID3397E_FILE01" 	LRECL=1500 TRUNCOVER;
	INPUT 
@1 FILLER $111.
@112 ZIP $5.
@117 P4 $4.
@121 APTF $1.
@122 CRRT $4.
@126 FPST $2.
@128 N2CTY 3.
@131 N2DMA 4.
@135 CBSA $5.
@140 N2NCY $1.
@141 LAT $9.
@150 LNG $9.
@159 N2BTN $4.
@163 N2TSC $2.
@165 N2TBG $1.
@166 N2BC $4.
@170 FILLER $10.
@180 FILLER $10.
@190 FILLER $10.
@200 FILLER $1.
@201 OPA $1.
@202 FILLER $2.
@204 FILLER $1.
@205 GEND $1.
@206 RACE $1.
@207 FAMP $1.
@208 POEP $1.
@209 ILOR 2.
@211 ESTDII30 $1.
@212 APT $1.
@213 ESTINC30 $1.
@214 ZINCDEC 1.
@215 MR $1.
@216 MOBPLUS $1.
@217 FILLER $1.
@218 BC $1.
@219 RC $1.
@220 FILLER $4.
@224 CD 2.
@226 VETERAN $1.
@227 TINS 3.
@230 FILLER $1.
@231 CA00 1.
@232 CA03 1.
@233 CA06 1.
@234 CA11 1.
@235 CA16 1.
@236 FILLER $1.
@237 TUOC $4.
@241 ONLA $1.
@242 VFLAG $2.
@244 PRDM $1.
@245 HHID $9.
@254 NONO2 $12.
@266 TELE $10.
@276 CPPD $6.
@282 DPVS $1.
@283 PUBH $1.
@284 SSON $1.
@285 VAC $1.
@286 FILLER $3.
@289 N2ALV $1.
@290 NETW30 $1.
@291 FILLER $4.
@295 TUOCS $4.
@299 FILLER $1.
@300 PRESSURV $1.
@301 DOBMM 2.
@303 DOBDD 2.
@305 DOBCCYY 4.
@309 IOR $1.
@310 IHH $1.
@311 IIND $1.
@312 ICAT $1.
@313 IDOG $1.
@314 IC $1.
@315 ICCO $1.
@316 ICDO $1.
@317 ICFI $1.
@318 ICOT $1.
@319 ICPL $1.
@320 ICSP $1.
@321 ICST $1.
@322 ICFA $1.
@323 IPAE $1.
@324 IPDI $1.
@325 IPOT $1.
@326 IPSR $1.
@327 IPVM $1.
@328 IRAE $1.
@329 IRDI $1.
@330 IROT $1.
@331 IRSR $1.
@332 IRVM $1.
@333 IDC $1.
@334 IVET $1.
@335 IGC $1.
@336 IHBA $1.
@337 IHBI $1.
@338 IHCR $1.
@339 IHCI $1.
@340 IHCG $1.
@341 IHCO $1.
@342 IHCF $1.
@343 IHCA $1.
@344 IHGA $1.
@345 IHHI $1.
@346 IHHS $1.
@347 IHKN $1.
@348 IHLO $1.
@349 IHQU $1.
@350 IHSE $1.
@351 IHSW $1.
@352 IHTH $1.
@353 IHWO $1.
@354 IHWI $1.
@355 IHPH $1.
@356 IOCD $1.
@357 IOCE $1.
@358 IOMO $1.
@359 IORV $1.
@360 IOSW $1.
@361 IOLP $1.
@362 IMBO $1.
@363 IMBM $1.
@364 IMCH $1.
@365 IMCL $1.
@366 IMCO $1.
@367 IMFO $1.
@368 IMGI $1.
@369 IMHO $1.
@370 IMMA $1.
@371 IMOT $1.
@372 IMJE $1.
@373 IMPS $1.
@374 IMDV $1.
@375 INVE $1.
@376 INOT $1.
@377 INLI $1.
@378 INMU $1.
@379 INSB $1.
@380 INRE $1.
@381 IMGO $1.
@382 IMCS $1.
@383 IMCU $1.
@384 IMJA $1.
@385 IMOR $1.
@386 IMRH $1.
@387 IMRO $1.
@388 IMSO $1.
@389 IOHO $1.
@390 IOMA $1.
@391 IOSA $1.
@392 IOTR $1.
@393 IOPT $1.
@394 IREA $1.
@395 IRAS $1.
@396 IRBI $1.
@397 IRBF $1.
@398 IRBT $1.
@399 IRCH $1.
@400 IRCK $1.
@401 IRCO $1.
@402 IRCL $1.
@403 IRFA $1.
@404 IRHI $1.
@405 IRIN $1.
@406 IRME $1.
@407 IRMI $1.
@408 IRMY $1.
@409 IRNA $1.
@410 IRPE $1.
@411 IRRO $1.
@412 IRSF $1.
@413 IRST $1.
@414 IRSP $1.
@415 IRWO $1.
@416 IDAN $1.
@417 IDAC $1.
@418 IDCH $1.
@419 IDEN $1.
@420 IDHE $1.
@421 IDOT $1.
@422 IDPC $1.
@423 IDPL $1.
@424 IDRE $1.
@425 IDVE $1.
@426 ISOT $1.
@427 ISCA $1.
@428 ISBB $1.
@429 ISBO $1.
@430 ISBK $1.
@431 ISFS $1.
@432 ISFO $1.
@433 ISFI $1.
@434 ISGO $1.
@435 ISHO $1.
@436 ISHU $1.
@437 ISNA $1.
@438 ISSN $1.
@439 ISWA $1.
@440 ISRU $1.
@441 ISSC $1.
@442 ISTE $1.
@443 ISWL $1.
@444 ISPO $1.
@445 ITBU $1.
@446 ITBI $1.
@447 ITPU $1.
@448 ITPI $1.
@449 ITCV $1.
@450 ITFV $1.
@451 IVCR $1.
@452 IORE $1.
@453 IOSE $1.
@454 IOOW $1.
@455 IOST $1.
@456 IOMG $1.
@457 ITFF $1.
@458 ISBY $1.
@459 IDWGHT $1.
@460 IDNAT $1.
@461 IDVIT $1.
@462 ISDT $6.
@468 EMDM $1.
@469 BDOC $6.
@475 BANK $1.
@476 BNKI $1.
@477 FILLER $1.
@478 CNBC $6.
@484 PRESCA $1.
@485 SRET $1.
@486 MSR $1.
@487 SPEC $1.
@488 SPEA $1.
@489 FINB $1.
@490 FINI $1.
@491 CSR $1.
@492 HOMO $1.
@493 SPGD $1.
@494 TVMA $1.
@495 GROC $1.
@496 MISC $1.
@497 CACD $6.
@503 TRAD 1.
@504 CDB1 $6.
@510 CGN1 $1.
@511 CDB2 $6.
@517 CGN2 $1.
@518 CDB3 $6.
@524 CGN3 $1.
@525 CDB4 $6.
@531 CGN4 $1.
@532 CO $1.
@533 PART $1.
@534 VOTE $1.
@535 HTFH $1.
@536 BOAT $1.
@537 COLLEGE $1.
@538 FILLER $3.
@541 MEDA 2.
@543 FILLER $1.
@544 PDPE 2.
@546 FILLER $6.
@552 FILLER $10.
@562 SGDI $1.
@563 SGFA $1.
@564 SGHL $1.
@565 SGLL $1.
@566 SGOE $1.
@567 SGPA $1.
@568 SGSE $1.
@569 SGTC $1.
@570 SGUP $1.
@571 INSHOP $1.
@572 CONSHOP $1.
@573 SOHOIN $1.
@574 SOHOHH $1.
@575 HHCOMP $1.
@576 AIRCRAFT $1.
@577 PILOTS $1.
@578 ATV $1.
@579 SNOW $1.
@580 CCW $1.
@581 N65P $1.
@582 N6064 $1.
@583 N5059 $1.
@584 N4049 $1.
@585 N3039 $1.
@586 N2029 $1.
@587 N1819 $1.
@588 U18 $1.
@589 INMEDI $1.
@590 ISHUM $1.
@591 ITTIME $1.
@592 IMBBND $1.
@593 IMALT $1.
@594 IHEX $1.
@595 IHSCRAP $1.
@596 IHCLF $1.
@597 IHCAC $1.
@598 IHORSE $1.
@599 IRTHRLL $1.
@600 GENERS 2.
@602 BLSTORDR $4.
@606 BLONORDR $4.
@610 BLOFORDR $4.
@614 FILLER $6.
@620 BTOTLORD 2.
@622 BTOTLDOL 4.
@626 BONORDER 2.
@628 BONDOLLR 4.
@632 BOFORDER 2.
@634 BOFDOLLR 4.
@638 BAVGDOLL 4.
@642 BAVGONDL 4.
@646 BAVGOFDL 4.
@650 OAGEN $1.
@651 OABIGTLL $1.
@652 OACHILD $1.
@653 OATEEN $1.
@654 OAMEN $1.
@655 OAPETITE $1.
@656 OAPLUS $1.
@657 OAWOMEN $1.
@658 OANOGEN $1.
@659 OARTS $1.
@660 OCOLLECT $1.
@661 ONOVEL $1.
@662 OAUTO $1.
@663 OBOOKS $1.
@664 OCHILDPR $1.
@665 OCOMPHMO $1.
@666 OCRAFTS $1.
@667 OFOOD $1.
@668 OGEN $1.
@669 OGIFT $1.
@670 OSPECFD $1.
@671 OHOLIDAY $1.
@672 OSPECGFT $1.
@673 OSTATION $1.
@674 OBEAUTY $1.
@675 OHEALTH $1.
@676 OPERCARE $1.
@677 OELECTNC $1.
@678 OHMFURN $1.
@679 OFURNITR $1.
@680 OHSWARE $1.
@681 OLINENS $1.
@682 OHMCARE $1.
@683 OGARDEN $1.
@684 OJEWELRY $1.
@685 OMUSIC $1.
@686 OOTHER $1.
@687 OPETS $1.
@688 OPHOTO $1.
@689 OSPORTS $1.
@690 OTRAVEL $1.
@691 OVIDDVD $1.
@692 BPAMEX $1.
@693 BPCREDIT $1.
@694 BPDISCVR $1.
@695 BPHOUSE $1.
@696 BPMASTER $1.
@697 BPRETAIL $1.
@698 BPVISA $1.
@699 BUYER $1.
@700 HOMSTAT $1.
@701 NOC19 3.
@704 POC19 $1.
@705 NAH19 3.
@708 NPH19 3.
@711 FILLER $4.
@715 FILLER $1.
@716 MEDSUP $1.
@717 CALAGE 2.
@719 IRDEBIT $1.
@720 IDLOW $1.
@721 IOAPPLE $1.
@722 IOCABLE $1.
@723 IOHISPED $1.
@724 IODVR $1.
@725 IOGPS $1.
@726 IODVDPLR $1.
@727 IOHDTV $1.
@728 IOHMTHR $1.
@729 IOSATRAD $1.
@730 IOSATELL $1.
@731 IOVIDGAM $1.
@732 IOELEOT $1.
@733 IODIGITL $1.
@734 IOSMART $1.
@735 IHBEAD $1.
@736 IHDIY $1.
@737 IHGREEN $1.
@738 IHNETWRK $1.
@739 IHSPTWEL $1.
@740 IN401K $1.
@741 INCD $1.
@742 INIRA $1.
@743 IODOCTOR $1.
@744 IOFULL $1.
@745 IONURSE $1.
@746 IOPART $1.
@747 IOTEACHR $1.
@748 IOATV $1.
@749 IRBEST $1.
@750 IRBKCLUB $1.
@751 IRCOMICS $1.
@752 IRFINAN $1.
@753 IRHMGRDN $1.
@754 IRSELF $1.
@755 IRTRLENT $1.
@756 ISEXTREM $1.
@757 ISMTRCRS $1.
@758 ISSKATE $1.
@759 ISSNWBRD $1.
@760 ISROLLER $1.
@761 IVPARKS $1.
@762 ISDATEON $6.
@768 QHH $1.
@769 QIND $1.
@770 IHIGHSCL $1.
@771 PRESHI $1.
@772 IHGF $1.
@773 IHGO $1.
@774 IHGV $1.
@775 IPDECOR $1.
@776 INTSTMED $1.
@777 INNOMEDI $1.
@778 PRESMI $1.
@779 IPBEAUTY $1.
@780 IPCLUB $1.
@781 IPFFOOD $1.
@782 IPSPCBTY $1.
@783 IPCOUPON $1.
@784 IRMAGAZN $1.
@785 PRESRI $1.
@786 PRESTI $1.
@787 ITRV $1.
@788 LOWD $1.
@789 HIR $1.
@790 TRVL $1.
@791 FINC $1.
@792 CE $1.
@793 FURN $1.
@794 HOMI $1.
@795 MEMW $1.
@796 OILC $1.
@797 SPENDPAT 2.
@799 RORDERDT $4.
@803 RORDERS 2.
@805 RDOLLARS 4.
@809 RAGEN $1.
@810 RABIGTLL $1.
@811 RACHILD $1.
@812 RATEEN $1.
@813 RAMEN $1.
@814 RAPETITE $1.
@815 RAPLUS $1.
@816 RAWOMEN $1.
@817 RANOGEN $1.
@818 RARTS $1.
@819 RAUTO $1.
@820 RBEAUTY $1.
@821 RBOOKS $1.
@822 RCHILDPR $1.
@823 RCOLLECT $1.
@824 RCOMPHMO $1.
@825 RCRAFTS $1.
@826 RELECTNC $1.
@827 RFOOD $1.
@828 RFURNITR $1.
@829 RGARDEN $1.
@830 RGEN $1.
@831 RGIFT $1.
@832 RHEALTH $1.
@833 RHOLIDAY $1.
@834 RHMCARE $1.
@835 RHMFURN $1.
@836 RHSWARE $1.
@837 RJEWELRY $1.
@838 RLINENS $1.
@839 RMUSIC $1.
@840 RNOVEL $1.
@841 ROTHER $1.
@842 RPERCARE $1.
@843 RPETS $1.
@844 RPHOTO $1.
@845 RSPECFD $1.
@846 RSPECGFT $1.
@847 RSPORTS $1.
@848 RSTATION $1.
@849 RTRAVEL $1.
@850 RVIDDVD $1.
@851 RETAIL $1.
@852 INDMARST $1.
@853 HHMARST $1.
@854 Z4HMVALU $4.
@858 FILLER $10.
@868 FILLER $9.
@877 DESTGRP 2.
@879 DEVICE 2.
@881 ONLINE 2.
@883 DIGIPLAY $2.
@885 AILM $1.
@886 AACD $1.
@887 AACN $1.
@888 AALL $1.
@889 AASN $1.
@890 AALZ $1.
@891 AANG $1.
@892 AANX $1.
@893 AART $1.
@894 AASM $1.
@895 AAST $1.
@896 AADD $1.
@897 ABCK $1.
@898 ABED $1.
@899 ABBL $1.
@900 ABLD $1.
@901 ABCE $1.
@902 ACNC $1.
@903 ACNK $1.
@904 ABRC $1.
@905 ACFS $1.
@906 ACLD $1.
@907 ACGH $1.
@908 ACRN $1.
@909 ADAN $1.
@910 ADNT $1.
@911 ADEP $1.
@912 ADBT $1.
@913 ADT1 $1.
@914 ADT2 $1.
@915 ADTU $1.
@916 ADGS $1.
@917 ADRY $1.
@918 AECZ $1.
@919 AERC $1.
@920 AFOD $1.
@921 AFTA $1.
@922 AFTC $1.
@923 AGAS $1.
@924 AGRD $1.
@925 AHAR $1.
@926 AHRL $1.
@927 AHRA $1.
@928 AHRT $1.
@929 AHED $1.
@930 AHEM $1.
@931 AHBP $1.
@932 AHCH $1.
@933 AINS $1.
@934 AIRR $1.
@935 AIBS $1.
@936 AKID $1.
@937 ALAC $1.
@938 ALNG $1.
@939 AMAC $1.
@940 AMEN $1.
@941 AMGR $1.
@942 AMLS $1.
@943 ANAS $1.
@944 ANOS $1.
@945 AOSA $1.
@946 AOST $1.
@947 APAN $1.
@948 APRK $1.
@949 APAR $1.
@950 ARES $1.
@951 ARHM $1.
@952 ATTH $1.
@953 ASKN $1.
@954 ASIR $1.
@955 ASNR $1.
@956 ASTK $1.
@957 ATNF $1.
@958 AVIS $1.
@959 AWGH $1.
@960 PACNE $1.
@961 PADHDADD $1.
@962 PALLERG $1.
@963 PALZHMR $1.
@964 PANXIETY $1.
@965 PARTRSCS $1.
@966 PARTHITS $1.
@967 PASTHMA $1.
@968 PATHFOOT $1.
@969 PBACKPN $1.
@970 PBED $1.
@971 PBIPOLAR $1.
@972 PBLADDER $1.
@973 PBLOOD $1.
@974 PBRSTCAN $1.
@975 PBRONCH $1.
@976 PCANCER $1.
@977 PCARPAL $1.
@978 PCHRNCPN $1.
@979 PTRIAL $1.
@980 PCLDSORE $1.
@981 PCOLON $1.
@982 PCOPD $1.
@983 PCROHNS $1.
@984 PDANDRUF $1.
@985 PDNT $1.
@986 PDEPRESS $1.
@987 PDIABETE $1.
@988 PDIATYP1 $1.
@989 PDIATYP2 $1.
@990 PDRY $1.
@991 PECZEMA $1.
@992 PEMPHYSM $1.
@993 PEPIL $1.
@994 PFALLERG $1.
@995 PGAS $1.
@996 PGASTRIT $1.
@997 PGERDACD $1.
@998 PGINGIVT $1.
@999 PHAIRLSS $1.
@1000 PHOFFERS $1.
@1001 PHRL $1.
@1002 PHEARTAT $1.
@1003 PHRTACID $1.
@1004 PHEM $1.
@1005 PHGBLPRS $1.
@1006 PHGCHOLS $1.
@1007 PHORM $1.
@1008 PIBS $1.
@1009 PIMPOTNC $1.
@1010 PINSOMNA $1.
@1011 PKID $1.
@1012 PLAC $1.
@1013 PMENOPS $1.
@1014 PMENSTRL $1.
@1015 PMIGRHED $1.
@1016 PMULTSCL $1.
@1017 PNLFUNGS $1.
@1018 PNSLALRG $1.
@1019 POBESITY $1.
@1020 POSTEOA $1.
@1021 POSTEOP $1.
@1022 PPAIN $1.
@1023 PJPAIN $1.
@1024 PPRK $1.
@1025 PHANDICP $1.
@1026 PPRESENT $1.
@1027 PPROSCAN $1.
@1028 PPROSTAT $1.
@1029 PPSORIAS $1.
@1030 PRHEUMTS $1.
@1031 PSINUSIT $1.
@1032 PSIR $1.
@1033 PSNORING $1.
@1034 PSPINAL $1.
@1035 PULCER $1.
@1036 PCOLITIS $1.
@1037 PVIS $1.
@1038 PWEIGHT $1.
@1039 AILMENTS $1.
@1040 ABBA $1.
@1041 ABRT $1.
@1042 ADEN $1.
@1043 ADDE $1.
@1044 ADIS $1.
@1045 ADOM $1.
@1046 ADOT $1.
@1047 ADUM $1.
@1048 ACDE $1.
@1049 ACNT $1.
@1050 ACNS $1.
@1051 ACPM $1.
@1052 PADHDADM $1.
@1053 PALLERGY $1.
@1054 PANTIDEP $1.
@1055 PARTHRTS $1.
@1056 PASTHMAM $1.
@1057 PCORRECT $1.
@1058 PBIPMEDS $1.
@1059 PDETROL $1.
@1060 PCONSTIP $1.
@1061 PDENTURE $1.
@1062 PDIADIET $1.
@1063 PDIAINSU $1.
@1064 PORALINJ $1.
@1065 PDIAORAL $1.
@1066 PDIAOTHR $1.
@1067 PMONITOR $1.
@1068 PGLCOPHG $1.
@1069 PHEARING $1.
@1070 PHRTMEDS $1.
@1071 PHEARTRX $1.
@1072 PHEMORR $1.
@1073 PHBPMED $1.
@1074 PCHODIET $1.
@1075 PCHOLMED $1.
@1076 PIMITREX $1.
@1077 PLASERVC $1.
@1078 PLIPITOR $1.
@1079 PNAILFUN $1.
@1080 POBESITM $1.
@1081 POSTMEDS $1.
@1082 PPREMARN $1.
@1083 PSCAR $1.
@1084 PSKINMED $1.
@1085 PSLPMEDS $1.
@1086 PVIAGRA $1.
@1087 PVITSUPP $1.
@1088 PWHEELCH $1.
@1089 AGPAIN $1.
@1090 AGNOSE $1.
@1091 AGCANCER $1.
@1092 AGBLOOD $1.
@1093 AGDIAB $1.
@1094 AGDIG $1.
@1095 AGFEM $1.
@1096 AGFOOT $1.
@1097 AGHEART $1.
@1098 AGJOINT $1.
@1099 AGMALE $1.
@1100 AGAST $1.
@1101 AGMOUTH $1.
@1102 AGPROG $1.
@1103 AGPSYCO $1.
@1104 AGRESP $1.
@1105 AGSKIN $1.
@1106 AGURINAR $1.
@1107 AGVSN $1.
@1108 AGWEIGHT $1.
@1109 ETHEID $2.
@1111 REL $1.
@1112 ETHLANG $2.
@1114 COFO $2.
@1116 ASSIMIL $1.
@1117 C210MYS 1.
@1118 C210MAH 2.
@1120 C210PWH 2.
@1122 C210PHI 2.
@1124 C210PBL 2.
@1126 C210HMI 3.
@1129 C210CIP 2.
@1131 C210POO 2.
@1133 C210HVA 3.
@1136 C210PSU 2.
@1138 C210MOB 2.
@1140 C210BLU 2.
@1142 C210WHT 2.
@1144 C210PMR 2.
@1146 C210PWC 2.
@1148 C210AUTO 2.
@1150 C210EBI 3.
@1153 C210PDV 2.
@1155 C210PMV 2.
@1157 C210APVT 2.
@1159 C210BPVT 2.
@1161 C210M200 2.
@1163 C210B200 2.
@1165 C210KSES 3.
@1168 KLNK $15.
@1183 NXDP $1.
@1184 NXIP $1.
@1185 NXOA $1.
@1186 NXSD $1.
@1187 C210PDSC 2.
@1189 C210PDUC 2.
@1191 ETHGRPCD $1.
@1192 MATCHLEVEL $1.
@1193 CUST_REF 8.
@1201 PLCY_REF 8.
@1209 CLIENT_GENDER $13.
@1222 FILLER $231.
@1453 CLIENT_ZIP $10.
@1463 PRIMARY_INSURED_FLAG $20.
@1483 PRODUCT_PLCY_LIVES $18.

	;
RUN;



*%HTM_QCREPORT(RAW, KBM_DATA,/PROJECTS/GENWORTH_2016/02_CODES/DATALOAD/QC_KBM_DATA/NEW , FREQSIZE=40, SAMPLESIZE=20);
