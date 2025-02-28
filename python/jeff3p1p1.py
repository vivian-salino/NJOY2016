#!/usr/local/bin/python
from PyNjoy import *
import os
jeff3p1 = PyNjoy()
evaluationDir = os.getcwd() + "/../../ENDF/JEFF-3.1.1/"
jeff3p1.evaluationName = "../output/JEFF-3.1.1/"
jeff3p1.execDir = ".."
jeff3p1.nstr = 22
jeff3p1.iwt = 4
jeff3p1.Espectra = None
jeff3p1.autolib = (2.76792, 677.2873, 0.00125)
jeff3p1.serpent = True

jeff3p1.legendre = 3
jeff3p1.hmat = "H1_H2O"
jeff3p1.mat = 125
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0125_0.ASC"
jeff3p1.scatteringLaw =  evaluationDir + "JEFF31TS_INDIV/JEFF31TS0001_1.ASC"
jeff3p1.scatteringMat = 1
jeff3p1.temperatures = ( 293.6, 323.6, 373.6, 423.6, 473.6, 523.6, 573.6, 647.2, 800.0 )
jeff3p1.fission = None
jeff3p1.dilutions = None
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.suff = ( 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08 )
jeff3p1.scatName = "lwtr"
jeff3p1.za = 1001
jeff3p1.acer()

jeff3p1.legendre = 1
jeff3p1.hmat = "H2_D2O"
jeff3p1.mat = 128
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0128_0.ASC"
jeff3p1.scatteringLaw =  evaluationDir + "JEFF31TS_INDIV/JEFF31TS0011_1.ASC"
jeff3p1.scatteringMat = 11
jeff3p1.temperatures = ( 293.6, 323.6, 373.6, 423.6, 473.6, 523.6, 573.6, 643.9 )
jeff3p1.fission = None
jeff3p1.dilutions = None
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.suff = ( 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07 )
jeff3p1.scatName = "hwtr"
jeff3p1.za = 1002
jeff3p1.acer()

jeff3p1.hmat = "H1_CH2"
jeff3p1.mat =  125
jeff3p1.temperatures = ( 293.6,  350. )
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0125_0.ASC"
jeff3p1.scatteringLaw =  evaluationDir + "JEFF31TS_INDIV/JEFF31TS0037_1.ASC"
jeff3p1.scatteringMat = 37
jeff3p1.fission = None
jeff3p1.dilutions = None
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.suff = ( 0.00, 0.01 )
jeff3p1.scatName = "poly"
jeff3p1.za = 1001
jeff3p1.acer()

jeff3p1.hmat = "H1_ZRH"
jeff3p1.mat =  125
jeff3p1.temperatures = ( 293.6, 400., 500., 600., 700., 800., 1000., 1200. )
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0125_0.ASC"
jeff3p1.scatteringLaw =  evaluationDir + "JEFF31TS_INDIV/JEFF31TS0007_1.ASC"
jeff3p1.scatteringMat = 7
jeff3p1.fission = None
jeff3p1.dilutions = None
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.suff = ( 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07 )
jeff3p1.scatName = "zrh"
jeff3p1.za = 1001
jeff3p1.acer()

jeff3p1.hmat = "Be9"
jeff3p1.mat = 425
jeff3p1.temperatures = ( 293.6 , 400 , 500 , 600 , 700 , 800 , 1000 , 1200)
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0425_0.ASC"
jeff3p1.scatteringLaw =  evaluationDir + "JEFF31TS_INDIV/JEFF31TS0026_1.ASC"
jeff3p1.scatteringMat = 26
jeff3p1.fission = None
jeff3p1.dilutions = None
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.suff = ( 0.00 , 0.01 , 0.02 , 0.03 , 0.04 , 0.05 , 0.06 , 0.07)
jeff3p1.scatName = "be"
jeff3p1.za = 4009
jeff3p1.acer()

jeff3p1.hmat = "C0_GR"
jeff3p1.mat = 600
jeff3p1.temperatures = ( 293.6 , 400 , 500 , 600 , 700 , 800 , 1000 , 1200 , 1600 , 2000 )
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0600_0.ASC"
jeff3p1.scatteringLaw =  evaluationDir + "JEFF31TS_INDIV/JEFF31TS0031_1.ASC"
jeff3p1.scatteringMat = 31
jeff3p1.fission = None
jeff3p1.dilutions = None
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.suff = ( 0.00 , 0.01 , 0.02 , 0.03 , 0.04 , 0.05 , 0.06 , 0.07 , 0.08 , 0.09 )
jeff3p1.scatName = "grph"
jeff3p1.za = 6000
jeff3p1.acer()

jeff3p1.scatName = None
jeff3p1.scatteringLaw = None
jeff3p1.temperatures = ( 293., 550., 900., 1200., 2000. )
jeff3p1.suff = ( 0.02, 0.05, 0.09, 0.12, 0.20 )
jeff3p1.fission = None
jeff3p1.dilutions = None

jeff3p1.hmat = "H1"
jeff3p1.mat = 125
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0125_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 1001
jeff3p1.acer()

jeff3p1.hmat = "H2"
jeff3p1.mat = 128
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0128_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 1002
jeff3p1.acer()

jeff3p1.hmat = "H3"
jeff3p1.mat = 131
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0131_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 1003
jeff3p1.acer()

jeff3p1.hmat = "He3"
jeff3p1.mat = 225
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0225_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 2003
jeff3p1.acer()

jeff3p1.hmat = "He4"
jeff3p1.mat = 228
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0228_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 2004
jeff3p1.acer()

jeff3p1.hmat = "Li6"
jeff3p1.mat = 325
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0325_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 3006
jeff3p1.acer()

jeff3p1.hmat = "Li7"
jeff3p1.mat = 328
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0328_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 3007
jeff3p1.acer()

jeff3p1.hmat = "B10"
jeff3p1.mat = 525
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0525_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 5010
jeff3p1.acer()

jeff3p1.hmat = "B11"
jeff3p1.mat = 528
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0528_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 5011
jeff3p1.acer()

jeff3p1.hmat = "C0"
jeff3p1.mat = 600
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0600_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 6000
jeff3p1.acer()

jeff3p1.hmat = "N14"
jeff3p1.mat = 725
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0725_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 7014
jeff3p1.acer()

jeff3p1.hmat = "N15"
jeff3p1.mat = 728
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0728_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 7015
jeff3p1.acer()

jeff3p1.hmat = "O16"
jeff3p1.mat = 825
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0825_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 8016
jeff3p1.acer()

jeff3p1.hmat = "O17"
jeff3p1.mat = 828
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0828_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 8017
jeff3p1.acer()

jeff3p1.hmat = "F19"
jeff3p1.mat = 925
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N0925_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 9019
jeff3p1.acer()

jeff3p1.hmat = "Na23"
jeff3p1.mat = 1125
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1125_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 11023
jeff3p1.acer()

jeff3p1.hmat = "Mg24"
jeff3p1.mat = 1225
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1225_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 12024
jeff3p1.acer()

jeff3p1.hmat = "Mg25"
jeff3p1.mat = 1228
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1228_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 12025
jeff3p1.acer()

jeff3p1.hmat = "Mg26"
jeff3p1.mat = 1231
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1231_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 12026
jeff3p1.acer()

jeff3p1.hmat = "Al27"
jeff3p1.mat = 1325
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1325_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 13027
jeff3p1.acer()

jeff3p1.hmat = "Si28"
jeff3p1.mat = 1425
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1425_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 14028
jeff3p1.acer()

jeff3p1.hmat = "Si29"
jeff3p1.mat = 1428
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1428_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 14029
jeff3p1.acer()

jeff3p1.hmat = "Si30"
jeff3p1.mat = 1431
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1431_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 14030
jeff3p1.acer()

jeff3p1.hmat = "P31"
jeff3p1.mat = 1525
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1525_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 15031
jeff3p1.acer()

jeff3p1.hmat = "S32"
jeff3p1.mat = 1625
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1625_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 16032
jeff3p1.acer()

jeff3p1.hmat = "S33"
jeff3p1.mat = 1628
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1628_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 16033
jeff3p1.acer()

jeff3p1.hmat = "S34"
jeff3p1.mat = 1631
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1631_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 16034
jeff3p1.acer()

jeff3p1.hmat = "S36"
jeff3p1.mat = 1637
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1637_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 16036
jeff3p1.acer()

jeff3p1.hmat = "Cl35"
jeff3p1.mat = 1725
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1725_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 17035
jeff3p1.acer()

jeff3p1.hmat = "Cl37"
jeff3p1.mat = 1731
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1731_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 17037
jeff3p1.acer()

jeff3p1.hmat = "K39"
jeff3p1.mat = 1925
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1925_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 19039
jeff3p1.acer()

jeff3p1.hmat = "K40"
jeff3p1.mat = 1928
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1928_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 19040
jeff3p1.acer()

jeff3p1.hmat = "K41"
jeff3p1.mat = 1931
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N1931_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 19041
jeff3p1.acer()

jeff3p1.hmat = "Ca40"
jeff3p1.mat = 2025
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2025_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 20040
jeff3p1.acer()

jeff3p1.hmat = "Ca42"
jeff3p1.mat = 2031
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2031_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 20042
jeff3p1.acer()

jeff3p1.hmat = "Ca43"
jeff3p1.mat = 2034
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2034_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 20043
jeff3p1.acer()

jeff3p1.hmat = "Ca44"
jeff3p1.mat = 2037
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2037_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 20044
jeff3p1.acer()

jeff3p1.hmat = "Ca46"
jeff3p1.mat = 2043
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2043_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 20046
jeff3p1.acer()

jeff3p1.hmat = "Ca48"
jeff3p1.mat = 2049
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2049_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 20048
jeff3p1.acer()

jeff3p1.hmat = "Ti46"
jeff3p1.mat = 2225
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2225_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 22046
jeff3p1.acer()

jeff3p1.hmat = "Ti47"
jeff3p1.mat = 2228
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2228_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 22047
jeff3p1.acer()

jeff3p1.hmat = "Ti48"
jeff3p1.mat = 2231
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2231_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 22048
jeff3p1.acer()

jeff3p1.hmat = "Ti49"
jeff3p1.mat = 2234
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2234_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 22049
jeff3p1.acer()

jeff3p1.hmat = "Ti50"
jeff3p1.mat = 2237
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2237_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 22050
jeff3p1.acer()

jeff3p1.hmat = "V0"
jeff3p1.mat = 2300
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2300_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 23000
jeff3p1.acer()

jeff3p1.hmat = "Cr50"
jeff3p1.mat = 2425
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2425_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 24050
jeff3p1.acer()

jeff3p1.hmat = "Cr52"
jeff3p1.mat = 2431
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2431_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 4.1677
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 24052
jeff3p1.acer()
jeff3p1.dilutions = None

jeff3p1.hmat = "Cr53"
jeff3p1.mat = 2434
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2434_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 24053
jeff3p1.acer()

jeff3p1.hmat = "Cr54"
jeff3p1.mat = 2437
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2437_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 24054
jeff3p1.acer()

jeff3p1.hmat = "Mn55"
jeff3p1.mat = 2525
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2525_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 25055
jeff3p1.acer()

jeff3p1.hmat = "Fe54"
jeff3p1.mat = 2625
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2625_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 26054
jeff3p1.acer()

jeff3p1.hmat = "Fe56"
jeff3p1.mat = 2631
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2631_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 3.7243
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 26056
jeff3p1.acer()
jeff3p1.dilutions = None

jeff3p1.hmat = "Fe57"
jeff3p1.mat = 2634
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2634_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 26057
jeff3p1.acer()

jeff3p1.hmat = "Fe58"
jeff3p1.mat = 2637
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2637_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 26058
jeff3p1.acer()

jeff3p1.hmat = "Co59"
jeff3p1.mat = 2725
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2725_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 27059
jeff3p1.acer()

jeff3p1.hmat = "Ni58"
jeff3p1.mat = 2825
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2825_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 28058
jeff3p1.acer()

jeff3p1.hmat = "Ni60"
jeff3p1.mat = 2831
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2831_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 28060
jeff3p1.acer()

jeff3p1.hmat = "Ni61"
jeff3p1.mat = 2834
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2834_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 28061
jeff3p1.acer()

jeff3p1.hmat = "Ni62"
jeff3p1.mat = 2837
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2837_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 28062
jeff3p1.acer()

jeff3p1.hmat = "Ni64"
jeff3p1.mat = 2843
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2843_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 28064
jeff3p1.acer()

jeff3p1.hmat = "Cu63"
jeff3p1.mat = 2925
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2925_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 29063
jeff3p1.acer()

jeff3p1.hmat = "Cu65"
jeff3p1.mat = 2931
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N2931_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 29065
jeff3p1.acer()

jeff3p1.hmat = "Zn0"
jeff3p1.mat = 3000
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3000_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 30000
jeff3p1.acer()

jeff3p1.hmat = "W182"
jeff3p1.mat =  7431
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7431_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 74182
jeff3p1.acer()

jeff3p1.hmat = "W183"
jeff3p1.mat = 7434
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7434_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 74183
jeff3p1.acer()

jeff3p1.hmat = "W184"
jeff3p1.mat = 7437
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7437_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 74184
jeff3p1.acer()

jeff3p1.hmat = "W186"
jeff3p1.mat =  7443
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7443_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 74186
jeff3p1.acer()

jeff3p1.hmat = "Pb204"
jeff3p1.mat =  8225
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N8225_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 82204
jeff3p1.acer()

jeff3p1.hmat = "Pb206"
jeff3p1.mat =  8231
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N8231_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 82206
jeff3p1.acer()

jeff3p1.hmat = "Pb207"
jeff3p1.mat =  8234
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N8234_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 82207
jeff3p1.acer()

jeff3p1.hmat = "Pb208"
jeff3p1.mat =  8237
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N8237_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 82208
jeff3p1.acer()

jeff3p1.hmat = "Bi209"
jeff3p1.mat =  8325
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N8325_0.ASC"
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 83209
jeff3p1.acer()

jeff3p1.hmat = "Zr90"
jeff3p1.mat = 4025
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4025_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 6.5144
jeff3p1.dilutions = ( 1.e10, 10000.0,  3866.97, 1495.35, 578.2475, 223.6068, 86.4682, 33.4370, 12.9300, 5.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 40090
jeff3p1.acer()

jeff3p1.hmat = "Zr91"
jeff3p1.mat = 4028
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4028_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 6.5144
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 40091
jeff3p1.acer()

jeff3p1.hmat = "Zr92"
jeff3p1.mat = 4031
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4031_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 6.5144
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 40092
jeff3p1.acer()

jeff3p1.hmat = "Zr93"
jeff3p1.mat = 4034
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4034_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 40093
jeff3p1.acer()

jeff3p1.hmat = "Zr94"
jeff3p1.mat = 4037
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4037_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 6.5144
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 40094
jeff3p1.acer()

jeff3p1.hmat = "Zr95"
jeff3p1.mat = 4040
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4040_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 40095
jeff3p1.acer()

jeff3p1.hmat = "Zr96"
jeff3p1.mat = 4043
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4043_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 6.5144
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 40096
jeff3p1.acer()

jeff3p1.hmat = "Th230"
jeff3p1.mat = 9034
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9034_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 8.7040
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 90230
jeff3p1.acer()

jeff3p1.hmat = "Th232"
jeff3p1.mat = 9040
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9040_0.ASC"
jeff3p1.fission = 2 # fission with delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.8699
jeff3p1.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849, 94.5317612, \
56.3173141, 33.5510521, 19.9880447, 11.9078817, 7.09412289, 4.22632504, \
2.51783395, 1.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 90232
jeff3p1.acer()

jeff3p1.hmat = "Pa231"
jeff3p1.mat = 9131
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9131_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 8.7266
jeff3p1.eFiss = 1.7E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 91231
jeff3p1.acer()

jeff3p1.hmat = "Pa233"
jeff3p1.mat = 9137
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9137_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 9.9950
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 91233
jeff3p1.acer()

jeff3p1.hmat = "U232"
jeff3p1.mat = 9219
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9219_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 12.0687
jeff3p1.eFiss = 1.8E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 92232
jeff3p1.acer()

jeff3p1.hmat = "U233"
jeff3p1.mat = 9222
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9222_0.ASC"
jeff3p1.fission = 2 # fission with delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 12.2989
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 92233
jeff3p1.acer()

jeff3p1.hmat = "U234"
jeff3p1.mat = 9225
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9225_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.0210
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 92234
jeff3p1.acer()

jeff3p1.hmat = "U235"
jeff3p1.mat = 9228
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9228_0.ASC"
jeff3p1.fission = 2 # fission with delayed neutrons
jeff3p1.ss = (2.76792, 2.47875e4)
jeff3p1.potential = 11.6070
jeff3p1.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849,  94.5317612, \
56.3173141, 33.5510521, 19.9880447, 11.9078817, 7.09412289, 4.22632504, \
2.51783395, 1.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 92235
jeff3p1.acer()

jeff3p1.hmat = "U236"
jeff3p1.mat = 9231
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9231_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.9954
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 92236
jeff3p1.acer()

jeff3p1.hmat = "U237"
jeff3p1.mat = 9234
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9234_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.5000
jeff3p1.eFiss = 1.8E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 92237
jeff3p1.acer()

jeff3p1.hmat = "U238"
jeff3p1.mat = 9237
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9237_0.ASC"
jeff3p1.fission = 2 # fission with delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.1710
jeff3p1.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849, 94.5317612, \
56.3173141, 33.5510521, 19.9880447, 11.9078817, 7.09412289, 4.22632504, \
2.51783395, 1.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 92238
jeff3p1.acer()

jeff3p1.hmat = "Np236"
jeff3p1.mat = 9343
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9343_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.eFiss = 1.8E2
jeff3p1.potential = 12.1427
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 93236
jeff3p1.acer()

jeff3p1.hmat = "Np237"
jeff3p1.mat = 9346
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9346_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.4369
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.branchingN2N = 0.75
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 93237
jeff3p1.acer()
jeff3p1.branchingN2N = None

jeff3p1.hmat = "Np238"
jeff3p1.mat = 9349
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9349_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.eFiss = 1.8E2
jeff3p1.potential = 10.4000
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 93238
jeff3p1.acer()

jeff3p1.hmat = "Np239"
jeff3p1.mat = 9352
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9352_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.4979
jeff3p1.eFiss = 1.8E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 93239
jeff3p1.acer()

jeff3p1.hmat = "Pu236"
jeff3p1.mat = 9428
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9428_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.2458
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 94236
jeff3p1.acer()

jeff3p1.hmat = "Pu237"
jeff3p1.mat = 9431
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9431_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.5209
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 94237
jeff3p1.acer()

jeff3p1.hmat = "Pu238"
jeff3p1.mat = 9434
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9434_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.8897
jeff3p1.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849, 94.5317612, \
56.3173141, 33.5510521, 19.9880447, 11.9078817, 7.09412289, 4.22632504, \
2.51783395, 1.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 94238
jeff3p1.acer()

jeff3p1.hmat = "Pu239"
jeff3p1.mat = 9437
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9437_0.ASC"
jeff3p1.fission = 2 # fission with delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.8897
jeff3p1.dilutions = ( 1.e10, 10000.0, 6311.33494, 3983.29436, 2513.99016, \
1586.66319, 1001.39615, 632.014573, 398.885514, 251.749976, 158.887822, \
100.279413, 63.2896882, 39.9442369, 25.2101426, 15.9109633, 10.0419406, \
6.33780423, 4.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 94239
jeff3p1.acer()

jeff3p1.hmat = "Pu240"
jeff3p1.mat = 9440
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9440_0.ASC"
jeff3p1.fission = 2 # fission with delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 9.9091
jeff3p1.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849, 94.5317612, \
56.3173141, 33.5510521, 19.9880447, 11.9078817, 7.09412289, 4.22632504, \
2.51783395, 1.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 94240
jeff3p1.acer()

jeff3p1.hmat = "Pu241"
jeff3p1.mat = 9443
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9443_0.ASC"
jeff3p1.fission = 2 # fission with delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.2156
jeff3p1.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849, 94.5317612, \
56.3173141, 33.5510521, 19.9880447, 11.9078817, 7.09412289, 4.22632504, \
2.51783395, 1.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 94241
jeff3p1.acer()

jeff3p1.hmat = "Pu242"
jeff3p1.mat = 9446
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9446_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.6961
jeff3p1.dilutions = ( 1.e10, 1.e5, 55118.5161, 30380.5178, 16745.2959, \
9229.76155, 5087.30922, 2804.05024, 1545.55137, 851.885253, 469.546659, \
258.807233, 142.650752, 78.6270025, 43.3380508, 23.8872981, 13.1663284, \
7.25708715, 4.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 94242
jeff3p1.acer()

jeff3p1.hmat = "Pu243"
jeff3p1.mat = 9449
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9449_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.2000
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 94243
jeff3p1.acer()

jeff3p1.hmat = "Am241"
jeff3p1.mat = 9543
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9543_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.0329
jeff3p1.branchingNG = 0.115
jeff3p1.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849, 94.5317612, \
56.3173141, 33.5510521, 19.9880447, 11.9078817, 7.09412289, 4.22632504, \
2.51783395, 1.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 95241
jeff3p1.acer()
jeff3p1.branchingNG = None

jeff3p1.hmat = "Am242"
jeff3p1.mat = 9546
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9546_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.2000
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 95242
jeff3p1.acer()

jeff3p1.hmat = "Am242m"
jeff3p1.mat = 9547
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9547_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.2000
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 95342
jeff3p1.acer()

jeff3p1.hmat = "Am243"
jeff3p1.mat = 9549
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9549_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.8237
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 95243
jeff3p1.acer()

jeff3p1.hmat = "Cm241"
jeff3p1.mat = 9628
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9628_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.1788
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 96241
jeff3p1.acer()

jeff3p1.hmat = "Cm242"
jeff3p1.mat = 9631
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9631_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.2000
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 96242
jeff3p1.acer()

jeff3p1.hmat = "Cm243"
jeff3p1.mat = 9634
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9634_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.2832
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 96243
jeff3p1.acer()

jeff3p1.hmat = "Cm244"
jeff3p1.mat = 9637
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9637_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.3200
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 96244
jeff3p1.acer()

jeff3p1.hmat = "Cm245"
jeff3p1.mat = 9640
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9640_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.3900
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 96245
jeff3p1.acer()

jeff3p1.hmat = "Cm246"
jeff3p1.mat = 9643
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9643_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.2758
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 96246
jeff3p1.acer()

jeff3p1.hmat = "Cm247"
jeff3p1.mat = 9646
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9646_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.0000
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 96247
jeff3p1.acer()

jeff3p1.hmat = "Cm248"
jeff3p1.mat = 9649
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9649_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 10.3971
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 96248
jeff3p1.acer()

jeff3p1.hmat = "Bk249"
jeff3p1.mat = 9752
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9752_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.0983
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 97249
jeff3p1.acer()

jeff3p1.hmat = "Cf249"
jeff3p1.mat = 9852
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9852_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 11.1510
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 98249
jeff3p1.acer()

jeff3p1.hmat = "Cf250"
jeff3p1.mat = 9855
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9855_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 9.8800
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 98250
jeff3p1.acer()

jeff3p1.hmat = "Cf251"
jeff3p1.mat = 9858
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9858_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 9.8800
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 98251
jeff3p1.acer()

jeff3p1.hmat = "Cf252"
jeff3p1.mat = 9861
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N9861_0.ASC"
jeff3p1.fission = 1 # fission without delayed neutrons
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 9.8000
jeff3p1.eFiss = 1.9E2
jeff3p1.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, 158.676849, 56.3173141 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 98252
jeff3p1.acer()

# Process the fission products:

jeff3p1.scatteringLaw = None
jeff3p1.legendre = 0
jeff3p1.fission = None
jeff3p1.dilutions = None
jeff3p1.eFiss = None

jeff3p1.hmat = "Ge72"
jeff3p1.mat = 3231
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3231_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 32072
jeff3p1.acer()

jeff3p1.hmat = "Ge73"
jeff3p1.mat = 3234
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3234_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 32073
jeff3p1.acer()

jeff3p1.hmat = "Ge74"
jeff3p1.mat = 3237
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3237_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 32074
jeff3p1.acer()

jeff3p1.hmat = "Ge76"
jeff3p1.mat = 3243
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3243_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 32076
jeff3p1.acer()

jeff3p1.hmat = "As75"
jeff3p1.mat = 3325
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3325_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 33075
jeff3p1.acer()

jeff3p1.hmat = "Se76"
jeff3p1.mat = 3431
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3431_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 34076
jeff3p1.acer()

jeff3p1.hmat = "Se77"
jeff3p1.mat = 3434
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3434_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 34077
jeff3p1.acer()

jeff3p1.hmat = "Se78"
jeff3p1.mat = 3437
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3437_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 34078
jeff3p1.acer()

jeff3p1.hmat = "Se79"
jeff3p1.mat = 3440
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3440_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 34079
jeff3p1.acer()

jeff3p1.hmat = "Se80"
jeff3p1.mat = 3443
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3443_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 34080
jeff3p1.acer()

jeff3p1.hmat = "Se82"
jeff3p1.mat = 3449
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3449_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 34082
jeff3p1.acer()

jeff3p1.hmat = "Br79"
jeff3p1.mat = 3525
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3525_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 35079
jeff3p1.acer()

jeff3p1.hmat = "Br81"
jeff3p1.mat = 3531
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3531_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 35081
jeff3p1.acer()

jeff3p1.hmat = "Kr80"
jeff3p1.mat = 3631
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3631_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 36080
jeff3p1.acer()

jeff3p1.hmat = "Kr82"
jeff3p1.mat = 3637
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3637_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 36082
jeff3p1.acer()

jeff3p1.hmat = "Kr83"
jeff3p1.mat = 3640
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3640_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 36083
jeff3p1.acer()

jeff3p1.hmat = "Kr84"
jeff3p1.mat = 3643
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3643_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 36084
jeff3p1.acer()

jeff3p1.hmat = "Kr85"
jeff3p1.mat = 3646
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3646_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 36085
jeff3p1.acer()

jeff3p1.hmat = "Kr86"
jeff3p1.mat = 3649
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3649_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 36086
jeff3p1.acer()

jeff3p1.hmat = "Rb85"
jeff3p1.mat = 3725
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3725_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 37085
jeff3p1.acer()

jeff3p1.hmat = "Rb87"
jeff3p1.mat = 3731
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3731_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 37087
jeff3p1.acer()

jeff3p1.hmat = "Sr86"
jeff3p1.mat = 3831
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3831_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 38086
jeff3p1.acer()

jeff3p1.hmat = "Sr87"
jeff3p1.mat = 3834
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3834_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 38087
jeff3p1.acer()

jeff3p1.hmat = "Sr88"
jeff3p1.mat = 3837
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3837_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 38088
jeff3p1.acer()

jeff3p1.hmat = "Sr89"
jeff3p1.mat = 3840
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3840_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 38089
jeff3p1.acer()

jeff3p1.hmat = "Sr90"
jeff3p1.mat = 3843
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3843_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 38090
jeff3p1.acer()

jeff3p1.hmat = "Y89"
jeff3p1.mat = 3925
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3925_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 39089
jeff3p1.acer()

jeff3p1.hmat = "Y90"
jeff3p1.mat = 3928
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3928_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 39090
jeff3p1.acer()

jeff3p1.hmat = "Y91"
jeff3p1.mat = 3931
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N3931_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 39091
jeff3p1.acer()

jeff3p1.hmat = "Nb93"
jeff3p1.mat = 4125
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4125_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 41093
jeff3p1.acer()

jeff3p1.hmat = "Nb94"
jeff3p1.mat = 4128
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4128_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 41094
jeff3p1.acer()

jeff3p1.hmat = "Nb95"
jeff3p1.mat = 4131
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4131_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 41095
jeff3p1.acer()

jeff3p1.hmat = "Mo92"
jeff3p1.mat = 4225
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4225_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 6.1140
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 42092
jeff3p1.acer()

jeff3p1.hmat = "Mo94"
jeff3p1.mat = 4231
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4231_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 6.1140
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 42094
jeff3p1.acer()

jeff3p1.hmat = "Mo95"
jeff3p1.mat = 4234
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4234_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 6.1140
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 42095
jeff3p1.acer()

jeff3p1.hmat = "Mo96"
jeff3p1.mat = 4237
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4237_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 6.1140
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 42096
jeff3p1.acer()

jeff3p1.hmat = "Mo97"
jeff3p1.mat = 4240
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4240_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 6.1140
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 42097
jeff3p1.acer()

jeff3p1.hmat = "Mo98"
jeff3p1.mat = 4243
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4243_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 6.1140
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 42098
jeff3p1.acer()

jeff3p1.hmat = "Mo99"
jeff3p1.mat = 4246
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4246_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 42099
jeff3p1.acer()

jeff3p1.hmat = "Mo100"
jeff3p1.mat = 4249
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4249_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.22773e5)
jeff3p1.potential = 6.1140
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 42100
jeff3p1.acer()

jeff3p1.hmat = "Tc99"
jeff3p1.mat = 4331
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4331_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 43099
jeff3p1.acer()

jeff3p1.hmat = "Ru99"
jeff3p1.mat = 4434
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4434_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 44099
jeff3p1.acer()

jeff3p1.hmat = "Ru100"
jeff3p1.mat = 4437
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4437_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 44100
jeff3p1.acer()

jeff3p1.hmat = "Ru101"
jeff3p1.mat = 4440
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4440_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 44101
jeff3p1.acer()

jeff3p1.hmat = "Ru102"
jeff3p1.mat = 4443
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4443_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 44102
jeff3p1.acer()

jeff3p1.hmat = "Ru103"
jeff3p1.mat = 4446
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4446_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 44103
jeff3p1.acer()

jeff3p1.hmat = "Ru104"
jeff3p1.mat = 4449
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4449_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 44104
jeff3p1.acer()

jeff3p1.hmat = "Ru105"
jeff3p1.mat = 4452
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4452_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 44105
jeff3p1.acer()

jeff3p1.hmat = "Ru106"
jeff3p1.mat = 4455
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4455_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 44106
jeff3p1.acer()

jeff3p1.hmat = "Rh103"
jeff3p1.mat = 4525
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4525_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 45103
jeff3p1.acer()

jeff3p1.hmat = "Rh105"
jeff3p1.mat = 4531
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4531_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 45105
jeff3p1.acer()

jeff3p1.hmat = "Pd104"
jeff3p1.mat = 4631
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4631_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 46104
jeff3p1.acer()

jeff3p1.hmat = "Pd105"
jeff3p1.mat = 4634
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4634_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 46105
jeff3p1.acer()

jeff3p1.hmat = "Pd106"
jeff3p1.mat = 4637
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4637_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 46106
jeff3p1.acer()

jeff3p1.hmat = "Pd107"
jeff3p1.mat = 4640
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4640_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 46107
jeff3p1.acer()

jeff3p1.hmat = "Pd108"
jeff3p1.mat = 4643
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4643_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 46108
jeff3p1.acer()

jeff3p1.hmat = "Pd110"
jeff3p1.mat = 4649
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4649_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 46110
jeff3p1.acer()

jeff3p1.hmat = "Ag107"
jeff3p1.mat = 4725
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4725_0.ASC"
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 5.4739
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 47107
jeff3p1.acer()

jeff3p1.hmat = "Ag109"
jeff3p1.mat = 4731
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4731_0.ASC"
jeff3p1.branchingNG = 0.941
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 5.3316
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 47109
jeff3p1.acer()
jeff3p1.branchingNG = None

jeff3p1.hmat = "Ag110m"
jeff3p1.mat = 4735
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4735_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 47310
jeff3p1.acer()

jeff3p1.hmat = "Ag111"
jeff3p1.mat = 4737
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4737_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 47111
jeff3p1.acer()

jeff3p1.hmat = "Cd106"
jeff3p1.mat = 4825
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4825_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 48106
jeff3p1.acer()

jeff3p1.hmat = "Cd108"
jeff3p1.mat = 4831
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4831_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 48108
jeff3p1.acer()

jeff3p1.hmat = "Cd110"
jeff3p1.mat = 4837
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4837_0.ASC"
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 5.1762
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 48110
jeff3p1.acer()

jeff3p1.hmat = "Cd111"
jeff3p1.mat = 4840
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4840_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 48111
jeff3p1.acer()

jeff3p1.hmat = "Cd112"
jeff3p1.mat = 4843
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4843_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 48112
jeff3p1.acer()

jeff3p1.hmat = "Cd113"
jeff3p1.mat = 4846
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4846_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 48113
jeff3p1.acer()

jeff3p1.hmat = "Cd114"
jeff3p1.mat = 4849
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4849_0.ASC"
jeff3p1.branchingNG = 0.079383
jeff3p1.makeFp()
jeff3p1.za = 48114
jeff3p1.acer()
jeff3p1.branchingNG = None

jeff3p1.hmat = "Cd115m"
jeff3p1.mat = 4853
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4853_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 48315
jeff3p1.acer()

jeff3p1.hmat = "Cd116"
jeff3p1.mat = 4855
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4855_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 48116
jeff3p1.acer()

jeff3p1.hmat = "In113"
jeff3p1.mat = 4925
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4925_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 49113
jeff3p1.acer()

jeff3p1.hmat = "In115"
jeff3p1.mat = 4931
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N4931_0.ASC"
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 5.0695
jeff3p1.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 49115
jeff3p1.acer()

jeff3p1.hmat = "Sn112"
jeff3p1.mat = 5025
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5025_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50112
jeff3p1.acer()

jeff3p1.hmat = "Sn114"
jeff3p1.mat = 5031
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5031_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50114
jeff3p1.acer()

jeff3p1.hmat = "Sn115"
jeff3p1.mat = 5034
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5034_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50115
jeff3p1.acer()

jeff3p1.hmat = "Sn116"
jeff3p1.mat = 5037
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5037_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50116
jeff3p1.acer()

jeff3p1.hmat = "Sn117"
jeff3p1.mat = 5040
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5040_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50117
jeff3p1.acer()

jeff3p1.hmat = "Sn118"
jeff3p1.mat = 5043
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5043_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50118
jeff3p1.acer()

jeff3p1.hmat = "Sn119"
jeff3p1.mat = 5046
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5046_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50119
jeff3p1.acer()

jeff3p1.hmat = "Sn120"
jeff3p1.mat = 5049
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5049_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50120
jeff3p1.acer()

jeff3p1.hmat = "Sn122"
jeff3p1.mat = 5055
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5055_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50122
jeff3p1.acer()

jeff3p1.hmat = "Sn123"
jeff3p1.mat = 5058
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5058_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50123
jeff3p1.acer()

jeff3p1.hmat = "Sn124"
jeff3p1.mat = 5061
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5061_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50124
jeff3p1.acer()

jeff3p1.hmat = "Sn125"
jeff3p1.mat = 5064
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5064_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50125
jeff3p1.acer()

jeff3p1.hmat = "Sn126"
jeff3p1.mat = 5067
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5067_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 50126
jeff3p1.acer()

jeff3p1.hmat = "Sb121"
jeff3p1.mat = 5125
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5125_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 51121
jeff3p1.acer()

jeff3p1.hmat = "Sb123"
jeff3p1.mat = 5131
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5131_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 51123
jeff3p1.acer()

jeff3p1.hmat = "Sb124"
jeff3p1.mat = 5134
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5134_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 51124
jeff3p1.acer()

jeff3p1.hmat = "Sb125"
jeff3p1.mat = 5137
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5137_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 51125
jeff3p1.acer()

jeff3p1.hmat = "Sb126"
jeff3p1.mat = 5140
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5140_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 51126
jeff3p1.acer()

jeff3p1.hmat = "Te122"
jeff3p1.mat = 5231
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5231_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 52122
jeff3p1.acer()

jeff3p1.hmat = "Te123"
jeff3p1.mat = 5234
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5234_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 52123
jeff3p1.acer()

jeff3p1.hmat = "Te124"
jeff3p1.mat = 5237
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5237_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 52124
jeff3p1.acer()

jeff3p1.hmat = "Te125"
jeff3p1.mat = 5240
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5240_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 52125
jeff3p1.acer()

jeff3p1.hmat = "Te126"
jeff3p1.mat = 5243
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5243_0.ASC"
jeff3p1.branchingNG = 0.091528
jeff3p1.makeFp()
jeff3p1.za = 52126
jeff3p1.acer()
jeff3p1.branchingNG = None

jeff3p1.hmat = "Te127m"
jeff3p1.mat = 5247
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5247_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 52327
jeff3p1.acer()

jeff3p1.hmat = "Te128"
jeff3p1.mat = 5249
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5249_0.ASC"
jeff3p1.branchingNG = 0.031894
jeff3p1.makeFp()
jeff3p1.za = 52128
jeff3p1.acer()
jeff3p1.branchingNG = None

jeff3p1.hmat = "Te129m"
jeff3p1.mat = 5253
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5253_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 52329
jeff3p1.acer()

jeff3p1.hmat = "Te130"
jeff3p1.mat = 5255
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5255_0.ASC"
jeff3p1.branchingNG = 0.069035
jeff3p1.makeFp()
jeff3p1.za = 52130
jeff3p1.acer()
jeff3p1.branchingNG = None

jeff3p1.hmat = "Te132"
jeff3p1.mat = 5261
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5261_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 52132
jeff3p1.acer()

jeff3p1.hmat = "I127"
jeff3p1.mat = 5325
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5325_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 53127
jeff3p1.acer()

jeff3p1.hmat = "I129"
jeff3p1.mat = 5331
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5331_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 53129
jeff3p1.acer()

jeff3p1.hmat = "I130"
jeff3p1.mat = 5334
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5334_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 53130
jeff3p1.acer()

jeff3p1.hmat = "I131"
jeff3p1.mat = 5337
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5337_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 53131
jeff3p1.acer()

jeff3p1.hmat = "I135"
jeff3p1.mat = 5349
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5349_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 53135
jeff3p1.acer()

jeff3p1.hmat = "Xe128"
jeff3p1.mat = 5437
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5437_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 54128
jeff3p1.acer()

jeff3p1.hmat = "Xe129"
jeff3p1.mat = 5440
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5440_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 54129
jeff3p1.acer()

jeff3p1.hmat = "Xe130"
jeff3p1.mat = 5443
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5443_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 54130
jeff3p1.acer()

jeff3p1.hmat = "Xe131"
jeff3p1.mat = 5446
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5446_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 54131
jeff3p1.acer()

jeff3p1.hmat = "Xe132"
jeff3p1.mat = 5449
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5449_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 54132
jeff3p1.acer()

jeff3p1.hmat = "Xe133"
jeff3p1.mat = 5452
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5452_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 54133
jeff3p1.acer()

jeff3p1.hmat = "Xe134"
jeff3p1.mat = 5455
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5455_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 54134
jeff3p1.acer()

jeff3p1.hmat = "Xe135"
jeff3p1.mat = 5458
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5458_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 54135
jeff3p1.acer()

jeff3p1.hmat = "Xe136"
jeff3p1.mat = 5461
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5461_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 54136
jeff3p1.acer()

jeff3p1.hmat = "Cs133"
jeff3p1.mat = 5525
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5525_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 55133
jeff3p1.acer()

jeff3p1.hmat = "Cs134"
jeff3p1.mat = 5528
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5528_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 55134
jeff3p1.acer()

jeff3p1.hmat = "Cs135"
jeff3p1.mat = 5531
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5531_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 55135
jeff3p1.acer()

jeff3p1.hmat = "Cs136"
jeff3p1.mat = 5534
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5534_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 55136
jeff3p1.acer()

jeff3p1.hmat = "Cs137"
jeff3p1.mat = 5537
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5537_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 55137
jeff3p1.acer()

jeff3p1.hmat = "Ba134"
jeff3p1.mat = 5637
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5637_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 56134
jeff3p1.acer()

jeff3p1.hmat = "Ba135"
jeff3p1.mat = 5640
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5640_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 56135
jeff3p1.acer()

jeff3p1.hmat = "Ba136"
jeff3p1.mat = 5643
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5643_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 56136
jeff3p1.acer()

jeff3p1.hmat = "Ba137"
jeff3p1.mat = 5646
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5646_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 56137
jeff3p1.acer()

jeff3p1.hmat = "Ba138"
jeff3p1.mat = 5649
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5649_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 56138
jeff3p1.acer()

jeff3p1.hmat = "Ba140"
jeff3p1.mat = 5655
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5655_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 56140
jeff3p1.acer()

jeff3p1.hmat = "La139"
jeff3p1.mat = 5728
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5728_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 57139
jeff3p1.acer()

jeff3p1.hmat = "La140"
jeff3p1.mat = 5731
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5731_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 57140
jeff3p1.acer()

jeff3p1.hmat = "Ce140"
jeff3p1.mat = 5837
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5837_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 58140
jeff3p1.acer()

jeff3p1.hmat = "Ce141"
jeff3p1.mat = 5840
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5840_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 58141
jeff3p1.acer()

jeff3p1.hmat = "Ce142"
jeff3p1.mat = 5843
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5843_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 58142
jeff3p1.acer()

jeff3p1.hmat = "Ce143"
jeff3p1.mat = 5846
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5846_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 58143
jeff3p1.acer()

jeff3p1.hmat = "Ce144"
jeff3p1.mat = 5849
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5849_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 58144
jeff3p1.acer()

jeff3p1.hmat = "Pr141"
jeff3p1.mat = 5925
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5925_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 59141
jeff3p1.acer()

jeff3p1.hmat = "Pr142"
jeff3p1.mat = 5928
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5928_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 59142
jeff3p1.acer()

jeff3p1.hmat = "Pr143"
jeff3p1.mat = 5931
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N5931_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 59143
jeff3p1.acer()

jeff3p1.hmat = "Nd142"
jeff3p1.mat = 6025
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6025_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 60142
jeff3p1.acer()

jeff3p1.hmat = "Nd143"
jeff3p1.mat = 6028
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6028_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 60143
jeff3p1.acer()

jeff3p1.hmat = "Nd144"
jeff3p1.mat = 6031
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6031_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 60144
jeff3p1.acer()

jeff3p1.hmat = "Nd145"
jeff3p1.mat = 6034
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6034_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 60145
jeff3p1.acer()

jeff3p1.hmat = "Nd146"
jeff3p1.mat = 6037
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6037_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 60146
jeff3p1.acer()

jeff3p1.hmat = "Nd147"
jeff3p1.mat = 6040
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6040_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 60147
jeff3p1.acer()

jeff3p1.hmat = "Nd148"
jeff3p1.mat = 6043
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6043_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 60148
jeff3p1.acer()

jeff3p1.hmat = "Nd150"
jeff3p1.mat = 6049
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6049_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 60150
jeff3p1.acer()

jeff3p1.hmat = "Pm147"
jeff3p1.mat = 6149
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6149_0.ASC"
jeff3p1.branchingNG = 0.470
jeff3p1.makeFp()
jeff3p1.za = 61147
jeff3p1.acer()
jeff3p1.branchingNG = None

jeff3p1.hmat = "Pm148"
jeff3p1.mat = 6152
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6152_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 61148
jeff3p1.acer()

jeff3p1.hmat = "Pm148m"
jeff3p1.mat = 6153
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6153_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 61348
jeff3p1.acer()

jeff3p1.hmat = "Pm149"
jeff3p1.mat = 6155
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6155_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 61149
jeff3p1.acer()

jeff3p1.hmat = "Pm151"
jeff3p1.mat = 6161
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6161_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 61151
jeff3p1.acer()

jeff3p1.hmat = "Sm144"
jeff3p1.mat = 6225
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6225_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 62144
jeff3p1.acer()

jeff3p1.hmat = "Sm147"
jeff3p1.mat = 6234
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6234_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 62147
jeff3p1.acer()

jeff3p1.hmat = "Sm148"
jeff3p1.mat = 6237
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6237_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 62148
jeff3p1.acer()

jeff3p1.hmat = "Sm149"
jeff3p1.mat = 6240
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6240_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 62149
jeff3p1.acer()

jeff3p1.hmat = "Sm150"
jeff3p1.mat = 6243
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6243_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 62150
jeff3p1.acer()

jeff3p1.hmat = "Sm151"
jeff3p1.mat = 6246
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6246_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 62151
jeff3p1.acer()

jeff3p1.hmat = "Sm152"
jeff3p1.mat = 6249
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6249_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 62152
jeff3p1.acer()

jeff3p1.hmat = "Sm153"
jeff3p1.mat = 6252
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6252_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 62153
jeff3p1.acer()

jeff3p1.hmat = "Sm154"
jeff3p1.mat = 6255
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6255_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 62154
jeff3p1.acer()

jeff3p1.hmat = "Eu151"
jeff3p1.mat = 6325
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6325_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 63151
jeff3p1.acer()

jeff3p1.hmat = "Eu152"
jeff3p1.mat = 6328
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6328_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 63152
jeff3p1.acer()

jeff3p1.hmat = "Eu153"
jeff3p1.mat = 6331
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6331_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 63153
jeff3p1.acer()

jeff3p1.hmat = "Eu154"
jeff3p1.mat = 6334
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6334_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 63154
jeff3p1.acer()

jeff3p1.hmat = "Eu155"
jeff3p1.mat = 6337
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6337_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 63155
jeff3p1.acer()

jeff3p1.hmat = "Eu156"
jeff3p1.mat = 6340
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6340_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 63156
jeff3p1.acer()

jeff3p1.hmat = "Eu157"
jeff3p1.mat = 6343
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6343_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 63157
jeff3p1.acer()

jeff3p1.hmat = "Gd154"
jeff3p1.mat = 6431
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6431_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.6587
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 64154
jeff3p1.acer()

jeff3p1.hmat = "Gd155"
jeff3p1.mat = 6434
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6434_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 8.0425
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 64155
jeff3p1.acer()

jeff3p1.hmat = "Gd156"
jeff3p1.mat = 6437
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6437_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 8.2448
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 64156
jeff3p1.acer()

jeff3p1.hmat = "Gd157"
jeff3p1.mat = 6440
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6440_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.6454
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 64157
jeff3p1.acer()

jeff3p1.hmat = "Gd158"
jeff3p1.mat = 6443
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6443_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 5.3093
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 64158
jeff3p1.acer()

jeff3p1.hmat = "Gd160"
jeff3p1.mat = 6449
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6449_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 5.8107
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 64160
jeff3p1.acer()

jeff3p1.hmat = "Tb159"
jeff3p1.mat = 6525
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6525_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 65159
jeff3p1.acer()

jeff3p1.hmat = "Tb160"
jeff3p1.mat = 6528
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6528_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 65160
jeff3p1.acer()

jeff3p1.hmat = "Dy160"
jeff3p1.mat = 6637
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6637_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.0686
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 66160
jeff3p1.acer()

jeff3p1.hmat = "Dy161"
jeff3p1.mat = 6640
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6640_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.0686
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 66161
jeff3p1.acer()

jeff3p1.hmat = "Dy162"
jeff3p1.mat = 6643
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6643_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.6846
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 66162
jeff3p1.acer()

jeff3p1.hmat = "Dy163"
jeff3p1.mat = 6646
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6646_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.0686
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 66163
jeff3p1.acer()

jeff3p1.hmat = "Dy164"
jeff3p1.mat = 6649
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6649_0.ASC"
jeff3p1.fission = None
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.6846
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 66164
jeff3p1.acer()

jeff3p1.hmat = "Hf174"
jeff3p1.mat = 7225
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7225_0.ASC"
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.1385
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 72174
jeff3p1.acer()

jeff3p1.hmat = "Hf176"
jeff3p1.mat = 7231
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7231_0.ASC"
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.1935
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 72176
jeff3p1.acer()

jeff3p1.hmat = "Hf177"
jeff3p1.mat = 7234
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7234_0.ASC"
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.2202
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 72177
jeff3p1.acer()

jeff3p1.hmat = "Hf178"
jeff3p1.mat = 7237
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7237_0.ASC"
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.2469
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 72178
jeff3p1.acer()

jeff3p1.hmat = "Hf179"
jeff3p1.mat = 7240
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7240_0.ASC"
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.2736
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 72179
jeff3p1.acer()

jeff3p1.hmat = "Hf180"
jeff3p1.mat = 7243
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7243_0.ASC"
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.3004
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 72180
jeff3p1.acer()

jeff3p1.hmat = "Ta181"
jeff3p1.mat = 7328
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N7328_0.ASC"
jeff3p1.ss = (2.76792, 1.66156e4)
jeff3p1.potential = 7.6454
jeff3p1.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p1.pendf()
jeff3p1.gendf()
jeff3p1.draglib()
jeff3p1.za = 73181
jeff3p1.acer()

jeff3p1.dilutions = None

jeff3p1.hmat = "Ho165"
jeff3p1.mat = 6725
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6725_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 67165
jeff3p1.acer()

jeff3p1.hmat = "Er166"
jeff3p1.mat = 6837
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6837_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 68166
jeff3p1.acer()

jeff3p1.hmat = "Er167"
jeff3p1.mat = 6840
jeff3p1.evaluationFile = evaluationDir + "JEFF311N_0_IND/JEFF311N6840_0.ASC"
jeff3p1.makeFp()
jeff3p1.za = 68167
jeff3p1.acer()

# Process the burnup chain:

jeff3p1.fissionFile = evaluationDir + "JEFF311NFY.ASC"
jeff3p1.decayFile = evaluationDir + "JEFF311RDD_ALL.OUT"
jeff3p1.burnup()
