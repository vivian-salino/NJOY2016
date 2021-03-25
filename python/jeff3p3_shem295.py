#!/usr/local/bin/python
from PyNjoy import *
import os
jeff3p3 = PyNjoy()
evaluationDir = os.getcwd() + "/../../ENDF/JEFF-3.3/"
jeff3p3.evaluationName = "../output/JEFF-3.3_shem295/"
jeff3p3.execDir = ".."
jeff3p3.nstr = 25
jeff3p3.iwt = 4
jeff3p3.Espectra = None
jeff3p3.autolib = (4.632489, 1.11377e4, 0.0005)
jeff3p3.serpent = True

jeff3p3.legendre = 3
jeff3p3.hmat = "H1_H2O"
jeff3p3.mat = 125
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/1-H-1g.jeff33"
jeff3p3.scatteringLaw =  evaluationDir + "JEFF33-tsl/tsl-HinH2O.jeff33"
jeff3p3.scatteringMat = 1
jeff3p3.temperatures = ( 293.6, 323.6, 373.6, 423.6, 473.6, 523.6, 573.6, 647.2, 800.0 )
jeff3p3.fission = None
jeff3p3.dilutions = None
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.suff = ( 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08 )
jeff3p3.scatName = "lwtr"
jeff3p3.za = 1001
jeff3p3.acer()

jeff3p3.legendre = 1
jeff3p3.hmat = "H2_D2O"
jeff3p3.mat = 128
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/1-H-2g.jeff33"
jeff3p3.scatteringLaw =  evaluationDir + "JEFF33-tsl/tsl-DinD2O.jeff33"
jeff3p3.scatteringMat = 11
jeff3p3.temperatures = ( 293.6, 323.6, 373.6, 423.6, 473.6, 523.6, 573.6, 623.6 )
jeff3p3.fission = None
jeff3p3.dilutions = None
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.suff = ( 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07 )
jeff3p3.scatName = "hwtr"
jeff3p3.za = 1002
jeff3p3.acer()

jeff3p3.hmat = "H1_CH2"
jeff3p3.mat = 125
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/1-H-1g.jeff33"
jeff3p3.scatteringLaw =  evaluationDir + "JEFF33-tsl/tsl-HinCH2.jeff33"
jeff3p3.scatteringMat = 37
jeff3p3.temperatures = ( 293.6,  350. )
jeff3p3.fission = None
jeff3p3.dilutions = None
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.suff = ( 0.00, 0.01 )
jeff3p3.scatName = "poly"
jeff3p3.za = 1001
jeff3p3.acer()

jeff3p3.hmat = "H1_ZRH"
jeff3p3.mat = 125
jeff3p3.temperatures = ( 293.6, 400., 500., 600., 700., 800., 1000., 1200. )
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/1-H-1g.jeff33"
jeff3p3.scatteringLaw =  evaluationDir + "JEFF33-tsl/tsl-HinZrH.jeff33"
jeff3p3.scatteringMat = 7
jeff3p3.fission = None
jeff3p3.dilutions = None
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.suff = ( 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07 )
jeff3p3.scatName = "zrh"
jeff3p3.za = 1001
jeff3p3.acer()

jeff3p3.hmat = "Be9"
jeff3p3.mat = 425
jeff3p3.temperatures = ( 293.6 , 400 , 500 , 600 , 700 , 800 , 1000 , 1200)
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/4-Be-9g.jeff33"
jeff3p3.scatteringLaw =  evaluationDir + "JEFF33-tsl/tsl-Be.jeff33"
jeff3p3.scatteringMat = 26
jeff3p3.fission = None
jeff3p3.dilutions = None
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.suff = ( 0.00 , 0.01 , 0.02 , 0.03 , 0.04 , 0.05 , 0.06 , 0.07)
jeff3p3.scatName = "be"
jeff3p3.za = 4009
jeff3p3.acer()

jeff3p3.hmat = "C0_GR"
jeff3p3.mat = 600
jeff3p3.temperatures = ( 293.6 , 400 , 500 , 600 , 700 , 800 , 1000 , 1200 , 1600 , 2000 )
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/6-C-0g.jeff33"
jeff3p3.scatteringLaw =  evaluationDir + "JEFF33-tsl/tsl-Graphite.jeff33"
jeff3p3.scatteringMat = 31
jeff3p3.fission = None
jeff3p3.dilutions = None
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.suff = ( 0.00 , 0.01 , 0.02 , 0.03 , 0.04 , 0.05 , 0.06 , 0.07 , 0.08 , 0.09 )
jeff3p3.scatName = "grph"
jeff3p3.za = 6000
jeff3p3.acer()

jeff3p3.scatteringLaw = None
jeff3p3.temperatures = ( 293., 550., 900., 1200., 2000. )
jeff3p3.suff = ( 0.02, 0.05, 0.09, 0.12, 0.20 )
jeff3p3.fission = None
jeff3p3.dilutions = None

jeff3p3.hmat = "H1"
jeff3p3.mat = 125
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/1-H-1g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 1001
jeff3p3.acer()

jeff3p3.hmat = "H2"
jeff3p3.mat = 128
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/1-H-2g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 1002
jeff3p3.acer()

jeff3p3.hmat = "H3"
jeff3p3.mat = 131
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/1-H-3g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 1003
jeff3p3.acer()

jeff3p3.hmat = "He3"
jeff3p3.mat = 225
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/2-He-3g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 2003
jeff3p3.acer()

jeff3p3.hmat = "He4"
jeff3p3.mat = 228
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/2-He-4g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 2004
jeff3p3.acer()

jeff3p3.hmat = "Li6"
jeff3p3.mat = 325
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/3-Li-6g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 3006
jeff3p3.acer()

jeff3p3.hmat = "Li7"
jeff3p3.mat = 328
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/3-Li-7g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 3007
jeff3p3.acer()

jeff3p3.hmat = "B10"
jeff3p3.mat = 525
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/5-B-10g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 5010
jeff3p3.acer()

jeff3p3.hmat = "B11"
jeff3p3.mat = 528
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/5-B-11g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 5011
jeff3p3.acer()

jeff3p3.hmat = "C0"
jeff3p3.mat = 600
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/6-C-0g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 6000
jeff3p3.acer()

jeff3p3.hmat = "N14"
jeff3p3.mat = 725
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/7-N-14g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 7014
jeff3p3.acer()

jeff3p3.hmat = "N15"
jeff3p3.mat = 728
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/7-N-15g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 7015
jeff3p3.acer()

jeff3p3.hmat = "O16"
jeff3p3.mat = 825
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/8-O-16g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 8016
jeff3p3.acer()

jeff3p3.hmat = "O17"
jeff3p3.mat = 828
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/8-O-17g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 8017
jeff3p3.acer()

jeff3p3.hmat = "F19"
jeff3p3.mat = 925
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/9-F-19g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 9019
jeff3p3.acer()

jeff3p3.hmat = "Na23"
jeff3p3.mat = 1125
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/11-Na-23g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 11023
jeff3p3.acer()

jeff3p3.hmat = "Mg24"
jeff3p3.mat = 1225
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/12-Mg-24g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 12024
jeff3p3.acer()

jeff3p3.hmat = "Mg25"
jeff3p3.mat = 1228
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/12-Mg-25g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 12025
jeff3p3.acer()

jeff3p3.hmat = "Mg26"
jeff3p3.mat = 1231
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/12-Mg-26g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 12026
jeff3p3.acer()

jeff3p3.hmat = "Al27"
jeff3p3.mat = 1325
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/13-Al-27g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 13027
jeff3p3.acer()

jeff3p3.hmat = "Si28"
jeff3p3.mat = 1425
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/14-Si-28g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 14028
jeff3p3.acer()

jeff3p3.hmat = "Si29"
jeff3p3.mat = 1428
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/14-Si-29g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 14029
jeff3p3.acer()

jeff3p3.hmat = "Si30"
jeff3p3.mat = 1431
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/14-Si-30g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 14030
jeff3p3.acer()

jeff3p3.hmat = "P31"
jeff3p3.mat = 1525
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/15-P-31g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 15031
jeff3p3.acer()

jeff3p3.hmat = "S32"
jeff3p3.mat = 1625
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/16-S-32g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 16032
jeff3p3.acer()

jeff3p3.hmat = "S33"
jeff3p3.mat = 1628
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/16-S-33g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 16033
jeff3p3.acer()

jeff3p3.hmat = "S34"
jeff3p3.mat = 1631
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/16-S-34g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 16034
jeff3p3.acer()

jeff3p3.hmat = "S36"
jeff3p3.mat = 1637
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/16-S-36g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 16036
jeff3p3.acer()

jeff3p3.hmat = "Cl35"
jeff3p3.mat = 1725
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/17-Cl-35g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 17035
jeff3p3.acer()

jeff3p3.hmat = "Cl37"
jeff3p3.mat = 1731
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/17-Cl-37g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 17037
jeff3p3.acer()

jeff3p3.hmat = "K39"
jeff3p3.mat = 1925
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/19-K-39g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 19039
jeff3p3.acer()

jeff3p3.hmat = "K40"
jeff3p3.mat = 1928
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/19-K-40g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 19040
jeff3p3.acer()

jeff3p3.hmat = "K41"
jeff3p3.mat = 1931
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/19-K-41g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 19041
jeff3p3.acer()

jeff3p3.hmat = "Ca40"
jeff3p3.mat = 2025
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/20-Ca-40g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 20040
jeff3p3.acer()

jeff3p3.hmat = "Ca42"
jeff3p3.mat = 2031
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/20-Ca-42g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 20042
jeff3p3.acer()

jeff3p3.hmat = "Ca43"
jeff3p3.mat = 2034
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/20-Ca-43g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 20043
jeff3p3.acer()

jeff3p3.hmat = "Ca44"
jeff3p3.mat = 2037
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/20-Ca-44g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 20044
jeff3p3.acer()

jeff3p3.hmat = "Ca46"
jeff3p3.mat = 2043
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/20-Ca-46g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 20046
jeff3p3.acer()

jeff3p3.hmat = "Ca48"
jeff3p3.mat = 2049
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/20-Ca-48g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 20048
jeff3p3.acer()

jeff3p3.hmat = "Ti46"
jeff3p3.mat = 2225
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/22-Ti-46g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 22046
jeff3p3.acer()

jeff3p3.hmat = "Ti47"
jeff3p3.mat = 2228
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/22-Ti-47g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 22047
jeff3p3.acer()

jeff3p3.hmat = "Ti48"
jeff3p3.mat = 2231
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/22-Ti-48g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 22048
jeff3p3.acer()

jeff3p3.hmat = "Ti49"
jeff3p3.mat = 2234
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/22-Ti-49g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 22049
jeff3p3.acer()

jeff3p3.hmat = "Ti50"
jeff3p3.mat = 2237
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/22-Ti-50g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 22050
jeff3p3.acer()

jeff3p3.hmat = "V48"
jeff3p3.mat = 2319
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/23-V-48g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 23048
jeff3p3.acer()

jeff3p3.hmat = "V49"
jeff3p3.mat = 2322
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/23-V-49g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 23049
jeff3p3.acer()

jeff3p3.hmat = "V50"
jeff3p3.mat = 2325
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/23-V-50g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 23050
jeff3p3.acer()

jeff3p3.hmat = "V51"
jeff3p3.mat = 2328
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/23-V-51g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 23051
jeff3p3.acer()

jeff3p3.hmat = "Cr50"
jeff3p3.mat = 2425
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/24-Cr-50g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 24050
jeff3p3.acer()

jeff3p3.hmat = "Cr51"
jeff3p3.mat = 2428
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/24-Cr-51g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 24051
jeff3p3.acer()

jeff3p3.hmat = "Cr52"
jeff3p3.mat = 2431
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/24-Cr-52g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 4.1677
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 24052
jeff3p3.acer()
jeff3p3.dilutions = None

jeff3p3.hmat = "Cr53"
jeff3p3.mat = 2434
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/24-Cr-53g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 24053
jeff3p3.acer()

jeff3p3.hmat = "Cr54"
jeff3p3.mat = 2437
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/24-Cr-54g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 24054
jeff3p3.acer()

jeff3p3.hmat = "Mn55"
jeff3p3.mat = 2525
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/25-Mn-55g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 25055
jeff3p3.acer()

jeff3p3.hmat = "Fe54"
jeff3p3.mat = 2625
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/26-Fe-54g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 26054
jeff3p3.acer()

jeff3p3.hmat = "Fe56"
jeff3p3.mat = 2631
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/26-Fe-56g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 3.7243
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 26056
jeff3p3.acer()
jeff3p3.dilutions = None

jeff3p3.hmat = "Fe57"
jeff3p3.mat = 2634
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/26-Fe-57g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 26057
jeff3p3.acer()

jeff3p3.hmat = "Fe58"
jeff3p3.mat = 2637
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/26-Fe-58g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 26058
jeff3p3.acer()

jeff3p3.hmat = "Co59"
jeff3p3.mat = 2725
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/27-Co-59g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 27059
jeff3p3.acer()

jeff3p3.hmat = "Ni58"
jeff3p3.mat = 2825
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/28-Ni-58g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 28058
jeff3p3.acer()

jeff3p3.hmat = "Ni60"
jeff3p3.mat = 2831
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/28-Ni-60g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 28060
jeff3p3.acer()

jeff3p3.hmat = "Ni61"
jeff3p3.mat = 2834
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/28-Ni-61g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 28061
jeff3p3.acer()

jeff3p3.hmat = "Ni62"
jeff3p3.mat = 2837
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/28-Ni-62g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 28062
jeff3p3.acer()

jeff3p3.hmat = "Ni64"
jeff3p3.mat = 2843
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/28-Ni-64g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 28064
jeff3p3.acer()

jeff3p3.hmat = "Cu63"
jeff3p3.mat = 2925
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/29-Cu-63g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 29063
jeff3p3.acer()

jeff3p3.hmat = "Cu65"
jeff3p3.mat = 2931
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/29-Cu-65g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 29065
jeff3p3.acer()

jeff3p3.hmat = "Zn64"
jeff3p3.mat = 3025
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/30-Zn-64g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 30064
jeff3p3.acer()

jeff3p3.hmat = "Zn66"
jeff3p3.mat = 3031
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/30-Zn-66g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 30066
jeff3p3.acer()

jeff3p3.hmat = "Zn67"
jeff3p3.mat = 3034
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/30-Zn-67g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 30067
jeff3p3.acer()

jeff3p3.hmat = "Zn68"
jeff3p3.mat = 3037
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/30-Zn-68g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 30068
jeff3p3.acer()

jeff3p3.hmat = "Zn70"
jeff3p3.mat = 3043
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/30-Zn-70g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 30070
jeff3p3.acer()

jeff3p3.hmat = "Ga69"
jeff3p3.mat = 3125
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/31-Ga-69g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 31069
jeff3p3.acer()

jeff3p3.hmat = "Ga71"
jeff3p3.mat = 3131
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/31-Ga-71g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 31071
jeff3p3.acer()

jeff3p3.hmat = "W182"
jeff3p3.mat =  7431
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/74-W-182g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 74182
jeff3p3.acer()

jeff3p3.hmat = "W183"
jeff3p3.mat = 7434
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/74-W-183g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 74183
jeff3p3.acer()

jeff3p3.hmat = "W184"
jeff3p3.mat = 7437
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/74-W-184g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 74184
jeff3p3.acer()

jeff3p3.hmat = "W186"
jeff3p3.mat =  7443
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/74-W-186g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 74186
jeff3p3.acer()

jeff3p3.hmat = "Re187"
jeff3p3.mat =  7531
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/75-Re-187g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 75187
jeff3p3.acer()

jeff3p3.hmat = "Au197"
jeff3p3.mat =  7925
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/79-Au-197g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 79197
jeff3p3.acer()

jeff3p3.hmat = "Pb204"
jeff3p3.mat =  8225
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/82-Pb-204g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 82204
jeff3p3.acer()

jeff3p3.hmat = "Pb206"
jeff3p3.mat =  8231
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/82-Pb-206g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 82206
jeff3p3.acer()

jeff3p3.hmat = "Pb207"
jeff3p3.mat =  8234
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/82-Pb-207g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 82207
jeff3p3.acer()

jeff3p3.hmat = "Pb208"
jeff3p3.mat =  8237
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/82-Pb-208g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 82208
jeff3p3.acer()

jeff3p3.hmat = "Bi209"
jeff3p3.mat =  8325
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/83-Bi-209g.jeff33"
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 83209
jeff3p3.acer()

jeff3p3.hmat = "Th230"
jeff3p3.mat = 9034
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/90-Th-230g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 8.7040
jeff3p3.eFiss = 1.7406E+02
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 90230
jeff3p3.acer()

#Use JEFF-3.1.2 Th232 to have 8 delayed neutron groups
jeff3p3.hmat = "Th232"
jeff3p3.mat = 9040
#jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/90-Th-232g.jeff33"
jeff3p3.evaluationFile = evaluationDir + "../JEFF-3.1.2/JEFF312N/JEFF312N9040_0.ASC"
jeff3p3.fission = 2 # fission with delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 11.8699
jeff3p3.eFiss = 1.7193E+02
jeff3p3.dilutions = ( 1.e10, 94.5317612, 56.3173141, 33.5510521, 19.9880447, \
11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.eFiss = 1.7193E+02
jeff3p3.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 90232
jeff3p3.acer()

jeff3p3.hmat = "Th233"
jeff3p3.mat = 9043
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/90-Th-233g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (22.53556, 1.227732e5)
jeff3p3.potential = 11.8699
jeff3p3.eFiss = 1.7193E+02
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 90233
jeff3p3.acer()

jeff3p3.hmat = "Pa231"
jeff3p3.mat = 9131
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/91-Pa-231g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 8.7266
jeff3p3.eFiss = 1.7E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 91231
jeff3p3.acer()

jeff3p3.hmat = "Pa233"
jeff3p3.mat = 9137
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/91-Pa-233g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 9.9950
jeff3p3.eFiss = 1.7558E+02
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 91233
jeff3p3.acer()

jeff3p3.hmat = "U232"
jeff3p3.mat = 9219
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/92-U-232g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 12.0687
jeff3p3.eFiss = 1.8E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 92232
jeff3p3.acer()

jeff3p3.hmat = "U233"
jeff3p3.mat = 9222
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/92-U-233g.jeff33"
jeff3p3.fission = 2 # fission with delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 12.2989
jeff3p3.eFiss = 1.8087E+02
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 92233
jeff3p3.acer()

jeff3p3.hmat = "U234"
jeff3p3.mat = 9225
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/92-U-234g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.0210
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 92234
jeff3p3.acer()

jeff3p3.hmat = "U235"
jeff3p3.mat = 9228
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/92-U-235g.jeff33"
jeff3p3.fission = 2 # fission with delayed neutrons
jeff3p3.ss = (4.632489, 2.499908e4)
jeff3p3.potential = 11.6070
jeff3p3.dilutions = ( 1.e10, 94.5317612, 56.3173141, 33.5510521, 19.9880447, \
11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 92235
jeff3p3.acer()

jeff3p3.hmat = "U236"
jeff3p3.mat = 9231
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/92-U-236g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.9954
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 92236
jeff3p3.acer()

jeff3p3.hmat = "U237"
jeff3p3.mat = 9234
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/92-U-237g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.5000
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 92237
jeff3p3.acer()

jeff3p3.hmat = "U238"
jeff3p3.mat = 9237
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/92-U-238g.jeff33"
jeff3p3.fission = 2 # fission with delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 11.17103
jeff3p3.dilutions = ( 1.e10, 94.5317612, 56.3173141, 33.5510521, 19.9880447, \
11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 92238
jeff3p3.acer()

jeff3p3.hmat = "Np236"
jeff3p3.mat = 9343
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/93-Np-236g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.eFiss = 1.8E2
jeff3p3.potential = 12.1427
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 93236
jeff3p3.acer()

jeff3p3.hmat = "Np237"
jeff3p3.mat = 9346
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/93-Np-237g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 11.4369
jeff3p3.branchingN2N = 0.801
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 93237
jeff3p3.acer()
jeff3p3.branchingN2N = None

jeff3p3.hmat = "Np238"
jeff3p3.mat = 9349
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/93-Np-238g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.eFiss = 1.8E2
jeff3p3.potential = 10.4000
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 93238
jeff3p3.acer()

jeff3p3.hmat = "Np239"
jeff3p3.mat = 9352
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/93-Np-239g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.eFiss = 1.8E2
jeff3p3.potential = 10.4979
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 93239
jeff3p3.acer()

jeff3p3.hmat = "Pu236"
jeff3p3.mat = 9428
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/94-Pu-236g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.eFiss = 1.8E2
jeff3p3.potential = 11.2458
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 94236
jeff3p3.acer()

jeff3p3.hmat = "Pu237"
jeff3p3.mat = 9431
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/94-Pu-237g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.5209
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 94237
jeff3p3.acer()

jeff3p3.hmat = "Pu238"
jeff3p3.mat = 9434
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/94-Pu-238g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.8897
jeff3p3.dilutions = ( 1.e10, 94.5317612, 56.3173141, 33.5510521, 19.9880447, \
11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 94238
jeff3p3.acer()

jeff3p3.hmat = "Pu239"
jeff3p3.mat = 9437
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/94-Pu-239g.jeff33"
jeff3p3.fission = 2 # fission with delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.8897
jeff3p3.dilutions = ( 1.e10, 158.887822, 100.279413, 63.2896882, \
39.9442369, 25.2101426, 15.9109633, 10.0419406, 6.33780423, 4.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.dilutions = ( 1.e10, 10000.0, 6311.33494, 3983.29436, 2513.99016, \
1586.66319, 1001.39615, 632.014573, 398.885514, 251.749976 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 94239
jeff3p3.acer()

jeff3p3.hmat = "Pu240"
jeff3p3.mat = 9440
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/94-Pu-240g.jeff33"
jeff3p3.fission = 2 # fission with delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 9.9091
jeff3p3.dilutions = ( 1.e10, 94.5317612, 56.3173141, 33.5510521, 19.9880447, \
11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 94240
jeff3p3.acer()

jeff3p3.hmat = "Pu241"
jeff3p3.mat = 9443
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/94-Pu-241g.jeff33"
jeff3p3.fission = 2 # fission with delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 11.2156
jeff3p3.dilutions = ( 1.e10, 94.5317612, 56.3173141, 33.5510521, 19.9880447, \
11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 94241
jeff3p3.acer()

jeff3p3.hmat = "Pu242"
jeff3p3.mat = 9446
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/94-Pu-242g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.6961
jeff3p3.dilutions = ( 1.e10,  469.546659, 258.807233, 142.650752, \
78.6270025, 43.3380508, 23.8872981, 13.1663284, 7.25708715, 4.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.dilutions = ( 1.e10, 1.e5, 55118.5161, 30380.5178, 16745.2959, \
9229.76155, 5087.30922, 2804.05024, 1545.55137, 851.885253 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 94242
jeff3p3.acer()

jeff3p3.hmat = "Pu243"
jeff3p3.mat = 9449
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/94-Pu-243g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.2000
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 94243
jeff3p3.acer()

jeff3p3.hmat = "Pu244"
jeff3p3.mat = 9452
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/94-Pu-244g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.1000
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 94244
jeff3p3.acer()

jeff3p3.hmat = "Am241"
jeff3p3.mat = 9543
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/95-Am-241g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 11.0329
jeff3p3.branchingNG = 0.115
jeff3p3.dilutions = ( 1.e10, 94.5317612, 56.3173141, 33.5510521, 19.9880447, \
11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.branchingNG = 0.115
jeff3p3.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
1259.67004, 750.448669, 447.079956, 266.347961, 158.676849 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 95241
jeff3p3.acer()
jeff3p3.branchingNG = None

jeff3p3.hmat = "Am242"
jeff3p3.mat = 9546
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/95-Am-242g.jeff33"
jeff3p3.fission = 0 # no fission matrix!!!
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.2000
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 95242
jeff3p3.acer()

jeff3p3.hmat = "Am242m"
jeff3p3.mat = 9547
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/95-Am-242m.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.2000
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 95342
jeff3p3.acer()

jeff3p3.hmat = "Am243"
jeff3p3.mat = 9549
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/95-Am-243g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 11.8237
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 95243
jeff3p3.acer()

jeff3p3.hmat = "Cm241"
jeff3p3.mat = 9628
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/96-Cm-241g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.1788
jeff3p3.eFiss = 1.9245E+02
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 96241
jeff3p3.acer()

jeff3p3.hmat = "Cm242"
jeff3p3.mat = 9631
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/96-Cm-242g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.2000
jeff3p3.eFiss = 1.9142E+02
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 96242
jeff3p3.acer()

jeff3p3.hmat = "Cm243"
jeff3p3.mat = 9634
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/96-Cm-243g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 11.2832
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 96243
jeff3p3.acer()

jeff3p3.hmat = "Cm244"
jeff3p3.mat = 9637
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/96-Cm-244g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.3200
jeff3p3.eFiss = 1.9049E+02
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 96244
jeff3p3.acer()

jeff3p3.hmat = "Cm245"
jeff3p3.mat = 9640
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/96-Cm-245g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 11.3900
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 96245
jeff3p3.acer()

jeff3p3.hmat = "Cm246"
jeff3p3.mat = 9643
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/96-Cm-246g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.2758
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 96246
jeff3p3.acer()

jeff3p3.hmat = "Cm247"
jeff3p3.mat = 9646
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/96-Cm-247g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.0000
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 96247
jeff3p3.acer()

jeff3p3.hmat = "Cm248"
jeff3p3.mat = 9649
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/96-Cm-248g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 10.3971
jeff3p3.eFiss = 1.8885E+02
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 96248
jeff3p3.acer()

jeff3p3.hmat = "Bk249"
jeff3p3.mat = 9752
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/97-Bk-249g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 11.0983
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 97249
jeff3p3.acer()

jeff3p3.hmat = "Cf249"
jeff3p3.mat = 9852
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/98-Cf-249g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 11.1510
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 98249
jeff3p3.acer()

jeff3p3.hmat = "Cf250"
jeff3p3.mat = 9855
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/98-Cf-250g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 9.8800
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 98250
jeff3p3.acer()

jeff3p3.hmat = "Cf251"
jeff3p3.mat = 9858
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/98-Cf-251g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 9.8800
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 98251
jeff3p3.acer()

jeff3p3.hmat = "Cf252"
jeff3p3.mat = 9861
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/98-Cf-252g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 9.8000
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 98252
jeff3p3.acer()

jeff3p3.hmat = "Cf253"
jeff3p3.mat = 9864
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/98-Cf-253g.jeff33"
jeff3p3.fission = 1 # fission without delayed neutrons
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 9.7600
jeff3p3.eFiss = 1.9E2
jeff3p3.dilutions = ( 1.e10, 10000.0, 3549.18335, 1259.67004, 447.079956, \
158.676849, 56.3173141 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 98253
jeff3p3.acer()

# Process the fission products:

jeff3p3.scatteringLaw = None
jeff3p3.legendre = 0
jeff3p3.fission = None
jeff3p3.dilutions = None
jeff3p3.eFiss = None

jeff3p3.hmat = "Ge72"
jeff3p3.mat = 3231
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/32-Ge-72g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 32072
jeff3p3.acer()

jeff3p3.hmat = "Ge73"
jeff3p3.mat = 3234
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/32-Ge-73g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 32073
jeff3p3.acer()

jeff3p3.hmat = "Ge74"
jeff3p3.mat = 3237
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/32-Ge-74g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 32074
jeff3p3.acer()

jeff3p3.hmat = "Ge76"
jeff3p3.mat = 3243
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/32-Ge-76g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 32076
jeff3p3.acer()

jeff3p3.hmat = "As75"
jeff3p3.mat = 3325
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/33-As-75g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 33075
jeff3p3.acer()

jeff3p3.hmat = "Se76"
jeff3p3.mat = 3431
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/34-Se-76g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 34076
jeff3p3.acer()

jeff3p3.hmat = "Se77"
jeff3p3.mat = 3434
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/34-Se-77g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 34077
jeff3p3.acer()

jeff3p3.hmat = "Se78"
jeff3p3.mat = 3437
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/34-Se-78g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 34078
jeff3p3.acer()

jeff3p3.hmat = "Se79"
jeff3p3.mat = 3440
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/34-Se-79g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 34079
jeff3p3.acer()

jeff3p3.hmat = "Se80"
jeff3p3.mat = 3443
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/34-Se-80g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 34080
jeff3p3.acer()

jeff3p3.hmat = "Se82"
jeff3p3.mat = 3449
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/34-Se-82g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 34082
jeff3p3.acer()

jeff3p3.hmat = "Br79"
jeff3p3.mat = 3525
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/35-Br-79g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 35079
jeff3p3.acer()

jeff3p3.hmat = "Br81"
jeff3p3.mat = 3531
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/35-Br-81g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 35081
jeff3p3.acer()

jeff3p3.hmat = "Kr80"
jeff3p3.mat = 3631
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/36-Kr-80g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 36080
jeff3p3.acer()

jeff3p3.hmat = "Kr82"
jeff3p3.mat = 3637
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/36-Kr-82g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 36082
jeff3p3.acer()

jeff3p3.hmat = "Kr83"
jeff3p3.mat = 3640
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/36-Kr-83g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 36083
jeff3p3.acer()

jeff3p3.hmat = "Kr84"
jeff3p3.mat = 3643
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/36-Kr-84g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 36084
jeff3p3.acer()

jeff3p3.hmat = "Kr85"
jeff3p3.mat = 3646
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/36-Kr-85g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 36085
jeff3p3.acer()

jeff3p3.hmat = "Kr86"
jeff3p3.mat = 3649
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/36-Kr-86g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 36086
jeff3p3.acer()

jeff3p3.hmat = "Rb85"
jeff3p3.mat = 3725
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/37-Rb-85g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 37085
jeff3p3.acer()

jeff3p3.hmat = "Rb87"
jeff3p3.mat = 3731
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/37-Rb-87g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 37087
jeff3p3.acer()

jeff3p3.hmat = "Sr86"
jeff3p3.mat = 3831
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/38-Sr-86g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 38086
jeff3p3.acer()

jeff3p3.hmat = "Sr87"
jeff3p3.mat = 3834
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/38-Sr-87g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 38087
jeff3p3.acer()

jeff3p3.hmat = "Sr88"
jeff3p3.mat = 3837
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/38-Sr-88g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 38088
jeff3p3.acer()

jeff3p3.hmat = "Sr89"
jeff3p3.mat = 3840
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/38-Sr-89g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 38089
jeff3p3.acer()

jeff3p3.hmat = "Sr90"
jeff3p3.mat = 3843
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/38-Sr-90g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 38090
jeff3p3.acer()

jeff3p3.hmat = "Y89"
jeff3p3.mat = 3925
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/39-Y-89g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 39089
jeff3p3.acer()

jeff3p3.hmat = "Y90"
jeff3p3.mat = 3928
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/39-Y-90g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 39090
jeff3p3.acer()

jeff3p3.hmat = "Y91"
jeff3p3.mat = 3931
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/39-Y-91g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 39091
jeff3p3.acer()

jeff3p3.hmat = "Zr90"
jeff3p3.mat = 4025
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/40-Zr-90g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 6.5144
jeff3p3.dilutions = ( 1.e10, 10000.0,  3866.97, 1495.35, 578.2475, 223.6068, 86.4682, 33.4370, 12.9300, 5.0 )
jeff3p3.autolib = (4.632489, 3.481068e3, 0.0005)
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.autolib = (4.632489, 1.11377e4, 0.0005)
jeff3p3.za = 40090
jeff3p3.acer()

jeff3p3.hmat = "Zr91"
jeff3p3.mat = 4028
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/40-Zr-91g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 6.5144
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 40091
jeff3p3.acer()

jeff3p3.hmat = "Zr92"
jeff3p3.mat = 4031
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/40-Zr-92g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 6.5144
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 40092
jeff3p3.acer()

jeff3p3.hmat = "Zr93"
jeff3p3.mat = 4034
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/40-Zr-93g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 40093
jeff3p3.acer()

jeff3p3.hmat = "Zr94"
jeff3p3.mat = 4037
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/40-Zr-94g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 6.5144
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 40094
jeff3p3.acer()

jeff3p3.hmat = "Zr95"
jeff3p3.mat = 4040
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/40-Zr-95g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 40095
jeff3p3.acer()

jeff3p3.hmat = "Zr96"
jeff3p3.mat = 4043
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/40-Zr-96g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 6.5144
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 40096
jeff3p3.acer()

jeff3p3.hmat = "Nb93"
jeff3p3.mat = 4125
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/41-Nb-93g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 41093
jeff3p3.acer()

jeff3p3.hmat = "Nb94"
jeff3p3.mat = 4128
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/41-Nb-94g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 41094
jeff3p3.acer()

jeff3p3.hmat = "Nb95"
jeff3p3.mat = 4131
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/41-Nb-95g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 41095
jeff3p3.acer()

jeff3p3.hmat = "Mo92"
jeff3p3.mat = 4225
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/42-Mo-92g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 6.1140
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 42092
jeff3p3.acer()

jeff3p3.hmat = "Mo94"
jeff3p3.mat = 4231
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/42-Mo-94g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 6.1140
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 42094
jeff3p3.acer()

jeff3p3.hmat = "Mo95"
jeff3p3.mat = 4234
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/42-Mo-95g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 6.1140
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 42095
jeff3p3.acer()

jeff3p3.hmat = "Mo96"
jeff3p3.mat = 4237
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/42-Mo-96g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 6.1140
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 42096
jeff3p3.acer()

jeff3p3.hmat = "Mo97"
jeff3p3.mat = 4240
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/42-Mo-97g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 6.1140
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 42097
jeff3p3.acer()

jeff3p3.hmat = "Mo98"
jeff3p3.mat = 4243
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/42-Mo-98g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 6.1140
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 42098
jeff3p3.acer()

jeff3p3.hmat = "Mo99"
jeff3p3.mat = 4246
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/42-Mo-99g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 42099
jeff3p3.acer()

jeff3p3.hmat = "Mo100"
jeff3p3.mat = 4249
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/42-Mo-100g.jeff33"
jeff3p3.fission = None
jeff3p3.ss = (4.632489, 3.206464e5)
jeff3p3.potential = 6.1140
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 42100
jeff3p3.acer()

jeff3p3.hmat = "Tc99"
jeff3p3.mat = 4331
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/43-Tc-99g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 6.4675
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 43099
jeff3p3.acer()

jeff3p3.hmat = "Ru99"
jeff3p3.mat = 4434
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/44-Ru-99g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 44099
jeff3p3.acer()

jeff3p3.hmat = "Ru100"
jeff3p3.mat = 4437
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/44-Ru-100g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 44100
jeff3p3.acer()

jeff3p3.hmat = "Ru101"
jeff3p3.mat = 4440
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/44-Ru-101g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 44101
jeff3p3.acer()

jeff3p3.hmat = "Ru102"
jeff3p3.mat = 4443
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/44-Ru-102g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 44102
jeff3p3.acer()

jeff3p3.hmat = "Ru103"
jeff3p3.mat = 4446
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/44-Ru-103g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 44103
jeff3p3.acer()

jeff3p3.hmat = "Ru104"
jeff3p3.mat = 4449
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/44-Ru-104g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 44104
jeff3p3.acer()

jeff3p3.hmat = "Ru105"
jeff3p3.mat = 4452
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/44-Ru-105g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 44105
jeff3p3.acer()

jeff3p3.hmat = "Ru106"
jeff3p3.mat = 4455
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/44-Ru-106g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 44106
jeff3p3.acer()

jeff3p3.hmat = "Rh103"
jeff3p3.mat = 4525
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/45-Rh-103g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 45103
jeff3p3.acer()

jeff3p3.hmat = "Rh105"
jeff3p3.mat = 4531
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/45-Rh-105g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 45105
jeff3p3.acer()

jeff3p3.hmat = "Pd104"
jeff3p3.mat = 4631
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/46-Pd-104g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 46104
jeff3p3.acer()

jeff3p3.hmat = "Pd105"
jeff3p3.mat = 4634
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/46-Pd-105g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 46105
jeff3p3.acer()

jeff3p3.hmat = "Pd106"
jeff3p3.mat = 4637
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/46-Pd-106g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 46106
jeff3p3.acer()

jeff3p3.hmat = "Pd107"
jeff3p3.mat = 4640
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/46-Pd-107g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 46107
jeff3p3.acer()

jeff3p3.hmat = "Pd108"
jeff3p3.mat = 4643
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/46-Pd-108g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 46108
jeff3p3.acer()

jeff3p3.hmat = "Pd110"
jeff3p3.mat = 4649
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/46-Pd-110g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 46110
jeff3p3.acer()

jeff3p3.hmat = "Ag107"
jeff3p3.mat = 4725
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/47-Ag-107g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 5.4739
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 47107
jeff3p3.acer()

jeff3p3.hmat = "Ag109"
jeff3p3.mat = 4731
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/47-Ag-109g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 5.3316
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.za = 47109
jeff3p3.acer()
jeff3p3.draglib()

jeff3p3.hmat = "Ag111"
jeff3p3.mat = 4737
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/47-Ag-111g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 47111
jeff3p3.acer()

jeff3p3.hmat = "Cd106"
jeff3p3.mat = 4825
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/48-Cd-106g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 48106
jeff3p3.acer()

jeff3p3.hmat = "Cd108"
jeff3p3.mat = 4831
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/48-Cd-108g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 48108
jeff3p3.acer()

jeff3p3.hmat = "Cd110"
jeff3p3.mat = 4837
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/48-Cd-110g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 5.1762
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 48110
jeff3p3.acer()

jeff3p3.hmat = "Cd111"
jeff3p3.mat = 4840
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/48-Cd-111g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 48111
jeff3p3.acer()

jeff3p3.hmat = "Cd112"
jeff3p3.mat = 4843
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/48-Cd-112g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 48112
jeff3p3.acer()

jeff3p3.hmat = "Cd113"
jeff3p3.mat = 4846
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/48-Cd-113g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 48113
jeff3p3.acer()

jeff3p3.hmat = "Cd114"
jeff3p3.mat = 4849
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/48-Cd-114g.jeff33"
jeff3p3.branchingNG = 0.079383
jeff3p3.makeFp()
jeff3p3.za = 48114
jeff3p3.acer()
jeff3p3.branchingNG = None

jeff3p3.hmat = "Cd115m"
jeff3p3.mat = 4853
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/48-Cd-115m.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 48315
jeff3p3.acer()

jeff3p3.hmat = "Cd116"
jeff3p3.mat = 4855
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/48-Cd-116g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 48116
jeff3p3.acer()

jeff3p3.hmat = "In113"
jeff3p3.mat = 4925
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/49-In-113g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 49113
jeff3p3.acer()

jeff3p3.hmat = "In115"
jeff3p3.mat = 4931
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/49-In-115g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 5.0695
jeff3p3.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 49115
jeff3p3.acer()

jeff3p3.hmat = "Sn112"
jeff3p3.mat = 5025
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-112g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50112
jeff3p3.acer()

jeff3p3.hmat = "Sn114"
jeff3p3.mat = 5031
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-114g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50114
jeff3p3.acer()

jeff3p3.hmat = "Sn115"
jeff3p3.mat = 5034
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-115g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50115
jeff3p3.acer()

jeff3p3.hmat = "Sn116"
jeff3p3.mat = 5037
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-116g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50116
jeff3p3.acer()

jeff3p3.hmat = "Sn117"
jeff3p3.mat = 5040
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-117g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50117
jeff3p3.acer()

jeff3p3.hmat = "Sn118"
jeff3p3.mat = 5043
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-118g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50118
jeff3p3.acer()

jeff3p3.hmat = "Sn119"
jeff3p3.mat = 5046
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-119g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50119
jeff3p3.acer()

jeff3p3.hmat = "Sn120"
jeff3p3.mat = 5049
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-120g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50120
jeff3p3.acer()

jeff3p3.hmat = "Sn122"
jeff3p3.mat = 5055
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-122g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50122
jeff3p3.acer()

jeff3p3.hmat = "Sn123"
jeff3p3.mat = 5058
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-123g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50123
jeff3p3.acer()

jeff3p3.hmat = "Sn124"
jeff3p3.mat = 5061
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-124g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50124
jeff3p3.acer()

jeff3p3.hmat = "Sn125"
jeff3p3.mat = 5064
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-125g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50125
jeff3p3.acer()

jeff3p3.hmat = "Sn126"
jeff3p3.mat = 5067
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/50-Sn-126g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 50126
jeff3p3.acer()

jeff3p3.hmat = "Sb121"
jeff3p3.mat = 5125
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/51-Sb-121g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 51121
jeff3p3.acer()

jeff3p3.hmat = "Sb123"
jeff3p3.mat = 5131
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/51-Sb-123g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 51123
jeff3p3.acer()

jeff3p3.hmat = "Sb124"
jeff3p3.mat = 5134
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/51-Sb-124g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 51124
jeff3p3.acer()

jeff3p3.hmat = "Sb125"
jeff3p3.mat = 5137
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/51-Sb-125g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 51125
jeff3p3.acer()

jeff3p3.hmat = "Sb126"
jeff3p3.mat = 5140
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/51-Sb-126g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 51126
jeff3p3.acer()

jeff3p3.hmat = "Te122"
jeff3p3.mat = 5231
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-122g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 52122
jeff3p3.acer()

jeff3p3.hmat = "Te123"
jeff3p3.mat = 5234
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-123g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 52123
jeff3p3.acer()

jeff3p3.hmat = "Te124"
jeff3p3.mat = 5237
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-124g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 52124
jeff3p3.acer()

jeff3p3.hmat = "Te125"
jeff3p3.mat = 5240
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-125g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 52125
jeff3p3.acer()

jeff3p3.hmat = "Te126"
jeff3p3.mat = 5243
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-126g.jeff33"
jeff3p3.branchingNG = 0.091528
jeff3p3.makeFp()
jeff3p3.za = 52126
jeff3p3.acer()
jeff3p3.branchingNG = None

jeff3p3.hmat = "Te127m"
jeff3p3.mat = 5247
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-127m.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 52327
jeff3p3.acer()

jeff3p3.hmat = "Te128"
jeff3p3.mat = 5249
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-128g.jeff33"
jeff3p3.branchingNG = 0.031894
jeff3p3.makeFp()
jeff3p3.za = 52128
jeff3p3.acer()
jeff3p3.branchingNG = None

jeff3p3.hmat = "Te129m"
jeff3p3.mat = 5253
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-129m.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 52329
jeff3p3.acer()

jeff3p3.hmat = "Te130"
jeff3p3.mat = 5255
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-130g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 52130
jeff3p3.acer()

jeff3p3.hmat = "Te131m"
jeff3p3.mat = 5259
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-131m.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 52131
jeff3p3.acer()

jeff3p3.hmat = "Te132"
jeff3p3.mat = 5261
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/52-Te-132g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 52132
jeff3p3.acer()

jeff3p3.hmat = "I127"
jeff3p3.mat = 5325
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/53-I-127g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 4.5239
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 53127
jeff3p3.acer()

jeff3p3.hmat = "I129"
jeff3p3.mat = 5331
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/53-I-129g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 5.8221
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 53129
jeff3p3.acer()

jeff3p3.hmat = "I130"
jeff3p3.mat = 5334
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/53-I-130g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 53130
jeff3p3.acer()

jeff3p3.hmat = "I131"
jeff3p3.mat = 5337
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/53-I-131g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 53131
jeff3p3.acer()

jeff3p3.hmat = "I135"
jeff3p3.mat = 5349
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/53-I-135g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 53135
jeff3p3.acer()

jeff3p3.hmat = "Xe128"
jeff3p3.mat = 5437
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/54-Xe-128g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 54128
jeff3p3.acer()

jeff3p3.hmat = "Xe129"
jeff3p3.mat = 5440
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/54-Xe-129g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 54129
jeff3p3.acer()

jeff3p3.hmat = "Xe130"
jeff3p3.mat = 5443
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/54-Xe-130g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 54130
jeff3p3.acer()

jeff3p3.hmat = "Xe131"
jeff3p3.mat = 5446
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/54-Xe-131g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 54131
jeff3p3.acer()

jeff3p3.hmat = "Xe132"
jeff3p3.mat = 5449
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/54-Xe-132g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 54132
jeff3p3.acer()

jeff3p3.hmat = "Xe133"
jeff3p3.mat = 5452
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/54-Xe-133g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 54133
jeff3p3.acer()

jeff3p3.hmat = "Xe134"
jeff3p3.mat = 5455
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/54-Xe-134g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 54134
jeff3p3.acer()

jeff3p3.hmat = "Xe135"
jeff3p3.mat = 5458
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/54-Xe-135g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 54135
jeff3p3.acer()

jeff3p3.hmat = "Xe136"
jeff3p3.mat = 5461
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/54-Xe-136g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 54136
jeff3p3.acer()

jeff3p3.hmat = "Cs133"
jeff3p3.mat = 5525
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/55-Cs-133g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 55133
jeff3p3.acer()

jeff3p3.hmat = "Cs134"
jeff3p3.mat = 5528
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/55-Cs-134g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 55134
jeff3p3.acer()

jeff3p3.hmat = "Cs135"
jeff3p3.mat = 5531
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/55-Cs-135g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 55135
jeff3p3.acer()

jeff3p3.hmat = "Cs136"
jeff3p3.mat = 5534
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/55-Cs-136g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 55136
jeff3p3.acer()

jeff3p3.hmat = "Cs137"
jeff3p3.mat = 5537
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/55-Cs-137g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 55137
jeff3p3.acer()

jeff3p3.hmat = "Ba134"
jeff3p3.mat = 5637
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/56-Ba-134g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 56134
jeff3p3.acer()

jeff3p3.hmat = "Ba135"
jeff3p3.mat = 5640
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/56-Ba-135g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 56135
jeff3p3.acer()

jeff3p3.hmat = "Ba136"
jeff3p3.mat = 5643
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/56-Ba-136g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 56136
jeff3p3.acer()

jeff3p3.hmat = "Ba137"
jeff3p3.mat = 5646
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/56-Ba-137g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 56137
jeff3p3.acer()

jeff3p3.hmat = "Ba138"
jeff3p3.mat = 5649
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/56-Ba-138g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 56138
jeff3p3.acer()

jeff3p3.hmat = "Ba140"
jeff3p3.mat = 5655
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/56-Ba-140g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 56140
jeff3p3.acer()

jeff3p3.hmat = "La138"
jeff3p3.mat = 5725
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/57-La-138g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 57138
jeff3p3.acer()

jeff3p3.hmat = "La139"
jeff3p3.mat = 5728
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/57-La-139g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 57139
jeff3p3.acer()

jeff3p3.hmat = "La140"
jeff3p3.mat = 5731
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/57-La-140g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 57140
jeff3p3.acer()

jeff3p3.hmat = "Ce140"
jeff3p3.mat = 5837
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/58-Ce-140g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 58140
jeff3p3.acer()

jeff3p3.hmat = "Ce141"
jeff3p3.mat = 5840
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/58-Ce-141g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 58141
jeff3p3.acer()

jeff3p3.hmat = "Ce142"
jeff3p3.mat = 5843
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/58-Ce-142g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 58142
jeff3p3.acer()

jeff3p3.hmat = "Ce143"
jeff3p3.mat = 5846
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/58-Ce-143g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 58143
jeff3p3.acer()

jeff3p3.hmat = "Ce144"
jeff3p3.mat = 5849
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/58-Ce-144g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 58144
jeff3p3.acer()

jeff3p3.hmat = "Pr141"
jeff3p3.mat = 5925
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/59-Pr-141g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 59141
jeff3p3.acer()

jeff3p3.hmat = "Pr142"
jeff3p3.mat = 5928
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/59-Pr-142g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 59142
jeff3p3.acer()

jeff3p3.hmat = "Pr143"
jeff3p3.mat = 5931
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/59-Pr-143g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 59143
jeff3p3.acer()

jeff3p3.hmat = "Nd142"
jeff3p3.mat = 6025
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/60-Nd-142g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 60142
jeff3p3.acer()

jeff3p3.hmat = "Nd143"
jeff3p3.mat = 6028
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/60-Nd-143g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 60143
jeff3p3.acer()

jeff3p3.hmat = "Nd144"
jeff3p3.mat = 6031
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/60-Nd-144g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 60144
jeff3p3.acer()

jeff3p3.hmat = "Nd145"
jeff3p3.mat = 6034
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/60-Nd-145g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 60145
jeff3p3.acer()

jeff3p3.hmat = "Nd146"
jeff3p3.mat = 6037
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/60-Nd-146g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 60146
jeff3p3.acer()

jeff3p3.hmat = "Nd147"
jeff3p3.mat = 6040
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/60-Nd-147g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 60147
jeff3p3.acer()

jeff3p3.hmat = "Nd148"
jeff3p3.mat = 6043
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/60-Nd-148g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 60148
jeff3p3.acer()

jeff3p3.hmat = "Nd150"
jeff3p3.mat = 6049
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/60-Nd-150g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 60150
jeff3p3.acer()

jeff3p3.hmat = "Pm147"
jeff3p3.mat = 6149
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/61-Pm-147g.jeff33"
jeff3p3.branchingNG = 0.470
jeff3p3.makeFp()
jeff3p3.za = 61147
jeff3p3.acer()
jeff3p3.branchingNG = None

jeff3p3.hmat = "Pm148"
jeff3p3.mat = 6152
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/61-Pm-148g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 61148
jeff3p3.acer()

jeff3p3.hmat = "Pm148m"
jeff3p3.mat = 6153
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/61-Pm-148m.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 61348
jeff3p3.acer()

jeff3p3.hmat = "Pm149"
jeff3p3.mat = 6155
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/61-Pm-149g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 61149
jeff3p3.acer()

jeff3p3.hmat = "Pm151"
jeff3p3.mat = 6161
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/61-Pm-151g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 61151
jeff3p3.acer()

jeff3p3.hmat = "Sm147"
jeff3p3.mat = 6234
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/62-Sm-147g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 62147
jeff3p3.acer()

jeff3p3.hmat = "Sm148"
jeff3p3.mat = 6237
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/62-Sm-148g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 62148
jeff3p3.acer()

jeff3p3.hmat = "Sm149"
jeff3p3.mat = 6240
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/62-Sm-149g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 62149
jeff3p3.acer()

jeff3p3.hmat = "Sm150"
jeff3p3.mat = 6243
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/62-Sm-150g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 62150
jeff3p3.acer()

jeff3p3.hmat = "Sm151"
jeff3p3.mat = 6246
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/62-Sm-151g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 62151
jeff3p3.acer()

jeff3p3.hmat = "Sm152"
jeff3p3.mat = 6249
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/62-Sm-152g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 62152
jeff3p3.acer()

jeff3p3.hmat = "Sm153"
jeff3p3.mat = 6252
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/62-Sm-153g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 62153
jeff3p3.acer()

jeff3p3.hmat = "Sm154"
jeff3p3.mat = 6255
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/62-Sm-154g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 62154
jeff3p3.acer()

jeff3p3.hmat = "Eu151"
jeff3p3.mat = 6325
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/63-Eu-151g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 63151
jeff3p3.acer()

jeff3p3.hmat = "Eu152"
jeff3p3.mat = 6328
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/63-Eu-152g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 63152
jeff3p3.acer()

jeff3p3.hmat = "Eu153"
jeff3p3.mat = 6331
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/63-Eu-153g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 63153
jeff3p3.acer()

jeff3p3.hmat = "Eu154"
jeff3p3.mat = 6334
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/63-Eu-154g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 63154
jeff3p3.acer()

jeff3p3.hmat = "Eu155"
jeff3p3.mat = 6337
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/63-Eu-155g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 63155
jeff3p3.acer()

jeff3p3.hmat = "Eu156"
jeff3p3.mat = 6340
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/63-Eu-156g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 63156
jeff3p3.acer()

jeff3p3.hmat = "Eu157"
jeff3p3.mat = 6343
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/63-Eu-157g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 63157
jeff3p3.acer()

jeff3p3.hmat = "Gd152"
jeff3p3.mat = 6425
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/64-Gd-152g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 8.0425
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 64152
jeff3p3.acer()

jeff3p3.hmat = "Gd154"
jeff3p3.mat = 6431
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/64-Gd-154g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 6.6723
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 64154
jeff3p3.acer()

jeff3p3.hmat = "Gd155"
jeff3p3.mat = 6434
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/64-Gd-155g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 6.3376
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 64155
jeff3p3.acer()

jeff3p3.hmat = "Gd156"
jeff3p3.mat = 6437
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/64-Gd-156g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.3792
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 64156
jeff3p3.acer()

jeff3p3.hmat = "Gd157"
jeff3p3.mat = 6440
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/64-Gd-157g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 6.6063
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 64157
jeff3p3.acer()

jeff3p3.hmat = "Gd158"
jeff3p3.mat = 6443
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/64-Gd-158g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.6454
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 64158
jeff3p3.acer()

jeff3p3.hmat = "Gd160"
jeff3p3.mat = 6449
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/64-Gd-160g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.0241
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 64160
jeff3p3.acer()

jeff3p3.dilutions = None

jeff3p3.hmat = "Tb159"
jeff3p3.mat = 6525
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/65-Tb-159g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 65159
jeff3p3.acer()

jeff3p3.hmat = "Tb160"
jeff3p3.mat = 6528
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/65-Tb-160g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 65160
jeff3p3.acer()

jeff3p3.hmat = "Dy160"
jeff3p3.mat = 6637
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/66-Dy-160g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 6.9861
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 66160
jeff3p3.acer()

jeff3p3.hmat = "Dy161"
jeff3p3.mat = 6640
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/66-Dy-161g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.0121
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 66161
jeff3p3.acer()

jeff3p3.hmat = "Dy162"
jeff3p3.mat = 6643
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/66-Dy-162g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 4.5681
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 66162
jeff3p3.acer()

jeff3p3.hmat = "Dy163"
jeff3p3.mat = 6646
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/66-Dy-163g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.0639
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 66163
jeff3p3.acer()

jeff3p3.hmat = "Dy164"
jeff3p3.mat = 6649
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/66-Dy-164g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.0897
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 66164
jeff3p3.acer()

jeff3p3.hmat = "Dy165"
jeff3p3.mat = 6652
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/66-Dy-165g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 66165
jeff3p3.acer()

jeff3p3.hmat = "Hf174"
jeff3p3.mat = 7225
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/72-Hf-174g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.1385
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 72174
jeff3p3.acer()

jeff3p3.hmat = "Hf176"
jeff3p3.mat = 7231
#jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/72-Hf-176g.jeff33"
jeff3p3.evaluationFile = evaluationDir + "../JEFF-3.2/JEFF32N/n-72-Hf-176.jeff32"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.1935
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 72176
jeff3p3.acer()

jeff3p3.hmat = "Hf177"
jeff3p3.mat = 7234
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/72-Hf-177g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.2202
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 72177
jeff3p3.acer()

jeff3p3.hmat = "Hf178"
jeff3p3.mat = 7237
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/72-Hf-178g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.2469
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 72178
jeff3p3.acer()

jeff3p3.hmat = "Hf179"
jeff3p3.mat = 7240
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/72-Hf-179g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.2736
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 72179
jeff3p3.acer()

jeff3p3.hmat = "Hf180"
jeff3p3.mat = 7243
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/72-Hf-180g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.3004
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 72180
jeff3p3.acer()

jeff3p3.hmat = "Ta181"
jeff3p3.mat = 7328
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/73-Ta-181g.jeff33"
jeff3p3.ss = (4.632489, 1.858471e4)
jeff3p3.potential = 7.6454
jeff3p3.dilutions = ( 1.e10, 10000.0, 4216.96552, 1778.27959, 749.894278, \
316.227791, 133.352152,  56.2341357, 23.7137381, 10.0 )
jeff3p3.pendf()
jeff3p3.gendf()
jeff3p3.draglib()
jeff3p3.za = 73181
jeff3p3.acer()

jeff3p3.dilutions = None

jeff3p3.hmat = "Ho165"
jeff3p3.mat = 6725
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/67-Ho-165g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 67165
jeff3p3.acer()

jeff3p3.hmat = "Er166"
jeff3p3.mat = 6837
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/68-Er-166g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 68166
jeff3p3.acer()

jeff3p3.hmat = "Er167"
jeff3p3.mat = 6840
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/68-Er-167g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 68167
jeff3p3.acer()

jeff3p3.hmat = "Er168"
jeff3p3.mat = 6843
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/68-Er-168g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 68168
jeff3p3.acer()

jeff3p3.hmat = "Er170"
jeff3p3.mat = 6849
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/68-Er-170g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 68170
jeff3p3.acer()

jeff3p3.hmat = "Lu176"
jeff3p3.mat = 7128
jeff3p3.evaluationFile = evaluationDir + "JEFF33-n/71-Lu-176g.jeff33"
jeff3p3.makeFp()
jeff3p3.za = 71176
jeff3p3.acer()

# Process the burnup chain (JEFF-3.3):

jeff3p3.fissionFile = evaluationDir + "JEFF33-nfy.asc"
jeff3p3.decayFile = evaluationDir + "JEFF33-rdd_all.asc"
jeff3p3.burnup()
