#!/usr/bin/env python
from PyNjoy_mp import *
import os, sys, copy, glob
from collections import OrderedDict
# Parallel distribution of the library generation
# Based on jeff3p3_shem295.py
# Vivian SALINO - IRSN - 03/2021
# Based on work from Richard Chambon (Ecole Polytechnique) and Javier Ortensi (Idaho National laboratory)

#############################################################################
#  file locations & options
#############################################################################
# Check that SANDY is compiled -- otherwise, compile it
sandy = os.path.expanduser("~") + "/.local/bin/sandy"
if not os.path.exists(sandy):
    print("SANDY does not appear to be available -- compiling it")
    os.system("./Compile_SANDY.sh")
    if not os.path.exists(sandy):
        raise Exception("SANDY install did not go as expected.")
# Retrieve isotope that shall be processed
iso = str(sys.argv[2])
# folder name where computations are performed
evalName   = "../output/SANDY/" + iso
os.system('mkdir -p ' + evalName)
# relative path to the 'njoy' code from current folder
execDir    = ".."
# evaluation folder paths
evalDir    = os.getcwd() + "/../../ENDF/JEFF-3.3/"
scatLawDir = os.getcwd() + "/../../ENDF/JEFF-3.3/"

njoy_jobs = VectPyNjoy()
njoy_jobs.setncpu(ncpu=int(sys.argv[1]))

nrand = 300

if iso == "H1" or iso == "H1_H2O":
    endf = "1-H-1g.jeff33"
elif iso == "B10":
    endf = "5-B-10g.jeff33"
elif iso == "B11":
    endf = "5-B-11g.jeff33"
elif iso == "O16":
    endf = "8-O-16g.jeff33"
elif iso == "U235":
    endf = "92-U-235g.jeff33"
elif iso == "U238":
    endf = "92-U-238g.jeff33"
#############################################################################
#  Calling SANDY, which will create PENDF files (RECONR) and draw random
#  samples
#############################################################################
njoy = " --njoy " + os.getcwd() + "/" + execDir + "/njoy "
srand = "-S " + str(nrand) + " "
nproc = "-N " + sys.argv[1] + " "
outdir = "--outdir " + evalName + " "
pathendf = evalDir + "JEFF33-n/" + endf
os.system(sandy + njoy + srand + nproc + outdir + pathendf)

#############################################################################
#  PyNjoy calls for BROADR, THERM, PURR, GROUPR, DRAGR and ACER
#############################################################################

job_ref = lib_base(evalName, execDir, evalDir, scatLawDir)
job_ref.nstr = 25
job_ref.eaf = 0
job_ref.fission = None
job_ref.dilutions = None
job_ref.Espectra = None
job_ref.autolib = (4.632489, 1.11377e4, 0.0005)
job_ref.serpent = True
job_ref.fp = 0
job_ref.concat = 1
job_ref.temperatures = ( 293., 550., 900., 1200., 2000. )
job_ref.suff = ( 0.02, 0.05, 0.09, 0.12, 0.20 )
job_ref.performRECONR = False

#############################################################################
#  H1_H2O (bound in light water) and H1 (free gas)
#############################################################################
c1 = len(njoy_jobs)
job_ref_nom = copy.deepcopy(job_ref)

for irand in range(0,nrand):

  if iso == 'H1_H2O':
    this_job = copy.deepcopy(job_ref_nom)
    this_job.legendre = 3
    this_job.hmat = "H1_H2O_" + str(irand).zfill(3)
    this_job.mat = 125
    this_job.evaluationFile = this_job.evaluationDir + "JEFF33-n/" + endf
    this_job.scatteringLaw =  this_job.evaluationDir + "JEFF33-tsl/tsl-HinH2O.jeff33"
    this_job.pendfFile = os.getcwd() + "/" + evalName + "/" + endf + "-" + str(irand + 1)
    this_job.scatteringMat = 1
    this_job.temperatures = ( 293.6, 323.6, 373.6, 423.6, 473.6, 523.6, 573.6, 647.2, 800.0 )
    this_job.fission = None
    this_job.dilutions = None
    this_job.suff = ( 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08 )
    this_job.scatName = "lwtr"
    this_job.za = 1001
    njoy_jobs.append(this_job)

  if iso == 'H1':
    this_job = copy.deepcopy(job_ref_nom)
    this_job.hmat = iso + "_" + str(irand).zfill(3)
    this_job.mat = 125
    this_job.evaluationFile = this_job.evaluationDir + "JEFF33-n/" + endf
    this_job.pendfFile = os.getcwd() + "/" + evalName + "/" + endf + "-" + str(irand + 1)
    this_job.za = 1001
    njoy_jobs.append(this_job)

print('Number of jobs from H1 and H1_H2O : '+ str(len(njoy_jobs) - c1))

#############################################################################
#  B10
#############################################################################
c1 = len(njoy_jobs)
job_ref_nom = copy.deepcopy(job_ref)

for irand in range(0,nrand):

  if iso == 'B10':
    this_job = copy.deepcopy(job_ref_nom)
    this_job.hmat = iso + "_" + str(irand).zfill(3)
    this_job.mat = 525
    this_job.evaluationFile = this_job.evaluationDir + "JEFF33-n/" + endf
    this_job.pendfFile = os.getcwd() + "/" + evalName + "/" + endf + "-" + str(irand + 1)
    this_job.za = 5010
    njoy_jobs.append(this_job)

print('Number of jobs from B10 : '+ str(len(njoy_jobs) - c1))

#############################################################################
#  B11
#############################################################################
c1 = len(njoy_jobs)
job_ref_nom = copy.deepcopy(job_ref)

for irand in range(0,nrand):

  if iso == 'B11':
    this_job = copy.deepcopy(job_ref_nom)
    this_job.hmat = iso + "_" + str(irand).zfill(3)
    this_job.mat = 528
    this_job.evaluationFile = this_job.evaluationDir + "JEFF33-n/" + endf
    this_job.pendfFile = os.getcwd() + "/" + evalName + "/" + endf + "-" + str(irand + 1)
    this_job.za = 5011
    njoy_jobs.append(this_job)

print('Number of jobs from B11 : '+ str(len(njoy_jobs) - c1))

#############################################################################
#  O16
#############################################################################
c1 = len(njoy_jobs)
job_ref_nom = copy.deepcopy(job_ref)

for irand in range(0,nrand):

  if iso == 'O16':
    this_job = copy.deepcopy(job_ref_nom)
    this_job.hmat = iso + "_" + str(irand).zfill(3)
    this_job.mat = 825
    this_job.evaluationFile = this_job.evaluationDir + "JEFF33-n/" + endf
    this_job.pendfFile = os.getcwd() + "/" + evalName + "/" + endf + "-" + str(irand + 1)
    this_job.za = 8016
    njoy_jobs.append(this_job)

print('Number of jobs from O16 : '+ str(len(njoy_jobs) - c1))

#############################################################################
#  U235, U238
#############################################################################
c1 = len(njoy_jobs)

for irand in range(0,nrand):

  if iso == 'U235':
    this_job = copy.deepcopy(job_ref)
    this_job.hmat = iso + "_" + str(irand).zfill(3)
    this_job.mat = 9228
    this_job.evaluationFile = this_job.evaluationDir + "JEFF33-n/" + endf
    this_job.pendfFile = os.getcwd() + "/" + evalName + "/" + endf + "-" + str(irand + 1)
    this_job.fission = 2 # fission with delayed neutrons
    this_job.ss = (4.632489, 2.499908e4)
    this_job.potential = 11.6070
    this_job.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
    1259.67004, 750.448669, 447.079956, 266.347961, 158.676849, 94.5317612, 56.3173141, 33.5510521, 19.9880447, \
    11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
    this_job.za = 92235
    njoy_jobs.append(this_job)

  if iso == 'U238':
    this_job = copy.deepcopy(job_ref)
    this_job.hmat = iso + "_" + str(irand).zfill(3)
    this_job.mat = 9237
    this_job.evaluationFile = this_job.evaluationDir + "JEFF33-n/" + endf
    this_job.pendfFile = os.getcwd() + "/" + evalName + "/" + endf + "-" + str(irand + 1)
    this_job.fission = 2 # fission with delayed neutrons
    this_job.ss = (4.632489, 3.206464e5)
    this_job.potential = 11.1710
    this_job.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, 1259.67004, 750.448669, 447.079956, 266.347961, 158.676849, 94.5317612, 56.3173141, 33.5510521, 19.9880447, 11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
    this_job.za = 92238
    njoy_jobs.append(this_job)

print('Number of jobs from U235 and U238 : '+ str(len(njoy_jobs) - c1))

##############################################################################
##  check file locations
##############################################################################
for this_job in njoy_jobs:
  if not os.path.isfile(os.path.expandvars(this_job.evaluationFile)):
    raise PyNjoyError("evaluation file " + this_job.evaluationFile + " not found")
  if this_job.scatteringLaw != None:
    if not os.path.isfile(os.path.expandvars(this_job.scatteringLaw)):
      raise PyNjoyError("scatteringLaw file " + this_job.scatteringLaw + " not found")

##############################################################################
##  Launching computations
##############################################################################
njoy_jobs.pendf()
njoy_jobs.gendf()
njoy_jobs.dendf()
njoy_jobs.acer()
njoy_jobs.dconcat()

# In the current state of DRAGR, calling 'burnup' method is necessary to
# retrieve previously computed Q values (more precisely energy released by any
# reaction ; necessary to compute power distributions) and to insert them into
# Draglib file. Any decay and fission yield file may be used. When no burnup
# steps are performed, the choice of these files has no impact.
this_job = copy.deepcopy(job_ref)
this_job.fissionFile = evalDir + "JEFF33-nfy.asc"
this_job.decayFile   = evalDir + "JEFF33-rdd_all.asc"
this_job.burnup()
