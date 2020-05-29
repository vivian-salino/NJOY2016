#!/usr/bin/env python
from PyNjoy_mp import *
import os, sys, time, copy, getpass
from collections import OrderedDict
# Parallel distribution of the library generation
# Based on jeff3p1p1.py (172g)
# Vivian SALINO - IRSN - 05/2020
# Based on work from Richard Chambon (Ecole Polytechnique) and Javier Ortensi (Idaho National laboratory)

tim0=time.time()
print('___TIMER___: BEGIN @' + str(tim0))
print('___TIMER___: BEGIN = 0      s')
#############################################################################
#  file locations & options
#############################################################################
#iso = 'O16'
iso = str(sys.argv[2])
# folder name where computations are performed
evalName   = "/SCRATCH/ORION/" + getpass.getuser() + "/Njoy/TENDL/" + iso
if not os.path.isdir(evalName): os.mkdir(evalName)
# relative path to the 'njoy' code from current folder
execDir    = ".."
# evaluation folder paths
evalDir    = os.path.expanduser('~') + "/work/evaluations/TENDL-2019/" + iso + "/"
scatLawDir = os.path.expanduser('~') + "/work/evaluations/TENDL-2019/" + iso + "/"

njoy_jobs = VectPyNjoy()
njoy_jobs.setncpu(ncpu=int(sys.argv[1]))

nrand = 300
#nrand = 2

if iso == 'Zr91' and nrand > 180:
  nrand = 180
elif iso == 'Zr92' and nrand > 200:
  nrand = 200
elif iso == 'Zr94' and nrand > 200:
  nrand = 200
elif iso == 'Zr96' and nrand > 250:
  nrand = 200

job_ref = lib_base(evalName, execDir, evalDir, scatLawDir)
job_ref.nstr = 22
job_ref.eaf = 0
job_ref.fission = None
job_ref.dilutions = None
job_ref.Espectra = None
job_ref.autolib = (2.76792, 677.2873, 0.00125)
job_ref.fp = 0
job_ref.concat = 1

# The following GROUPR bugs are regularly encountered with
# U238 if too many temperatures and dilutions are used :
# With 19 dilutions and more than 1 temperature, following GROUPR bug is encountered with U238 :
# (1) https://github.com/njoy/NJOY2016/issues/161
# (2) segfault in line (according to gdb)
# xhi=sedist(khi+4+2*nrhi+2*nphi)
#job_ref.temperatures = ( 293., 550., 900., 1200., 2000. )
#job_ref.suff = ( 0.02, 0.05, 0.09, 0.12, 0.20 )
job_ref.temperatures = ( 293., 550., 900. )
job_ref.suff = ( 0.02, 0.05, 0.09 )
# If only 1 temperature is fitted, following value shall be used for Tihange start-up before previous bugs are corrected (at the very least)
#job_ref.temperatures = [ 559.26111 ]
#job_ref.suff = [ 0.05 ]

tim = time.time()
print('___TIMER___: General options done =' + str(tim - tim0) + ' s')
#############################################################################
#  O16
#############################################################################
c1 = len(njoy_jobs)
job_ref_nom = copy.deepcopy(job_ref)

for irand in range(0,nrand):

  if iso == 'O16':
    this_job = copy.deepcopy(job_ref_nom)
    this_job.hmat = "O16" + "_" + str(irand).zfill(3)
    this_job.mat = 825
    this_job.evaluationFile = this_job.evaluationDir + "n-O016-rand-" + str(irand).zfill(4)
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
    this_job.hmat = "U235" + "_" + str(irand).zfill(3)
    this_job.mat = 9228
    this_job.evaluationFile = this_job.evaluationDir + "U235-n_rand_" + str(irand).zfill(4)
    this_job.fission = 2 # fission with delayed neutrons
    this_job.ss = (2.76792, 2.47875e4)
    this_job.potential = 11.6070
    this_job.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, \
    1259.67004, 750.448669, 447.079956, 266.347961, 158.676849, 94.5317612, 56.3173141, 33.5510521, 19.9880447, \
    11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
    this_job.za = 92235
    njoy_jobs.append(this_job)

  if iso == 'U238':
    this_job = copy.deepcopy(job_ref)
    this_job.hmat = "U238" + "_" + str(irand).zfill(3)
    this_job.mat = 9237
    this_job.evaluationFile = this_job.evaluationDir + "U238-n_rand_" + str(irand).zfill(4)
    this_job.fission = 2 # fission with delayed neutrons
    this_job.ss = (2.76792, 1.22773e5)
    this_job.potential = 11.1710
    this_job.dilutions = ( 1.e10, 10000.0, 5957.50244, 3549.18335, 2114.42676, 1259.67004, 750.448669, 447.079956, 266.347961, 158.676849, 94.5317612, 56.3173141, 33.5510521, 19.9880447, 11.9078817, 7.09412289, 4.22632504, 2.51783395, 1.5 )
    this_job.za = 92238
    njoy_jobs.append(this_job)

print('Number of jobs from U235 and U238 : '+ str(len(njoy_jobs) - c1))

#############################################################################
#  Zr
#############################################################################
c1 = len(njoy_jobs)
job_ref_fp = copy.deepcopy(job_ref)
job_ref_fp.legendre = 0
job_ref_fp.fp = 1
job_ref_fp.ss = (2.76792, 1.66156e4)

for irand in range(0,nrand):

  if iso == 'Zr90':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Zr90" + "_" + str(irand).zfill(3)
    this_job.mat = 4025
    this_job.evaluationFile = this_job.evaluationDir + "n-Zr090-rand-" + str(irand).zfill(4)
    this_job.potential = 6.5144
    this_job.dilutions = ( 1.e10, 10000.0,  3866.97, 1495.35, 578.2475, 223.6068, 86.4682, 33.4370, 12.9300, 5.0 )
    this_job.za = 40090
    njoy_jobs.append(this_job)

  if iso == 'Zr91':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Zr91" + "_" + str(irand).zfill(3)
    this_job.mat = 4028
    this_job.evaluationFile = this_job.evaluationDir + "n-Zr091-rand-" + str(irand).zfill(4)
    this_job.potential = 6.5144
    this_job.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
    this_job.za = 40091
    njoy_jobs.append(this_job)

  if iso == 'Zr92':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Zr92" + "_" + str(irand).zfill(3)
    this_job.mat = 4031
    this_job.evaluationFile = this_job.evaluationDir + "n-Zr092-rand-" + str(irand).zfill(4)
    this_job.potential = 6.5144
    this_job.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
    this_job.za = 40092
    njoy_jobs.append(this_job)

  if iso == 'Zr94':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Zr94" + "_" + str(irand).zfill(3)
    this_job.mat = 4037
    this_job.evaluationFile = this_job.evaluationDir + "n-Zr094-rand-" + str(irand).zfill(4)
    this_job.potential = 6.5144
    this_job.dilutions = dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
    this_job.za = 40094
    njoy_jobs.append(this_job)

  if iso == 'Zr96':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Zr96" + "_" + str(irand).zfill(3)
    this_job.mat = 4043
    this_job.evaluationFile = this_job.evaluationDir + "n-Zr096-rand-" + str(irand).zfill(4)
    this_job.potential = 6.5144
    this_job.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
    this_job.za = 40096
    njoy_jobs.append(this_job)

print('Number of jobs from Zr isotopes : '+ str(len(njoy_jobs) - c1))

tim1 = time.time()
print('___TIMER___: All jobs definition done =' + str(tim1 - tim) + ' s')
tim = tim1

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

# As it is, calling 'burnup' method is necessary to retrieve previously computed Q values (more precisely energy released by any reaction ; necessary to compute power distributions) and to insert them into Draglib file. Any decay and fission yield file may be used. When no burnup steps are performed, the choice of these files has no impact.
this_job = copy.deepcopy(job_ref)
this_job.fissionFile = os.path.expanduser('~') + "/work/evaluations/Jeff3.1.1/JEFF311NFY.ASC"
this_job.decayFile   = os.path.expanduser('~') + "/work/evaluations/Jeff3.1.1/JEFF311RDD_ALL.OUT"
this_job.burnup()
