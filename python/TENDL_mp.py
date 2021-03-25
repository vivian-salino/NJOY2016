#!/usr/bin/env python
from PyNjoy_mp import *
import os, sys, copy, glob
from collections import OrderedDict
# Parallel distribution of the library generation
# Based on jeff3p3_shem295.py
# Vivian SALINO - IRSN - 05/2020
# Based on work from Richard Chambon (Ecole Polytechnique) and Javier Ortensi (Idaho National laboratory)

#############################################################################
#  file locations & options
#############################################################################
iso = str(sys.argv[2])
# folder name where computations are performed
evalName   = "../output/TENDL-2019/" + iso
if not os.path.isdir(evalName): os.mkdir(evalName)
# relative path to the 'njoy' code from current folder
execDir    = ".."
# evaluation folder paths
evalDir    = os.getcwd() + "/../../ENDF/TENDL-2019/" + iso + "/"
scatLawDir = os.getcwd() + "/../../ENDF/TENDL-2019/" + iso + "/"

njoy_jobs = VectPyNjoy()
njoy_jobs.setncpu(ncpu=int(sys.argv[1]))

# Count the available ENDF files, capping at 300 samples
nrand = len(glob.glob(evalDir + '/*')) - 1
if nrand > 300:
    nrand = 300

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

#############################################################################
#  Zr
#############################################################################
c1 = len(njoy_jobs)
job_ref_fp = copy.deepcopy(job_ref)
job_ref_fp.legendre = 0
job_ref_fp.fp = 1
job_ref_fp.ss = (4.632489, 1.858471e4)

for irand in range(0,nrand):

  if iso == 'Zr90':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Zr90" + "_" + str(irand).zfill(3)
    this_job.mat = 4025
    this_job.evaluationFile = this_job.evaluationDir + "n-Zr090-rand-" + str(irand).zfill(4)
    this_job.potential = 6.5144
    this_job.dilutions = ( 1.e10, 10000.0,  3866.97, 1495.35, 578.2475, 223.6068, 86.4682, 33.4370, 12.9300, 5.0 )
    this_job.za = 40090
    this_job.autolib = (4.632489, 3.481068e3, 0.0005)
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
    this_job.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
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

#############################################################################
#  AIC
#############################################################################
c1 = len(njoy_jobs)
job_ref_fp = copy.deepcopy(job_ref)
job_ref_fp.legendre = 0
job_ref_fp.fp = 1
job_ref_fp.ss = (4.632489, 1.858471e4)

for irand in range(0,nrand):

  if iso == 'Ag107':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Ag107" + "_" + str(irand).zfill(3)
    this_job.mat = 4725
    this_job.evaluationFile = this_job.evaluationDir + "n-Ag107-rand-" + str(irand).zfill(4)
    this_job.potential = 5.4739
    this_job.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
    this_job.za = 47107
    njoy_jobs.append(this_job)

  if iso == 'Ag109':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Ag109" + "_" + str(irand).zfill(3)
    this_job.mat = 4731
    this_job.evaluationFile = this_job.evaluationDir + "n-Ag109-rand-" + str(irand).zfill(4)
    this_job.potential = 5.3316
    this_job.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
    this_job.za = 47109
    njoy_jobs.append(this_job)

  if iso == 'In115':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "In115" + "_" + str(irand).zfill(3)
    this_job.mat = 4931
    this_job.evaluationFile = this_job.evaluationDir + "n-In115-rand-" + str(irand).zfill(4)
    this_job.potential = 5.0695
    this_job.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
    this_job.za = 49115
    njoy_jobs.append(this_job)

  if iso == 'Cd106':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Cd106" + "_" + str(irand).zfill(3)
    this_job.mat = 4825
    this_job.evaluationFile = this_job.evaluationDir + "n-Cd106-rand-" + str(irand).zfill(4)
    this_job.za = 48106
    njoy_jobs.append(this_job)

  if iso == 'Cd108':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Cd108" + "_" + str(irand).zfill(3)
    this_job.mat = 4831
    this_job.evaluationFile = this_job.evaluationDir + "n-Cd108-rand-" + str(irand).zfill(4)
    this_job.za = 48108
    njoy_jobs.append(this_job)

  if iso == 'Cd110':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Cd110" + "_" + str(irand).zfill(3)
    this_job.mat = 4837
    this_job.evaluationFile = this_job.evaluationDir + "n-Cd110-rand-" + str(irand).zfill(4)
    this_job.potential = 5.1762
    this_job.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
    this_job.za = 48110
    njoy_jobs.append(this_job)

  if iso == 'Cd111':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Cd111" + "_" + str(irand).zfill(3)
    this_job.mat = 4840
    this_job.evaluationFile = this_job.evaluationDir + "n-Cd111-rand-" + str(irand).zfill(4)
    this_job.za = 48111
    njoy_jobs.append(this_job)

  if iso == 'Cd112':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Cd112" + "_" + str(irand).zfill(3)
    this_job.mat = 4843
    this_job.evaluationFile = this_job.evaluationDir + "n-Cd112-rand-" + str(irand).zfill(4)
    this_job.za = 48112
    njoy_jobs.append(this_job)

  if iso == 'Cd113':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Cd113" + "_" + str(irand).zfill(3)
    this_job.mat = 4846
    this_job.evaluationFile = this_job.evaluationDir + "n-Cd113-rand-" + str(irand).zfill(4)
    this_job.za = 48113
    njoy_jobs.append(this_job)

  if iso == 'Cd114':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Cd114" + "_" + str(irand).zfill(3)
    this_job.mat = 4849
    this_job.evaluationFile = this_job.evaluationDir + "n-Cd114-rand-" + str(irand).zfill(4)
    this_job.za = 48114
    njoy_jobs.append(this_job)

  if iso == 'Cd116':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Cd116" + "_" + str(irand).zfill(3)
    this_job.mat = 4855
    this_job.evaluationFile = this_job.evaluationDir + "n-Cd116-rand-" + str(irand).zfill(4)
    this_job.za = 48116
    njoy_jobs.append(this_job)

print('Number of jobs from AIC isotopes : '+ str(len(njoy_jobs) - c1))

#############################################################################
#  Stainless steel
#############################################################################
c1 = len(njoy_jobs)
job_ref_fp = copy.deepcopy(job_ref)
job_ref_fp.legendre = 0
job_ref_fp.fp = 1
job_ref_fp.ss = (4.632489, 1.858471e4)

for irand in range(0,nrand):

  if iso == 'Cr52':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Cr52" + "_" + str(irand).zfill(3)
    this_job.mat = 2431
    this_job.evaluationFile = this_job.evaluationDir + "n-Cr052-rand-" + str(irand).zfill(4)
    this_job.potential = 4.1677
    this_job.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
    this_job.za = 24052
    njoy_jobs.append(this_job)

  if iso == 'Fe54':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Fe54" + "_" + str(irand).zfill(3)
    this_job.mat = 2625
    this_job.evaluationFile = this_job.evaluationDir + "n-Fe054-rand-" + str(irand).zfill(4)
    this_job.za = 26054
    njoy_jobs.append(this_job)

  if iso == 'Fe56':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Fe56" + "_" + str(irand).zfill(3)
    this_job.mat = 2631
    this_job.evaluationFile = this_job.evaluationDir + "n-Fe056-rand-" + str(irand).zfill(4)
    this_job.potential = 3.7243
    this_job.dilutions = ( 1.e10, 10000.0, 3546.31, 1257.43, 445.8898, 158.1139, 56.0677, 19.8818, 7.0501, 2.5 )
    this_job.za = 26056
    njoy_jobs.append(this_job)

  if iso == 'Ni58':
    this_job = copy.deepcopy(job_ref_fp)
    this_job.hmat = "Ni58" + "_" + str(irand).zfill(3)
    this_job.mat = 2825
    this_job.evaluationFile = this_job.evaluationDir + "n-Ni058-rand-" + str(irand).zfill(4)
    this_job.za = 28058
    njoy_jobs.append(this_job)

print('Number of jobs from SS isotopes : '+ str(len(njoy_jobs) - c1))

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
this_job.fissionFile = evalDir + "../../JEFF-3.3/JEFF33-nfy.asc"
this_job.decayFile   = evalDir + "../../JEFF-3.3/JEFF33-rdd_all.asc"
this_job.burnup()
