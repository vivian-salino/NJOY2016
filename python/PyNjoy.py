#
#-----------------------------------------------------------------------
#
#Purpose:
# automatic generation of Njoy input data, including dragr data.
# Generation of DRAGLIB and ACELIB.
#
#Copyright:
# Copyright (C) 2003 Ecole Polytechnique de Montreal
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version
#
#Author(s): A. Hebert and R. Karthikeyan
#
#-----------------------------------------------------------------------
#
import os,time
class PyNjoyError(Exception):
    """Exception indicating an error in PyNjoy."""
class PyNjoy:
  def __init__(self):
    self.iwt = 4
    self.legendre = 1
    self.legendregg = 6
    self.scatteringLaw = None
    self.eFiss = None
    self.branchingNG = None
    self.branchingN2N = None
    self.gstr = 0
    self.oldlib = None
    self.purr = None
    self.serpent = None
    self.sgref = 1.0E10
    self.yields = None
    self.iburn = -1
    self.ip1opt = 0
    self.ifprod = 0
    self.jp1 = 0
    self.iverw = 4
    self.draglibFile = None
    self.chainFileName = None
  def pendf(self, eaf=0):
    print " --- make pendf for " + self.hmat + " ---"
    myCwd = os.getcwd()
    myNjoy = myCwd + '/' + self.execDir + "/njoy<file_data"
    if not os.path.isfile(os.path.expandvars(self.evaluationFile)):
      raise PyNjoyError("evaluation file " + self.evaluationFile + " not found")
    if not os.path.isdir(self.evaluationName): os.system('mkdir -p ' + self.evaluationName)
    os.chdir(self.evaluationName)
    textDil=""
    if self.dilutions:
      nbDil = len(self.dilutions)
      for dil in self.dilutions:
        textDil = textDil + " " + "%E"%dil
    else:
      nbDil = 0
    nbTmp = len(self.temperatures)
    textTmp=""
    for tmp in self.temperatures:
      textTmp = textTmp + " " + "%E"%tmp

    matsab_inc = 221
    iform   = 0
    nbAtoms = 1
    elasOpt = 0
    if self.scatteringLaw:
      if self.scatteringMat == 1:    # H1_H20
        nbAtoms = 2
        matsab_inc = 222               # h2o
      elif self.scatteringMat == 7:  # H_ZRH
        nbAtoms = 2
        elasOpt = 1
        matsab_inc = 225               # zrhyd
      elif self.scatteringMat == 11: # H2_D20
        nbAtoms = 2
        matsab_inc = 228               # d2o
      elif self.scatteringMat == 26: # Be
        elasOpt = 1
        matsab_inc = 231               # be
      elif self.scatteringMat == 27: # BeO
        nbAtoms = 2
        elasOpt = 1
        matsab_inc = 233               # beo
      elif self.scatteringMat == 31: # C_GRAPH
        elasOpt = 1
        matsab_inc = 229               # graph
      elif self.scatteringMat == 37: # H_CH2
        nbAtoms = 2
        matsab_inc = 223               # poly
      elif self.scatteringMat == 40: # H_C6H6 (benzine)
        nbAtoms = 2
        matsab_inc = 227               # benz
      elif self.scatteringMat == 58: # Zr_ZRH
        nbAtoms = 2
        elasOpt = 1
        matsab_inc = 235               # zrhyd
      unitLaw = -27
      matLaw = self.scatteringMat
      typeLaw = 2
      os.system("ln -s " + self.scatteringLaw + " tape26")
    else:
      unitLaw = 0
      matLaw = 0
      typeLaw = 1
      nbAtoms = 1
      elasOpt = 0
    # The thermal domain is different according to the energy mesh : it stops
    # at 2.76792 eV for the 99-group structure and goes up to 4 eV for all the
    # other group structures.
    if self.nstr == 35:
      emaxthermal = 2.76792
    else:
      emaxthermal = 4.0
    htime = time.ctime(time.time())
    self.__dict__.update({"textDil": textDil, \
                          "nbDil"  : nbDil,   \
                          "textTmp": textTmp, \
                          "nbTmp"  : nbTmp,   \
                          "unitLaw": unitLaw, \
                          "matLaw" : matLaw,  \
                          "typeLaw": typeLaw, \
                          "iform"  : iform,   \
                          "nbAtoms": nbAtoms, \
                          "elasOpt": elasOpt, \
                          "htime"  : htime,   \
                          "emaxthermal": emaxthermal, \
                          "matsab_inc" : matsab_inc})
    #
    if self.scatteringLaw:
      text_data = """
      moder
      20 -21
      moder
      26 -27
      """
    else:
      text_data = """
      moder
      20 -21
      """
    text_data = text_data + """
    reconr
    -21 -22
    'pendf tape from %(evaluationName)s'/
    %(mat)d 1/
    0.001  0.  0.005/
    '%(hmat)s from %(evaluationName)s at %(htime)s' /
    0/
    broadr
    -21 -22 -23
    %(mat)d %(nbTmp)d/
    0.001/
    %(textTmp)s/
    0/
    """%self.__dict__
    if self.serpent:
      # GASPR and HEATR options from Riku Tuominen's runnjoy_kermas.pl (VTT)
      text_data = text_data + """
      gaspr
      -21 -23 -24/
      """%self.__dict__
      # Mode 2: photon energy is deposited locally
      text_data = text_data + """
      heatr
      -21 -24 -34/
      %(mat)d 8 0 0 1 1/
      302 303 304 318 401 402 443 444/
      moder
      -34 -23
      """%self.__dict__
      # Mode 3 : photons are transported
      text_data = text_data + """
      heatr
      -21 -24 -36/
      %(mat)d 8 0 0 0 1/
      302 303 304 318 401 402 443 444/
      """%self.__dict__
    if self.dilutions and (self.purr or self.serpent):
      text_data = text_data + """
    purr
    -21 -23 -24
    %(mat)d %(nbTmp)d %(nbDil)d 20 32/
    %(textTmp)s/
    %(textDil)s/
    0/
      """%self.__dict__
      if self.serpent:
        text_data = text_data + """
    purr
    -21 -36 -37
    %(mat)d %(nbTmp)d %(nbDil)d 20 32/
    %(textTmp)s/
    %(textDil)s/
    0/
    moder
    -37 66
      """%self.__dict__
    elif self.dilutions:
      text_data = text_data + """
    unresr
    -21 -23 -24
    %(mat)d %(nbTmp)d %(nbDil)d 1/
    %(textTmp)s/
    %(textDil)s/
    0/
      """%self.__dict__
    else:
        if self.serpent:
          text_data = text_data + """
    moder
    -36 66
      """%self.__dict__
    if self.dilutions:
      text_data = text_data + """
    thermr
    0 -24 -35
    0 %(mat)d 16 %(nbTmp)d %(typeLaw)d 0 %(iform)d %(nbAtoms)d 221 0
    %(textTmp)s/
    0.001 %(emaxthermal)d
    moder
    -35 29
    stop
      """%self.__dict__
    else:
     if self.scatteringLaw:
      text_data = text_data + """
    thermr
    %(unitLaw)d -23 -35
    %(matLaw)d %(mat)d 16 %(nbTmp)d %(typeLaw)d %(elasOpt)d %(iform)d %(nbAtoms)d %(matsab_inc)d 0/
    %(textTmp)s/
    0.001 %(emaxthermal)d
    moder
    -35 29
    stop
      """%self.__dict__
     elif eaf == 0:
      text_data = text_data + """
    thermr
    0 -23 -35
    0 %(mat)d 16 %(nbTmp)d %(typeLaw)d 0 %(iform)d %(nbAtoms)d 221 0
    %(textTmp)s/
    0.001 %(emaxthermal)d
    moder
    -35 29
    stop
      """%self.__dict__
     else:
      text_data = text_data + """
    moder
    -23 29
    stop
      """%self.__dict__
    file_data = open("file_data",'w')
    file_data.write(text_data)
    file_data.close()
    os.system("ln -s " + self.evaluationFile + " tape20")
    os.system(myNjoy)
    os.system("mv tape29 pendf" + self.hmat)
    if self.serpent:
      os.system("mv tape66 supkerma" + self.hmat)
    os.system("mv file_data file_data_pendf" + self.hmat)
    os.system("mv output out_pendf_" + self.hmat)
    os.system("chmod 644 out_pendf_" + self.hmat)
    for fileName in os.listdir(os.getcwd()):
      if fileName[:4] == 'tape': os.remove(fileName)
    os.chdir(myCwd)
  #
  def gendf(self, eaf=0):
    print " --- make gendf for " + self.hmat + " ---"
    myCwd = os.getcwd()
    myNjoy = myCwd + '/' + self.execDir + "/njoy<file_data"
    if not os.path.isfile(os.path.expandvars(self.evaluationFile)):
      raise PyNjoyError("evaluation file " + self.evaluationFile + " not found")
    os.chdir(self.evaluationName)
    matsab_inc = 221
    matsab_coh = 0
    if self.scatteringLaw:
      if self.scatteringMat == 1:
        matsab_inc = 222
      elif self.scatteringMat == 7:
        matsab_inc = 225
        matsab_coh = 226
      elif self.scatteringMat == 11:
        matsab_inc = 228
      elif self.scatteringMat == 26:
        matsab_inc = 231
        matsab_coh = 232
      elif self.scatteringMat == 27:
        matsab_inc = 233
        matsab_coh = 234
      elif self.scatteringMat == 31:
        matsab_inc = 229
        matsab_coh = 230
      elif self.scatteringMat == 37:
        matsab_inc = 223
        matsab_coh = 224
      elif self.scatteringMat == 40:
        matsab_inc = 227
      elif self.scatteringMat == 58:
        matsab_inc = 235
        matsab_coh = 236
    if self.iwt:
      newiwt = self.iwt
    else:
      newiwt = 4
    if self.dilutions:
      newiwt = -abs(newiwt)
      nbDil = len(self.dilutions)
      textDil=""
      for dil in self.dilutions:
        textDil = textDil + " " + "%E"%dil
    else:
      newiwt = abs(newiwt)
      nbDil = 1
      textDil="1.0e10"
    nbTmp = len(self.temperatures)
    if nbTmp > 10: raise PyNjoyError("cannot have more than 10 temperatures")
    textTmp=""
    for tmp in self.temperatures:
      textTmp = textTmp + " " + "%E"%tmp
    htime = time.ctime(time.time())
    self.__dict__.update({"textDil": textDil, \
                          "nbDil"  : nbDil,   \
                          "textTmp": textTmp, \
                          "nbTmp"  : nbTmp,   \
                          "htime"  : htime,   \
                          "newiwt" : newiwt,  \
                          "matsab_inc" : matsab_inc, \
                          "matsab_coh" : matsab_coh, \
                          "autosup": self.autolib[1]})
    #
    text_data = """
    moder
    20 -21
    moder
    29 -25
    groupr
    -21 -25 0 -26 0 0
    %(mat)d %(nstr)d %(gstr)d %(newiwt)d %(legendre)d %(nbTmp)d %(nbDil)d 1 0
    '%(hmat)s from %(evaluationName)s at %(htime)s' /
    %(textTmp)s/
    %(textDil)s/
    """%self.__dict__
    if newiwt < 0:
      text_data = text_data + """
      %(autosup)f %(potential)f 20000 / Homog. Flux Calc.Param
      """%self.__dict__
    if newiwt == 1 or newiwt == -1:
      text_data = text_data + self.wght
    elif newiwt == 4 or newiwt == -4:
      text_data = text_data + """
      0.2 0.0253 820.3e3 1.40e6 / iwt=4 parameters
      """
    for tmp in self.temperatures:
      if eaf == 0:
        if matsab_coh != 0:
          text_data = text_data + """
          3/
          3 %(matsab_inc)d /
          3 %(matsab_coh)d /
          """%self.__dict__
        else:
          text_data = text_data + """
          3/
          3 %(matsab_inc)d /
          """%self.__dict__
      else:
        text_data = text_data + """
        3/
        """
      if self.fission:
        text_data = text_data + """
        3 452 /
        """
      if self.fission == 2:
        text_data = text_data + """
        3 455 /
        5 455 /
        """
      if self.gstr != 0:
        text_data = text_data + """
        16 / photon interaction matrices
        """
      if eaf == 0:
        if matsab_coh != 0:
          text_data = text_data + """
          6 /
          6 %(matsab_inc)d /
          6 %(matsab_coh)d /
          0/
          """%self.__dict__
        else:
          text_data = text_data + """
          6 /
          6 %(matsab_inc)d /
          0/
          """%self.__dict__
      else:
        text_data = text_data + """
        6 /
        0/
        """
    text_data = text_data + """
    0/
    moder
    -26 30
    stop
    """
    file_data = open("file_data",'w')
    file_data.write(text_data)
    file_data.close()
    os.system("ln -s " + self.evaluationFile + " tape20")
    os.system("ln -s pendf" + self.hmat + " tape29")
    os.system(myNjoy)
    os.system("mv file_data file_data_gendf" + self.hmat)
    os.system("mv tape30 gendf" + self.hmat)
    os.system("mv output out_gendf_" + self.hmat)
    os.system("chmod 644 out_gendf_" + self.hmat)
    for fileName in os.listdir(os.getcwd()):
      if fileName[:4] == 'tape': os.remove(fileName)
    os.chdir(myCwd)
  #
  def gamma(self):
    print " --- make gamma gendf for " + self.hmatgg + " ---"
    myCwd = os.getcwd()
    myNjoy = myCwd + '/' + self.execDir + "/njoy<file_data"
    if not os.path.isfile(os.path.expandvars(self.evaluationFile)):
      raise PyNjoyError("evaluation file " + self.evaluationFile + " not found")
    if not os.path.isdir(self.evaluationName): os.system('mkdir -p ' + self.evaluationName)
    os.chdir(self.evaluationName)
    htime = time.ctime(time.time())
    self.__dict__.update({"htime"  : htime})
    #
    text_data = """
    moder
    40 -41
    reconr
    -41 -42
    'pendf tape from %(evaluationName)s'/
    %(matgg)d 1/
    0.002 /
    '%(hmatgg)s (gamma) from %(evaluationName)s at %(htime)s' /
    0/
    gaminr
    -41 -42 0 -43
    %(matgg)d %(gstr)d 3 %(legendregg)d 1
    '%(hmatgg)s (gamma) from %(evaluationName)s at %(htime)s' /
    -1 /
    0 /
    moder
    -43 44
    stop
    """%self.__dict__
    file_data = open("file_data",'w')
    file_data.write(text_data)
    file_data.close()
    os.system("ln -s " + self.evaluationFile + " tape40")
    os.system(myNjoy)
    os.system("mv tape44 gamma" + self.hmatgg)
    os.system("mv file_data file_data_gamma" + self.hmatgg)
    os.system("mv output out_gamma_" + self.hmatgg)
    os.system("chmod 644 out_gamma_" + self.hmatgg)
    for fileName in os.listdir(os.getcwd()):
      if fileName[:4] == 'tape': os.remove(fileName)
    os.chdir(myCwd)
  #
  def draglib(self, fp=0):
    myCwd = os.getcwd()
    myNjoy = myCwd + '/' + self.execDir + "/njoy<file_data"
    if not os.path.isfile(os.path.expandvars(self.evaluationFile)):
      raise PyNjoyError("evaluation file " + self.evaluationFile + " not found")
    evaluationNameBase =  os.path.basename(self.evaluationName)
    os.chdir(self.evaluationName)
    if self.oldlib: os.system("cp " + myCwd + "/" + self.oldlib + " draglib" + evaluationNameBase)
    if os.path.isfile("drag"): os.remove("drag")
    if os.path.isfile("draglib" + evaluationNameBase):
      print " file draglib" + evaluationNameBase + " exists"
      iold = 29
      name1 = "draglib" + evaluationNameBase + ".bis.gz"
      if not os.path.isfile(name1):
        os.system("cp draglib" + evaluationNameBase + \
        " draglib" + evaluationNameBase + ".bis")
        os.system("gzip draglib" + evaluationNameBase + ".bis")
      else:
        name2 = "draglib" + evaluationNameBase + ".bis2.gz"
        os.system("cp draglib" + evaluationNameBase + \
        " draglib" + evaluationNameBase + ".bis2")
        os.system("gzip draglib" + evaluationNameBase + ".bis2")
        if os.path.isfile(name2):
          len1 = os.stat(name1).st_size
          len2 = os.stat(name2).st_size
          if len2 > len1:
            os.remove(name1)
            os.rename(name2, name1)
          else:
            os.remove(name2)
      os.system("mv draglib" + evaluationNameBase + " tape29")
      print " append data for " + self.hmat + " to existing draglib file"
    else:
      iold = 0
      print " create a new draglib file for " + self.hmat
    htime = time.ctime(time.time())
    #
    if self.Espectra:
      nbEsp = len(self.Espectra) - 1
      if nbEsp != 4:
        raise PyNjoyError("four energy-dependent fission spectra expected")
    else:
      nbEsp = 0
    #
    if self.dilutions:
      self.__dict__.update({"htime": htime,      \
                            "iold" : iold,       \
                            "ss0"  : self.ss[0], \
                            "ss1"  : self.ss[1], \
                            "auto0": self.autolib[0], \
                            "auto1": self.autolib[1], \
                            "auto2": self.autolib[2], \
                            "fp"   : fp, \
                            "nbEsp": nbEsp})
      if self.Espectra:
        self.__dict__.update({"eesp0" : self.Espectra[0], \
                              "eesp1" : self.Espectra[1], \
                              "eesp2" : self.Espectra[2], \
                              "eesp3" : self.Espectra[3], \
                              "eesp4" : self.Espectra[4]})

      text_data = """
      moder
      20 -21
      moder
      22 -23
      moder
      24 -25
      dragr
      -21 -23 -25 0 0 %(iold)d 30 %(fp)d/
      """%self.__dict__
      if iold == 0:
        text_data = text_data + """
        'draglib from %(evaluationName)s at %(htime)s'/
        """%self.__dict__
      text_data = text_data + """
      %(mat)d %(hmat)s %(nbEsp)d /
      '%(hmat)s from %(evaluationName)s (%(mat)d) at %(htime)s' /
      """%self.__dict__
      if nbEsp > 0:
        text_data = text_data + """
      %(eesp0)E %(eesp1)E %(eesp2)E %(eesp3)E %(eesp4)E /
        """%self.__dict__
      text_data = text_data + """
      %(ss0)E %(ss1)E /
      %(auto0)E %(auto1)E %(auto2)E /
      0/
      stop
      """%self.__dict__
    else:
      self.__dict__.update({"htime": htime, \
                            "iold": iold,   \
                            "fp": fp,       \
                            "nbEsp": nbEsp})
      if self.Espectra:
        self.__dict__.update({"eesp0" : self.Espectra[0], \
                              "eesp1" : self.Espectra[1], \
                              "eesp2" : self.Espectra[2], \
                              "eesp3" : self.Espectra[3], \
                              "eesp4" : self.Espectra[4]})
      text_data = """
      moder
      20 -21
      moder
      24 -25
      dragr
      -21 0 -25 0 0 %(iold)d 30 %(fp)d/
      """%self.__dict__
      if iold == 0:
        text_data = text_data + """
        'draglib from %(evaluationName)s at %(htime)s'/
        """%self.__dict__
      text_data = text_data + """
      %(mat)d %(hmat)s %(nbEsp)d /
      '%(hmat)s from %(evaluationName)s (%(mat)d) at %(htime)s' /
      """%self.__dict__
      if nbEsp > 0:
        text_data = text_data + """
      %(eesp0)E %(eesp1)E %(eesp2)E %(eesp3)E %(eesp4)E /
        """%self.__dict__
      text_data = text_data + """
      0.1 1.0E10 /
      0/
      stop
      """
    file_data = open("file_data",'w')
    file_data.write(text_data)
    file_data.close()
    os.system("ln -s " + self.evaluationFile + " tape20")
    if self.dilutions:
      os.system("ln -s pendf" + self.hmat + " tape22")
    os.system("ln -s gendf" + self.hmat + " tape24")
    os.system(myNjoy)
    os.system("mv file_data file_data_dendf" + self.hmat)
    stats = os.stat("tape30")                                 # get the stats of the file
    size = stats[6]                                           # extract the file size in bytes from the stats list
    if size > 8 :
      os.system("mv tape30 draglib"+evaluationNameBase)
    else:
      os.system("mv tape29 draglib"+evaluationNameBase)
      raise PyNjoyError("draglib file for " + self.hmat + " not created")
    file_in = open('output','r')
    file_out = open("out_draglib_" + self.hmat,'w')
    while 1:
      line=file_in.readline()
      if not line: break
      ind=line.find('???????????')
      if ind != -1:
        if not self.eFiss: raise PyNjoyError("self.eFiss instance variable not set")
        line = line[:ind] + "%E"%self.eFiss + line[ind+11:]
        self.eFiss = None
      if self.branchingNG:
        ind=line.find(' ng ')
        if ind != -1:
          jnd=line[ind+3:].find(' 0.000 ')
          if jnd == -1: raise PyNjoyError("unable to set the isomeric ng branching ratio")
          line = line[:ind+jnd+4] + "%5.3f"%self.branchingNG + line[ind+jnd+9:]
          self.branchingNG = None
      if self.branchingN2N:
        ind=line.find(' n2n ')
        if ind != -1:
          jnd=line[ind+4:].find(' 0.000 ')
          if jnd == -1: raise PyNjoyError("unable to set the isomeric n2n branching ratio")
          line = line[:ind+jnd+5] + "%5.3f"%self.branchingN2N + line[ind+jnd+10:]
          self.branchingN2N = None
      file_out.writelines(line)
    file_out.close()
    file_in.close()
    os.remove('output')
    os.system("chmod 644 out_draglib_" + self.hmat)
    for fileName in os.listdir(os.getcwd()):
      if fileName[:4] == 'temp': os.remove(fileName)
      if fileName[:4] == 'tape': os.remove(fileName)
    os.chdir(myCwd)
  #
  def matxs(self):
    print " --- make matxs for " + self.hmat + " ---"
    myCwd = os.getcwd()
    myNjoy = myCwd + '/' + self.execDir + "/njoy<file_data"
    os.chdir(self.evaluationName)
    listGro = [ 0, 239, 30, 27, 50, 68, 100, 35, 69, 187, 70, 620, 80, 100,
    640, 174, 175, 172, 33, 1968, 315, 172, 175, 281, 349, 89 ]
    listGro2 = [ 0, 94, 12, 21, 22, 48, 24, 36, 38, 42 ]
    htime = time.ctime(time.time())
    self.__dict__.update({"nbGro": listGro[self.nstr - 1], \
                          "htime": htime})
    if self.gstr == 0:
      text_data = """
      moder
      24 -25
      matxsr
      -25 0 28/
      1 '%(hmat)s from %(evaluationName)s (%(mat)d) at %(htime)s'/
      1 2 1 1
      'neutron library'/
      'n'
      %(nbGro)d
      'nscat' 'ntherm'/
      1 1
      1 1
      %(hmat)s %(mat)d /
      stop
      """%self.__dict__
    else:
      self.__dict__.update({"nbGro2": listGro2[self.gstr - 1]})
      text_data = """
      moder
      24 -25
      moder
      26 -27
      matxsr
      -25 -27 28/
      1 '%(hmat)s coupled-set from %(evaluationName)s (%(mat)d+%(matgg)d) at %(htime)s'/
      2 3 1 1
      'neutron-gamma library'/
      'n' 'g'
      %(nbGro)d %(nbGro2)d
      'nscat' 'ng' 'gscat' 'ntherm'/
      1 1 2 1
      1 2 2 1
      %(hmat)s %(mat)d %(matgg)d/
      stop
      """%self.__dict__
      os.system("ln -s gamma" + self.hmatgg + " tape26")
    file_data = open("file_data",'w')
    file_data.write(text_data)
    file_data.close()
    os.system("ln -s gendf" + self.hmat + " tape24")
    os.system(myNjoy)
    os.system("mv file_data file_data_matxs" + self.hmat)
    os.system("mv tape28 matxs" + self.hmat)
    os.system("mv output out_matxs_" + self.hmat)
    os.system("chmod 644 out_matxs_" + self.hmat)
    for fileName in os.listdir(os.getcwd()):
      if fileName[:4] == 'tape': os.remove(fileName)
    os.chdir(myCwd)
#
  def wims(self):
    print " --- make wimslib for " + self.hmat + " ---"
    myCwd = os.getcwd()
    myNjoy = myCwd + '/' + self.execDir + "/njoy<file_data"
    os.chdir(self.evaluationName)
    nbTmp = len(self.temperatures)
    matsab_inc = 221
    matsab_coh = 0
    if self.scatteringLaw:
      if self.scatteringMat == 1:
        matsab_inc = 222
      elif self.scatteringMat == 7:
        matsab_inc = 225
        matsab_coh = 226
      elif self.scatteringMat == 11:
        matsab_inc = 228
      elif self.scatteringMat == 26:
        matsab_inc = 231
        matsab_coh = 232
      elif self.scatteringMat == 27:
        matsab_inc = 233
        matsab_coh = 234
      elif self.scatteringMat == 31:
        matsab_inc = 229
        matsab_coh = 230
      elif self.scatteringMat == 37:
        matsab_inc = 223
        matsab_coh = 224
      elif self.scatteringMat == 40:
        matsab_inc = 227
      elif self.scatteringMat == 58:
        matsab_inc = 235
        matsab_coh = 236
    if self.yields:
        iburn = 1
    else:
        iburn = 0
    if self.legendre == 0:
        ip1opt = 1
    else:
        ip1opt = 0
    self.__dict__.update({"nbTmp"  : nbTmp,          \
                          "matsab_inc" : matsab_inc, \
                          "matsab_coh" : matsab_coh, \
                          "iburn" : iburn, \
                          "ip1opt" : ip1opt, })
    text_data = """
    moder
    24 -25
    wimsr
    -25 28
    2  %(iverw)d  9
    """%self.__dict__
    if self.nstr == 9:
       nrg = 13
       text_data = text_data + """
    69 14 13 10
       """
    elif self.nstr == 18:
       nrg = 47
       text_data = text_data + """
    172 45 47 34
       """
    elif self.nstr == 22:
       nrg = 47
       text_data = text_data + """
    172 45 47 34
       """
    elif self.nstr == 29:
       nrg = 57
       text_data = text_data + """
    281 36 57 36
       """
    else:
       raise PyNjoyError("group structure " + self.nstr + " not implemented")
    if self.iverw == 4:
       if self.dilutions:
          text_data = text_data + """
          %(mat)d 0 %(wmat)f %(iburn)d
          0 0 %(sgref)f %(nbTmp)d %(potential)f %(matsab_inc)d %(matsab_coh)d 1 0 0 %(ifprod)d %(jp1)d/
          """%self.__dict__
       else:
          text_data = text_data + """
          %(mat)d 0 %(wmat)f %(iburn)d
          0 0 %(sgref)f 0 0.0 %(matsab_inc)d %(matsab_coh)d 1 0 0 %(ifprod)d %(jp1)d/
          """%self.__dict__
    elif self.iverw == 5 and self.fission:
       if self.dilutions:
          text_data = text_data + """
          %(mat)d 0 %(wmat)f %(iburn)d
          0 0 %(sgref)f %(nbTmp)d %(potential)f %(matsab_inc)d %(matsab_coh)d %(ip1opt)d 0 1 %(ifprod)d %(jp1)d/
          """%self.__dict__
       else:
          text_data = text_data + """
          %(mat)d 0 %(wmat)f %(iburn)d
          0 0 %(sgref)f 0 0.0 %(matsab_inc)d %(matsab_coh)d %(ip1opt)d 0 1 %(ifprod)d %(jp1)d/
          """%self.__dict__
    elif self.iverw == 5:
       if self.dilutions:
          text_data = text_data + """
          %(mat)d 0 %(wmat)f %(iburn)d
          0 0 %(sgref)f %(nbTmp)d %(potential)f %(matsab_inc)d %(matsab_coh)d %(ip1opt)d 0 0 %(ifprod)d %(jp1)d/
          """%self.__dict__
       else:
          text_data = text_data + """
          %(mat)d 0 %(wmat)f %(iburn)d
          0 0 %(sgref)f 0 0.0 %(matsab_inc)d %(matsab_coh)d %(ip1opt)d 0 0 %(ifprod)d %(jp1)d/
          """%self.__dict__
    else:
       raise PyNjoyError("invalid value of iverw (4 or 5 expected).")

    if self.iburn == 1:
       text_data = text_data + self.yields
    for i in range(nrg):
       text_data = text_data + " %(goldstein)f"%self.__dict__
    if self.jp1 > 0:
       text_data = text_data + self.p1flx

    text_data = text_data + """
    stop
    """
    file_data = open("file_data",'w')
    file_data.write(text_data)
    file_data.close()
    os.system("ln -s gendf" + self.hmat + " tape24")
    os.system(myNjoy)
    os.system("mv file_data file_data_wims" + self.hmat)
    os.system("mv tape28 wimslib" + self.hmat)
    os.system("mv output out_wims_" + self.hmat)
    os.system("chmod 644 out_wims_" + self.hmat)
    for fileName in os.listdir(os.getcwd()):
      if fileName[:4] == 'tape': os.remove(fileName)
    os.chdir(myCwd)
#
  def makeFp(self, eaf=0):
    self.scatteringLaw = None
    self.fission = None
    self.Espectra = None
    self.dilutions = None
    keeplegendre = self.legendre
    self.legendre = 0
    self.pendf(eaf)
    self.gendf(eaf)
    self.draglib(fp=1)
    self.legendre = keeplegendre
#
  def burnup(self):
    myCwd = os.getcwd()
    myNjoy = myCwd + '/' + self.execDir + "/njoy<tempFile"
    evaluationNameBase =  os.path.basename(self.evaluationName)
    if not os.path.isdir(self.evaluationName): os.system('mkdir -p ' + self.evaluationName)
    os.chdir(self.evaluationName)
    if os.path.isfile("drag"): os.remove("drag")
    if os.path.isfile("draglib" + evaluationNameBase):
      iold = 29
      name1 = "draglib" + evaluationNameBase + ".bis.gz"
      if not os.path.isfile(name1):
        os.system("cp draglib" + evaluationNameBase + \
        " draglib" + evaluationNameBase + ".bis")
        os.system("gzip draglib" + evaluationNameBase + ".bis")
      else:
        name2 = "draglib" + evaluationNameBase + ".bis2.gz"
        os.system("cp draglib" + evaluationNameBase + \
        " draglib" + evaluationNameBase + ".bis2")
        os.system("gzip draglib" + evaluationNameBase + ".bis2")
        if os.path.isfile(name2):
          len1 = os.stat(name1).st_size
          len2 = os.stat(name2).st_size
          if len2 > len1:
            os.remove(name1)
            os.rename(name2, name1)
          else:
            os.remove(name2)
      os.system("mv draglib" + evaluationNameBase + " tape29")
    elif self.draglibFile:
      iold = 29
      os.system("ln -s " + self.draglibFile + " tape29")
    else:
      iold = 0
    htime = time.ctime(time.time())
    self.__dict__.update({"iold" : iold , "htime" : htime})
    text_data = """
    dragr
    0 0 0 23 24 %(iold)d 30/
"""%self.__dict__
    if iold == 0:
      text_data = text_data + """
      'draglib from %(evaluationName)s at %(htime)s'/
"""%self.__dict__
    file_data = open("file_data",'w')
    file_data.write(text_data)
    file_data.close()
    yield_file = os.path.expandvars(self.fissionFile)
    if os.path.isdir(yield_file):
      tape23 = open("tape23",'w')
      tape23.write("LIBRARY, DUMMY TAPE HEADER\n")
      for fileName in os.listdir(yield_file):
        if fileName.endswith(".endf"):
          file = open(yield_file + fileName,'r')
          lines = file.readlines()
          n = len(lines)
          tape23.writelines(lines[1:n-1])
          file.close()
      tape23.writelines(lines[n-1:n])
      tape23.close()
    else:
      os.system("ln -s " + self.fissionFile + " tape23")
    decay_file = os.path.expandvars(self.decayFile)
    if os.path.isdir(decay_file):
      tape24 = open("tape24",'w')
      tape24.write("LIBRARY, DUMMY TAPE HEADER\n")
      for fileName in os.listdir(decay_file):
        if fileName.endswith(".endf"):
          file = open(decay_file + fileName,'r')
          lines = file.readlines()
          n = len(lines)
          tape24.writelines(lines[1:n-1])
          file.close()
      tape24.writelines(lines[n-1:n])
      tape24.close()
    else:
      os.system("ln -s " + self.decayFile + " tape24")
    chainFileName = 'chain' + evaluationNameBase
    listFiles = os.listdir(os.getcwd())
    if self.chainFileName:
      print 'User-defined burnup chain file named',chainFileName
      os.system("ln -s " + self.chainFileName + chainFileName)
    elif chainFileName not in listFiles:
      print 'Make the burnup chain file named',chainFileName
      data_dict = {}
      mat_dict = {}
      for fileName in listFiles:
        if fileName[:11] == 'out_draglib':
          file = open(fileName,'r')
          while 1:
            line=file.readline()
            if not line: break
            if line[:41] == ' isotopic specification line for material':
              mat = line[42:49]
              line=file.readline()
              line=file.readline()
              info = ''
              key = line.split()[0]
              while line[:6] != ' -----':
                info = info + line
                line=file.readline()
              data_dict[key] = info[:-1]
              mat_dict[key] = 10*int(mat)
              if key.find('_') != -1: mat_dict[key] = mat_dict[key] - 1
      dictKeys = data_dict.keys()
      dictKeys.sort(lambda a, b: mat_dict[a]-mat_dict[b])
      chainFile = open(chainFileName,'w')
      for key in dictKeys:
        line = data_dict[key]
        pos = 0
        while pos != -1:
          pos = line.find('\n')
          chainFile.write(line[:pos-1] + '\n')
          line = line[pos+1:]
      chainFile.write("end /\n")
      chainFile.write("stop\n")
      chainFile.close()
    else:
      print 'Use existing burnup chain file named',chainFileName
    os.system("cat file_data " + chainFileName + " > tempFile")
    os.system(myNjoy)
    os.system("mv tape30 draglib" + evaluationNameBase)
    os.system("mv output out_draglib_burnup")
    os.system("chmod 644 out_draglib_burnup")
    os.system("mv tempFile file_data_burnup")
    for fileName in os.listdir(os.getcwd()):
      if fileName[:4] == 'temp': os.remove(fileName)
      if fileName[:4] == 'tape': os.remove(fileName)
    os.remove("file_data")
    os.chdir(myCwd)
#
  def acer(self):
    myCwd = os.getcwd()
    myNjoy = myCwd + '/' + self.execDir + "/njoy<file_data"
    if not os.path.isfile(os.path.expandvars(self.evaluationFile)):
      raise PyNjoyError("evaluation file " + self.evaluationFile + " not found")
    os.chdir(self.evaluationName)
    os.system("ln -s " + self.evaluationFile + " tape20")
    os.system("ln -s pendf" + self.hmat + " tape29")
    for itmp, tmp in enumerate(self.temperatures):
      textTmp = "%E"%tmp
      self.__dict__.update({"textSuff": "%f"%self.suff[itmp], \
                            "textTmp": textTmp})
      text_data = """
      moder
      20 -21
      moder
      29 -25
      acer
      -21 -25 0 38 39
      1 0 1 %(textSuff)s/
      'pendf tape from %(evaluationName)s'/
      %(mat)d  %(textTmp)s /
      1 1/
      0.001/
      acer / Check ACE files
      0 38 0 40 41
      7 1 1 -1/
      /
      """%self.__dict__
      if self.scatteringLaw:
        matsab_inc = 221
        matsab_coh = 0
        MixedAtoms = 1
        elasOpt = 0
        if self.scatteringMat == 1:    # H1_H20
          matsab_inc = 222               # h2o
        elif self.scatteringMat == 7:  # H_ZRH
          elasOpt = 1
          matsab_inc = 225               # zrhyd
          matsab_coh = 226               # zrhyd$
        elif self.scatteringMat == 11: # H2_D20
          matsab_inc = 228               # d2o
        elif self.scatteringMat == 26: # Be
          elasOpt = 1
          matsab_inc = 231               # be
          matsab_coh = 232               # be$
        elif self.scatteringMat == 27: # BeO
          MixedAtoms = 2
          elasOpt = 1
          matsab_inc = 233               # beo
          matsab_inc = 234               # beo$
        elif self.scatteringMat == 31: # C_GRAPH
          elasOpt = 1
          matsab_inc = 229               # graph
          matsab_inc = 230               # graph$
        elif self.scatteringMat == 37: # H_CH2
          matsab_inc = 223               # poly
        elif self.scatteringMat == 40: # H_C6H6 (benzine)
          MixedAtoms = 2
          matsab_inc = 227               # benz
        elif self.scatteringMat == 58: # Zr_ZRH
          elasOpt = 1
          matsab_inc = 235               # zrhyd
          matsab_inc = 236               # zrhyd$
        self.__dict__.update({"MixedAtoms": MixedAtoms})
        text_data = text_data + """
      acer
      -21 -25 0 48 49
      2 0 1 %(textSuff)s/
      'pendf tape from %(evaluationName)s'/
      %(mat)d  %(textTmp)s %(scatName)s/
      %(za)d 0 0 /
      %(matsab_inc)d 16 %(matsab_coh)d %(elasOpt)d %(MixedAtoms)d 4.0 0 0 /
      acer / Check ACE files
      0 48 0 50 51
      7 1 1 -1/
      /
      stop
      """%self.__dict__
      else:
        text_data = text_data + """
      stop
      """
      file_data = open("file_data",'w')
      file_data.write(text_data)
      file_data.close()
      os.system(myNjoy)
      if self.scatteringLaw:
       scatsuffix = self.scatName
      else:
       scatsuffix = ''
      filename = str(self.za) + scatsuffix + "_" + "%.0f"%tmp + "K"
      if os.path.isfile("tape48"):
       acefilename = filename + "_tsl.ace"
       xsdirfilename = filename + "_tsl.xsdir"
       os.system("mv tape48 " + acefilename)
       os.system("mv tape49 " + xsdirfilename)
      if os.path.isfile("tape38"):
       acefilename = filename + ".ace"
       xsdirfilename = filename + ".xsdir"
       os.system("mv tape38 " + acefilename)
       os.system("mv tape39 " + xsdirfilename)
       print "ACE file for " + self.hmat + " created"
       # Add KERMA and URES data for SERPENT energy deposition model
       os.system(myCwd + "/Add_KERMA_URES_Serpent.sh "
                 + acefilename + " "
                 + self.evaluationFile + " "
                 + "pendf" + self.hmat + " "
                 + "supkerma" + self.hmat)
      else:
       raise PyNjoyError("ACE or TSL file for " + self.hmat + " not created")
  #
      os.system("mv file_data file_data_ace" + self.hmat + "_" + "%.0f"%tmp + "K")
      os.system("mv output out_ace_" + self.hmat + "_" + "%.0f"%tmp + "K")
      os.system("chmod 644 out_ace_" + self.hmat + "_" + "%.0f"%tmp + "K")
    for fileName in os.listdir(os.getcwd()):
      if fileName[:4] == 'tape': os.remove(fileName)
    os.chdir(myCwd)
