# PyNjoy2016

PyNjoy2016 combines
+  a friendly fork of NJOY2016 (see https://github.com/njoy/NJOY2016), and
+  a Python automation tool designed to streamline production with NJOY2016 and derived from PyNjoy2012 (see https://www.polymtl.ca/merlin/pynjoy2012.htm).

## Differences with NJOY2016
The main difference with NJOY2016 is the inclusion of the DRAGR module. It is used to produce libraries in Draglib format from NJOY2016 computations.

This project is not intended to last, but to merge into NJOY2016 main branch. We believe it's the right thing to do because - as stated in the introduction of [NJOY manual](https://github.com/njoy/NJOY2016-manual/raw/master/njoy16.pdf) - the mission of NJOY is
"to take the basic data from the nuclear data library and convert it into the forms needed for applications."

Only Python automation scripts are vowed to persist as a separate (user) project. Also, it is understandable that some small changes cannot be merged into the main branch, such as the few (16) lines useful to process famous but incorrect nuclear data files (in moder, reconr, unresr and purr).

## Obtaining, compiling and using PyNjoy2016

To retrieve and compile PyNjoy2016, execute the following commands:
```
git clone https://github.com/IRSN/PyNjoy2016.git
cd PyNjoy2016
cmake .
make
cd tests
make test
```

All tests should pass. Draglib and eventually ACE files can then be produced with:
```
cd ../python
python jeff3p3_shem295.py
```

You will probably need to modify the following variables in this Python script:
* `evaluationDir`, pointing toward your directory containing the ENDF files,
* `evaluationName`, indicating your output directory.

Other nuclear data evaluations and other energy meshes can be processed using the Python scripts that can be found in the fourth table on [this page](https://www.polymtl.ca/merlin/libraries.htm).

## Documentation
The documentation for NJOY2016 is found in the [NJOY2016-manual](https://github.com/njoy/NJOY2016-manual) repository. There, you can find a [pre-compiled PDF](https://github.com/njoy/NJOY2016-manual/raw/master/njoy16.pdf) of the NJOY2016 manual.

The LaTeX documentation for DRAGR, RESKR and ELECTR modules is found [here](http://www.polymtl.ca/merlin/downloads/arch_njoy2012_epm_mp_up137.tgz). There, you can find a [pre-compiled PDF](http://www.polymtl.ca/merlin/downloads/njoy12_rev.pdf) of this manual.

A tutorial for PyNjoy automation scripts is found [here](http://www.polymtl.ca/merlin/downloads/IGE361.pdf).

## License and Copyright
This software is distributed and copyrighted according to the [LICENSE](LICENSE) file.
