#!/bin/sh
cd ../sandy
# Modify the RECONR options used by SANDY, to make them consistent with
# PyNjoy2016's RECONR options.
sed -i 's/\[\"{} 0. \/\".format(err)]/["0.001  0.  0.005\/"]/' sandy/njoy.py
# Compile
python3 setup.py install --user
