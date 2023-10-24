# Practical_and_Accurate
A clone of ZHAireS modified to allow for the Practical and Accurate approach to coherent radio calculations

# Methodology
The Practical and Accurate (P&A) approach to calculating radio emission from air showers involves the summation of particle tracks within 4-D volumes in spacetime with sides that adhere to the Fraunhofer limit. A basic description of the physics involved is detailed in the attached proceedings from ICRC2023.

The bulk of the methodology is found in /src/aires/Cpp_GridMaker_2.cpp. Tracks are generated in src/aires/eplusmins.f and passed to the C++ script using the "update_vector" function (using iso c binding), where they are histogrammed into non-uniform 4D bins in spacetime. In src/aires/Aires.f, after all the generated tracks have been passed, the function "calculate_radio" is called, which calculates the electric fields from the summed currents. The final text file containing the calculated electric fields is saved in the directory where the shower is generated and can be compared directly to the "timefresnel-root.dat" file if radio calculations are toggled on during runtime.

# Prerequisites:
* For running ZHAireS: I don't think any? If I'm wrong on this, update this section!  
* For plotting: numpy, matplotlib, scipy  
* For easy steering (recommended): [Remy Prechelt's zhaires.py python wrapper for ZHAireS](https://github.com/rprechelt/zhaires.py)

# Building P&A ZHAireS:
1) Compile the src/aires/grid.f90 linking script: "gfortran -c grid.f90"
2) Compile the src/aires/Cpp_gridmaker_2.cpp P&A C++ code: "g++ -c Cpp_GridMaker_2.cpp"
3) Perform the ZHAireS install: "./doinstall 0" or follow the ZHAireS "Install.HowTo" document

# How to use P&A ZHAireS:
Exactly the same as ZHAireS (for now). Either use a steering file or the zhaires.py python wrapper. An example using the latter is given in "example_runfile.py"
