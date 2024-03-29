
# load modules

cd version45/openmalaria

ml CMake/3.13.3-GCCcore-8.2.0
ml iomkl/2019.01
ml CMake/3.13.3-GCCcore-8.2.0
ml GSL/2.5-iomkl-2019.01
ml XSD/4.0.0-GCCcore-8.2.0
ml Xerces-C++/3.2.2-GCCcore-8.2.0


# copy in new files

cd model/Host/WithinHost
rm WHVivax.cpp
rm WHVivax.h
cp B2j/WHVivax_B2j.cpp WHVivax.cpp
cp B2j/WHVivax.h WHVivax.h
cd ..
cd ..
cd ..


# compile with new files

cd build

cmake -DCMAKE_CXX_COMPILER='icpc' -DCMAKE_C_COMPILER='icc' -DCMAKE_BUILD_TYPE=Release -DXSD_INCLUDE_DIRS=/scicore/soft/apps/XSD/4.0.0-GCCcore-8.2.0/include -DXERCESC_INCLUDE_DIRS=/scicore/soft/apps/Xerces-C++/3.2.2-GCCcore-8.2.0/include -DGSL_INCLUDE_DIR=/scicore/soft/apps/GSL/2.5-iccifort-2019.1.144-GCC-8.2.0-2.31.1/include ..

make


# run scenarios

cp 4p3long_B2/scenarioVivax_png_4p3longDur_B2_eir12_trt0p1.xml scenario.xml
./openMalaria -s scenario.xml
mv output.txt 4p3long_B2/output/output0p1.txt
cp 4p3long_B2/scenarioVivax_png_4p3longDur_B2_eir12_trt0p2.xml scenario.xml
./openMalaria -s scenario.xml
mv output.txt 4p3long_B2/output/output0p2.txt
cp 4p3long_B2/scenarioVivax_png_4p3longDur_B2_eir12_trt0p3.xml scenario.xml
./openMalaria -s scenario.xml
mv output.txt 4p3long_B2/output/output0p3.txt
cp 4p3long_B2/scenarioVivax_png_4p3longDur_B2_eir12_trt0p4.xml scenario.xml
./openMalaria -s scenario.xml
mv output.txt 4p3long_B2/output/output0p4.txt
cp 4p3long_B2/scenarioVivax_png_4p3longDur_B2_eir12_trt0p5.xml scenario.xml
./openMalaria -s scenario.xml
mv output.txt 4p3long_B2/output/output0p5.txt
cp 4p3long_B2/scenarioVivax_png_4p3longDur_B2_eir12_trt0p6.xml scenario.xml
./openMalaria -s scenario.xml
mv output.txt 4p3long_B2/output/output0p6.txt
cp 4p3long_B2/scenarioVivax_png_4p3longDur_B2_eir12_trt0p7.xml scenario.xml
./openMalaria -s scenario.xml
mv output.txt 4p3long_B2/output/output0p7.txt
cp 4p3long_B2/scenarioVivax_png_4p3longDur_B2_eir12_trt0p8.xml scenario.xml
./openMalaria -s scenario.xml
mv output.txt 4p3long_B2/output/output0p8.txt
cp 4p3long_B2/scenarioVivax_png_4p3longDur_B2_eir12_trt0p9.xml scenario.xml
./openMalaria -s scenario.xml
mv output.txt 4p3long_B2/output/output0p9.txt
cp 4p3long_B2/scenarioVivax_png_4p3longDur_B2_eir12_trt0.xml scenario.xml
./openMalaria -s scenario.xml
mv output.txt 4p3long_B2/output/output0.txt










