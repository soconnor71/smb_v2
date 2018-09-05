mkdir -p builds
(cd .. && make clean all)
cp -f ../*.raw builds
cp -f ../*.bin builds
cp -f ../*.und builds
cp -f ../*.map builds
cp -f ../*.chr builds
cp -f ../*.inc builds
cp -f ../*.json builds
cp -f ../*.py builds
cp -f ../smbex.nes public
rm -f builds/practice.bin
python genscenarios.py > public/scenarios.html

