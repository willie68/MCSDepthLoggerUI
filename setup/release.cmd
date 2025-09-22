rem download latest releases of osmltools and go_mapproxy
setup\dra download -a -i -o ./tools/ willie68/osmltools
setup\dra download -a -i -o ./tools/ willie68/go_mapproxy

iscc setup/setup.iss

setup\zip -j ./dist/MCSDepthLoggerUISetup.zip ./dist/MCSDepthLoggerUISetup.exe