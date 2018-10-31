- if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    brew update;
    fi
  - if [ "$TRAVIS_OS_NAME" == "linux" ]; then
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install.sh)";
    /home/linuxbrew/.linuxbrew/bin/brew install hdf5;
    fi

#!/bin/bash

if [ "$TRAVIS_OS_NAME" == "osx" ]; then # use homebrew version
  brew update
  brew install hdf5
  echo "brew install finished"
else # install from source
  cd ..
  wget "$HDF5_RELEASE_URL/hdf5-${HDF5_VERSION%.*}/hdf5-$HDF5_VERSION/src/hdf5-$HDF5_VERSION.tar.gz"
  tar -xzf "hdf5-$HDF5_VERSION.tar.gz"
  cd "hdf5-$HDF5_VERSION"
  ./configure --prefix=/usr/local
  sudo make install
  cd ../hdf5r
fi
