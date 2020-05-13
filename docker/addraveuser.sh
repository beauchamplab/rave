#!/usr/bin/with-contenv bash

echo $1

cmd=$0
username=$1
password=$2

if [ -z $username ]; then
  echo "ERROR: Cannot find username"
  echo "USAGE: $cmd username password"
  exit 1
fi


if [ -z $password ]; then
  echo "ERROR: Cannot find password or empry password"
  echo "USAGE: $cmd username password"
  exit 1
fi

RUN_R="Rscript --no-save --no-restore --no-site-file --no-init-file"

# Confirm
echo "Username: $username"
echo "Password: $password"
while true; do
    read -p "Confirm adding user? [Yes/y or No/n]: " yn
    case $yn in
        [Yy]* ) add=1; break;;
        [Nn]* ) add=0; break;;
        * ) echo "Please answer yes or no.";;
    esac
done

if [ $add -gt 0 ]; then
  echo "Creating user..."
  # Add user with home dir
  useradd -m $username || exit 1
  
  # Add user to rstudio group
  usermod -a -G rstudio-users $username
  
  # set password
  echo $username:$password | chpasswd
  
  echo "Initializing... (might take a while)"
  # compile RAVE init 
  
  # make dirs
  mkdir -m 777 -p /data/shared/others/three_brain
  mkdir -m 777 -p /data/shared/rave_data/data_dir
  mkdir -m 777 -p /data/shared/rave_data/raw_dir
  chmod -R 777 /data/shared
  
  mkdir -m 777 -p "/home/$username/rave_data/"
  chmod -R 777 "/home/$username/rave_data"
  
  # link to shared dirs
  ln -s /data/shared/others "/home/$username/rave_data/"
  
  # initialize N27
  su -c "/usr/local/bin/extractraveinit" - $username
  # extractraveinit
  chmod -R 777 /data/shared/
  
  # create r profile
  touch "/home/$username/.Rprofile"
  chmod 777 "/home/$username/.Rprofile"
  echo "options(bitmapType=\"cairo\")" > "/home/$username/.Rprofile"
  echo "Sys.setenv('RETICULATE_PYTHON'='/usr/bin/python')" >> "/home/$username/.Rprofile"
  echo "tryCatch(startup::startup(), error=function(ex) message(\".Rprofile error: \", conditionMessage(ex)))" >> "/home/$username/.Rprofile"
  echo "" >> "/home/$username/.Rprofile"
  
else
  echo "Aborted"
  exit 0
fi

echo "Done."
exit 0

