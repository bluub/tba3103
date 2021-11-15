#!/bin/sh -e
#
# This is used for a school project.
# NEVER use this for production!
#
# This script assumes the following:
#   - the OS is compatible with debian (recommended: ubuntu 20.04)
#   - it's being run by root (hence the lack of sudo)

# update & install the required apt packages for R
apt update

apt install -y dirmngr gnupg apt-transport-https ca-certificates software-properties-common

apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'

apt install -y r-base

# install gdal because we need it for leaflet
apt install -y libpq-dev gdal-bin libgdal-dev

# install the R packages need for the script
Rscript setup/install_packages.R

# Copy the cleaning R script to the root folder & set the permissions for it
cp clean_data.R /root/clean_data.R
chmod 755 /root/clean_data.R 

# Copy the shell script to the daily cron job folder that will call the cleaning R script
cp setup/update_eu_ecdc_data.sh /etc/cron.daily/update_eu_ecdc_data
chmod 755 /etc/cron.daily/update_eu_ecdc_data

# Make the directory needed for the cleaned files
mkdir -p /var/www/csv

# Run the script so that we get the data immediately instead of having to wait for a day
Rscript clean_data.R

# Copy our RShiny application scripts to root folder
cp -a rshiny_app/ /root/rshiny_app/

# Copy our service to systemctl folder & set permissions for it
cp setup/rshiny_service /etc/systemd/system/rshiny.service
chmod 700 /etc/systemd/system/rshiny.service

# Update systemctl to make it run on boot & start the service
systemctl enable rshiny
systemctl start rshiny

# Install nginx & copy the sites enabled for it
apt install -y nginx

cp setup/nginx_sites_enabled /etc/nginx/sites-enabled/default

# Restart nginx for it to take effect
systemctl restart nginx


# Setup UFW to only enable SSH & HTTP access
ufw allow 'Nginx HTTP'
ufw allow 'OpenSSH'
ufw --force enable