
FROM rocker/rstudio-stable:devel




COPY addraveuser.sh /usr/local/bin/addraveuser
COPY extractraveinit.R /usr/local/bin/extractraveinit
COPY install_rpkg_alt.R /usr/local/bin/install_rpkg_alt


RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libxml2-dev \
    zlib1g-dev \
    libgit2-dev \
    libfftw3-dev \
    libv8-dev \
    libhdf5-dev \
  && groupadd rstudio-users \
  && usermod -a -G rstudio-users rstudio \
  && cp /etc/pam.d/login /etc/pam.d/rstudio \
  && chmod 777 /usr/local/bin/addraveuser \
  && chmod 777 /usr/local/bin/extractraveinit \
  && chmod 777 /usr/local/bin/install_rpkg_alt \
  && mkdir -p /data/init \
  && chmod -R 777 /data \
  && mkdir -m 777 -p /usr/lib/R/library \
  && mkdir -m 777 -p /usr/local/lib/R/site-library \
  && chmod 777 /usr/lib/R/library \
  && chmod 777 /usr/local/lib/R/site-library
    

# Install RAVE

RUN /usr/local/bin/install2.r dipsaus threeBrain \
  && /usr/local/bin/install_rpkg_alt Rcpp dipsaus rutabaga threeBrain rave ravebuiltins \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
	&& rm -rf /var/lib/apt/lists/* \
	&& wget -O /data/init/N27.zip https://github.com/dipterix/threeBrain-sample/releases/download/1.0.0/N27.zip


# RUN wget -O /data/init/YAB.zip https://github.com/beauchamplab/rave/releases/download/v0.1.8-beta/demo_YAB.zip


CMD ["/init"]



## build docker
# docker build --tag rave:latest .
# docker run --publish 8787:8787 --detach --name rave -e ROOT=TRUE -e PASSWORD=password rave:20200512

# docker tag rave:latest dipterix/rave:20200512
# docker push dipterix/rave:20200512
# docker tag dipterix/rave:latest dipterix/rave:latest
# docker push dipterix/rave:latest
# docker run -p 8787:8787 -d --name rave --mount type=bind,source="/Volumes/OneDrive/data",target=/data/ext -e ROOT=TRUE -e PASSWORD=password dipterix/rave:latest
# docker exec -ti rave addraveuser beauchamplab ...
