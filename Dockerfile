FROM r-base as system

# we cannot install R in this
# FROM python:3.10-slim-bullseye as system
# LABEL org.opencontainers.image.authors="Agreena Technology Limited <data-eng@agreena.com>"

RUN apt update -y && apt upgrade -y
RUN apt install build-essential dirmngr gnupg apt-transport-https ca-certificates \
       software-properties-common wget gpg python3-pip -y

FROM system as base
# set user without root access
RUN useradd -rm -d /home/appuser -s /bin/bash -g root -G sudo -u 1001 appuser

WORKDIR /home/appuser

# install R
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | gpg --dearmor -o /usr/share/keyrings/r-project.gpg
RUN echo "deb [signed-by=/usr/share/keyrings/r-project.gpg] https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/" | tee -a /etc/apt/sources.list.d/r-project.list
RUN apt update && apt install --no-install-recommends r-base

# install required R packages
# see DESCRIPTION->Imports
# this step takes ~10 minutes
RUN Rscript -e 'install.packages(c(\
"SoilR","stats","maps","jsonlite","nasapower","dplyr",\
"ggplot2","ggpubr","tidyr","ggnewscale","utils","rlang",\
"stringr","digest","jsonlite","httr","writexl","tidyverse","readr","readxl"))'

FROM base as applayer

USER appuser
WORKDIR /home/appuser
COPY requirements.txt .

RUN pip install --no-cache-dir -r requirements.txt
