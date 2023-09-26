FROM r-base as system

# we cannot install R in this
# FROM python:3.10-slim-bullseye as system
# LABEL org.opencontainers.image.authors="Agreena Technology Limited <data-eng@agreena.com>"

RUN apt update -y && apt upgrade -y
RUN apt install build-essential dirmngr gnupg apt-transport-https ca-certificates \
       software-properties-common wget gpg python3-pip python3-venv python3-dev -y

FROM system as base

RUN useradd -m -s /bin/bash appuser
RUN usermod -aG sudo appuser
# set user without root access
# RUN useradd -rm -d /home/appuser -s /bin/bash -g root -G sudo -u 1001 -m appuser && chown appuser /home/appuser

WORKDIR /home/appuser

# install R
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | gpg --dearmor -o /usr/share/keyrings/r-project.gpg
RUN echo "deb [signed-by=/usr/share/keyrings/r-project.gpg] https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/" | tee -a /etc/apt/sources.list.d/r-project.list
RUN apt update && apt install --no-install-recommends r-base

# install required R packages
# see DESCRIPTION->Imports
# this step takes ~10 minutes
# NOTE: use `renv`
RUN Rscript -e 'install.packages(c(\
"SoilR","stats","maps","jsonlite","nasapower","dplyr",\
"ggplot2","ggpubr","tidyr","ggnewscale","utils","rlang",\
"stringr","digest","jsonlite","httr","writexl","tidyverse","readr","readxl"))'

FROM base as applayer

RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
COPY requirements.txt .
RUN /opt/venv/bin/python3 -m pip install --upgrade pip setuptools
RUN /opt/venv/bin/python3 -m pip install --no-cache-dir -r requirements.txt

FROM applayer as runtime
# USER appuser
WORKDIR /home/appuser
COPY . .
