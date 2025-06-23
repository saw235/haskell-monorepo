#!/bin/bash
# This script sets up the development environment based on the Dockerfile.
# It installs system dependencies, Node.js via nvm, Chrome, Chromedriver, and project dependencies.

# Exit immediately if a command exits with a non-zero status.
set -e

echo "--- Starting Environment Setup ---"

# --- 1. Install System Dependencies ---
echo "Updating package lists and installing system dependencies..."
sudo apt-get update
sudo apt-get install -y \
    curl \
    wget \
    unzip \
    gnupg \
    jq \
    default-jre \
    pkg-config \
    libgmp-dev \
    libffi-dev \
    zlib1g-dev \
    libncurses5-dev \
    libtinfo-dev \
    build-essential \
    libssl-dev \
    libbz2-dev \
    libreadline-dev \
    libsqlite3-dev \
    libncursesw5-dev \
    xz-utils \
    tk-dev \
    liblzma-dev \
    python3-openssl \
    git \
    && sudo rm -rf /var/lib/apt/lists/*

# --- 1a. Fix libtinfo5 for Bazel ---
echo "Installing libtinfo5 for Bazel..."
wget -q http://security.ubuntu.com/ubuntu/pool/universe/n/ncurses/libtinfo5_6.3-2ubuntu0.1_amd64.deb
sudo dpkg -i libtinfo5_6.3-2ubuntu0.1_amd64.deb || sudo apt-get -f install -y
rm libtinfo5_6.3-2ubuntu0.1_amd64.deb

# --- 2. Install Node.js using nvm ---
# The Dockerfile used the node:22-slim base image. This section replicates that.
echo "Installing nvm and Node.js v22..."
export NVM_DIR="$HOME/.nvm"
# Check if nvm is already installed to avoid re-installing
if [ ! -s "$NVM_DIR/nvm.sh" ]; then
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash
fi
# Source nvm to make it available in the current script session
. "$NVM_DIR/nvm.sh"
nvm install 22
nvm use 22
nvm alias default 22

# --- 2a. Install Python using pyenv ---
echo "Installing pyenv and Python 3.12.3..."
if [ ! -d "$HOME/.pyenv" ]; then
  curl https://pyenv.run | bash
fi
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
pyenv install --skip-existing 3.12.3
pyenv global 3.12.3

# --- 3. Install Bazelisk ---
echo "Installing Bazelisk globally via npm..."
npm install -g @bazel/bazelisk

# --- 3.5. Install Chrome dependencies ---
echo "Installing Chrome dependencies..."
sudo apt-get update
sudo apt-get install -y fonts-liberation xdg-utils

# --- 4. Install Google Chrome ---
echo "Installing Google Chrome..."
wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
# Use dpkg to install and apt-get -f install to fix any missing dependencies
sudo dpkg -i google-chrome-stable_current_amd64.deb || sudo apt-get -f install -y
rm google-chrome-stable_current_amd64.deb

# --- 5. Install Chromedriver ---
echo "Installing a fixed version of Chromedriver..."
wget -q https://storage.googleapis.com/chrome-for-testing-public/126.0.6478.126/linux64/chromedriver-linux64.zip
unzip chromedriver-linux64.zip
sudo mv chromedriver-linux64/chromedriver /usr/bin/
rm chromedriver-linux64.zip
rm -rf chromedriver-linux64
sudo chmod +x /usr/bin/chromedriver

# --- 6. Download Selenium Server ---
echo "Downloading Selenium Server Standalone JAR..."
wget -q https://github.com/SeleniumHQ/selenium/releases/download/selenium-3.9.1/selenium-server-standalone-3.9.1.jar -O ./selenium-server-standalone-3.9.1.jar

# move selenium-server-standalone-3.9.1.jar to /usr/bin/selenium-server-standalone.jar
sudo mv selenium-server-standalone-3.9.1.jar /usr/bin/selenium-server-standalone.jar

# --- Install libtinfo5 for Bazel ---
wget -q http://security.ubuntu.com/ubuntu/pool/universe/n/ncurses/libtinfo5_6.3-2ubuntu0.1_amd64.deb
sudo apt install -y ./libtinfo5_6.3-2ubuntu0.1_amd64.deb