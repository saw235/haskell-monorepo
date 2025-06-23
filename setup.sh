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
    python3 \
    python3-pip \
    pkg-config \
    libgmp-dev \
    libffi-dev \
    zlib1g-dev \
    libncurses5-dev \
    libtinfo-dev \
    && sudo rm -rf /var/lib/apt/lists/*

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

# --- 3. Install Bazelisk ---
echo "Installing Bazelisk globally via npm..."
npm install -g @bazel/bazelisk

# --- 4. Install Google Chrome ---
echo "Installing Google Chrome..."
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
# Use dpkg to install and apt-get -f install to fix any missing dependencies
sudo dpkg -i google-chrome-stable_current_amd64.deb || sudo apt-get -f install -y
rm google-chrome-stable_current_amd64.deb

# --- 5. Install Chromedriver ---
echo "Installing a fixed version of Chromedriver..."
wget https://storage.googleapis.com/chrome-for-testing-public/126.0.6478.126/linux64/chromedriver-linux64.zip
unzip chromedriver-linux64.zip
sudo mv chromedriver-linux64/chromedriver /usr/bin/
rm chromedriver-linux64.zip
rm -rf chromedriver-linux64
sudo chmod +x /usr/bin/chromedriver

# --- 6. Download Selenium Server ---
echo "Downloading Selenium Server Standalone JAR..."
wget https://github.com/SeleniumHQ/selenium/releases/download/selenium-3.9.1/selenium-server-standalone-3.9.1.jar -O ./selenium-server-standalone-3.9.1.jar

