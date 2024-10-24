set -e  # Exit immediately if a command exits with a non-zero status

echo "Updating package lists..."
sudo apt update

echo "Installing system dependencies..."
sudo apt install -y \
  git-all \
  r-base \
  libssl-dev \
  libcurl4-openssl-dev \
  libfontconfig1-dev \
  libfreetype6-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev \
  libxml2-dev \
  libgit2-dev \
  pkg-config \
  build-essential \
  libyaml-dev \
  libgit2-dev \
  libgit2-28

echo "System dependencies installed successfully."