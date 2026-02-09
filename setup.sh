#!/bin/bash


# script post-install for arch (from shell)

REPO_URL="https://github.com/giovanni-norbedo/dotfiles-2026.git"
DOTFILES_DIR="$HOME/dotfiles-2026"
AUR_HELPER="yay"


# end if any error
set -euo pipefail

log() {
    echo -e "\e[34m[SETUP]\e[0m $1"
}

warn() {
    echo -e "\e[33m[WARN]\e[0m $1"
}

# check if running as root
if [ "$EUID" -eq 0 ]; then
    error "Please run as a normal user"
fi


log "# UPDATE"
sudo pacman -Syu --noconfirm


log "# SET it KEYBOARD"
sudo loadkeys it
sudo localectl set-keymap it


log "# PACKAGES"
sudo pacman -S --noconfirm --needed \
    git \
    base-devel \
    xorg-server \
    xorg-xinit \
    xmonad \
    xmonad-contrib \
    polybar \
    kitty \
    rofi \
    feh \
    dunst \
    picom \
    thunar \
    gvfs \
    firefox \
    unzip \
    openssh \
    man-db \
    ttf-jetbrains-mono-nerd \
    ttf-font-awesome \
    ttf-fantasque-nerd \
    networkmanager \
    bluez \
    bluez-utils \
    ly \
    pipewire \
    pipewire-pulse \
    pipewire-alsa \
    wireplumber \
    pavucontrol \
    python \
    python-pip \
    xclip \
    brightnessctl \
    playerctl \
    acpi \
    htop \
    zsh \
    zsh-autosuggestions \
    zsh-syntax-highlighting \
    starship \
    yazi \
    bat \
    eza \
    fzf \
    zoxide

log "# INSTALLING AUR HELPER ($AUR_HELPER)"
if ! command -v $AUR_HELPER &> /dev/null; then
    log "# Installing $AUR_HELPER..."
    BUILD_DIR=$(mktemp -d)
    git clone "https://aur.archlinux.org/$AUR_HELPER.git" "$BUILD_DIR"
    pushd "$BUILD_DIR"
    makepkg -si --noconfirm
    popd
    rm -rf "$BUILD_DIR"
else
    log "# $AUR_HELPER already installed."
fi


log "# AUR PACKAGES"
yay -S --noconfirm --needed \
    vscodium-bin \
    tree


log "# GIT SETUP"
git config --global user.email "norbedo@proton.me"
git config --global user.name "Giovanni Norbedo"


log "# CLONE DOTFILES"
if [ ! -d "$DOTFILES_DIR" ]; then
    git clone $REPO_URL $DOTFILES_DIR
else
    log "# Dotfiles already cloned. Pulling latest changes..."
    cd $DOTFILES_DIR
    git pull
    cd - > /dev/null
fi


log "# LINK CONFIGS"

link_config() {
    local src="$DOTFILES_DIR/$1"
    local dest="$2"
    local dest_dir=$(dirname "$dest")
    local date_str=$(date +%Y%m%d-%H%M%S)

    if [ ! -e "$src" ]; then
        warn "Source $src does not exist. Skipping..."
        return 1
    fi

    mkdir -p "$dest_dir"

    if [ -L "$dest" ]; then
        local current_target=$(readlink "$dest")
        if [ "$current_target" == "$src" ]; then
            log "# Skipping $dest, already correctly linked."
            return 0
        else
            warn "Incorrect link at $dest. Removing..."
            rm "$dest"
        fi
    
    elif [ -e "$dest" ]; then
        warn "Existing config found at $dest. Backup: $dest.$date_str.bak"
        mv "$dest" "$dest.$date_str.bak"
    fi

    log "# Linking $src -> $dest"
    ln -s "$src" "$dest"
}

log "# .CONFIG DIR"

mkdir -p "$HOME/.config"

for config_dir in "$DOTFILES_DIR/config"/*; do
    name=$(basename "$config_dir")
    
    if [ "$name" == "." ] || [ "$name" == ".." ]; then continue; fi

    link_config "config/$name" "$HOME/.config/$name"
done


if [ -f "$DOTFILES_DIR/polybar/launch.sh" ]; then
    chmod +x "$DOTFILES_DIR/polybar/launch.sh"
fi

# link_config "bash/bashrc"         "$HOME/.bashrc"
# link_config "xinit/xinitrc"       "$HOME/.xinitrc"
# .zshrc, .profile

link_config "zshrc" "$HOME/.zshrc"
link_config "nanorc" "$HOME/.nanorc"


log "# XMONAD CONFIG"
xmonad --recompile || warn "Failed to recompile xmonad."


log "# FINISHED"
echo "Installation complete. Please reboot."
