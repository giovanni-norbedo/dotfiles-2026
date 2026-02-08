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


log "UPDATE"
sudo pacman -Syu --noconfirm


log "SET it KEYBOARD"
sudo loadkeys it
sudo localectl set-keymap it


log "PACKAGES"
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
    htop 

log "INSTALLING AUR HELPER ($AUR_HELPER)"
if ! command -v $AUR_HELPER &> /dev/null; then
    log "Installing $AUR_HELPER..."
    BUILD_DIR=$(mktemp -d)
    git clone "https://aur.archlinux.org/$AUR_HELPER.git" "$BUILD_DIR"
    pushd "$BUILD_DIR"
    makepkg -si --noconfirm
    popd
    rm -rf "$BUILD_DIR"
else
    log "$AUR_HELPER already installed."
fi


log "AUR PACKAGES"
# todo


log "GIT SETUP"
git config --global user.email "norbedo@proton.me"
git config --global user.name "Giovanni Norbedo"


log "CLONE DOTFILES"
if [ ! -d "$DOTFILES_DIR" ]; then
    git clone $REPO_URL $DOTFILES_DIR
else
    log "Dotfiles already cloned. Pulling latest changes..."
    cd $DOTFILES_DIR
    git pull
    cd - > /dev/null
fi


log "LINK CONFIGS"

link_config() {
    local src="$DOTFILES_DIR/$1"
    local dest="$2"
    local dest_dir=$(dirname "$dest")
    local date_str=$(date +%Y%m%d-%H%M%S)

    if [ ! -e "$src" ]; then
        warn "Source file $src does not exist. Skipping $1."
        return 1
    fi

    mkdir -p "$dest_dir"

    if [ -L "$dest" ]; then
        local current_target=$(readlink "$dest")
        if [ "$current_target" == "$src" ]; then
            log "Skipping $dest, already correctly linked."
            return 0
        else
            warn "Incorrect link found at $dest. Removing..."
            rm "$dest"
        fi
    
    elif [ -e "$dest" ]; then
        warn "File found at $dest. Creating backup: $dest.$date_str.bak"
        mv "$dest" "$dest.$date_str.bak"
    fi

    log "Linking $src -> $dest"
    ln -s "$src" "$dest"
}


link_config "xmonad/xmonad.hs"    "$HOME/.config/xmonad/xmonad.hs"

link_config "polybar/config.ini"  "$HOME/.config/polybar/config.ini"
link_config "kitty/kitty.conf"    "$HOME/.config/kitty/kitty.conf"
link_config "rofi/config.rasi"    "$HOME/.config/rofi/config.rasi"
link_config "dunst/dunstrc"       "$HOME/.config/dunst/dunstrc"
link_config "picom/picom.conf"    "$HOME/.config/picom/picom.conf"

link_config "bash/bashrc"         "$HOME/.bashrc"
link_config "xinit/xinitrc"       "$HOME/.xinitrc"


log "XMONAD CONFIG"
xmonad --recompile || warn "Failed to recompile xmonad."


log "ENABLE SERVICES"
sudo systemctl enable --now ly.service
sudo systemctl enable --now NetworkManager
sudo systemctl enable --now bluetooth


log "FINISHED"
echo "Installation complete. Please reboot."


