loadkeys it

iwctl --passphrase "PASSWORD" station wlan0 connect "SSID"

curl -O "https://raw.githubusercontent.com/giovanni-norbedo/dotfiles-2026/main/user_configuration.json"

archinstall --config user_configuration.json

# set up Disk and Authentication

# reboot and shell

curl -O "https://raw.githubusercontent.com/giovanni-norbedo/dotfiles-2026/main/setup.sh"