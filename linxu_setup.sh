hotkeysRC=~/.config/kglobalshortcutsrc

# Remove application launching shortcuts.
sed -i 's/_launch=[^,]*/_launch=none/g' $hotkeysRC

# Remove other global shortcuts.
sed -i 's/^\([^_].*\)=[^,]*/\1=none/g' $hotkeysRC

# Reload hotkeys.
kquitapp5 kglobalaccel && sleep 2s && kglobalaccel5 &

git config --global credential.helper 'store --file ~/.my-credentials'
git config --global user.name "Gary Glover"
git config --global user.email "light.bed5489@fourleafclover.uk"

sudo pacman -S ttf-fira-code

#sudo pacman -S picom

sudo pamac install compton-old-git

sudo pacman -S nyxt

sudo pamac install powershell-bin
sudo ln -T /usr/bin/pwsh /usr/bin/powershell -s

sudo pacman -S terraform
sudo pamac install terraform-ls

sudo pamac install bluetuith
