pamac update --force-refresh --no-confirm --aur

sudo pacman -Ry libva-vdpau-driver

pamac install --no-confirm \
      linux-headers \
      ttf-fira-code \
      compton-old-git \
      nyxt \
      powershell-bin \
      terraform terraform-ls \
      bluetuith \
      pacmixer \
      slock xss-lock \
      arandr \
      unzip \
      libva-nvidia-driver \
      obs-studio v4l2loopback-dkms v4l2loopback-utils luajit obs-streamfx-unstable \
      obs-source-clone

git clone https://github.com/royshil/obs-backgroundremoval.git ~/obs
cd ~/obs/scripts
makepkg -s
