{

  allowUnfree = true;

  # https://nixos.org/wiki/Enable_Browser_Plugins

  firefox = {
    enableGoogleTalkPlugin = true;
    enableAdobeFlash = false;
  };

  chromium = {
    enablePepperFlash = true;
    enablePepperPDF = true;
  };

  # https://nixos.org/nixpkgs/manual/#sec-modify-via-packageOverrides

  packageOverrides = pkgs: rec {
    gnupg = pkgs.gnupg.override {
      inherit pinentry;
      x11Support = true;
    };

    pinentry = pkgs.pinentry.override {
      ncurses = pkgs.ncurses;
      gtk2 = pkgs.gtk2;
    };
  };

}
