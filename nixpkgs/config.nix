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

}
