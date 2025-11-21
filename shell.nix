{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    
    # SDL2 core and extensions
    SDL2
    SDL2_image
    SDL2_ttf
    SDL2_mixer
    SDL2_net
    SDL2_gfx
    unzip
    
    # Build tools and dependencies
    pkg-config
    gcc
    gnumake
    
    # OpenGL (for 3D rendering if needed)
    libGL
    libGLU
  ];

  # Set environment variables for SDL2
  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
    pkgs.SDL2
    pkgs.SDL2_image
    pkgs.SDL2_ttf
    pkgs.SDL2_mixer
    pkgs.SDL2_net
    pkgs.SDL2_gfx
    pkgs.libGL
    pkgs.libGLU
  ];

  # Helpful for compilation
  PKG_CONFIG_PATH = "${pkgs.SDL2.dev}/lib/pkgconfig:${pkgs.SDL2_image}/lib/pkgconfig:${pkgs.SDL2_ttf}/lib/pkgconfig";
}