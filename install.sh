#!/bin/sh

source ./env.zsh
sbcl_url=https://sourceforge.net/projects/sbcl/files/sbcl/1.2.11/sbcl-1.2.11-x86-64-darwin-binary.tar.bz2
quicklisp_url=http://beta.quicklisp.org/quicklisp.lisp

# Install SDL
printf "\e[91mInstall SDL from the MSC\e[0m"
read
# Install sblc
curl -L "${sbcl_url}" | tar xf -
cd sbcl*
./install.sh
cd -
# Install quicklisp
printf "\e[91mWhen asked press ENTER and then 7\e[0m"
read
curl -LO "${sbcl_url}"
sbcl --load quicklisp.lisp <<EOF
(quicklisp-quickstart:install :path
 (merge-pathnames ".sbcl/quicklisp"
  (user-homedir-pathname)))
(ql:add-to-init-file)
(ql:quickload "lispbuilder-sdl")
EOF
rm quicklisp.lisp
cd ~/.sbcl/quicklisp/dists/quicklisp/software/lispbuilder-*/lispbuilder-sdl/cocoahelper
make
cd -
sbcl <<EOF
(ql:quickload "lispbuilder-sdl")
EOF
