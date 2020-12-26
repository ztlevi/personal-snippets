{ pkgs, lib, ... }: {
  emacs_fff = lib.trace "${lib.getLib pkgs.emacs}" "";
}

# with import <nixpkgs> { };
# let textdata = ./foo.txt;
# in runCommand "alldata" { } ''
#   echo "=this is a header=" >> $out
#   cat ${textdata} >> $out
# ''
