#!/bin/bash -e
# shellcheck disable=SC2086

NAME=keter
TARGET=${1:-jammy}
ARCH=${2:-amd64}
VER=$(sed -nr '/^Version:/ {s|Version: *||; s|^(([0-9][.])+)([0-9]+)$|\1zoomin\3|; p}' keter.cabal)

BINNAME=${NAME}-${VER}-${TARGET}
DEBNAME=${NAME}_${VER}-${TARGET}_${ARCH}.deb

FPM=$(which fpm)


set -e

die () { test -n "$@" && echo "$@"; exit 1; }

cd "$(dirname "$0")" #-- wherever called from, work under packaging/
echo "CWD in $PWD"

# ensure we have fpm
[[ -n "$FPM" ]] || die "No FPM found! Install from https://rubygems.org/gems/fpm"

#-- compiling is hard enough -- even more so across arch/OS. don't try to automate.
#-- instead, this script asserts expected location of the binary, bailing if absent.

[[ -e "$BINNAME" ]] || die "Compile the binary for $TARGET $ARCH,
  and put into $BINNAME"

rm -rf DEBDIST $DEBNAME

# make folder structure
mkdir -vp DEBDIST/{bin,etc}
mkdir -vp DEBDIST/{incoming,log,paused,temp}

# install files, keeping permissions
cp -vp $BINNAME DEBDIST/bin/
ln -vs $BINNAME DEBDIST/bin/keter
cp -vp app-crash-hook DEBDIST/bin/
cp -vp keter-config.yaml anyHost.html DEBDIST/etc/


# use fpm to generate the debian package.
echo building .deb package
$FPM --name $NAME \
     --version $VER \
     --description "Keter is an appserver for Yesod webapps written in Haskell" \
     --url "https://github.com/zoominsoftware/keter" \
     --maintainer "Max <maksym.ivanov@zoominsoftware.com>" \
     --architecture $ARCH \
     --depends libc6 \
     --depends libgmp10 \
     --depends zlib1g \
     -s dir -t deb \
     --deb-dist $TARGET \
     --deb-upstream-changelog ../ChangeLog.md \
     --deb-user keter-deploy \
     --deb-group keter-deploy \
     --deb-systemd keter.service \
     --deb-systemd-enable \
     --prefix /opt/keter \
     --package $DEBNAME \
     -C DEBDIST .

echo DONE!
