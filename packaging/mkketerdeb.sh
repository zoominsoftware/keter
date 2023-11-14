#!/bin/bash
NAME=keter
if test -z "$VER" ; then
    VER=2.0.1-zoomin112
fi
TARGET=jammy2204
ARCH=amd64

BINNAME=${NAME}-${VER}-${TARGET}
DEBNAME=${NAME}_${VER}_${ARCH}.deb

export PATH=$PATH:/var/lib/gems/1.8/bin
FPM=$(which fpm)
STACK=$(which stack)


set -e

# ensure we have fpm, or try to install as necessary.
if [[ -z "$FPM" ]]; then
    sudo apt install ruby-dev gcc rubygems
    sudo gem install fpm
    FPM=$(which fpm)
fi

# Build project
# Make sure either stack is present.
if [[ ! -x "$STACK" ]]; then
    echo "Error: You need stack"
    if [[ ! -f $($STACK exec which $NAME) ]]; then
        echo Building using stack...
        $STACK build
        echo done
    fi
fi

rm -rf $NAME-$VER $DEBNAME

# make folder structure
echo copying files
mkdir -p $NAME-$VER/{bin,etc}
mkdir -p $NAME-$VER/{incoming,log,paused,temp}

# copy the keter binary into /bin
if [[ -f $($STACK exec which $NAME) ]]; then
    cp -p extra-bin/keter-* $NAME-$VER/bin
    cp -p $($STACK exec which $NAME) $NAME-$VER/bin/$BINNAME
    ln -s $BINNAME $NAME-$VER/bin/keter
fi

cp -p app-crash-hook $NAME-$VER/bin/
cp keter-config.yaml anyHost.html $NAME-$VER/etc/

# use fpm to generate the debian package.
echo building deb file
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
     --deb-user keter-deploy \
     --deb-group keter-deploy \
     --deb-systemd keter.service \
     --deb-systemd-enable \
     --prefix /opt/keter \
     -C $NAME-$VER .

echo DONE!
