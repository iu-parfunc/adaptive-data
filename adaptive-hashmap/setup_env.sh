# source me

# Bring the GHC with Data.Compact into path:

DIR=opt/ghc-mutable-cnf-0.2/bin/

# Lame hack to run in different places:
if [ -d ~parfunc/$DIR ]; then
    NEWGHC=~parfunc/$DIR
elif [ -d ~crest-team/$DIR ]; then
    NEWGHC=~crest-team/$DIR
else
    NEWGHC=$HOME/$DIR
fi

export PATH=$NEWGHC:$PATH
